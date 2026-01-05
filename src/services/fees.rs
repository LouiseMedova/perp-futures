use primitive_types::U256;

use crate::state::{Claimables, PoolBalances, Position};
use crate::types::{AssetId, MarketId, OraclePrices, Order, OrderType, TokenAmount, Usd};

/// Per-step trading fees for a single position change.
#[derive(Debug, Clone)]
pub struct StepFees {
    pub position_fee_usd: Usd,
    pub position_fee_tokens: TokenAmount,
    pub liquidation_fee_usd: Usd,
    pub liquidation_fee_tokens: TokenAmount,
    pub market_id: MarketId,
    pub fee_asset: AssetId,
}

fn div_ceil(n: U256, d: U256) -> Result<U256, String> {
    if d.is_zero() {
        return Err("division_by_zero".into());
    }
    let q = n / d;
    let r = n % d;
    Ok(if r.is_zero() { q } else { q + U256::one() })
}

/// High-level interface for fee calculation and distribution.
///
/// The same interface is used for:
///  - Increase order
///  - Decrease / close order
///  - Liquidation
pub trait FeesService {
    /// Compute position + liquidation fees for a single step.
    ///
    /// `balance_was_improved` comes from pricing (price impact service) and
    /// indicates whether this trade reduced OI imbalance.
    fn compute_fees(
        &self,
        pos: &Position,
        order: &Order,
        prices: &OraclePrices,
        balance_was_improved: bool,
        size_delta_usd: Usd,
    ) -> Result<StepFees, String>;

    fn apply_fees(
        &self,
        pools: &mut PoolBalances,
        claimables: &mut Claimables,
        step_fees: &StepFees,
    );
}

#[derive(Debug, Clone, Default)]
pub struct BasicFeesService {
    /// Trading fee in basis points (e.g. 10 = 0.1%, 30 = 0.3%)
    pub position_fee_bps_increase: u32,
    pub position_fee_bps_decrease: u32,
    pub liquidation_fee_bps: u32,

    /// % discount on position fee (not in bps, just integer percent) if
    /// the trade improves OI balance.
    pub helpful_rebate_percent: u32,
}

impl BasicFeesService {
    pub fn new(
        increase_bps: u32,
        decrease_bps: u32,
        liquidation_bps: u32,
        helpful_rebate_percent: u32,
    ) -> Self {
        Self {
            position_fee_bps_increase: increase_bps,
            position_fee_bps_decrease: decrease_bps,
            liquidation_fee_bps: liquidation_bps,
            helpful_rebate_percent,
        }
    }

    fn base_position_fee_bps(&self, order_type: OrderType) -> u32 {
        match order_type {
            OrderType::Increase => self.position_fee_bps_increase,
            OrderType::Decrease => self.position_fee_bps_decrease,
            OrderType::Liquidation => 0,
        }
    }
}

impl FeesService for BasicFeesService {
    fn compute_fees(
        &self,
        pos: &Position,
        order: &Order,
        prices: &OraclePrices,
        balance_was_improved: bool,
        size_delta_usd: Usd,
    ) -> Result<StepFees, String> {
        let notional_usd = size_delta_usd;

        // 1) Position fee bps with optional rebate for helpful trades.
        let mut pos_bps = self.base_position_fee_bps(order.order_type);
        if balance_was_improved && pos_bps > 0 && self.helpful_rebate_percent > 0 {
            // effective_bps = pos_bps * (100 - rebate%) / 100
            pos_bps = pos_bps.saturating_mul(100 - self.helpful_rebate_percent) / 100;
        }

        // position_fee_usd = notional_usd * pos_bps / 10_000
        let position_fee_usd = notional_usd
            .checked_mul(U256::from(pos_bps))
            .ok_or("position_fee_mul_overflow")?
            / U256::from(10_000u64);
        // 2) Liquidation fee only for liquidation orders.
        let liquidation_fee_usd: Usd = if order.order_type == OrderType::Liquidation {
            notional_usd
                .checked_mul(U256::from(self.liquidation_fee_bps as u64))
                .ok_or("liquidation_fee_mul_overflow")?
                / U256::from(10_000u64)
        } else {
            U256::zero()
        };
        // 3) Convert USD → collateral tokens.
        let p = prices.collateral_price_min;
        let position_fee_tokens = div_ceil(position_fee_usd, p)?;
        let liquidation_fee_tokens = div_ceil(liquidation_fee_usd, p)?;

        println!("position_fee_usd {:?}", position_fee_usd);
        println!("position_fee_tokens {:?}", position_fee_tokens);
        Ok(StepFees {
            position_fee_usd,
            position_fee_tokens,
            liquidation_fee_usd,
            liquidation_fee_tokens,
            market_id: pos.key.market_id,
            fee_asset: pos.key.collateral_token,
        })
    }

    fn apply_fees(
        &self,
        pools: &mut PoolBalances,
        _claimables: &mut Claimables,
        step_fees: &StepFees,
    ) {
        // All position + liquidation fees go to the pool for now.
        let total_fee_tokens = step_fees.position_fee_tokens + step_fees.liquidation_fee_tokens;

        if total_fee_tokens.is_zero() {
            return;
        }

        pools.add_fee_to_pool(step_fees.market_id, step_fees.fee_asset, total_fee_tokens);
    }
}
