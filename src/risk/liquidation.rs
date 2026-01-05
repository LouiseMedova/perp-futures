use primitive_types::{U256};

use crate::risk::RiskCfg;
use crate::state::{MarketState, Position};
use crate::types::{OraclePrices, Side, SignedU256, Timestamp, Usd};
use crate::math::rounding::{Rounding, div_round};
use crate::math;
use crate::math::pnl;
use crate::services::{funding, borrowing};

/// Fee config for liquidation preview.
#[derive(Clone, Copy, Debug)]
pub struct LiquidationFeeCfg {
    /// Position/close fee (bps). E.g. 10 = 0.10%.
    pub close_position_fee_bps: u32,
    /// Additional liquidation fee (bps). E.g. 50 = 0.50%.
    pub liquidation_fee_bps: u32,
}

#[derive(Clone, Debug)]
pub struct LiquidationPreview {
    pub collateral_value_usd: U256,
    pub pnl_usd: SignedU256,
    pub price_impact_usd: SignedU256, // included as negative-only in equity
    pub borrowing_fee_usd: U256,
    pub funding_fee_usd: SignedU256, // preview delta; included as positive-only cost
    pub close_fees_usd: U256,         // position + liquidation fees (USD)
    pub equity_usd: SignedU256,       // final equity (signed)
    pub required_usd: U256,
    pub is_liquidatable: bool,
}

/// collateral_value_usd = collateral_atoms * collateral_price_min (USD per atom)
fn collateral_value_usd(pos: &Position, prices: &OraclePrices) -> Result<U256, String> {
    if prices.collateral_price_min.is_zero() {
        return Err("invalid_collateral_price_min".into());
    }
    pos.collateral_amount
        .checked_mul(prices.collateral_price_min)
        .ok_or("collateral_value_overflow".into())
}


/// required_usd = max(min_collateral_usd, size_usd * min_collateral_factor_fp / factor_scale)
pub fn required_collateral_usd(pos: &Position, risk: RiskCfg) -> Result<U256, String> {
    if risk.factor_scale.is_zero() {
        return Err("invalid_factor_scale".into());
    }
    let required_by_leverage = pos
        .size_usd
        .checked_mul(risk.min_collateral_factor_fp)
        .ok_or("required_by_leverage_mul_overflow")?
        / risk.factor_scale;

    Ok(required_by_leverage.max(risk.min_collateral_usd))
}


/// close_fees_usd = size_usd * (close_fee_bps + liq_fee_bps) / 10_000
fn close_fees_usd(size_usd: U256, fee_cfg: LiquidationFeeCfg) -> U256 {
    let total_bps: U256 = U256::from(fee_cfg.close_position_fee_bps)
        .saturating_add(U256::from(fee_cfg.liquidation_fee_bps));
    size_usd.saturating_mul(total_bps) / U256::from(10_000u64)
}

/// For liquidation we typically do NOT allow “helpful” bonuses to save margin.
/// So: include only negative impact (cost), ignore positive.
fn negative_only(s: SignedU256) -> SignedU256 {
    if s.is_negative {
        s
    } else {
        SignedU256::zero()
    }
}

/// For liquidation we typically do NOT allow funding rewards to save margin.
/// So: include only positive funding cost, ignore rewards.
fn funding_cost_only(f: SignedU256) -> U256 {
    if f.is_negative {
        U256::zero()
    } else {
        f.mag
    }
}

/// Main predicate:
/// - computes equity at conservative oracle mark (your pnl::total_position_pnl_usd already uses min/max)
/// - subtracts preview borrowing/funding costs
/// - subtracts close fees
/// - includes negative-only price impact (if provided)
pub fn is_liquidatable_by_margin(
    market: &MarketState,
    pos: &Position,
    prices: &OraclePrices,
    now: Timestamp,
    risk: RiskCfg,
    fee_cfg: LiquidationFeeCfg,
    price_impact_usd_on_close: SignedU256,
) -> Result<LiquidationPreview, String> {
    if pos.size_usd.is_zero() || pos.size_tokens.is_zero() {
        return Err("position_empty".into());
    }
    let collateral_usd = collateral_value_usd(pos, prices)?;
    let required = required_collateral_usd(pos, risk)?;

    let borrowing_fee = borrowing::preview_borrowing_fee_usd(market, pos, now)?;
    let funding_fee = funding::preview_funding_fee_usd(market, pos, now)?;

    let close_fees = close_fees_usd(pos.size_usd, fee_cfg);
    
    // PnL at conservative mark (min for long, max for short).
    let pnl_usd = pnl::total_position_pnl_usd(pos, prices)?;

    // Negative-only close price impact.
    let impact_usd = negative_only(price_impact_usd_on_close);

    // Equity:
    // equity = collateral + pnl + impact - borrowing - close_fees - funding_cost
    let mut equity = SignedU256::pos(collateral_usd);
    equity = math::signed_add(equity, pnl_usd);
    equity = math::signed_add(equity, impact_usd);

    equity = math::signed_sub(equity, SignedU256::pos(borrowing_fee));
    equity = math::signed_sub(equity, SignedU256::pos(close_fees));

    let funding_cost = funding_cost_only(funding_fee);
    if !funding_cost.is_zero() {
        equity = math::signed_sub(equity, SignedU256::pos(funding_cost));
    }

    let is_liq = if equity.is_negative {
        true
    } else {
        equity.mag < required
    };
    Ok(LiquidationPreview {
        collateral_value_usd: collateral_usd,
        pnl_usd,
        price_impact_usd: impact_usd,
        borrowing_fee_usd: borrowing_fee,
        funding_fee_usd: funding_fee,
        close_fees_usd: close_fees,
        equity_usd: equity,
        required_usd: required,
        is_liquidatable: is_liq,
    })
}

/// Calculate liquidation price (USD(1e30) per 1 atom of index token).
///
/// IMPORTANT (MVP/conservative):
/// - uses costs preview at `now`
/// - ignores positive funding rewards
/// - ignores positive price impact
/// - uses only close fees (position + liquidation)
///
/// Derivation:
/// Let:
///   C = collateral_value_usd
///   R = required_usd
///   K = borrowing_fee + close_fees + funding_cost + negative_price_impact_cost
/// For Long:
///   equity = C + (T*P - entry) - K
///   liquidate when equity < R
///   boundary: C + T*P - entry - K = R
///   => T*P = entry + R + K - C
///   => P = (entry + R + K - C) / T   (round UP for long)
///
/// For Short:
///   pnl = entry - T*P
///   boundary: C + entry - T*P - K = R
///   => T*P = entry + C - K - R
///   => P = (entry + C - K - R) / T  (round DOWN for short)
pub fn calculate_liquidation_price(
    market: &MarketState,
    pos: &Position,
    prices: &OraclePrices,
    now: Timestamp,
    risk: RiskCfg,
    fee_cfg: LiquidationFeeCfg,
    price_impact_usd_on_close: SignedU256,
) -> Result<U256, String> {
    if pos.size_usd.is_zero() || pos.size_tokens.is_zero() {
        return Err("position_empty".into());
    }
    let c = collateral_value_usd(pos, prices)?;
    let r = required_collateral_usd(pos, risk)?;

    let borrowing_fee = borrowing::preview_borrowing_fee_usd(market, pos, now)?;
    let funding_fee = funding::preview_funding_fee_usd(market, pos, now)?;
    let funding_cost = funding_cost_only(funding_fee);

    let close_fees = close_fees_usd(pos.size_usd, fee_cfg);
    // negative-only impact cost in USD
    let impact_cost = if price_impact_usd_on_close.is_negative {
        price_impact_usd_on_close.mag
    } else {
        U256::zero()
    };

    let k = borrowing_fee
        .saturating_add(funding_cost)
        .saturating_add(close_fees)
        .saturating_add(impact_cost);
    let entry = pos.size_usd;
    let t = pos.size_tokens;

    let price = match pos.key.side {
        Side::Long => {
            // numer = entry + R + K - C
            let mut numer = entry
                .checked_add(r).ok_or("liq_price_overflow")?
                .checked_add(k).ok_or("liq_price_overflow")?;

            if numer <= c {
                U256::zero()
            } else {
                numer = numer - c;
                // round UP for long (liquidate earlier)
                div_round(numer, t, Rounding::Up)?
            }
        }
        Side::Short => {
            // numer = entry + C - K - R
            // if <=0 => 0
            let mut numer = entry.checked_add(c).ok_or("liq_price_overflow")?;
            if numer <= k.saturating_add(r) {
                U256::zero()
            } else {
                numer = numer - k - r;
                // round DOWN for short (liquidate earlier)
                div_round(numer, t, Rounding::Down)?
            }
        }
    };

    Ok(price)
}


#[cfg(test)]
mod tests {
    use super::*;
    use primitive_types::U256;
    use crate::types::{OraclePrices, SignedU256};
    use crate::state::{MarketState, Position, PositionKey};
    use crate::types::{AccountId, AssetId, MarketId, Side};

    fn usd(x: u64) -> U256 { U256::from(x) * U256::exp10(30) }

    fn base_pos(side: Side) -> Position {
        Position {
            key: PositionKey {
                account: AccountId([1u8; 32]),
                market_id: MarketId(1),
                collateral_token: AssetId(10),
                side,
            },
            size_usd: usd(200),          // entry notional $200
            size_tokens: U256::from(2),  // 2 atoms/tokens of index
            collateral_amount: U256::from(50), // 50 collateral tokens/atoms
            pending_impact_tokens: SignedU256::zero(),
            funding_index: SignedU256::zero(),
            borrowing_index: U256::zero(),
            opened_at: 1,
            last_updated_at: 1,
        }
    }

    fn base_market() -> MarketState {
        let mut m = MarketState::default();
        m.id = MarketId(1);
        m.oi_long_usd = usd(120_000);
        m.oi_short_usd = usd(80_000);
        m.liquidity_usd = usd(1_000_000);

        // indices already “at now” to make previews 0
        m.funding.last_updated_at = 100;
        m.borrowing.last_updated_at = 100;
        m
    }

    #[test]
    fn liquidation_price_long_basic() {
        // Long: entry=$200, collateral=$50, required=$20 => price_liq = (200+20-50)/2=85
        let market = base_market();
        let pos = base_pos(Side::Long);

        // collateral_price_min = $1 per atom, so collateral_value = 50*$1 = $50
        let prices = OraclePrices {
            index_price_min: usd(90),
            index_price_max: usd(90),
            collateral_price_min: usd(1),
            collateral_price_max: usd(1),
        };

        let mut risk = RiskCfg::default();
        // Make leverage requirement = 10% (max lev 10x) for deterministic test
        risk.factor_scale = U256::exp10(18);
        risk.min_collateral_factor_fp = risk.factor_scale / U256::from(10u64);
        risk.min_collateral_usd = usd(5);

        let fee_cfg = LiquidationFeeCfg { close_position_fee_bps: 0, liquidation_fee_bps: 0 };

        let p = calculate_liquidation_price(
            &market,
            &pos,
            &prices,
            100,
            risk,
            fee_cfg,
            SignedU256::zero(),
        ).unwrap();

        assert_eq!(p, usd(85));
    }

    #[test]
    fn is_liquidatable_true_when_equity_below_required() {
        let market = base_market();
        let pos = base_pos(Side::Long);

        // price_min=$70 => value=2*70=140, pnl=140-200=-60
        // equity = collateral(50) + pnl(-60) = -10 => liquidatable
        let prices = OraclePrices {
            index_price_min: usd(70),
            index_price_max: usd(70),
            collateral_price_min: usd(1),
            collateral_price_max: usd(1),
        };

        let mut risk = RiskCfg::default();
        risk.factor_scale = U256::exp10(18);
        risk.min_collateral_factor_fp = risk.factor_scale / U256::from(10u64); // 10%
        risk.min_collateral_usd = usd(5);

        let fee_cfg = LiquidationFeeCfg { close_position_fee_bps: 0, liquidation_fee_bps: 0 };

        let prev = is_liquidatable_by_margin(
            &market,
            &pos,
            &prices,
            100,
            risk,
            fee_cfg,
            SignedU256::zero(),
        ).unwrap();

        assert!(prev.is_liquidatable);
        assert!(prev.equity_usd.is_negative);
    }

    #[test]
    fn is_liquidatable_false_when_safe() {
        let market = base_market();
        let pos = base_pos(Side::Long);

        // price_min=$90 => value=180, pnl=-20; equity=50-20=30, required=20 => safe
        let prices = OraclePrices {
            index_price_min: usd(90),
            index_price_max: usd(90),
            collateral_price_min: usd(1),
            collateral_price_max: usd(1),
        };

        let mut risk = RiskCfg::default();
        risk.factor_scale = U256::exp10(18);
        risk.min_collateral_factor_fp = risk.factor_scale / U256::from(10u64);
        risk.min_collateral_usd = usd(5);

        let fee_cfg = LiquidationFeeCfg { close_position_fee_bps: 0, liquidation_fee_bps: 0 };

        let prev = is_liquidatable_by_margin(
            &market,
            &pos,
            &prices,
            100,
            risk,
            fee_cfg,
            SignedU256::zero(),
        ).unwrap();

        assert!(!prev.is_liquidatable);
        assert!(!prev.equity_usd.is_negative);
        assert!(prev.equity_usd.mag >= prev.required_usd);
    }
}
