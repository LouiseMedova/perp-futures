use primitive_types::U256;

use crate::math;
use crate::oracle::Oracle;
use crate::risk;
use crate::services::borrowing::apply_borrowing_fees_to_pool;
use crate::services::price_impact::ImpactRebalanceConfig;
use crate::services::pricing::ExecutionPriceParams;
use crate::services::step_costs::{apply_step_costs_to_position, compute_step_costs};
use crate::services::*;
use crate::state::{
    Claimables, MarketState, PoolBalances, Position, PositionKey, PositionStore, State,
};
use crate::types::{
    AssetId, OraclePrices, Order, OrderId, OrderType, Side, SignedU256, Timestamp, TokenAmount, Usd,
};
pub struct Executor<S: ServicesBundle, O: Oracle> {
    pub state: State,
    pub services: S,
    pub oracle: O,
}

impl<S: ServicesBundle, O: Oracle> Executor<S, O> {
    pub fn new(state: State, services: S, oracle: O) -> Self {
        Self {
            state,
            services,
            oracle,
        }
    }

    pub fn submit_order(&mut self, order: Order) -> OrderId {
        self.state.orders.create(order)
    }

    pub fn execute_order(&mut self, now: Timestamp, order_id: OrderId) -> Result<(), String> {
        let mut order = match self.state.orders.get(order_id) {
            Some(o) => o.clone(),
            None => return Err("order_not_found".into()),
        };

        if now < order.valid_from {
            return Err("order_not_active_yet".into());
        }
        if now > order.valid_until {
            self.state.orders.remove(order_id);
            return Err("order_expired".into());
        }

        let State {
            positions,
            markets,
            pool_balances,
            claimables,
            orders,
        } = &mut self.state;

        let market: &mut MarketState = markets.entry(order.market_id).or_insert_with(|| {
            let mut m = MarketState::default();
            m.id = order.market_id;
            m
        });

        let prices = self.oracle.validate_and_get_prices(order.market_id)?;
        // Sync market-level time-based indices
        self.services.funding().update_indices(market, now);
        self.services.borrowing().update_index(market, now);

        let result = match order.order_type {
            OrderType::Increase => Self::increase_position_core(
                positions,
                pool_balances,
                claimables,
                market,
                &self.services,
                now,
                &order,
                &prices,
            ),
            OrderType::Decrease | OrderType::Liquidation => Self::decrease_position_core(
                positions,
                pool_balances,
                claimables,
                market,
                &self.services,
                now,
                &mut order,
                &prices,
            ),
        };

        if result.is_ok() {
            orders.remove(order_id);
        }

        result
    }

    fn increase_position_core(
        positions: &mut PositionStore,
        pool_balances: &mut PoolBalances,
        claimables: &mut Claimables,
        market: &mut MarketState,
        services: &S,
        now: Timestamp,
        order: &Order,
        prices: &OraclePrices,
    ) -> Result<(), String> {
        // Derive notional in USD from collateral and leverage (oracle-based).
        let size_delta_usd: Usd = derive_size_delta_usd(order, prices)?;
        if size_delta_usd.is_zero() {
            return Err("size_delta_usd_must_be_positive".into());
        }

        let key = PositionKey {
            account: order.account,
            market_id: order.market_id,
            collateral_token: order.collateral_token,
            side: order.side,
        };

        let pos: &mut Position = positions.get_or_insert_with(key, |k| {
            // Initial funding index depends on side (long/short).
            let initial_funding_index = match k.side {
                Side::Long => market.funding.cumulative_index_long,
                Side::Short => market.funding.cumulative_index_short,
            };

            Position {
                key: k,
                size_usd: U256::zero(),
                size_tokens: U256::zero(),
                collateral_amount: U256::zero(),
                pending_impact_tokens: SignedU256::zero(),
                funding_index: initial_funding_index,
                borrowing_index: market.borrowing.cumulative_factor,
                opened_at: now,
                last_updated_at: now,
            }
        });

        if order.collateral_delta_tokens > U256::zero() {
            pos.collateral_amount += order.collateral_delta_tokens;
        }

        // 3) Open interest snapshot before and after this increase.
        let oi_params = services.open_interest().for_increase(
            market.oi_long_usd,
            market.oi_short_usd,
            size_delta_usd,
            order.side,
        );

        let impact_cfg = ImpactRebalanceConfig::default_quadratic();

        let pricing = services.pricing();
        let price_impact_svc = services.price_impact();

        let exec = pricing
            .get_execution_price(
                price_impact_svc,
                ExecutionPriceParams {
                    oi: &oi_params,
                    impact_cfg: &impact_cfg,
                    side: order.side,
                    size_delta_usd,
                    direction: pricing::TradeDirection::Increase,
                    prices: *prices,
                },
            )
            .map_err(|e| format!("pricing_error: {:?}", e))?;

        // 6) Step costs: funding + borrowing + (position + liquidation) fees.
        //
        // balance_was_improved comes from the pricing step and indicates whether
        // this trade reduced the long/short imbalance (helpful trade).
        let step_costs = compute_step_costs(
            services.funding(),
            services.borrowing(),
            services.fees(),
            market,
            pos,
            claimables,
            prices,
            order,
            exec.balance_was_improved,
            size_delta_usd,
        )?;

        // 7) Apply total step costs to position collateral.
        //
        // This converts total_usd to collateral tokens via collateral_price_min
        // and subtracts from pos.collateral_amount, reverting on insufficient
        // collateral.
        apply_step_costs_to_position(pos, prices, &step_costs)?;

        // 8) Route trading fees (position + liquidation) into the pool.
        //
        //  - feeAmountForPool composed from position+liquidation;
        //  - funding rewards go to Claimables inside apply_funding_step;
        //  - borrowing will also be routed to pool below.
        services
            .fees()
            .apply_fees(pool_balances, claimables, &step_costs.trading_fees);

        // 9) Route borrowing fees (already converted to tokens) into the pool.
        apply_borrowing_fees_to_pool(
            pool_balances,
            pos.key.market_id,
            pos.key.collateral_token,
            step_costs.borrowing_tokens,
        );

        // 10) Update position size and pending impact amount.
        //
        // exec.base_size_delta_tokens  - tokens from pure sizeDeltaUsd / price
        // exec.price_impact_amount_tokens - bonus/penalty tokens due to price impact
        pos.size_usd += size_delta_usd;
        pos.size_tokens += exec.base_size_delta_tokens;
        pos.pending_impact_tokens =
            math::signed_add(pos.pending_impact_tokens, exec.price_impact_amount_tokens);
        pos.last_updated_at = now;

        match order.side {
            Side::Long => {
                market.oi_long_usd += size_delta_usd;
            }
            Side::Short => {
                market.oi_short_usd += size_delta_usd;
            }
        }
        // TODO (future work):
        //  - update market-level "total_pending_impact_tokens" if you keep it;
        //  - run min-collateral / max-leverage checks similar to GMX
        //    (willPositionCollateralBeSufficient + validatePosition);
        //  - handle referral and UI fees if you add them later.
        Ok(())
    }

    fn decrease_position_core(
        positions: &mut PositionStore,
        pool_balances: &mut PoolBalances,
        claimables: &mut Claimables,
        market: &mut MarketState,
        services: &S,
        now: Timestamp,
        order: &mut Order,
        prices: &OraclePrices,
    ) -> Result<(), String> {
        let key = PositionKey {
            account: order.account,
            market_id: order.market_id,
            collateral_token: order.collateral_token,
            side: order.side,
        };

        let is_liq = matches!(order.order_type, OrderType::Liquidation);

        let res: DecreaseResult = (|| -> Result<DecreaseResult, String> {
            let pos: &mut Position = positions
                .get_mut(&key)
                .ok_or_else(|| "position_not_found".to_string())?;

            // Basic invariants.
            if pos.size_usd.is_zero() || pos.size_tokens.is_zero() {
                return Err("position_empty_or_corrupted".into());
            }

            // Cap order fields to position bounds
            if order.size_delta_usd > pos.size_usd {
                order.size_delta_usd = pos.size_usd;
            }
            if order.withdraw_collateral_amount > pos.collateral_amount {
                order.withdraw_collateral_amount = pos.collateral_amount;
            }

            // Liquidation order must be full-close and withdraw=0
            if is_liq {
                order.size_delta_usd = pos.size_usd;
                order.withdraw_collateral_amount = U256::zero();
            }

            // Risk precheck (may clamp withdraw or force full close).
            // Note: this is a conservative check (no PnL / no fees included).
            let risk = risk::RiskCfg::default();
            let (mut size_delta_usd, mut withdraw_tokens, mut is_full_close) =
                risk::validation::precheck_decrease_and_withdraw(&pos, &order, prices, risk)?;

            // liquidation => full close always
            if is_liq {
                size_delta_usd = pos.size_usd;
                withdraw_tokens = U256::zero();
                is_full_close = true;
            }

            // Convert size_delta_usd -> size_delta_tokens (GMX rounding rules).
            // Full close: take all position tokens.
            // Partial close:
            //   - long : ceil(pos.size_tokens * size_delta_usd / pos.size_usd)
            //   - short: floor(pos.size_tokens * size_delta_usd / pos.size_usd)
            let size_delta_tokens =
                math::position::size_delta_in_tokens(&pos, size_delta_usd, is_full_close)?;

            // Realize a proportional part of pending impact.
            // pending_impact_realized_tokens = pos.pending_impact_tokens * size_delta_usd / pos.size_usd
            let pending_impact_realized_tokens =
                math::position::proportional_pending_impact_tokens(&pos, size_delta_usd)?;

            //  Pricing call (mainly to obtain balance_was_improved + impact)
            //    OI params for decrease: current -> next (subtract size_delta_usd)
            let oi_params = services.open_interest().for_decrease(
                market.oi_long_usd,
                market.oi_short_usd,
                size_delta_usd,
                order.side,
            );
            let impact_cfg = ImpactRebalanceConfig::default_quadratic();
            let exec = services
                .pricing()
                .get_execution_price(
                    services.price_impact(),
                    crate::services::pricing::ExecutionPriceParams {
                        oi: &oi_params,
                        impact_cfg: &impact_cfg,
                        side: order.side,
                        direction: crate::services::pricing::TradeDirection::Decrease,
                        size_delta_usd,
                        prices: *prices,
                    },
                )
                .map_err(|e| format!("pricing_error:{:?}", e))?;

            // Funding + borrowing + trading fees: compute and apply to position collateral.
            let step_costs = compute_step_costs(
                services.funding(),
                services.borrowing(),
                services.fees(),
                market,
                pos,
                claimables,
                prices,
                &order,
                exec.balance_was_improved,
                size_delta_usd,
            )?;

            if let Err(e) = apply_step_costs_to_position(pos, prices, &step_costs) {
                // Insolvent liquidation path: allow full close, seize remaining collateral.
                if is_liq && is_full_close {
                    let seized = pos.collateral_amount;
                    pos.collateral_amount = U256::zero();

                    // credit collateral to the pool as fees.
                    if seized > U256::zero() {
                        pool_balances.add_fee_to_pool(market.id, pos.key.collateral_token, seized);
                    }

                    // Update OI (full close).
                    match order.side {
                        Side::Long => {
                            market.oi_long_usd = market
                                .oi_long_usd
                                .checked_sub(size_delta_usd)
                                .ok_or("oi_long_underflow")?;
                        }
                        Side::Short => {
                            market.oi_short_usd = market
                                .oi_short_usd
                                .checked_sub(size_delta_usd)
                                .ok_or("oi_short_underflow")?;
                        }
                    }

                    // Close fields (we will remove from store after scope ends).
                    pos.size_usd = U256::zero();
                    pos.size_tokens = U256::zero();
                    pos.pending_impact_tokens = SignedU256::zero();
                    pos.last_updated_at = now;

                    return Ok(DecreaseResult {
                        should_remove: true,
                        collateral_asset: pos.key.collateral_token,
                        output_tokens: U256::zero(),
                    });
                }

                return Err(format!("insufficient_collateral_for_costs:{e}"));
            }

            // Route fees to pool / claimables.
            services
                .fees()
                .apply_fees(pool_balances, claimables, &step_costs.trading_fees);

            apply_borrowing_fees_to_pool(
                pool_balances,
                market.id,
                pos.key.collateral_token,
                step_costs.borrowing_tokens,
            );

            // Realize base PnL (mark-to-oracle) proportional to TΔ / T0.
            let total_pnl_usd = math::pnl::total_position_pnl_usd(pos, prices)?;
            let realized_base_pnl_usd =
                math::pnl::realized_pnl_usd(total_pnl_usd, size_delta_tokens, pos.size_tokens)?;

            // Realize proportional pending impact (stored from previous increases).
            // Conservative valuation (matches your earlier approach):
            //   if impactTokens > 0 => use index_price_min
            //   if impactTokens < 0 => use index_price_max
            // Realize pending impact to signed USD (conservative)
            let realized_pending_impact_usd: SignedU256 =
                impact_tokens_to_usd_conservative(pending_impact_realized_tokens, prices)?;

            // Include close price impact
            let realized_total_usd: SignedU256 = math::signed_add(
                math::signed_add(realized_base_pnl_usd, realized_pending_impact_usd),
                exec.price_impact_usd,
            );

            // Convert realized_total_usd into collateral token delta (signed):
            //   +Usd => floor(/ collateral_price_max)
            //   -Usd => -ceil(abs / collateral_price_min)
            let pnl_tokens_signed: SignedU256 =
                math::pnl::pnl_usd_to_collateral_tokens(realized_total_usd, prices)?;

            let collateral_asset = pos.key.collateral_token;
            let mut output_tokens: TokenAmount = U256::zero();

            // Settle PnL+impact vs pool and/or position collateral.
            if !pnl_tokens_signed.is_negative {
                let pay = pnl_tokens_signed.mag;

                // Profit / positive impact is paid from pool liquidity.
                pool_balances
                    .remove_liquidity(market.id, collateral_asset, pay)
                    .map_err(|_| "insufficient_pool_liquidity_for_payout".to_string())?;

                output_tokens = output_tokens.checked_add(pay).ok_or("output_overflow")?;
            } else {
                let loss = pnl_tokens_signed.mag;

                // Loss / negative impact is taken from position collateral and added to the pool.
                if loss > pos.collateral_amount {
                    if is_liq && is_full_close {
                        let seized = pos.collateral_amount;
                        pos.collateral_amount = U256::zero();
                        if !seized.is_zero() {
                            pool_balances.add_to_pool(market.id, collateral_asset, seized);
                        }
                    } else {
                        return Err("insufficient_collateral_for_negative_pnl".into());
                    }
                } else {
                    pos.collateral_amount -= loss;
                    pool_balances.add_to_pool(market.id, collateral_asset, loss);
                }
            }

            // Withdraw collateral (only if not liquidation).
            if !is_liq && !withdraw_tokens.is_zero() {
                let withdraw_actual = withdraw_tokens.min(pos.collateral_amount);
                pos.collateral_amount -= withdraw_actual;
                output_tokens = output_tokens
                    .checked_add(withdraw_actual)
                    .ok_or("output_overflow")?;
            } else {
                withdraw_tokens = U256::zero();
            }

            //  Update OI.
            match order.side {
                Side::Long => {
                    market.oi_long_usd = market
                        .oi_long_usd
                        .checked_sub(size_delta_usd)
                        .ok_or("oi_long_underflow")?;
                }
                Side::Short => {
                    market.oi_short_usd = market
                        .oi_short_usd
                        .checked_sub(size_delta_usd)
                        .ok_or("oi_short_underflow")?;
                }
            }

            //  Close or update position state.
            if is_full_close || size_delta_usd == pos.size_usd {
                // On full close, user receives all remaining collateral as well.
                let rest = pos.collateral_amount;
                pos.collateral_amount = U256::zero();

                if rest > U256::zero() {
                    output_tokens = output_tokens.checked_add(rest).ok_or("output_overflow")?;
                }

                // Zero out fields and mark as closed.
                pos.size_usd = U256::zero();
                pos.size_tokens = U256::zero();
                pos.pending_impact_tokens = SignedU256::zero();
                pos.last_updated_at = now;

                // Credit output into claimables (withdrawable balance).
                if !output_tokens.is_zero() {
                    claimables.add_fee(order.account, collateral_asset, output_tokens);
                }

                return Ok(DecreaseResult {
                    should_remove: true,
                    collateral_asset,
                    output_tokens,
                });
            }

            // Partial close.
            pos.size_usd = pos
                .size_usd
                .checked_sub(size_delta_usd)
                .ok_or("pos_size_usd_underflow")?;

            pos.size_tokens = pos
                .size_tokens
                .checked_sub(size_delta_tokens)
                .ok_or("pos_size_tokens_underflow")?;

            // Remove proportional pending impact (already realized above).
            // pending_impact_tokens -= realized_pending
            pos.pending_impact_tokens =
                math::signed_sub(pos.pending_impact_tokens, pending_impact_realized_tokens);

            pos.last_updated_at = now;

            // Post-check remaining position (leverage/collateral constraints).
            risk::validation::postcheck_remaining_position(pos, prices, risk)?;

            // Credit output into claimables (withdrawable balance).
            if output_tokens > U256::zero() {
                claimables.add_fee(order.account, collateral_asset, output_tokens);
            }

            Ok(DecreaseResult {
                should_remove: false,
                collateral_asset,
                output_tokens,
            })
        })()?;

        if res.should_remove {
            positions.remove(&key);
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
struct DecreaseResult {
    should_remove: bool,
    collateral_asset: AssetId,
    output_tokens: TokenAmount,
}

/// Derive size_delta_usd from collateral deposit and target leverage.
fn derive_size_delta_usd(order: &Order, prices: &OraclePrices) -> Result<Usd, String> {
    // 1) collateral_usd_1e30 = atoms * price_per_unit_1e30
    let collateral_usd = order
        .collateral_delta_tokens
        .checked_mul(prices.collateral_price_min)
        .ok_or("u256_mul_overflow")?;

    // 2) size_delta_usd = collateral_usd * leverage
    let lev = U256::from(order.target_leverage_x);
    let size_delta_usd = collateral_usd.checked_mul(lev).ok_or("u256_mul_overflow")?;

    Ok(size_delta_usd)
}

/// Convert signed impact tokens -> signed USD, conservative:
/// +tokens => * index_price_min
/// -tokens => * index_price_max
fn impact_tokens_to_usd_conservative(
    tokens: SignedU256,
    prices: &OraclePrices,
) -> Result<SignedU256, String> {
    if tokens.is_zero() {
        return Ok(SignedU256::zero());
    }
    let px = if tokens.is_negative {
        prices.index_price_max
    } else {
        prices.index_price_min
    };
    if px.is_zero() {
        return Err("invalid_index_price_for_pending_impact".into());
    }
    let mag = tokens
        .mag
        .checked_mul(px)
        .ok_or("pending_impact_usd_overflow")?;
    Ok(SignedU256 {
        is_negative: tokens.is_negative,
        mag,
    })
}
#[cfg(test)]
mod tests {
    use super::*;

    use crate::math::{signed_add, signed_sub};
    use crate::oracle::Oracle;
    use crate::services::BasicServicesBundle;
    use crate::services::borrowing::{BasicBorrowingService, BorrowingService};
    use crate::services::fees::{BasicFeesService, FeesService};
    use crate::services::funding::{BasicFundingService, FundingService};
    use crate::services::open_interest::{BasicOpenInterestService, OpenInterestService};
    use crate::services::price_impact::{BasicPriceImpactService, PriceImpactService};
    use crate::services::pricing::{BasicPricingService, PricingService};
    use crate::state::{Claimables, MarketState, PoolBalances, Position, PositionKey, State};
    use crate::types::{
        AccountId, AssetId, MarketId, OraclePrices, Order, OrderId, OrderType, Side, Timestamp,
        TokenAmount, Usd,
    };
    use primitive_types::U512;

    fn borrow_index_scale() -> U256 {
        U256::exp10(18)
    }
    const INCREASE_FEE_BPS: u32 = 10;
    const HELPFUL_REBATE_PERCENT: u32 = 20;
    const SECONDS_PER_DAY: u64 = 86_400;
    const BASE_RATE_PER_DAY_BPS: u64 = 1;
    const SLOPE_PER_DAY_BPS: u64 = 9;

    fn bps_per_day_to_fp_per_sec(bps_per_day: u64) -> U256 {
        let den = U256::from(10_000u64 * SECONDS_PER_DAY);
        mul_div_u256(U256::from(bps_per_day), borrow_index_scale(), den).unwrap()
    }

    fn funding_index_scale() -> U256 {
        U256::exp10(18) // 1e18
    }

    /// Desired funding rate (MVP): 1 bp/day = 0.01% per day when imbalance persists.
    /// Increase to 5..20 bps/day if you want stronger incentives.
    const DAILY_RATE_BPS: i128 = 1; // 0.01% / day

    const BPS_DENOM: i128 = 10_000;

    /// rate_fp_per_sec = SCALE * (DAILY_RATE_BPS / 10_000) / 86_400
    fn rate_fp_per_sec() -> U256 {
        (funding_index_scale() / U256::from(SECONDS_PER_DAY)) * U256::from(DAILY_RATE_BPS)
            / U256::from(BPS_DENOM)
    }
    /// Simple oracle that always returns fixed prices for a given market.
    struct TestOracle {
        prices: OraclePrices,
    }

    impl Oracle for TestOracle {
        fn validate_and_get_prices(&self, _market_id: MarketId) -> Result<OraclePrices, String> {
            Ok(self.prices)
        }
    }

    fn usd(x: u128) -> U256 {
        U256::from(x) * U256::exp10(30) // USD(1e30)
    }

    fn tok(x: u128) -> U256 {
        U256::from(x) // token atoms
    }

    fn leverage_x(x: i64) -> U256 {
        assert!(x > 0);
        U256::from(x as u64)
    }

    /// Convert whole tokens to atoms by decimals.
    /// Example: 5000 USDC (6 decimals) => 5000 * 1e6 atoms.
    fn to_atoms(tokens: u128, decimals: u8) -> U256 {
        U256::from(tokens) * U256::exp10(decimals as usize)
    }

    /// Normalize USD(1e30) per 1 token -> USD(1e30) per 1 atom.
    /// min: floor, max: ceil (conservative spread handling).
    fn normalize_price_per_atom(
        price_min_per_token: U256,
        price_max_per_token: U256,
        decimals: u8,
    ) -> (U256, U256) {
        let scale = U256::exp10(decimals as usize);
        let min_atom = price_min_per_token / scale; // floor
        let max_atom = div_ceil_u256(price_max_per_token, scale); // ceil
        (min_atom, max_atom)
    }
    fn div_ceil_u256(a: U256, b: U256) -> U256 {
        let q = a / b;
        let r = a % b;
        if r.is_zero() { q } else { q + 1 }
    }

    fn u512_to_u256_checked(x: U512) -> Result<U256, String> {
        let be = x.to_big_endian();
        if be[..32].iter().any(|&b| b != 0) {
            return Err("mul_div_overflow".into());
        }
        Ok(U256::from_big_endian(&be[32..]))
    }

    fn mul_div_u256(a: U256, b: U256, den: U256) -> Result<U256, String> {
        if den.is_zero() {
            return Err("mul_div_den_zero".into());
        }
        let prod = U512::from(a) * U512::from(b);
        let q = prod / U512::from(den);
        u512_to_u256_checked(q)
    }

    pub fn signed_mul_div_u256(
        value: U256,
        signed: SignedU256,
        denom: U256,
    ) -> Result<SignedU256, String> {
        if value.is_zero() || signed.mag.is_zero() {
            return Ok(SignedU256::zero());
        }
        let mag = mul_div_u256(value, signed.mag, denom)?;
        Ok(SignedU256 {
            is_negative: signed.is_negative,
            mag,
        })
    }
    /// Full workflow test:
    ///
    /// 1) Market starts mildly long-heavy (more longs than shorts).
    /// 2) First order: open a short position at time t1.
    /// 3) Time passes (1 hour).
    /// 4) Second order: increase the same short at time t2.
    ///
    /// We check:
    ///  - open interest updates (oi_long / oi_short),
    ///  - funding index + position funding snapshot,
    ///  - borrowing index + position borrowing snapshot,
    ///  - collateral decreases due to fees + funding + borrowing,
    ///  - pool receives fee tokens,
    ///  - user gets positive funding claimables on receiving side
    ///    (because shorts receive funding in long-heavy market).
    #[test]
    fn full_increase_flow_with_real_services() {
        let market_id: MarketId = MarketId(1);
        let account: AccountId = AccountId([2; 32]);
        let collateral_token: AssetId = AssetId(10);
        let long_asset: AssetId = AssetId(11);
        let short_asset: AssetId = AssetId(12);

        // Collateral = USDC-like (6 decimals)
        let collateral_decimals: u8 = 6;
        // Index asset = ETH (18 decimals)
        let eth_decimals: u8 = 18;

        // --- Price: "1 ETH costs 2.981 USD" ---
        let eth_usd_per_eth_token: U256 = usd(2_981);

        let eth_min_token = eth_usd_per_eth_token.saturating_sub(usd(1));
        let eth_max_token = eth_usd_per_eth_token.checked_add(usd(1)).unwrap();

        let (index_price_min, index_price_max) =
            normalize_price_per_atom(eth_min_token, eth_max_token, eth_decimals);

        // Collateral price (USDC): $1 per token, normalized per atom
        let (collateral_price_min, collateral_price_max) =
            normalize_price_per_atom(usd(1), usd(1), collateral_decimals);

        // Prices are "per smallest unit" already:
        // index_price_*: USD(1e30) per 1 index token atom
        // collateral_price_*: USD(1e30) per 1 collateral atom
        let oracle_prices = OraclePrices {
            index_price_min,
            index_price_max,
            collateral_price_min,
            collateral_price_max,
        };

        let services = BasicServicesBundle::default();
        let oracle = TestOracle {
            prices: oracle_prices,
        };

        let mut executor: Executor<BasicServicesBundle, TestOracle> =
            Executor::new(State::default(), services, oracle);

        let t0: Timestamp = 1_000_000;
        // first order execution time
        let t1: Timestamp = t0 + 60;
        // second execution, +1 hour
        let t2: Timestamp = t1 + 3600;

        // Initial market state: mildly long-heavy and non-zero liquidity
        let market = executor.state.markets.entry(market_id).or_insert_with(|| {
            let mut m = MarketState::default();
            m.id = market_id;
            m
        });

        // Example: longs = 120k, shorts = 80k -> long-heavy (+40k skew)
        market.oi_long_usd = usd(120_000);
        market.oi_short_usd = usd(80_000);
        market.liquidity_usd = usd(1_000_000);

        market.long_asset = long_asset;
        market.short_asset = short_asset;

        let m_before1 = executor.state.markets.get(&market_id).unwrap().clone();

        // STEP 1: first short increase at t1

        // User deposits 5_000 collateral tokens, leverage 4x → target 20_000 USD notional.
        let deposit_usdc_atoms = to_atoms(5_000, collateral_decimals);
        let mut order1 = Order {
            account,
            market_id,
            side: Side::Short,
            collateral_token,
            size_delta_usd: U256::zero(), // derived in executor
            collateral_delta_tokens: deposit_usdc_atoms,
            target_leverage_x: 4,
            order_type: OrderType::Increase,
            withdraw_collateral_amount: U256::zero(),
            created_at: t1,
            valid_from: t1 - 30,
            valid_until: t1 + 300,
        };

        let order1_id: OrderId = executor.submit_order(order1.clone());

        executor
            .execute_order(t1, order1_id)
            .expect("first execute_order must succeed");

        // Assertions after step 1

        // Order must be removed
        assert!(
            executor.state.orders.get(order1_id).is_none(),
            "order1 must be removed after execution"
        );

        // Position updated

        let pos_key = PositionKey {
            account,
            market_id,
            collateral_token,
            side: Side::Short,
        };
        let pos_after1 = executor
            .state
            .positions
            .get(&pos_key)
            .expect("position must exist after first execution")
            .clone();

        // Notional check: size_delta_usd from collateral * price * leverage
        let expected_size_delta1 = order1
            .collateral_delta_tokens
            .checked_mul(oracle_prices.collateral_price_min)
            .expect("collateral_usd overflow")
            .checked_mul(leverage_x(order1.target_leverage_x))
            .expect("size_delta_usd overflow");
        assert_eq!(
            expected_size_delta1,
            usd(20_000),
            "by construction: 5000 * 1 * 4 = 20_000 USD notional"
        );
        assert_eq!(
            pos_after1.size_usd, expected_size_delta1,
            "position size_usd must reflect derived size_delta_usd"
        );

        assert!(
            !pos_after1.size_tokens.is_zero(),
            "short position must have non-zero size_tokens"
        );

        // OI check
        let m_after1 = executor.state.markets.get(&market_id).unwrap().clone();
        assert_eq!(
            m_after1.oi_long_usd, m_before1.oi_long_usd,
            "long OI must not change for a short increase"
        );
        assert_eq!(
            m_after1.oi_short_usd,
            m_before1.oi_short_usd + expected_size_delta1,
            "short OI must increase by size_delta_usd"
        );

        // Funding / borrowing indices updated to t1.
        assert_eq!(
            m_after1.funding.last_updated_at, t1,
            "funding.last_updated_at must be updated to t1"
        );
        assert_eq!(
            m_after1.borrowing.last_updated_at, t1,
            "borrowing.last_updated_at must be updated to t1"
        );

        // Position snapshots must match current funding / borrowing indices for its side.
        assert_eq!(
            pos_after1.funding_index, m_after1.funding.cumulative_index_short,
            "short position funding_index must match market short index after step 1"
        );
        assert_eq!(
            pos_after1.borrowing_index, m_after1.borrowing.cumulative_factor,
            "position borrowing_index must match market borrowing factor after step 1"
        );

        // Collateral vs pool: all costs are pure trading fee on step 1
        assert!(
            !pos_after1.collateral_amount.is_zero(),
            "collateral must remain positive after first step"
        );

        // Pool must receive position fee tokens for the collateral asset.
        let fee_pool_after1 = executor
            .state
            .pool_balances
            .get_fee_for_pool(market_id, collateral_token);
        assert!(
            !fee_pool_after1.is_zero(),
            "after first step, pool must have some fee tokens from position fees"
        );

        // On step 1 there is:
        //  - NO funding payment (position size was 0 before snapshot),
        //  - NO borrowing payment (position size was 0 before, delta index = 0).
        //
        // So:
        //   deposit1 = position_collateral_after1 + trading_fee_tokens_to_pool
        let deposit1 = order1.collateral_delta_tokens;
        let collat_after1 = pos_after1.collateral_amount;
        assert!(
            deposit1 > collat_after1,
            "some part of deposited collateral must be spent on trading fees"
        );

        let spent_tokens_step1 = deposit1 - collat_after1;

        assert_eq!(
            spent_tokens_step1, fee_pool_after1,
            "all spent collateral tokens in step 1 must end up in the pool as trading fees"
        );

        // Funding claimables must still be zero after the first touch
        let claim_long_after1 = executor.state.claimables.get_funding(account, long_asset);
        let claim_short_after1 = executor.state.claimables.get_funding(account, short_asset);

        assert_eq!(
            claim_long_after1,
            U256::zero(),
            "no funding rewards should be claimable right after the first increase"
        );
        assert_eq!(
            claim_short_after1,
            U256::zero(),
            "no funding rewards should be claimable right after the first increase"
        );

        // STEP 2: second short increase at t2 (+1 hour)

        // Second order: deposit 1_000 collateral tokens with 4x leverage:
        //   collateral_usd2 = 1_000 * 1 = 1_000
        //   size_delta_usd2 = 1_000 * 4 = 4_000
        let collateral_delta_tokens2 = to_atoms(1_000, collateral_decimals);
        let target_leverage_x2 = 4;

        let order2 = Order {
            account,
            market_id,
            side: Side::Short,
            collateral_token,
            size_delta_usd: U256::zero(), // derived inside executor
            collateral_delta_tokens: collateral_delta_tokens2,
            target_leverage_x: target_leverage_x2,
            order_type: OrderType::Increase,
            withdraw_collateral_amount: U256::zero(),
            created_at: t2,
            valid_from: t2 - 30,
            valid_until: t2 + 300,
        };

        let order2_id: OrderId = executor.submit_order(order2.clone());

        // Snapshots BEFORE step 2
        let pos_before2 = pos_after1.clone();
        let m_before2 = executor.state.markets.get(&market_id).unwrap().clone();
        let fee_pool_before2 = fee_pool_after1;

        executor
            .execute_order(t2, order2_id)
            .expect("second execute_order must succeed");

        let pos_after2 = executor
            .state
            .positions
            .get(&pos_key)
            .cloned()
            .expect("position must exist after second execution");
        let m_after2 = executor.state.markets.get(&market_id).unwrap().clone();
        let fee_pool_after2 = executor
            .state
            .pool_balances
            .get_fee_for_pool(market_id, collateral_token);

        // size_usd and OI after step 2

        let collateral_usd2: Usd = collateral_delta_tokens2 * oracle_prices.collateral_price_min; // = 1000
        let expected_size_delta_usd2: Usd = collateral_usd2 * U256::from(target_leverage_x2); // = 4000

        assert_eq!(
            pos_after2.size_usd,
            pos_after1.size_usd + expected_size_delta_usd2,
            "position size_usd must increase by second order notional (4k)"
        );

        assert_eq!(
            m_after2.oi_long_usd, m_after1.oi_long_usd,
            "long OI must remain unchanged on second short increase"
        );
        assert_eq!(
            m_after2.oi_short_usd,
            m_after1.oi_short_usd + expected_size_delta_usd2,
            "short OI must additionally increase by 4k on step 2"
        );

        // Funding index evolution between t1 and t2

        let dt2: u64 = t2 - m_before2.funding.last_updated_at;
        assert_eq!(m_before2.funding.last_updated_at, t1);

        let long_oi2 = m_before2.oi_long_usd;
        let short_oi2 = m_before2.oi_short_usd;
        let total_oi2 = long_oi2 + short_oi2;
        assert!(!total_oi2.is_zero(), "total OI must be > 0 on step 2");

        let imbalance2 = long_oi2 - short_oi2; // still > 0 (long-heavy)
        assert!(
            !imbalance2.is_zero(),
            "market must remain long-heavy before second order"
        );

        // In BasicFundingService:
        //   delta_index_fp = rate_abs_fp_per_sec * dt
        //   if long-heavy:
        //      funding_long += delta_index_fp
        //      funding_short -= delta_index_fp
        let delta_index_funding_fp = rate_fp_per_sec() * U256::from(dt2 as i128);

        let expected_funding_index_long_after2 = signed_add(
            m_before2.funding.cumulative_index_long,
            SignedU256 {
                is_negative: false,
                mag: delta_index_funding_fp,
            },
        );
        let expected_funding_index_short_after2 = signed_sub(
            m_before2.funding.cumulative_index_short,
            SignedU256 {
                is_negative: false,
                mag: delta_index_funding_fp,
            },
        );

        assert_eq!(
            m_after2.funding.cumulative_index_long, expected_funding_index_long_after2,
            "funding long index must move by +delta_index_funding_fp"
        );
        assert_eq!(
            m_after2.funding.cumulative_index_short, expected_funding_index_short_after2,
            "funding short index must move by -delta_index_funding_fp"
        );

        // Position snapshot for short side must match new market short index.
        assert_eq!(
            pos_after2.funding_index, m_after2.funding.cumulative_index_short,
            "after step 2, short position funding_index must match market short index"
        );

        // Expected funding fee for step 2:
        //   delta_idx = funding_short_after2 - pos_before2.funding_index
        //   funding_fee_usd = size_usd * delta_idx / SCALE
        let delta_idx_funding2 = signed_sub(
            expected_funding_index_short_after2,
            pos_before2.funding_index,
        );

        let expected_funding_usd2: SignedU256 = signed_mul_div_u256(
            pos_before2.size_usd,
            delta_idx_funding2,
            funding_index_scale(),
        )
        .expect("signed_mul_div_u256 must not overflow");

        // For short in long-heavy market user should RECEIVE funding:
        assert!(
            expected_funding_usd2.is_negative && !expected_funding_usd2.mag.is_zero(),
            "short position must receive funding (negative cost_usd) on step 2"
        );

        println!("expected_funding_usd2 {:?}", expected_funding_usd2);
        // Borrowing index evolution between t1 and t2

        let dt2_borrow: u64 = t2 - m_before2.borrowing.last_updated_at;
        assert_eq!(m_before2.borrowing.last_updated_at, t1);

        let borrowed2 = (m_before2.oi_long_usd + m_before2.oi_short_usd);
        let liquidity2 = m_before2.liquidity_usd;
        assert!(liquidity2 > U256::zero());

        // Utilization in [0,1] * SCALE:
        let scale = borrow_index_scale();
        let mut util_fp2 = mul_div_u256(borrowed2, scale, liquidity2).expect("util mul/div");
        if util_fp2 > scale {
            util_fp2 = scale;
        }

        // In BasicBorrowingService:
        //   rate_per_sec_fp = base_rate + slope * util
        //   delta_index = rate_per_sec_fp * dt
        // rate_per_sec_fp = base + slope * util / SCALE
        let base_rate_fp_per_sec = bps_per_day_to_fp_per_sec(BASE_RATE_PER_DAY_BPS);
        let slope_fp_per_sec = bps_per_day_to_fp_per_sec(SLOPE_PER_DAY_BPS);

        let slope_term = mul_div_u256(slope_fp_per_sec, util_fp2, scale).expect("slope_term");
        let rate_per_sec_fp2 = base_rate_fp_per_sec + slope_term;

        // delta_index = rate * dt
        let delta_index_borrow_fp = rate_per_sec_fp2 * U256::from(dt2_borrow);
        let expected_borrow_factor_after2 =
            m_before2.borrowing.cumulative_factor + delta_index_borrow_fp;

        assert_eq!(
            m_after2.borrowing.cumulative_factor, expected_borrow_factor_after2,
            "borrowing cumulative factor must move according to utilization formula"
        );

        println!(
            "m_after2.borrowing.cumulative_factor {:?}",
            m_after2.borrowing.cumulative_factor
        );

        // Position snapshot must equal new factor.
        assert_eq!(
            pos_after2.borrowing_index, m_after2.borrowing.cumulative_factor,
            "after step 2, position borrowing_index must equal market borrowing factor"
        );

        // Expected borrowing cost (USD):
        let delta_idx_borrow2 = expected_borrow_factor_after2 - pos_before2.borrowing_index;
        let expected_borrowing_usd2 = mul_div_u256(
            pos_before2.size_usd,
            delta_idx_borrow2,
            borrow_index_scale(),
        )
        .expect("borrow fee mul/div");

        assert!(
            !expected_borrowing_usd2.is_zero(),
            "borrowing fee must be > 0 on step 2 with non-zero dt"
        );
        println!("expected_borrowing_usd2 {:?}", expected_borrowing_usd2);

        // Trading fee for second order

        let oi_params2 = executor.services.open_interest().for_increase(
            m_before2.oi_long_usd,
            m_before2.oi_short_usd,
            expected_size_delta_usd2,
            order2.side,
        );

        let exec_expected2 = executor
            .services
            .pricing()
            .get_execution_price(
                executor.services.price_impact(),
                crate::services::pricing::ExecutionPriceParams {
                    oi: &oi_params2,
                    impact_cfg: &ImpactRebalanceConfig::default_quadratic(),
                    side: order2.side,
                    size_delta_usd: expected_size_delta_usd2,
                    direction: pricing::TradeDirection::Increase,
                    prices: oracle_prices,
                },
            )
            .expect("pricing for second increase must succeed");

        assert!(
            exec_expected2.balance_was_improved,
            "second short on long-heavy market must also be helpful"
        );

        let mut effective_bps2: u32 = INCREASE_FEE_BPS;
        effective_bps2 = effective_bps2.saturating_mul(100 - HELPFUL_REBATE_PERCENT) / 100;

        let expected_trading_fee_usd2 =
            (expected_size_delta_usd2 * U256::from(effective_bps2) / U256::from(10_000));
        let expected_trading_fee_tokens2: TokenAmount =
            expected_trading_fee_usd2 / oracle_prices.collateral_price_min;

        // Pool fee increment on step 2
        //
        // Pool receives:
        //   - position fee tokens (trading),
        //   - borrowing fee tokens.
        //
        // Funding rewards go to Claimables (not to pool).
        let delta_fee_pool2 = fee_pool_after2 - fee_pool_before2;
        let expected_borrowing_tokens2: TokenAmount =
            expected_borrowing_usd2 / oracle_prices.collateral_price_min;

        assert_eq!(
            delta_fee_pool2,
            expected_trading_fee_tokens2 + expected_borrowing_tokens2,
            "pool fee increment on step 2 must equal trading fee tokens + borrowing fee tokens"
        );

        println!("delta_fee_pool2 {:?}", delta_fee_pool2);

        // Collateral after step 2
        //
        // Formula:
        //   collateral_after2 =
        //      collateral_before2
        //      + collateral_deposit2
        //      - total_cost_tokens2
        //
        // where:
        //   total_cost_usd2 = funding_usd2 + borrowing_usd2 + trading_usd2
        //   total_cost_tokens2 = total_cost_usd2 / collateral_price_min
        let expected_total_usd2: Usd = expected_borrowing_usd2 + expected_trading_fee_usd2;
        let expected_total_tokens2: TokenAmount =
            expected_total_usd2 / oracle_prices.collateral_price_min;

        let expected_collateral_after2: TokenAmount =
            pos_before2.collateral_amount + collateral_delta_tokens2 - expected_total_tokens2;

        assert_eq!(
            pos_after2.collateral_amount, expected_collateral_after2,
            "collateral after step 2 must equal coll_before + deposit2 - total_cost_tokens2"
        );

        // // // Funding claimables: user must have positive funding tokens somewhere

        // // let funding_long = executor.state.claimables.get_funding(account, long_asset);
        // // let funding_short = executor.state.claimables.get_funding(account, short_asset);

        // // assert!(
        // //     funding_long > 0 || funding_short > 0,
        // //     "after second step, user must have some positive funding claimable on at least one asset"
        // // );
    }
}
