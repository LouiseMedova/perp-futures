use primitive_types::U256;

use crate::math;
use crate::oracle::Oracle;
use crate::risk;
use crate::risk::{
    RiskCfg, liquidation,
    liquidation::{LiquidationFeeCfg, LiquidationPreview},
};
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

#[derive(Clone)]
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

    pub fn is_liquidatable_by_margin(
        &self,
        now: Timestamp,
        key: PositionKey,
    ) -> Result<LiquidationPreview, String> {
        let market = self
            .state
            .markets
            .get(&key.market_id)
            .ok_or("market_not_found")?;
        let pos = self.state.positions.get(&key).ok_or("position_not_found")?;
        let prices = self.oracle.validate_and_get_prices(key.market_id)?;

        let price_impact_usd_on_close =
            self.preview_close_price_impact_usd(market, pos, &prices)?;

        let risk = RiskCfg::default();

        // mvp
        let fee_cfg = LiquidationFeeCfg {
            close_position_fee_bps: 0,
            liquidation_fee_bps: 0,
        };

        liquidation::is_liquidatable_by_margin(
            market,
            pos,
            &prices,
            now,
            risk,
            fee_cfg,
            price_impact_usd_on_close,
        )
    }

    pub fn calculate_liquidation_price(
        &self,
        now: Timestamp,
        key: PositionKey,
    ) -> Result<U256, String> {
        let market = self
            .state
            .markets
            .get(&key.market_id)
            .ok_or("market_not_found")?;
        let pos = self.state.positions.get(&key).ok_or("position_not_found")?;
        let prices = self.oracle.validate_and_get_prices(key.market_id)?;

        let price_impact_usd_on_close =
            self.preview_close_price_impact_usd(market, pos, &prices)?;

        let risk = RiskCfg::default();

        // zero liquidation fee for mvp
        let fee_cfg = LiquidationFeeCfg {
            close_position_fee_bps: 0,
            liquidation_fee_bps: 0,
        };

        liquidation::calculate_liquidation_price(
            market,
            pos,
            &prices,
            now,
            risk,
            fee_cfg,
            price_impact_usd_on_close,
        )
    }

    fn preview_close_price_impact_usd(
        &self,
        market: &MarketState,
        pos: &Position,
        prices: &OraclePrices,
    ) -> Result<SignedU256, String> {
        let oi_params = self.services.open_interest().for_decrease(
            market.oi_long_usd,
            market.oi_short_usd,
            pos.size_usd,
            pos.key.side,
        );

        let impact_cfg = ImpactRebalanceConfig::default_quadratic();

        let exec = self
            .services
            .pricing()
            .get_execution_price(
                self.services.price_impact(),
                crate::services::pricing::ExecutionPriceParams {
                    oi: &oi_params,
                    impact_cfg: &impact_cfg,
                    side: pos.key.side,
                    direction: crate::services::pricing::TradeDirection::Decrease,
                    size_delta_usd: pos.size_usd,
                    prices: *prices,
                },
            )
            .map_err(|e| format!("pricing_error:{:?}", e))?;

        Ok(exec.price_impact_usd)
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

            println!("COLL AMOUNT {:?}", pos.collateral_amount);
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

            println!("REALISED BASE PNL {:?}", realized_base_pnl_usd);
            println!("REALISED BASE PNL {:?}", realized_pending_impact_usd);
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

            println!("PNL {:?}", pnl_tokens_signed);
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
#[path = "executor_tests/mod.rs"]
mod tests;
