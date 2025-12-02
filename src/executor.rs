use crate::oracle::Oracle;
use crate::services::borrowing::apply_borrowing_fees_to_pool;
use crate::services::price_impact::ImpactRebalanceConfig;
use crate::services::pricing::ExecutionPriceIncreaseParams;
use crate::services::step_costs::{apply_step_costs_to_position, compute_step_costs};
use crate::services::*;
use crate::state::{
    Claimables, MarketState, PoolBalances, Position, PositionKey, PositionStore, State,
};
use crate::types::{OraclePrices, Order, OrderId, OrderType, Side, Timestamp};
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
        let order = match self.state.orders.get(order_id) {
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
        // self.services.impact_pool().bleed(&mut market.impact_pool, now);
        // 1) Sync market-level time-based indices
        self.services.funding().update_indices(market, now);
        self.services.borrowing().update_index(market, now);

        // 6) Ветка по типу ордера
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
            OrderType::Decrease | OrderType::Liquidation => {
                Err("decrease/liquidation not implemented yet".into())
            }
        };

        // 7) Если успех — удаляем ордер из pending
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
                size_usd: 0,
                size_tokens: 0,
                collateral_amount: 0,
                pending_impact_tokens: 0,
                funding_index: initial_funding_index,
                borrowing_index: market.borrowing.cumulative_factor,
                opened_at: now,
                last_updated_at: now,
            }
        });

        // 3) Open interest snapshot before and after this increase.
        let oi_params = services.open_interest().for_increase(
            market.oi_long_usd,
            market.oi_short_usd,
            order.size_delta_usd,
            order.side,
        );

        let impact_cfg = ImpactRebalanceConfig::default_quadratic();

        let pricing = services.pricing();
        let price_impact_svc = services.price_impact();

        let exec = pricing
            .get_execution_price_for_increase(
                price_impact_svc,
                ExecutionPriceIncreaseParams {
                    oi: &oi_params,
                    impact_cfg: &impact_cfg,
                    side: order.side,
                    size_delta_usd: order.size_delta_usd,
                    prices: *prices, // OraclePrices у тебя Copy
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
        pos.size_usd += order.size_delta_usd;
        pos.size_tokens += exec.base_size_delta_tokens;
        pos.pending_impact_tokens += exec.price_impact_amount_tokens;
        pos.last_updated_at = now;

        match order.side {
            Side::Long => {
                market.oi_long_usd += order.size_delta_usd;
            }
            Side::Short => {
                market.oi_short_usd += order.size_delta_usd;
            }
        }
        // TODO (future work):
        //  - update market-level "total_pending_impact_tokens" if you keep it;
        //  - run min-collateral / max-leverage checks similar to GMX
        //    (willPositionCollateralBeSufficient + validatePosition);
        //  - handle referral and UI fees if you add them later.
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    /// Simple oracle that always returns fixed prices for a given market.
    struct TestOracle {
        prices: OraclePrices,
    }

    impl Oracle for TestOracle {
        fn validate_and_get_prices(&self, _market_id: MarketId) -> Result<OraclePrices, String> {
            Ok(self.prices)
        }
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

        // Prices:
        // index ≈ 2000 USD with small spread,
        // collateral ≈ 1 USD stablecoin (min/max equal).
        let oracle_prices = OraclePrices {
            index_price_min: 1_990,
            index_price_max: 2_010,
            collateral_price_min: 1,
            collateral_price_max: 1,
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
        market.oi_long_usd = 120_000;
        market.oi_short_usd = 80_000;
        // Reasonable liquidity so borrowing utilization is < 1.
        market.liquidity_usd = 1_000_000;

        market.long_asset = long_asset;
        market.short_asset = short_asset;

        let m_before1 = executor
            .state
            .markets
            .get(&market_id)
            .unwrap()
            .clone();

        // STEP 1: first short increase at t1
        // open short for 20k notional
        let mut order1 = Order {
            account,
            market_id,
            side: Side::Short,
            collateral_token,
            size_delta_usd: 20_000,
            order_type: OrderType::Increase,
            withdraw_collateral_amount: 0,
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
            .expect("position must exist after first execution");

        // Size in USD should equal 20k (from 0)
        assert_eq!(
            pos_after1.size_usd, 20_000,
            "after first increase, size_usd should be 20k"
        );
        // Some tokens must be opened
        assert!(
            pos_after1.size_tokens > 0,
            "short position must have non-zero size_tokens"
        );

        // Market OI: short side increases by 20k, long remains unchanged.
        let m_after1 = executor.state.markets.get(&market_id).unwrap();
        assert_eq!(
            m_after1.oi_long_usd, m_before1.oi_long_usd,
            "long OI must not change for short increase"
        );
        assert_eq!(
            m_after1.oi_short_usd,
            m_before1.oi_short_usd + 20_000,
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
            pos_after1.funding_index,
            m_after1.funding.cumulative_index_short,
            "short position funding_index must match market short index after step 1"
        );
        assert_eq!(
            pos_after1.borrowing_index,
            m_after1.borrowing.cumulative_factor,
            "position borrowing_index must match market borrowing factor after step 1"
        );

        assert!(
            pos_after1.collateral_amount > 0,
            "collateral must remain positive after first step"
        );

        // Pool must receive position fee tokens for the collateral asset.
        let fee_pool_after1 = executor
            .state
            .pool_balances
            .get_fee_for_pool(market_id, collateral_token);
        assert!(
            fee_pool_after1 > 0,
            "after first step, pool must have some fee tokens from position fees"
        );
    }
}
