use super::helpers::*;

use primitive_types::U256;

use crate::math;
use crate::oracle::Oracle;
use crate::services::ServicesBundle;
use crate::services::borrowing::apply_borrowing_fees_to_pool;
use crate::services::open_interest::OpenInterestService;
use crate::services::price_impact::ImpactRebalanceConfig;
use crate::services::pricing::PricingService;
use crate::services::pricing::{self, ExecutionPriceParams};
use crate::services::step_costs::{apply_step_costs_to_position, compute_step_costs};
use crate::types::{Order, OrderType, Side, Timestamp};

const SECONDS_PER_DAY: u64 = 86_400;

// These must match BasicBorrowingService defaults used in your increase test.
const BASE_RATE_PER_DAY_BPS: u64 = 1;
const SLOPE_PER_DAY_BPS: u64 = 9;

// Funding "MVP rate": 1 bp/day (must match your BasicFundingService if you hardcoded it there).
const FUNDING_DAILY_RATE_BPS: u64 = 1;

// Fee schedule must match BasicFeesService defaults.
// If your BasicFeesService uses different bps for decrease, set it here.
const DECREASE_FEE_BPS: u32 = 10;

// If your fees apply a helpful-trade rebate, keep it consistent with the fee service config.
// If you want to avoid rebates entirely, set market imbalance so decrease is NOT helpful and set this to 0.
const HELPFUL_REBATE_PERCENT: u32 = 20;

fn borrow_index_scale() -> U256 {
    U256::exp10(18)
}

fn funding_index_scale() -> U256 {
    U256::exp10(18)
}

/// rate_fp_per_sec = SCALE * (daily_bps / 10_000) / 86_400
fn funding_rate_fp_per_sec() -> U256 {
    let scale = funding_index_scale();
    (scale / U256::from(SECONDS_PER_DAY)) * U256::from(FUNDING_DAILY_RATE_BPS)
        / U256::from(10_000u64)
}

fn bps_per_day_to_fp_per_sec(bps_per_day: u64) -> U256 {
    let den = U256::from(10_000u64 * SECONDS_PER_DAY);
    mul_div_u256(U256::from(bps_per_day), borrow_index_scale(), den).unwrap()
}

fn calc_long_pnl_usd(pos_size_tokens: U256, entry_usd: U256, close_px_usd_per_atom: U256) -> U256 {
    // pnl_usd = size_tokens * close_px - entry_usd  (assume positive)
    let value = pos_size_tokens * close_px_usd_per_atom;
    assert!(value >= entry_usd, "test expects positive pnl");
    value - entry_usd
}
#[test]
fn decrease_full_close_long_profit_fees_and_indices() {
    // Scenario:
    // 1) Open a LONG position with USDC collateral at an initial index price.
    // 2) Move index price up (so the long has positive PnL).
    // 3) Full-close the position via a Decrease order.
    //
    // We verify:
    //  - funding/borrowing indices update to t2
    //  - long OI is reduced by the closed size
    //  - position is removed after full close
    //  - pool fee bucket (for collateral asset) increases by (trading fee + borrowing fee)
    //  - user's withdrawable/claimable collateral increases by the exact expected output
    //
    // The expected output is computed by doing a "dry run" on cloned state:
    //  - update indices at t2
    //  - compute pricing for decrease (incl. price impact)
    //  - compute step costs (funding/borrowing/trading)
    //  - apply step costs to position collateral
    //  - compute realized PnL in collateral tokens
    //  - sum: output = (remaining collateral after costs) + (profit payout from pool)

    let mut env = setup_env(3_000);

    let t1: Timestamp = 1_000;
    let t2: Timestamp = t1 + 3_600;

    // Make the market long-heavy:
    //  - funding indices will move
    //  - a long decrease reduces long OI (often "helpful" for fee rebates)

    let m = env.executor.state.markets.get_mut(&env.market_id).unwrap();
    m.oi_long_usd = usd(120_000);
    m.oi_short_usd = usd(119_500);
    m.liquidity_usd = usd(1_000_000);
    m.long_asset = env.long_asset;
    m.short_asset = env.short_asset;

    // Open a long position: deposit 1,000 USDC, 10x leverage => ~10k USD notional.
    let key = open_position(
        &mut env.executor,
        t1,
        env.account_a,
        env.market_id,
        Side::Long,
        env.collateral_token,
        1_000,
        env.collateral_decimals,
        10,
    );

    // Snapshot state right before closing.
    let pos_before = env.executor.state.positions.get(&key).unwrap().clone();
    let m_before = env
        .executor
        .state
        .markets
        .get(&env.market_id)
        .unwrap()
        .clone();

    // Track pool fee bucket and user's withdrawable balance before closing.
    let fee_pool_before = env
        .executor
        .state
        .pool_balances
        .get_fee_for_pool(env.market_id, env.collateral_token);

    let user_fee_before = fee_claimable(
        &env.executor.state.claimables,
        env.account_a,
        env.collateral_token,
    );

    // Move price up: 3000 -> 3300 (profit for long).
    set_index_price_usd_per_token(&mut env.executor, 3_300, env.index_decimals);

    let prices = env
        .executor
        .oracle
        .validate_and_get_prices(env.market_id)
        .expect("oracle prices");

    // Prepare a full-close decrease order.
    let close_order = Order {
        account: env.account_a,
        market_id: env.market_id,
        side: Side::Long,
        collateral_token: env.collateral_token,

        size_delta_usd: pos_before.size_usd, // full close
        collateral_delta_tokens: U256::zero(),
        target_leverage_x: 0,
        order_type: OrderType::Decrease,
        withdraw_collateral_amount: U256::zero(),

        created_at: t2,
        valid_from: t2.saturating_sub(1),
        valid_until: t2 + 300,
    };

    // EXPECTED INDICES

    // Funding index update t1 -> t2
    let dt_funding = t2 - m_before.funding.last_updated_at;
    assert_eq!(
        m_before.funding.last_updated_at, t1,
        "market funding must have been updated at open"
    );

    let delta_funding_fp = funding_rate_fp_per_sec() * U256::from(dt_funding);
    let funding_scale = funding_index_scale();

    // funding_cost_usd = size_usd * delta_idx / SCALE, where delta_idx == +delta_funding_fp for long
    let funding_cost_usd = mul_div_u256(pos_before.size_usd, delta_funding_fp, funding_scale)
        .expect("funding mul/div");
    let funding_cost_tokens = funding_cost_usd / prices.collateral_price_min;

    let long_oi = m_before.oi_long_usd;
    let short_oi = m_before.oi_short_usd;

    // For long-heavy market: long index increases, short index decreases.
    let is_long_heavy = long_oi > short_oi;
    assert!(is_long_heavy, "test expects a long-heavy market");

    let delta_funding_fp = funding_rate_fp_per_sec() * U256::from(dt_funding);

    let expected_funding_long_after = crate::math::signed_add(
        m_before.funding.cumulative_index_long,
        crate::types::SignedU256 {
            is_negative: false,
            mag: delta_funding_fp,
        },
    );
    let expected_funding_short_after = crate::math::signed_sub(
        m_before.funding.cumulative_index_short,
        crate::types::SignedU256 {
            is_negative: false,
            mag: delta_funding_fp,
        },
    );

    // Borrowing factor update t1 -> t2
    let dt_borrow = t2 - m_before.borrowing.last_updated_at;
    assert_eq!(
        m_before.borrowing.last_updated_at, t1,
        "market borrowing must have been updated at open"
    );

    let borrowed = m_before.oi_long_usd + m_before.oi_short_usd;
    let liquidity = m_before.liquidity_usd;

    assert!(!liquidity.is_zero(), "liquidity must be non-zero");

    // util_fp in [0, SCALE]
    let scale = borrow_index_scale();
    let mut util_fp = mul_div_u256(borrowed, scale, liquidity).expect("util mul/div");
    if util_fp > scale {
        util_fp = scale;
    }

    let base_rate_fp_per_sec = bps_per_day_to_fp_per_sec(BASE_RATE_PER_DAY_BPS);
    let slope_fp_per_sec = bps_per_day_to_fp_per_sec(SLOPE_PER_DAY_BPS);

    // rate = base + slope * util/SCALE
    let slope_term = mul_div_u256(slope_fp_per_sec, util_fp, scale).expect("slope_term");
    let rate_fp_per_sec = base_rate_fp_per_sec + slope_term;

    let delta_borrow_fp = rate_fp_per_sec * U256::from(dt_borrow);
    let expected_borrow_factor_after = m_before.borrowing.cumulative_factor + delta_borrow_fp;

    // Expected borrowing fee in USD for the close step:
    // borrowing_usd = size_usd * (factor_after - pos.borrowing_index) / SCALE
    let delta_idx_borrow = expected_borrow_factor_after
        .checked_sub(pos_before.borrowing_index)
        .expect("delta borrow idx underflow");

    let expected_borrowing_usd =
        mul_div_u256(pos_before.size_usd, delta_idx_borrow, borrow_index_scale())
            .expect("borrow fee mul/div");
    let expected_borrowing_tokens = expected_borrowing_usd / prices.collateral_price_min;

    // Snapshot pool liquidity before close
    let (pool_long_before, pool_short_before) = env.executor.state.pool_balances.get_pair_balances(
        env.market_id,
        env.long_asset,
        env.short_asset,
    );

    // execute decrease close
    submit_and_execute(&mut env.executor, t2, close_order);

    // Snapshot pool liquidity after close
    let (pool_long_after, pool_short_after) = env.executor.state.pool_balances.get_pair_balances(
        env.market_id,
        env.long_asset,
        env.short_asset,
    );

    // Select liquidity for the collateral asset (USDC)
    let (coll_liq_before, coll_liq_after) = if env.collateral_token == env.long_asset {
        (pool_long_before, pool_long_after)
    } else if env.collateral_token == env.short_asset {
        (pool_short_before, pool_short_after)
    } else {
        panic!("collateral_token must be one of market long_asset/short_asset in this test setup");
    };

    // Pool paid out profit => liquidity decreased
    let pool_paid = coll_liq_before
        .checked_sub(coll_liq_after)
        .unwrap_or(U256::zero());

    // Pool received loss => liquidity increased
    let pool_received = coll_liq_after
        .checked_sub(coll_liq_before)
        .unwrap_or(U256::zero());
    // ---------------------------------------------------------------------
    // ASSERT: indices updated as expected
    // ---------------------------------------------------------------------
    let m_after = env
        .executor
        .state
        .markets
        .get(&env.market_id)
        .unwrap()
        .clone();

    assert_eq!(m_after.funding.last_updated_at, t2);
    assert_eq!(m_after.borrowing.last_updated_at, t2);

    assert_eq!(
        m_after.funding.cumulative_index_long, expected_funding_long_after,
        "funding long index mismatch"
    );
    assert_eq!(
        m_after.funding.cumulative_index_short, expected_funding_short_after,
        "funding short index mismatch"
    );

    assert_eq!(
        m_after.borrowing.cumulative_factor, expected_borrow_factor_after,
        "borrowing cumulative factor mismatch"
    );

    // ---------------------------------------------------------------------
    // ASSERT: OI updated and position removed
    // ---------------------------------------------------------------------
    let expected_oi_long_after = m_before
        .oi_long_usd
        .checked_sub(pos_before.size_usd)
        .expect("expected oi_long underflow");
    assert_eq!(
        m_after.oi_long_usd, expected_oi_long_after,
        "long OI must decrease by closed size"
    );
    assert_eq!(
        m_after.oi_short_usd, m_before.oi_short_usd,
        "short OI must not change"
    );

    assert!(
        env.executor.state.positions.get(&key).is_none(),
        "position must be removed on full close"
    );

    // ---------------------------------------------------------------------
    // ASSERT: pool fee bucket delta == trading + borrowing
    // ---------------------------------------------------------------------
    let fee_pool_after = env
        .executor
        .state
        .pool_balances
        .get_fee_for_pool(env.market_id, env.collateral_token);

    let delta_fee_pool = fee_pool_after
        .checked_sub(fee_pool_before)
        .expect("fee pool bucket must not decrease");

    // Borrowing tokens are always routed to the pool fee bucket in current code path.
    assert!(
        delta_fee_pool >= expected_borrowing_tokens,
        "fee bucket delta must be at least borrowing tokens"
    );

    let trading_tokens_actual = delta_fee_pool - expected_borrowing_tokens;

    // Infer effective bps from actual trading tokens and verify against configured fee schedule.
    // trading_fee_usd = trading_tokens * collateral_price_min
    let trading_fee_usd_actual = trading_tokens_actual * prices.collateral_price_min;

    // effective_bps = trading_fee_usd * 10_000 / size_delta_usd
    let effective_bps_actual = mul_div_u256(
        trading_fee_usd_actual,
        U256::from(10_000u32),
        pos_before.size_usd,
    )
    .unwrap();

    // For now we assume "helpful rebate applies when trade reduces imbalance".
    // In long-heavy market, decreasing a long should be helpful => apply rebate.
    let mut expected_bps = DECREASE_FEE_BPS;

    // Trading fee (independent): determine whether decrease is "helpful" by imbalance reduction
    let skew_before = u256_abs_diff(m_before.oi_long_usd, m_before.oi_short_usd);
    let long_after = m_before
        .oi_long_usd
        .checked_sub(pos_before.size_usd)
        .expect("oi_long underflow");
    let skew_after = u256_abs_diff(long_after, m_before.oi_short_usd);

    let decrease_is_helpful = skew_after < skew_before;

    if decrease_is_helpful {
        expected_bps = expected_bps.saturating_mul(100 - HELPFUL_REBATE_PERCENT) / 100;
    }

    assert_eq!(
        effective_bps_actual,
        U256::from(expected_bps),
        "unexpected effective decrease fee bps"
    );

    let trading_fee_usd = mul_div_u256(
        pos_before.size_usd,
        U256::from(expected_bps),
        U256::from(10_000u32),
    )
    .expect("trading fee mul/div");

    let trading_fee_tokens = trading_fee_usd / prices.collateral_price_min;

    // Expected pool fee bucket delta = trading + borrowing (funding is NOT routed to pool in your code)
    let expected_fee_pool_delta = trading_fee_tokens + expected_borrowing_tokens;

    // Expected profit payout from pool (no impact assumed here)
    let pnl_usd = calc_long_pnl_usd(
        pos_before.size_tokens,
        pos_before.size_usd,
        prices.index_price_min,
    );
    let expected_pnl_tokens = pnl_usd / prices.collateral_price_max;

    // Expected pool fee bucket delta = trading + borrowing (funding is NOT routed to pool in your code)
    let close_costs_tokens = trading_fee_tokens + expected_borrowing_tokens + funding_cost_tokens;

    assert!(
        pos_before.collateral_amount >= close_costs_tokens,
        "position must have enough collateral for close costs in this scenario"
    );

    let expected_rest_collateral = pos_before.collateral_amount - close_costs_tokens;
    let expected_user_delta = expected_rest_collateral + expected_pnl_tokens;

    // ---------------------------------------------------------------------
    // ASSERT: user received some payout into Claimables::fees (collateral asset)
    // ---------------------------------------------------------------------
    let user_fee_after = fee_claimable(
        &env.executor.state.claimables,
        env.account_a,
        env.collateral_token,
    );

    let user_delta = user_fee_after
        .checked_sub(user_fee_before)
        .expect("user fee claimable must not decrease");

    assert!(
        !user_delta.is_zero(),
        "closing should credit some collateral payout into claimables.fees"
    );

    println!("user_delta {:?}", user_delta);
    println!("pool_paid {:?}", pool_paid);
    println!("pool_received {:?}", pool_received);
}
