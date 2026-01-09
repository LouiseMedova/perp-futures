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
use crate::types::{OraclePrices, Order, OrderType, Side, SignedU256, Timestamp};

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

fn calc_long_base_pnl_usd(
    pos_size_tokens: U256,
    entry_usd: U256,
    close_px_usd_per_atom: U256,
) -> SignedU256 {
    let value = pos_size_tokens * close_px_usd_per_atom;
    if value >= entry_usd {
        SignedU256::pos(value - entry_usd)
    } else {
        SignedU256::neg(entry_usd - value)
    }
}

/// Convert signed impact tokens -> signed USD, conservative:
/// +tokens => * index_price_min
/// -tokens => * index_price_max
fn impact_tokens_to_usd_conservative_local(
    tokens: SignedU256,
    prices: &OraclePrices,
) -> Result<SignedU256, String> {
    if tokens.mag.is_zero() {
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

    // ---------------------------------------------------------------------
    // Recompute expected pending impact from the OPEN (Increase) pricing.
    // ---------------------------------------------------------------------

    // Pre-open OI = post-open OI minus the opened size (for a long increase).
    let pre_oi_long = m_before
        .oi_long_usd
        .checked_sub(pos_before.size_usd)
        .expect("pre_oi_long underflow");
    let pre_oi_short = m_before.oi_short_usd;

    let oi_inc = env.executor.services.open_interest().for_increase(
        pre_oi_long,
        pre_oi_short,
        pos_before.size_usd,
        Side::Long,
    );
    let impact_cfg = ImpactRebalanceConfig::default_quadratic();
    let prices_open = env
        .executor
        .oracle
        .validate_and_get_prices(env.market_id)
        .expect("oracle prices open");
    let exec_inc = env
        .executor
        .services
        .pricing()
        .get_execution_price(
            env.executor.services.price_impact(),
            ExecutionPriceParams {
                oi: &oi_inc,
                impact_cfg: &impact_cfg,
                side: Side::Long,
                size_delta_usd: pos_before.size_usd,
                direction: pricing::TradeDirection::Increase,
                prices: prices_open,
            },
        )
        .expect("pricing increase");

    assert_eq!(
        pos_before.pending_impact_tokens, exec_inc.price_impact_amount_tokens,
        "position.pending_impact_usd must match pricing-derived pending impact at open"
    );

    // ---------------------------------------------------------------------
    // Move price up: 3000 -> 3300 (profit for long).
    // ---------------------------------------------------------------------
    set_index_price_usd_per_token(&mut env.executor, 3_300, env.index_decimals);

    let prices_close = env
        .executor
        .oracle
        .validate_and_get_prices(env.market_id)
        .expect("oracle prices close");

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

    // ---------------------------------------------------------------------
    // EXPECTED INDICES + STEP COSTS (funding/borrowing/trading)
    // ---------------------------------------------------------------------

    // Funding index update t1 -> t2
    let dt_funding = t2 - m_before.funding.last_updated_at;
    assert_eq!(
        m_before.funding.last_updated_at, t1,
        "market funding must have been updated at open"
    );

    let delta_funding_fp = funding_rate_fp_per_sec() * U256::from(dt_funding);

    let expected_funding_long_after = crate::math::signed_add(
        m_before.funding.cumulative_index_long,
        SignedU256 {
            is_negative: false,
            mag: delta_funding_fp,
        },
    );
    let expected_funding_short_after = crate::math::signed_sub(
        m_before.funding.cumulative_index_short,
        SignedU256 {
            is_negative: false,
            mag: delta_funding_fp,
        },
    );

    // Funding cost tokens (long-heavy => long pays funding)
    let funding_cost_usd =
        mul_div_u256(pos_before.size_usd, delta_funding_fp, funding_index_scale())
            .expect("funding mul/div");
    let funding_cost_tokens = funding_cost_usd / prices_close.collateral_price_min;

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
    let expected_borrowing_tokens = expected_borrowing_usd / prices_close.collateral_price_min;

    // Trading fee: determine helpfulness via pricing for DECREASE (so it matches engine behavior)
    let oi_dec = env.executor.services.open_interest().for_decrease(
        m_before.oi_long_usd,
        m_before.oi_short_usd,
        close_order.size_delta_usd,
        close_order.side,
    );

    let exec_dec = env
        .executor
        .services
        .pricing()
        .get_execution_price(
            env.executor.services.price_impact(),
            ExecutionPriceParams {
                oi: &oi_dec,
                impact_cfg: &impact_cfg,
                side: close_order.side,
                size_delta_usd: close_order.size_delta_usd,
                direction: pricing::TradeDirection::Decrease,
                prices: prices_close,
            },
        )
        .expect("pricing decrease");

    let mut expected_bps = DECREASE_FEE_BPS;
    if exec_dec.balance_was_improved {
        expected_bps = expected_bps.saturating_mul(100 - HELPFUL_REBATE_PERCENT) / 100;
    }

    let trading_fee_usd = mul_div_u256(
        pos_before.size_usd,
        U256::from(expected_bps),
        U256::from(10_000u32),
    )
    .expect("trading fee mul/div");
    let trading_fee_tokens = trading_fee_usd / prices_close.collateral_price_min;

    let expected_fee_pool_delta = trading_fee_tokens + expected_borrowing_tokens;

    // ---------------------------------------------------------------------
    // EXPECTED realized PnL TOKENS INCLUDING pending impact TOKENS
    // ---------------------------------------------------------------------

    // Base pnl in USD -> convert to collateral tokens with engine rounding
    let base_pnl_usd: SignedU256 = calc_long_base_pnl_usd(
        pos_before.size_tokens,
        pos_before.size_usd,
        prices_close.index_price_min,
    );

    // Stored pending impact (full close => realize 100%).
    let pending_impact_usd: SignedU256 =
        impact_tokens_to_usd_conservative_local(pos_before.pending_impact_tokens, &prices_close)
            .expect("pending impact tokens -> usd");

    let realized_total_usd = crate::math::signed_add(
        crate::math::signed_add(base_pnl_usd, pending_impact_usd),
        exec_dec.price_impact_usd,
    );

    let pnl_tokens_signed: SignedU256 =
        math::pnl::pnl_usd_to_collateral_tokens(realized_total_usd, &prices_close)
            .expect("total pnl usd->collateral tokens");
    // Costs are taken from collateral first

    let close_costs_tokens = trading_fee_tokens + expected_borrowing_tokens + funding_cost_tokens;

    assert!(
        pos_before.collateral_amount >= close_costs_tokens,
        "position must have enough collateral for close costs (else liquidation path)"
    );

    let mut expected_user_delta = pos_before.collateral_amount - close_costs_tokens;

    let (expected_pool_paid, expected_pool_received) = if pnl_tokens_signed.is_negative {
        // Loss / negative impact reduces user output; pool receives.
        expected_user_delta = expected_user_delta
            .checked_sub(pnl_tokens_signed.mag)
            .expect("loss must be covered in this scenario (else liquidation path)");
        (U256::zero(), pnl_tokens_signed.mag)
    } else {
        // Profit / positive impact increases user output; pool pays.
        expected_user_delta = expected_user_delta + pnl_tokens_signed.mag;
        (pnl_tokens_signed.mag, U256::zero())
    };

    // ---------------------------------------------------------------------
    // ACT: execute close and snapshot pool liquidity movement
    // ---------------------------------------------------------------------

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

    assert_eq!(
        delta_fee_pool, expected_fee_pool_delta,
        "fee bucket delta mismatch"
    );

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

    assert_eq!(user_delta, expected_user_delta, "user payout mismatch");
    assert!(
        !user_delta.is_zero(),
        "closing should credit some collateral payout into claimables.fees"
    );

    assert_eq!(pool_paid, expected_pool_paid, "pool payout mismatch");
    assert_eq!(pool_received, expected_pool_received, "pool receive mismatch");

}
