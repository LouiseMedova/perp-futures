use super::helpers::*;

use primitive_types::{U256, U512};

use crate::executor::Executor;
use crate::math::{signed_add, signed_sub};
use crate::services::open_interest::OpenInterestService;
use crate::services::price_impact::ImpactRebalanceConfig;
use crate::services::pricing;
use crate::services::pricing::PricingService;
use crate::services::{BasicServicesBundle, ServicesBundle};
use crate::state::{MarketState, PositionKey, State};
use crate::types::{
    AccountId, AssetId, MarketId, OraclePrices, Order, OrderId, OrderType, Side, SignedU256,
    Timestamp, TokenAmount, Usd,
};

fn borrow_index_scale() -> U256 {
    U256::exp10(18)
}

fn funding_index_scale() -> U256 {
    U256::exp10(18)
}

const INCREASE_FEE_BPS: u32 = 10;
const HELPFUL_REBATE_PERCENT: u32 = 20;

const SECONDS_PER_DAY: u64 = 86_400;
const BASE_RATE_PER_DAY_BPS: u64 = 1;
const SLOPE_PER_DAY_BPS: u64 = 9;

// Funding: 1 bp/day
const DAILY_RATE_BPS: u64 = 1;

fn rate_fp_per_sec() -> U256 {
    // SCALE * (DAILY_RATE_BPS / 10_000) / 86_400
    // Integer math, MVP-style.
    let scale = funding_index_scale();
    (scale / U256::from(SECONDS_PER_DAY)) * U256::from(DAILY_RATE_BPS) / U256::from(10_000u64)
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

fn bps_per_day_to_fp_per_sec(bps_per_day: u64) -> U256 {
    let den = U256::from(10_000u64 * SECONDS_PER_DAY);
    mul_div_u256(U256::from(bps_per_day), borrow_index_scale(), den).unwrap()
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

    // Price: 1 ETH = 2981 USD (with small +-1 spread per token, like you had)
    let eth_usd_per_eth_token: U256 = usd(2_981);
    let eth_min_token = eth_usd_per_eth_token.saturating_sub(usd(1));
    let eth_max_token = eth_usd_per_eth_token.checked_add(usd(1)).unwrap();

    let (index_price_min, index_price_max) =
        normalize_price_per_atom(eth_min_token, eth_max_token, eth_decimals);

    // Collateral $1
    let (collateral_price_min, collateral_price_max) =
        normalize_price_per_atom(usd(1), usd(1), collateral_decimals);

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
    let t1: Timestamp = t0 + 60;
    let t2: Timestamp = t1 + 3600;

    // Market starts long-heavy
    let market = executor.state.markets.entry(market_id).or_insert_with(|| {
        let mut m = MarketState::default();
        m.id = market_id;
        m
    });

    market.oi_long_usd = usd(120_000);
    market.oi_short_usd = usd(80_000);
    market.liquidity_usd = usd(1_000_000);

    market.long_asset = long_asset;
    market.short_asset = short_asset;

    let m_before1 = executor.state.markets.get(&market_id).unwrap().clone();

    // STEP 1: open short (deposit 5000 USDC, 4x => 20k USD)
    let deposit_usdc_atoms = to_atoms(5_000, collateral_decimals);

    let order1 = Order {
        account,
        market_id,
        side: Side::Short,
        collateral_token,
        size_delta_usd: U256::zero(),
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
        .expect("step1 execute must succeed");
    assert!(
        executor.state.orders.get(order1_id).is_none(),
        "order1 must be removed"
    );

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
        .expect("pos exists")
        .clone();

    let expected_size_delta1 = order1
        .collateral_delta_tokens
        .checked_mul(oracle_prices.collateral_price_min)
        .expect("collateral_usd overflow")
        .checked_mul(U256::from(order1.target_leverage_x))
        .expect("size_delta_usd overflow");

    assert_eq!(expected_size_delta1, usd(20_000));
    assert_eq!(pos_after1.size_usd, expected_size_delta1);
    assert!(!pos_after1.size_tokens.is_zero());

    let m_after1 = executor.state.markets.get(&market_id).unwrap().clone();
    assert_eq!(m_after1.oi_long_usd, m_before1.oi_long_usd);
    assert_eq!(
        m_after1.oi_short_usd,
        m_before1.oi_short_usd + expected_size_delta1
    );

    assert_eq!(m_after1.funding.last_updated_at, t1);
    assert_eq!(m_after1.borrowing.last_updated_at, t1);

    assert_eq!(
        pos_after1.funding_index,
        m_after1.funding.cumulative_index_short
    );
    assert_eq!(
        pos_after1.borrowing_index,
        m_after1.borrowing.cumulative_factor
    );

    let fee_pool_after1 = executor
        .state
        .pool_balances
        .get_fee_for_pool(market_id, collateral_token);
    assert!(!fee_pool_after1.is_zero());

    let deposit1 = order1.collateral_delta_tokens;
    let collat_after1 = pos_after1.collateral_amount;
    assert!(deposit1 > collat_after1);

    let spent_tokens_step1 = deposit1 - collat_after1;
    assert_eq!(spent_tokens_step1, fee_pool_after1);

    let claim_long_after1 = executor.state.claimables.get_funding(account, long_asset);
    let claim_short_after1 = executor.state.claimables.get_funding(account, short_asset);
    assert_eq!(claim_long_after1, U256::zero());
    assert_eq!(claim_short_after1, U256::zero());

    // STEP 2: after 1 hour, increase same short by deposit 1000 USDC, 4x => 4k USD
    let collateral_delta_tokens2 = to_atoms(1_000, collateral_decimals);

    let order2 = Order {
        account,
        market_id,
        side: Side::Short,
        collateral_token,
        size_delta_usd: U256::zero(),
        collateral_delta_tokens: collateral_delta_tokens2,
        target_leverage_x: 4,
        order_type: OrderType::Increase,
        withdraw_collateral_amount: U256::zero(),
        created_at: t2,
        valid_from: t2 - 30,
        valid_until: t2 + 300,
    };

    let order2_id: OrderId = executor.submit_order(order2.clone());

    let pos_before2 = pos_after1.clone();
    let m_before2 = executor.state.markets.get(&market_id).unwrap().clone();
    let fee_pool_before2 = fee_pool_after1;

    executor
        .execute_order(t2, order2_id)
        .expect("step2 execute must succeed");

    let pos_after2 = executor
        .state
        .positions
        .get(&pos_key)
        .expect("pos exists")
        .clone();
    let m_after2 = executor.state.markets.get(&market_id).unwrap().clone();
    let fee_pool_after2 = executor
        .state
        .pool_balances
        .get_fee_for_pool(market_id, collateral_token);

    let collateral_usd2: Usd = collateral_delta_tokens2 * oracle_prices.collateral_price_min;
    let expected_size_delta_usd2: Usd = collateral_usd2 * U256::from(4u32);

    assert_eq!(
        pos_after2.size_usd,
        pos_before2.size_usd + expected_size_delta_usd2
    );
    assert_eq!(m_after2.oi_long_usd, m_after1.oi_long_usd);
    assert_eq!(
        m_after2.oi_short_usd,
        m_after1.oi_short_usd + expected_size_delta_usd2
    );

    // Funding evolution t1..t2 (long-heavy => long index +, short index -)
    let dt2 = t2 - m_before2.funding.last_updated_at;
    assert_eq!(m_before2.funding.last_updated_at, t1);

    let delta_index_funding_fp = rate_fp_per_sec() * U256::from(dt2);

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
        m_after2.funding.cumulative_index_long,
        expected_funding_index_long_after2
    );
    assert_eq!(
        m_after2.funding.cumulative_index_short,
        expected_funding_index_short_after2
    );
    assert_eq!(
        pos_after2.funding_index,
        m_after2.funding.cumulative_index_short
    );

    // funding_usd2 sign sanity (short receives funding => negative cost)
    let delta_idx_funding2 = signed_sub(
        expected_funding_index_short_after2,
        pos_before2.funding_index,
    );
    let expected_funding_usd2: SignedU256 = signed_mul_div_u256(
        pos_before2.size_usd,
        delta_idx_funding2,
        funding_index_scale(),
    )
    .expect("signed mul/div");
    assert!(expected_funding_usd2.is_negative && !expected_funding_usd2.mag.is_zero());

    // Borrowing evolution t1..t2
    let dt2_borrow = t2 - m_before2.borrowing.last_updated_at;
    assert_eq!(m_before2.borrowing.last_updated_at, t1);

    let borrowed2 = m_before2.oi_long_usd + m_before2.oi_short_usd;
    let liquidity2 = m_before2.liquidity_usd;
    assert!(!liquidity2.is_zero());

    let scale = borrow_index_scale();
    let mut util_fp2 = mul_div_u256(borrowed2, scale, liquidity2).expect("util mul/div");
    if util_fp2 > scale {
        util_fp2 = scale;
    }

    let base_rate_fp_per_sec = bps_per_day_to_fp_per_sec(BASE_RATE_PER_DAY_BPS);
    let slope_fp_per_sec = bps_per_day_to_fp_per_sec(SLOPE_PER_DAY_BPS);
    let slope_term = mul_div_u256(slope_fp_per_sec, util_fp2, scale).expect("slope_term");
    let rate_per_sec_fp2 = base_rate_fp_per_sec + slope_term;

    let delta_index_borrow_fp = rate_per_sec_fp2 * U256::from(dt2_borrow);
    let expected_borrow_factor_after2 =
        m_before2.borrowing.cumulative_factor + delta_index_borrow_fp;

    assert_eq!(
        m_after2.borrowing.cumulative_factor,
        expected_borrow_factor_after2
    );
    assert_eq!(
        pos_after2.borrowing_index,
        m_after2.borrowing.cumulative_factor
    );

    let delta_idx_borrow2 = expected_borrow_factor_after2 - pos_before2.borrowing_index;
    let expected_borrowing_usd2 = mul_div_u256(
        pos_before2.size_usd,
        delta_idx_borrow2,
        borrow_index_scale(),
    )
    .expect("borrow fee");

    assert!(!expected_borrowing_usd2.is_zero());

    // Trading fee expectation for second order (helpful rebate)
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
        .expect("pricing must succeed");

    assert!(exec_expected2.balance_was_improved);

    let mut effective_bps2: u32 = INCREASE_FEE_BPS;
    effective_bps2 = effective_bps2.saturating_mul(100 - HELPFUL_REBATE_PERCENT) / 100;

    let expected_trading_fee_usd2 =
        expected_size_delta_usd2 * U256::from(effective_bps2) / U256::from(10_000u32);
    let expected_trading_fee_tokens2: TokenAmount =
        expected_trading_fee_usd2 / oracle_prices.collateral_price_min;

    // Pool fee bucket increment should be trading + borrowing (funding goes to claimables)
    let delta_fee_pool2 = fee_pool_after2 - fee_pool_before2;
    let expected_borrowing_tokens2: TokenAmount =
        expected_borrowing_usd2 / oracle_prices.collateral_price_min;

    assert_eq!(
        delta_fee_pool2,
        expected_trading_fee_tokens2 + expected_borrowing_tokens2
    );

    // Collateral after step2 should reflect: +deposit2 - (trading+borrowing)
    let expected_total_usd2: Usd = expected_borrowing_usd2 + expected_trading_fee_usd2;
    let expected_total_tokens2: TokenAmount =
        expected_total_usd2 / oracle_prices.collateral_price_min;

    let expected_collateral_after2 =
        pos_before2.collateral_amount + collateral_delta_tokens2 - expected_total_tokens2;

    assert_eq!(pos_after2.collateral_amount, expected_collateral_after2);
}
