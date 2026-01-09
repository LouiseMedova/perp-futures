use crate::{
    executor::Executor,
    oracle::Oracle,
    services::BasicServicesBundle,
    state::{MarketState, PositionKey, State},
    types::{
        AccountId, AssetId, MarketId, OraclePrices, Order, OrderId, OrderType, Side, SignedU256,
        Timestamp,
    },
};
use primitive_types::{U256, U512};

/// USD fixed-point scale used across the project (1e30).
pub const USD_SCALE: u128 = 1_000_000_000_000_000_000_000_000_000_000; // 1e30

#[allow(dead_code)]
pub fn usd(x: u128) -> U256 {
    U256::from(x) * U256::exp10(30) // USD(1e30)
}

#[allow(dead_code)]
pub fn to_atoms(tokens: u128, decimals: u8) -> U256 {
    U256::from(tokens) * U256::exp10(decimals as usize)
}

#[allow(dead_code)]
pub fn div_ceil_u256(a: U256, b: U256) -> U256 {
    let q = a / b;
    let r = a % b;
    if r.is_zero() { q } else { q + 1 }
}

/// Normalize USD(1e30) per 1 whole token -> USD(1e30) per 1 atom (10^-decimals).
/// - min: floor
/// - max: ceil (conservative)
#[allow(dead_code)]
pub fn normalize_price_per_atom(
    price_min_per_token: U256,
    price_max_per_token: U256,
    decimals: u8,
) -> (U256, U256) {
    let scale = U256::exp10(decimals as usize);
    let min_atom = price_min_per_token / scale; // floor
    let max_atom = div_ceil_u256(price_max_per_token, scale); // ceil
    (min_atom, max_atom)
}

/// Common test environment bundle: executor + IDs + decimals.
#[derive(Clone)]
pub struct TestEnv {
    pub executor: Executor<BasicServicesBundle, TestOracle>,
    pub market_id: MarketId,

    pub collateral_token: AssetId,
    pub long_asset: AssetId,
    pub short_asset: AssetId,

    pub collateral_decimals: u8,
    pub index_decimals: u8,

    pub account_a: AccountId,
    pub account_b: AccountId,
}

impl TestEnv {
    /// Convenience for building a position key for account_a by default.
    #[allow(dead_code)]
    pub fn key_a(&self, side: Side) -> PositionKey {
        PositionKey {
            account: self.account_a,
            market_id: self.market_id,
            collateral_token: self.collateral_token,
            side,
        }
    }

    #[allow(dead_code)]
    pub fn key_b(&self, side: Side) -> PositionKey {
        PositionKey {
            account: self.account_b,
            market_id: self.market_id,
            collateral_token: self.collateral_token,
            side,
        }
    }
}

/// Create a fully initialized executor + market + seeded pool collateral.
/// `initial_index_price_usd_per_token` is a whole-token price, e.g. 3000 for ETH=$3000.
/// Collateral assumed to be $1 (USDC-like) by default.
///
/// Notes:
/// - We seed `market.liquidity_usd` for borrowing utilization.
/// - We also seed actual pool collateral tokens via `pool_balances.add_to_pool`
///   so profit payout paths have liquidity.
#[allow(dead_code)]
pub fn setup_env(initial_index_price_usd_per_token: u128) -> TestEnv {
    let market_id: MarketId = MarketId(1);

    let collateral_token: AssetId = AssetId(10); // e.g. USDC
    let long_asset: AssetId = AssetId(11);
    let short_asset = collateral_token;

    // Typical decimals:
    let collateral_decimals: u8 = 6; // USDC
    let index_decimals: u8 = 18; // ETH/BTC-like

    // Index price per atom
    let (index_price_min, index_price_max) = normalize_price_per_atom(
        usd(initial_index_price_usd_per_token),
        usd(initial_index_price_usd_per_token),
        index_decimals,
    );

    // Collateral $1 per token -> per atom
    let (collateral_price_min, collateral_price_max) =
        normalize_price_per_atom(usd(1), usd(1), collateral_decimals);

    let prices = OraclePrices {
        index_price_min,
        index_price_max,
        collateral_price_min,
        collateral_price_max,
    };

    let oracle = TestOracle { prices };
    let services = BasicServicesBundle::default();

    let mut executor: Executor<BasicServicesBundle, TestOracle> =
        Executor::new(State::default(), services, oracle);

    // Ensure market exists with required fields for pricing/funding/borrowing.
    let m = executor.state.markets.entry(market_id).or_insert_with(|| {
        let mut mm = MarketState::default();
        mm.id = market_id;
        mm
    });

    m.long_asset = long_asset;
    m.short_asset = short_asset;

    // Borrowing / utilization sanity
    m.liquidity_usd = usd(5_000_000);
    m.oi_long_usd = U256::zero();
    m.oi_short_usd = U256::zero();

    // Seed pool liquidity in collateral tokens (needed for profit payout paths).
    let seed_collateral_atoms = to_atoms(5_000_000, collateral_decimals);
    executor
        .state
        .pool_balances
        .add_to_pool(market_id, collateral_token, seed_collateral_atoms);

    let account_a: AccountId = AccountId([1; 32]);
    let account_b: AccountId = AccountId([2; 32]);

    TestEnv {
        executor,
        market_id,
        collateral_token,
        long_asset,
        short_asset,
        collateral_decimals,
        index_decimals,
        account_a,
        account_b,
    }
}

/// A simple oracle with a mutable `prices` field.
/// In tests you will do: `executor.oracle.prices = new_prices;`.
#[derive(Clone, Copy, Debug)]
pub struct TestOracle {
    pub prices: OraclePrices,
}

impl Oracle for TestOracle {
    fn validate_and_get_prices(&self, _market_id: MarketId) -> Result<OraclePrices, String> {
        Ok(self.prices)
    }
}

/// Set index price by providing **whole-token** USD price (e.g. 6000 for $6000/ETH).
/// Internally it will be converted to USD(1e30) per atom (10^-index_decimals).
#[allow(dead_code)]
pub fn set_index_price_usd_per_token(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    usd_per_token: u128,
    index_decimals: u8,
) {
    let (px_atom, _) =
        normalize_price_per_atom(usd(usd_per_token), usd(usd_per_token), index_decimals);
    set_index_price_atom(executor, px_atom);
}

/// Set index price directly in **USD(1e30) per atom**.
/// Useful for liquidation threshold tests where `liq_price` is already in these units.
#[allow(dead_code)]
pub fn set_index_price_atom(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    px_atom: U256,
) {
    let mut p = executor.oracle.prices;
    p.index_price_min = px_atom;
    p.index_price_max = px_atom;
    executor.oracle.prices = p;
}

/// Submit an order and execute it. Asserts that the order is removed after successful execution.
#[allow(dead_code)]
pub fn submit_and_execute(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    now: Timestamp,
    order: Order,
) -> OrderId {
    let id: OrderId = executor.submit_order(order);
    executor
        .execute_order(now, id)
        .expect("execute_order must succeed");
    assert!(
        executor.state.orders.get(id).is_none(),
        "order must be removed after Ok execution"
    );
    id
}

/// Open (increase) a position by depositing collateral and specifying leverage.
/// Returns a `PositionKey` for convenience.
#[allow(dead_code)]
pub fn open_position(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    now: Timestamp,
    account: AccountId,
    market_id: MarketId,
    side: Side,
    collateral_token: AssetId,
    collateral_deposit_tokens: u128,
    collateral_decimals: u8,
    leverage_x: u32,
) -> PositionKey {
    let deposit_atoms = to_atoms(collateral_deposit_tokens, collateral_decimals);

    let order = Order {
        account,
        market_id,
        side,
        collateral_token,
        size_delta_usd: U256::zero(), // derived inside increase
        collateral_delta_tokens: deposit_atoms,
        target_leverage_x: leverage_x,
        order_type: OrderType::Increase,
        withdraw_collateral_amount: U256::zero(),
        created_at: now,
        valid_from: now.saturating_sub(1),
        valid_until: now + 300,
    };

    submit_and_execute(executor, now, order);

    PositionKey {
        account,
        market_id,
        collateral_token,
        side,
    }
}

/// Execute a full-close decrease (size_delta_usd = current pos.size_usd).
#[allow(dead_code)]
pub fn close_position_full(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    now: Timestamp,
    key: PositionKey,
) {
    let pos = executor
        .state
        .positions
        .get(&key)
        .expect("position must exist before full close")
        .clone();

    let order = Order {
        account: key.account,
        market_id: key.market_id,
        side: key.side,
        collateral_token: key.collateral_token,
        size_delta_usd: pos.size_usd,
        collateral_delta_tokens: U256::zero(),
        target_leverage_x: 1, // unused for decrease, but required by struct
        order_type: OrderType::Decrease,
        withdraw_collateral_amount: U256::zero(),
        created_at: now,
        valid_from: now.saturating_sub(1),
        valid_until: now + 300,
    };

    submit_and_execute(executor, now, order);
}

/// Execute a partial decrease with optional withdraw.
/// - `size_delta_usd` is clamped inside executor to position size.
/// - `withdraw_tokens` is clamped inside executor to available collateral.
#[allow(dead_code)]
pub fn close_position_partial_with_withdraw(
    executor: &mut Executor<BasicServicesBundle, TestOracle>,
    now: Timestamp,
    key: PositionKey,
    size_delta_usd: U256,
    withdraw_tokens: U256,
) {
    let order = Order {
        account: key.account,
        market_id: key.market_id,
        side: key.side,
        collateral_token: key.collateral_token,
        size_delta_usd,
        collateral_delta_tokens: U256::zero(),
        target_leverage_x: 1,
        order_type: OrderType::Decrease,
        withdraw_collateral_amount: withdraw_tokens,
        created_at: now,
        valid_from: now.saturating_sub(1),
        valid_until: now + 300,
    };

    submit_and_execute(executor, now, order);
}

/// Convenience: assert that a position does not exist.
#[allow(dead_code)]
pub fn assert_position_removed(
    executor: &Executor<BasicServicesBundle, TestOracle>,
    key: &PositionKey,
) {
    assert!(
        executor.state.positions.get(key).is_none(),
        "expected position to be removed; key={key:?}"
    );
}

/// Convenience: fetch and clone a position (panic if missing).
#[allow(dead_code)]
pub fn get_position(
    executor: &Executor<BasicServicesBundle, TestOracle>,
    key: &PositionKey,
) -> crate::state::Position {
    executor
        .state
        .positions
        .get(key)
        .expect("position must exist")
        .clone()
}

pub fn fee_claimable(cl: &crate::state::Claimables, acc: AccountId, asset: AssetId) -> U256 {
    cl.get_fee(acc, asset)
}
pub fn funding_claimable(cl: &crate::state::Claimables, acc: AccountId, asset: AssetId) -> U256 {
    cl.get_funding(acc, asset)
}

pub fn mul_div_u256(a: U256, b: U256, den: U256) -> Result<U256, String> {
    if den.is_zero() {
        return Err("mul_div_den_zero".into());
    }
    let prod = U512::from(a) * U512::from(b);
    let q = prod / U512::from(den);
    u512_to_u256_checked(q)
}

fn u512_to_u256_checked(x: U512) -> Result<U256, String> {
    let be = x.to_big_endian();

    if be[..32].iter().any(|&b| b != 0) {
        return Err("mul_div_overflow".into());
    }

    Ok(U256::from_big_endian(&be[32..]))
}

pub fn u256_abs_diff(a: U256, b: U256) -> U256 {
    if a >= b { a - b } else { b - a }
}

/// Convert signed impact tokens -> signed USD, conservative:
/// +tokens => * index_price_min
/// -tokens => * index_price_max
pub fn impact_tokens_to_usd_conservative(
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
