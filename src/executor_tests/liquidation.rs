use super::helpers::*;

use crate::types::Side;
use primitive_types::U256;

#[test]
fn is_liquidatable_by_margin_long_crosses_threshold() {
    let mut env = setup_env(3_000);
    let t = 1_000;

    // High leverage: 500 USDC * 20x => ~10k notional
    let key = open_position(
        &mut env.executor,
        t,
        env.account_a,
        env.market_id,
        Side::Long,
        env.collateral_token,
        500,
        env.collateral_decimals,
        20,
    );

    let liq_price = env
        .executor
        .calculate_liquidation_price(t, key)
        .expect("liq price calc must succeed");
    assert!(liq_price > U256::zero(), "liq_price must be > 0");

    // ~1% band around liq price, in "per atom" units
    let margin = (liq_price / U256::from(100u8)) + U256::from(1u8);

    // Above => NOT liquidatable for long
    let above = liq_price + margin;
    set_index_price_atom(&mut env.executor, above);

    let preview = env
        .executor
        .is_liquidatable_by_margin(t, key)
        .expect("must return preview");
    assert!(
        !preview.is_liquidatable,
        "long should NOT be liquidatable above threshold; liq_price={liq_price}, px={above}, preview={preview:?}"
    );

    // Below => liquidatable for long
    let below = liq_price.saturating_sub(margin);
    set_index_price_atom(&mut env.executor, below);

    let preview = env
        .executor
        .is_liquidatable_by_margin(t, key)
        .expect("must return preview");
    assert!(
        preview.is_liquidatable,
        "long should be liquidatable below threshold; liq_price={liq_price}, px={below}, preview={preview:?}"
    );
}

#[test]
fn is_liquidatable_by_margin_short_crosses_threshold() {
    let mut env = setup_env(3_000);
    let t = 2_000;

    let key = open_position(
        &mut env.executor,
        t,
        env.account_a,
        env.market_id,
        Side::Short,
        env.collateral_token,
        500,
        env.collateral_decimals,
        20,
    );

    let liq_price = env
        .executor
        .calculate_liquidation_price(t, key)
        .expect("liq price calc must succeed");
    assert!(liq_price > U256::zero(), "liq_price must be > 0");

    let margin = (liq_price / U256::from(100u8)) + U256::from(1u8);

    // Below => NOT liquidatable for short
    let below = liq_price.saturating_sub(margin);
    set_index_price_atom(&mut env.executor, below);

    let preview = env
        .executor
        .is_liquidatable_by_margin(t, key)
        .expect("must return preview");
    assert!(
        !preview.is_liquidatable,
        "short should NOT be liquidatable below threshold; liq_price={liq_price}, px={below}, preview={preview:?}"
    );

    // Above => liquidatable for short
    let above = liq_price + margin;
    set_index_price_atom(&mut env.executor, above);

    let preview = env
        .executor
        .is_liquidatable_by_margin(t, key)
        .expect("must return preview");
    assert!(
        preview.is_liquidatable,
        "short should be liquidatable above threshold; liq_price={liq_price}, px={above}, preview={preview:?}"
    );
}
