// src/services/pricing.rs

use crate::math;
use crate::services::open_interest::OpenInterestParams;
use crate::services::price_impact::{ImpactRebalanceConfig, PriceImpactService};
use crate::types::SignedU256;
use crate::types::{OraclePrices, Side, TokenAmount, Usd};
use primitive_types::U256;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TradeDirection {
    Increase,
    Decrease,
}

pub struct ExecutionPriceParams<'a> {
    /// Long / short OI before and after the action.
    pub oi: &'a OpenInterestParams,
    /// Market config for impact exponents and factors.
    pub impact_cfg: &'a ImpactRebalanceConfig,
    /// Side (long / short).
    pub side: Side,
    /// Increase or Decrease.
    pub direction: TradeDirection,
    /// Requested size delta in USD.
    pub size_delta_usd: Usd,
    /// Oracle min / max prices.
    pub prices: OraclePrices,
}

#[derive(Debug, Clone)]
pub struct ExecutionPriceResult {
    pub price_impact_usd: SignedU256,
    pub price_impact_amount_tokens: SignedU256,
    pub base_size_delta_tokens: TokenAmount,
    pub size_delta_tokens: TokenAmount,
    pub execution_price: Usd,
    pub balance_was_improved: bool,
}

/// Convert signed USD -> signed tokens(atoms) using a per-unit price.
/// For +USD: use round DOWN to minimize bonus.
/// For -USD: use round UP to maximize penalty.
fn signed_usd_to_tokens(
    usd: SignedU256,
    price_for_positive: U256,
    price_for_negative: U256,
) -> Result<SignedU256, String> {
    if usd.mag.is_zero() {
        return Ok(SignedU256::zero());
    }

    if !usd.is_negative {
        let mag =
            math::rounding::div_round(usd.mag, price_for_positive, math::rounding::Rounding::Down)?;
        Ok(SignedU256::pos(mag))
    } else {
        let mag =
            math::rounding::div_round(usd.mag, price_for_negative, math::rounding::Rounding::Up)?;
        Ok(SignedU256::neg(mag))
    }
}

/// High-level trait for pricing logic.
pub trait PricingService {
    fn get_execution_price(
        &self,
        price_impact: &dyn PriceImpactService,
        params: ExecutionPriceParams,
    ) -> Result<ExecutionPriceResult, String>;
}

/// Basic implementation that uses a PriceImpactService inside.
#[derive(Default, Clone)]
pub struct BasicPricingService;

impl PricingService for BasicPricingService {
    fn get_execution_price(
        &self,
        price_impact: &dyn PriceImpactService,
        params: ExecutionPriceParams,
    ) -> Result<ExecutionPriceResult, String> {
        let ExecutionPriceParams {
            oi,
            impact_cfg,
            side,
            direction,
            size_delta_usd,
            prices,
        } = params;

        // 0) trivial branch: sizeDeltaUsd == 0
        if size_delta_usd == U256::zero() {
            // No impact, just pick index price.
            let execution_price = match (direction, side) {
                (TradeDirection::Increase, Side::Long)
                | (TradeDirection::Decrease, Side::Short) => prices.index_price_max,
                (TradeDirection::Increase, Side::Short)
                | (TradeDirection::Decrease, Side::Long) => prices.index_price_min,
            };

            return Ok(ExecutionPriceResult {
                price_impact_usd: SignedU256::zero(),
                price_impact_amount_tokens: SignedU256::zero(),
                base_size_delta_tokens: U256::zero(),
                size_delta_tokens: U256::zero(),
                execution_price,
                balance_was_improved: false,
            });
        }

        // 1) compute priceImpactUsd from OI before/after
        let (price_impact_usd, balance_was_improved) =
            price_impact.compute_price_impact_usd(oi, impact_cfg)?;
        // 2) convert priceImpactUsd -> priceImpactAmount (tokens) ---
        //
        //  - if priceImpactUsd > 0:
        //        use indexPrice.max and round down (minimize bonus tokens)
        //  - if priceImpactUsd < 0:
        //        use indexPrice.min and round UP (maximize penalty tokens)
        let price_impact_amount_tokens = signed_usd_to_tokens(
            price_impact_usd,
            prices.index_price_max,
            prices.index_price_min,
        )?;
        // 3) baseSizeDeltaInTokens (without price impact)
        //
        // (Increase, Long) | (Decrease, Short): use indexPrice.max, floor
        // (Increase, Short)| (Decrease, Long) : use indexPrice.min, ceil

        let base_size_delta_tokens: TokenAmount =
            match (direction, side) {
                (TradeDirection::Increase, Side::Long)
                | (TradeDirection::Decrease, Side::Short) => math::rounding::div_round(
                    size_delta_usd,
                    prices.index_price_max,
                    math::rounding::Rounding::Down,
                )?,
                (TradeDirection::Increase, Side::Short)
                | (TradeDirection::Decrease, Side::Long) => math::rounding::div_round(
                    size_delta_usd,
                    prices.index_price_min,
                    math::rounding::Rounding::Up,
                )?,
            };
        //  4) total sizeDeltaInTokens including impact
        println!("base_size_delta_tokens {:?}", base_size_delta_tokens);
        println!(
            "price_impact_amount_tokens {:?}",
            price_impact_amount_tokens
        );
        let size_delta_tokens: TokenAmount = match (direction, side) {
            (TradeDirection::Increase, Side::Long) | (TradeDirection::Decrease, Side::Short) => {
                math::apply_signed_add(base_size_delta_tokens, price_impact_amount_tokens)?
            }
            (TradeDirection::Increase, Side::Short) | (TradeDirection::Decrease, Side::Long) => {
                math::apply_signed_sub(base_size_delta_tokens, price_impact_amount_tokens)?
            }
        };
        // If negative impact wipes more than base tokens -> underflow already caught above.
        if size_delta_tokens == U256::zero() {
            return Err("ZeroSizeTokensAfterImpact".into());
        }

        // TODO: acceptablePrice
        // 5) executionPrice = sizeDeltaUsd / sizeDeltaTokens
        // Both are in per-unit representations: USD(1e30) and tokens(atoms)
        let execution_price = math::rounding::div_round(
            size_delta_usd,
            size_delta_tokens,
            math::rounding::Rounding::Down,
        )?;

        Ok(ExecutionPriceResult {
            price_impact_usd,
            price_impact_amount_tokens,
            base_size_delta_tokens,
            size_delta_tokens,
            execution_price,
            balance_was_improved,
        })
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::services::open_interest::{OpenInterestParams, OpenInterestSnapshot};
//     use crate::services::price_impact::{BasicPriceImpactService, ImpactRebalanceConfig};
//     use crate::types::{OraclePrices, Side, Usd};

//     fn mk_oi(long0: Usd, short0: Usd, long1: Usd, short1: Usd) -> OpenInterestParams {
//         OpenInterestParams {
//             current: OpenInterestSnapshot {
//                 long_usd: long0,
//                 short_usd: short0,
//             },
//             next: OpenInterestSnapshot {
//                 long_usd: long1,
//                 short_usd: short1,
//             },
//         }
//     }

//     fn mk_prices(min: Usd, max: Usd) -> OraclePrices {
//         OraclePrices {
//             index_price_min: min,
//             index_price_max: max,
//             collateral_price_min: min,
//             collateral_price_max: max,
//         }
//     }

//     #[test]
//     fn zero_size_delta_returns_zero_impact_and_uses_oracle_price() {
//         let pricing = BasicPricingService::default();
//         let impact = BasicPriceImpactService::default();

//         let oi = mk_oi(100_000, 100_000, 100_000, 100_000);
//         let cfg = ImpactRebalanceConfig::default_quadratic();
//         let prices = mk_prices(1_000, 1_100);

//         let res = pricing
//             .get_execution_price_for_increase(
//                 &impact,
//                 ExecutionPriceIncreaseParams {
//                     oi: &oi,
//                     impact_cfg: &cfg,
//                     side: Side::Long,
//                     size_delta_usd: 0,
//                     prices,
//                 },
//             )
//             .expect("pricing should succeed for zero size");

//         // With zero size, we expect no impact and no tokens,
//         // only a "reference" execution price taken from oracle.
//         assert_eq!(res.price_impact_usd, 0);
//         assert_eq!(res.price_impact_amount_tokens, 0);
//         assert_eq!(res.base_size_delta_tokens, 0);
//         assert_eq!(res.size_delta_tokens, 0);
//         // For longs we pick index_price_max
//         assert_eq!(res.execution_price, prices.index_price_max);
//     }

//     #[test]
//     fn helpful_long_trade_gets_more_tokens_and_better_price_than_base() {
//         let pricing = BasicPricingService::default();
//         let impact = BasicPriceImpactService::default();

//         // Market is short-heavy (shorts > longs), opening a long helps rebalance.
//         // So this long trade is "helpful".
//         let oi = mk_oi(50_000, 150_000, 60_000, 150_000);
//         let cfg = ImpactRebalanceConfig::default_quadratic();
//         // Use same min/max to eliminate rounding differences
//         let prices = mk_prices(1_000, 1_000);

//         let size_delta_usd: Usd = 10_000;

//         let res = pricing
//             .get_execution_price_for_increase(
//                 &impact,
//                 ExecutionPriceIncreaseParams {
//                     oi: &oi,
//                     impact_cfg: &cfg,
//                     side: Side::Long,
//                     size_delta_usd,
//                     prices,
//                 },
//             )
//             .expect("pricing should succeed");

//         // Base tokens without any price impact:
//         let expected_base = size_delta_usd / prices.index_price_max;
//         assert_eq!(
//             res.base_size_delta_tokens, expected_base,
//             "Base tokens must match simple size / max_price division"
//         );

//         // Helpful trade should not penalize the user.
//         assert!(
//             res.price_impact_usd >= 0,
//             "Helpful long trade should have non-negative impact (ideally positive)"
//         );
//         assert!(
//             res.size_delta_tokens >= res.base_size_delta_tokens,
//             "Helpful long should receive at least as many tokens as the base amount"
//         );

//         // If we get more tokens for same USD size, execution price goes down (better).
//         let base_price = prices.index_price_max;
//         assert!(
//             res.execution_price <= base_price,
//             "Execution price for a helpful long should be no worse (<=) than the base price"
//         );
//     }

//     #[test]
//     fn harmful_long_trade_gets_fewer_tokens_and_worse_price_than_base() {
//         let pricing = BasicPricingService::default();
//         let impact = BasicPriceImpactService::default();

//         // Slightly long-heavy market.
//         // Initial: longs = 100_500, shorts = 100_000 (diff = 500)
//         // After:   longs = 101_000, shorts = 100_000 (diff = 1_000, imbalance increased)
//         //
//         // This is still a "harmful" trade (it makes the imbalance worse),
//         // but the imbalance is small enough that the negative impact
//         // does not exceed the order size.
//         let oi = mk_oi(100_500, 100_000, 101_000, 100_000);

//         let cfg = ImpactRebalanceConfig::default_quadratic();
//         let prices = mk_prices(1_000, 1_000);
//         let size_delta_usd: Usd = 10_000;

//         let res = pricing
//             .get_execution_price_for_increase(
//                 &impact,
//                 ExecutionPriceIncreaseParams {
//                     oi: &oi,
//                     impact_cfg: &cfg,
//                     side: Side::Long,
//                     size_delta_usd,
//                     prices,
//                 },
//             )
//             .expect("pricing should succeed for harmful long trade");

//         let expected_base = size_delta_usd / prices.index_price_max;
//         assert_eq!(
//             res.base_size_delta_tokens, expected_base,
//             "Base tokens must be computed as size / max_price for longs"
//         );

//         // Harmful trade: we still expect a penalty,
//         // but not so large that it wipes out the entire position.
//         assert!(
//             res.price_impact_usd <= 0,
//             "Harmful long trade should have non-positive impact (ideally negative)"
//         );
//         assert!(
//             res.size_delta_tokens <= res.base_size_delta_tokens,
//             "Harmful long should receive at most the base amount of tokens"
//         );

//         // Fewer tokens for the same USD size => higher execution price.
//         let base_price = prices.index_price_max;
//         assert!(
//             res.execution_price >= base_price,
//             "Execution price for harmful long should be no better (>=) than base price"
//         );
//     }

//     #[test]
//     fn short_rounding_uses_min_price_and_rounds_up() {
//         let pricing = BasicPricingService::default();
//         let impact = BasicPriceImpactService::default();

//         let oi = mk_oi(100_000, 100_000, 100_000, 100_000);
//         let cfg = ImpactRebalanceConfig::default_quadratic();

//         // Choose prices such that size / min_price is fractional,
//         // so we can see the ceil behaviour clearly.
//         let prices = mk_prices(1_000, 1_050);
//         let size_delta_usd: Usd = 10_001;

//         let res = pricing
//             .get_execution_price_for_increase(
//                 &impact,
//                 ExecutionPriceIncreaseParams {
//                     oi: &oi,
//                     impact_cfg: &cfg,
//                     side: Side::Short,
//                     size_delta_usd,
//                     prices,
//                 },
//             )
//             .expect("pricing should succeed");

//         // For shorts:
//         //   base_tokens = ceil(size_delta_usd / index_price_min)
//         let q = size_delta_usd / prices.index_price_min;
//         let r = size_delta_usd % prices.index_price_min;
//         let expected_base = if r == U256::zero() { q } else { q + 1 };

//         assert_eq!(
//             res.base_size_delta_tokens, expected_base,
//             "Short base tokens must be computed using min price with rounding up (ceil)"
//         );
//     }
// }
