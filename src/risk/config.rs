use crate::types::Usd;
use primitive_types::U256;

/// Generic fixed-point scale = 10^18.
fn fp_scale() -> U256 {
    U256::exp10(18)
}

pub fn usd_scale() -> U256 {
    U256::exp10(30)
}


/// Protocol-level risk constraints.
#[derive(Clone, Copy, Debug)]
pub struct RiskCfg {
    /// Remaining positions below this size are treated as dust and should be fully closed.
    /// USD(1e30)
    pub min_position_size_usd: Usd,

    /// Absolute minimum collateral required for a position to remain open.
    /// USD(1e30)
    pub min_collateral_usd: Usd,

    /// Maintenance margin factor vs position notional (fraction in FP(1e18)).
    ///
    /// Interpretation:
    ///   required_collateral_usd >= size_usd * min_collateral_factor_fp / factor_scale
    ///
    /// Example: max leverage 50x -> maintenance factor ~ 1/50 = 0.02.
    pub min_collateral_factor_fp: U256,

    /// Fixed-point scale used by `min_collateral_factor_fp`.
    pub factor_scale: U256,
}

impl RiskCfg {
    /// MVP defaults (reasonable baseline):
    /// - dust: $10
    /// - min collateral: $5
    /// - maintenance factor ~ 2% (50x)
    pub fn mvp() -> Self {
        Self::with_max_leverage_and_thresholds(
            50, // max_leverage_x
            10, // dust_usd (human USD)
            5,  // min_collateral_usd (human USD)
        )
    }


    /// Helper constructor: provide human-readable USD thresholds (no scale),
    /// and a max leverage which is converted to a maintenance factor.
    pub fn with_max_leverage_and_thresholds(
        max_leverage_x: u64,
        dust_usd: u64,
        min_collateral_usd: u64,
    ) -> Self {
        assert!(max_leverage_x > 0, "max_leverage_x must be > 0");

        let scale_fp = fp_scale();

        // min_collateral_factor_fp = 1 / maxLeverage in FP(1e18).
        // For 50x: 1e18 / 50 = 2e16 (2%).
        let min_collateral_factor_fp = scale_fp / U256::from(max_leverage_x);

        Self {
            min_position_size_usd: U256::from(dust_usd) * usd_scale(),
            min_collateral_usd: U256::from(min_collateral_usd) * usd_scale(),
            min_collateral_factor_fp,
            factor_scale: scale_fp,
        }
    }

}

impl Default for RiskCfg {
    fn default() -> Self {
        Self::mvp()
    }
}
