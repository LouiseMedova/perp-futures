// src/services/mod.rs

//! Сервисы протокола: прайсинг, фандинг, borrowing, комиссии, маржа, OI и impact pool.

pub mod borrowing;
pub mod borrowing_step;
pub mod fees;
pub mod funding;
pub mod funding_step;
pub mod impact_pool;
pub mod margin;
pub mod open_interest;
pub mod price_impact;
pub mod pricing;
pub mod step_costs;

pub use borrowing::BorrowingService;
pub use fees::{BasicFeesService, FeesService};
pub use funding::FundingService;
pub use impact_pool::ImpactPoolService;
pub use margin::MarginService;
pub use open_interest::OpenInterestService;
pub use price_impact::PriceImpactService;
pub use pricing::{BasicPricingService, PricingService};

use crate::state::MarketState;
use crate::state::{Claimables, PoolBalances};

pub trait ServicesBundle {
    type Pricing: PricingService;
    type PriceImpact: PriceImpactService;
    type ImpactPool: ImpactPoolService;
    type Funding: FundingService;
    type Borrowing: BorrowingService;
    type Fees: FeesService;
    type Margin: MarginService;
    type OpenInterest: OpenInterestService;

    fn pricing(&self) -> &Self::Pricing;
    fn price_impact(&self) -> &Self::PriceImpact;
    fn impact_pool(&self) -> &Self::ImpactPool;
    fn funding(&self) -> &Self::Funding;
    fn borrowing(&self) -> &Self::Borrowing;
    fn fees(&self) -> &Self::Fees;
    fn margin(&self) -> &Self::Margin;
    fn open_interest(&self) -> &Self::OpenInterest;
}

#[derive(Clone)]
pub struct BasicServicesBundle {
    pub price_impact: price_impact::BasicPriceImpactService,
    pub pricing: pricing::BasicPricingService,
    pub impact_pool: impact_pool::BasicImpactPoolService,
    pub funding: funding::BasicFundingService,
    pub borrowing: borrowing::BasicBorrowingService,
    pub fees: fees::BasicFeesService,
    pub margin: margin::BasicMarginService,
    pub open_interest: open_interest::BasicOpenInterestService,
}

impl Default for BasicServicesBundle {
    fn default() -> Self {
        let fees = BasicFeesService::new(
            10, // position_fee_bps_increase = 0.1%
            10, // position_fee_bps_decrease = 0.1%
            50, // liquidation_fee_bps = 0.5%
            20, // helpful_rebate_percent = 20%
        );
        Self {
            price_impact: price_impact::BasicPriceImpactService::default(),
            pricing: pricing::BasicPricingService::default(),
            impact_pool: impact_pool::BasicImpactPoolService::default(),
            funding: funding::BasicFundingService::default(),
            borrowing: borrowing::BasicBorrowingService::default(),
            fees,
            margin: margin::BasicMarginService::default(),
            open_interest: open_interest::BasicOpenInterestService::default(),
        }
    }
}

impl ServicesBundle for BasicServicesBundle {
    type Pricing = pricing::BasicPricingService;
    type PriceImpact = price_impact::BasicPriceImpactService;
    type ImpactPool = impact_pool::BasicImpactPoolService;
    type Funding = funding::BasicFundingService;
    type Borrowing = borrowing::BasicBorrowingService;
    type Fees = fees::BasicFeesService;
    type Margin = margin::BasicMarginService;
    type OpenInterest = open_interest::BasicOpenInterestService;

    fn pricing(&self) -> &Self::Pricing {
        &self.pricing
    }
    fn price_impact(&self) -> &Self::PriceImpact {
        &self.price_impact
    }
    fn impact_pool(&self) -> &Self::ImpactPool {
        &self.impact_pool
    }
    fn funding(&self) -> &Self::Funding {
        &self.funding
    }
    fn borrowing(&self) -> &Self::Borrowing {
        &self.borrowing
    }
    fn fees(&self) -> &Self::Fees {
        &self.fees
    }
    fn margin(&self) -> &Self::Margin {
        &self.margin
    }

    fn open_interest(&self) -> &Self::OpenInterest {
        &self.open_interest
    }
}
