// src/services/margin.rs

use crate::state::{MarketState, State};
use crate::types::Order;
use crate::types::*;
pub trait MarginService {
    // fn pre_check_increase(
    //     &self,
    //     state: &State,
    //     market: &MarketState,
    //     order: &Order,
    //     prices: &OraclePrices,
    // ) -> Result<(), String>;

    // fn post_check_increase(
    //     &self,
    //     state: &State,
    //     market: &MarketState,
    //     order: &Order,
    //     prices: &OraclePrices,
    // ) -> Result<(), String>;

    // fn pre_check_decrease(
    //     &self,
    //     state: &State,
    //     market: &MarketState,
    //     order: &Order,
    //     prices: &OraclePrices,
    // ) -> Result<(), String>;

    // fn post_check_decrease(
    //     &self,
    //     state: &State,
    //     market: &MarketState,
    //     order: &Order,
    //     prices: &OraclePrices,
    // ) -> Result<(), String>;

    // fn can_liquidate(
    //     &self,
    //     state: &State,
    //     market: &MarketState,
    //     order: &Order,
    //     prices: &OraclePrices,
    // ) -> bool;
}

#[derive(Default, Clone)]
pub struct BasicMarginService;

impl MarginService for BasicMarginService {}
