use std::collections::HashMap;

use primitive_types::U256;

use crate::types::{AccountId, AssetId, TokenAmount};

/// Claimables is a ledger of "rights to receive something later".
/// We don't move real tokens immediately; we just accumulate how much
/// each account can claim per asset.
#[derive(Debug, Default)]
pub struct Claimables {
    /// Funding claimables per (account, asset).
    ///
    /// Example: positive funding for the "receiver" side,
    /// denominated in the long / short token of the market.
    funding: HashMap<(AccountId, AssetId), TokenAmount>,

    /// Placeholder for other claimables (fees, rebates, etc.).
    ///
    /// Kept separate so you can route them differently if needed.
    fees: HashMap<(AccountId, AssetId), TokenAmount>,
}

impl Claimables {
    /// Add funding claimable for a given (account, asset).
    ///
    /// `amount` is expected to be >= 0 in normal flow.
    /// If amount == 0, this is a no-op.
    pub fn add_funding(&mut self, account: AccountId, asset: AssetId, amount: TokenAmount) {
        if amount == U256::zero() {
            return;
        }

        let key = (account, asset);
        let entry = self.funding.entry(key).or_insert(U256::zero());
        // Saturating in case someone passes a huge amount.
        *entry = entry.saturating_add(amount);
    }

    /// Read current funding claimable for (account, asset) without modifying it.
    pub fn get_funding(&self, account: AccountId, asset: AssetId) -> TokenAmount {
        self.funding
            .get(&(account, asset))
            .cloned()
            .unwrap_or(U256::zero())
    }

    /// Take (consume) the whole funding claimable for (account, asset).
    ///
    /// Returns the amount that was stored, and resets it to zero.
    /// This is the natural primitive for a `claim_funding` action.
    pub fn take_funding_all(&mut self, account: AccountId, asset: AssetId) -> TokenAmount {
        self.funding
            .remove(&(account, asset))
            .unwrap_or(U256::zero())
    }

    /// Add generic fee claimable (if later you want to route protocol/UI/referral fees).
    pub fn add_fee(&mut self, account: AccountId, asset: AssetId, amount: TokenAmount) {
        if amount == U256::zero() {
            return;
        }

        let key = (account, asset);
        let entry = self.fees.entry(key).or_insert(U256::zero());
        *entry = entry.saturating_add(amount);
    }

    /// Read fee claimable (for completeness).
    pub fn get_fee(&self, account: AccountId, asset: AssetId) -> TokenAmount {
        self.fees
            .get(&(account, asset))
            .cloned()
            .unwrap_or(U256::zero())
    }

    /// Take all fee claimables for (account, asset).
    pub fn take_fee_all(&mut self, account: AccountId, asset: AssetId) -> TokenAmount {
        self.fees.remove(&(account, asset)).unwrap_or(U256::zero())
    }
}
