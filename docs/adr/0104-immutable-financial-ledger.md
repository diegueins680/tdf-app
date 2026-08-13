# ADR-0104: Immutable financial ledger and manual payout gate

Status: Accepted — 2026-08-13

## Decision

Verified payment, fee, tax, refund, dispute, organizer/artist/provider payable, royalty, reserve,
recoupment, and settlement events post balanced immutable entries. Corrections are compensating
transactions. Settlement starts as dual-approved manual evidence; automated payout adapters remain
disabled until merchant product, KYC/tax/bank, reconciliation, legal, and production authorization
gates pass.

Mutable totals, nominal `Payment` rows as proof of escrow, and inferred payouts were rejected.
