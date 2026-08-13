# ADR-0102: Versioned money snapshots and atomic holds

Status: Accepted — 2026-08-13

## Decision

Products, rate cards, taxes, fees, quotes, and policies are versioned database records. Checkout
copies immutable line snapshots in integer minor units. Exclusive resources use database-enforced
time-range holds; counted inventory uses locked counters. Holds expire and are consumed exactly once.

Client totals and non-atomic "check then insert" availability were rejected. Existing display prices
remain seeded values until an authorized rate version replaces them.
