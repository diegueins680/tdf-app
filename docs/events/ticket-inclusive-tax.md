# Ticket prices with tax included

The versioned checkout policy can opt into `tax_included=true`. Existing policies
and purchased snapshots default to false and retain additive tax. This migration
does not approve an event's tax rate, activate a policy, or enable a provider.

For inclusive prices, advertised unit price × quantity less the advertised
promotion is the net face value. Buyer and organizer fees retain their approved
basis-point basis on that value. The total is net face value plus buyer fee.
Tax is extracted from this total with integer half-up rounding once per order:
`round(total * taxBps / (10000 + taxBps))`. Organizer payable is net face value
less organizer fee and included tax; negative liabilities are rejected. Thus
`total = organizer payable + platform fees + tax` in either tax mode. These
amounts are not net profit, payment-processor fees, or an automatic partner payout.
Mixed-rate items are not represented by a single blended/unapproved rate.

A USD20 inclusive ticket at a test rate of15% yields totals USD20/40/60/80 for
one/two/three/four tickets, and tax USD2.61/5.22/7.83/10.43. The test rate is not
an event-specific tax determination. Discounts use the advertised units. A buyer
fee, when approved, remains an extra advertised fee whose tax is also included.

`taxIncluded` is optional in policy and quote API schemas for compatibility with
older backends; omission means additive. The quote uses the purchased runtime
mode, not today's policy. The frontend labels included tax and uses the backend
total; it cannot submit a tax mode. Mobile ticket purchases use the same web
checkout. A type update does not claim a new native binary or a native E2E pass.

Database constraints check totals, payable and inclusive tax/fee arithmetic;
triggers bind the mode to the approved policy and prohibit changing purchased
mode. Published policies already use a generic immutable-field guard. An old
writer omitting the mode fails for an inclusive policy. Original historical audit
rows remain untouched; additive-default annotations are appended idempotently.

Deploy the additive migration before the new backend, validate old/new policies,
then configure any newly approved inclusive policy. Keep new inclusive policies
inactive until all serving backends and clients support them. Binary rollback
requires deactivating such policies first; retain the schema and purchased mode
for current orders, confirmations, refunds and accounting. The SQL rollback
refuses to erase that evidence.

Validation: actual PostgreSQL migration replay, historical-default preservation,
one-to-four-ticket orders, exact price/tax/liability checks, forged tax/mode,
immutable purchased and approved policy modes; Hspec amount and request tampering
regressions; web included-tax rendering. Run the existing
`scripts/test-public-ticket-checkout-runtime-migration.sh`, backend quality suite,
and `PublicEventTicketsPage.test.tsx`. Provider purchase/refund and production
release verification remain separate from these tests.
