# Review boundary follow-up

Direct Datafast/PayPal resource creation now requires the capabilities needed to
complete the checkout, even when a client bypasses availability discovery.
Marketplace contact coordination creates/reuses an unpaid order without selecting
a bank payment, creating an attempt or recording evidence. Pending online payments
still reject a contact switch. No provider is enabled and no settlement is inferred.
Canonical payment totals join the immutable checkout environment and group by it.
The additive DTO/OpenAPI field is displayed on separate UI cards; an older response
without the field explicitly says that its environment was not reported.
The web and mobile generated contracts must move together.

Verification: 2550 full backend examples passed. Two additional real PostgreSQL
regressions passed against an owned Unix-socket database: sale/rental contact
retries create zero intents/attempts/evidence and reject active online payments;
matching sandbox/production capture/refund totals remain distinct. The fixture
is a bounded handler/schema fixture, not a full production migration rehearsal.
Three operator UI tests passed, including environment labels and legacy fallback.
