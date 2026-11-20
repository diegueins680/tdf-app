# Offline event draft review

EVT-DRAFT-REVIEW-001 covers the schema-two review path in
`scripts/prepare-event-draft.mjs`. A review plan must declare a private event,
no public image or ticket URL, ordered event times and a positive safe integer
capacity. Its tiers must retain unique normalized codes, inactive inventory,
zero sales, safe integer nonnegative prices and positive quantities. Their
currency must equal the event currency, which must contain exactly three
uppercase ASCII letters. Allocated quantities may leave headroom but may not
exceed capacity; the event price must match the lowest tier price.

This currency rule checks syntax, not ISO membership, provider support or
exchange rates. The validator does not normalize or rewrite currency values.
Date parsing and the existing sale-window checks are retained; this contract
does not establish complete timezone or operational inventory semantics.

The schema-two CLI is offline only. `--validate-only` reports the validation
result without reading a flyer, requiring an administrator token, calling an
API or writing an import report. Without that flag it rejects a valid schema-two
plan before the legacy asset, token or API path. Schema-one validation and the
existing separately authorized import path remain unchanged.

Twenty finite Node tests cover the original four cases, twelve malformed or
missing currency cases, accepted matching currency strings, unchanged legacy
currency behavior and both CLI boundaries. The same tests produce twelve
assertion failures on the original equality-only implementation. CLI fixtures
disable network access and use synthetic data. This is regression evidence,
not a whole contract, ticketing, payment, server authorization or refinement
proof. Required hosted checks and independent review remain separate.
