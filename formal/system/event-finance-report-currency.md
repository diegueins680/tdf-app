# Consistent event finance reporting currency — PAY-EVENT-REPORT-CURRENCY-001

Authority: explicit owner decision on 29 October 2026: “Refuse totals until
contributing amounts match the event currency.” Existing explicit foreign-currency
entry authoring is intentional and remains supported. Reports have one currency
and no exchange-rate data. Canonical captured-ticket revenue already enforces
its event reporting currency separately.

A successful event finance summary and every budget actual amount must use the
existing event finance metadata/default currency resolver. Every nonzero amount
that contributes must have a valid normalized currency equal to that currency.
A mismatch returns HTTP500; it must never relabel, silently omit or convert a
contributing amount. An unreportable budget edit must refuse before its first
write, preserving the existing row. Explicit event currency overrides the default.

Contributor boundaries retain the existing semantics: validated manual entries
other than void contribute to posted or pending/commitment totals; canonical
captured order identifiers are excluded before inspecting legacy orders; only
paid, refunded and pending nonzero legacy order amounts contribute. Cancelled
and zero-price legacy orders do not require a conversion. Budget actuals retain
their exact posted, event, budget-line and normalized-direction filters. Unlinked,
opposite-direction and zero budget amounts do not require a conversion.

Authoring and individual entry listing continue to use the entry currency. No
provider call, financial row correction, schema/API shape, rate or new currency
catalog is introduced. Existing range checks and supported negative balances
remain. Historical malformed manual dimensions still refuse under their existing
validation. A budget actual with any nonzero contributing entry now invokes the same
event metadata validator as a summary, including its existing budget metadata
validation, even when the selected amounts sum to zero.

Assumptions: trusted existing stored metadata resolver and dimension/status
validation, Persistent/database reads, canonical captured-revenue currency checks
and native compiler. Existing separate reads do not establish a serializable
financial snapshot; refusal-before-write concerns observed entries. PostgreSQL
concurrency, historical data remediation, provider settlement and browser numeric
precision remain separate obligations. No production data is changed by tests.

Verification obligations: real typed Servant handler executions for foreign
creation/persistence plus report refusal, summary liability, budget list/update,
legacy statuses, explicit/default currency, free/void/cancelled/unrelated rows and
zero historical budget amounts. Negative controls must distinguish assertion
failures from fixture/build errors and must observe the unchanged budget row.
Per-commit execution evidence is recorded separately. Passing finite executions
are TESTED, not a proof of every implementation or concurrency behavior. Complete
implementation and refinement proof obligations remain UNKNOWN.
