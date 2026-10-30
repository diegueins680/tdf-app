# Event-finance CSV amount currency

Requirement: `UI-EVENT-FINANCE-CSV-CURRENCY-001`.

Authority is the owner's financial-integrity mandate and established support for
explicit foreign-currency finance entries. The existing currency-reporting contract
(`PAY-EVENT-REPORT-CURRENCY-001`, qualified source
`340be46ca3dda5a8376edbbeca2ed59528b02d47`) says individual entry authoring and listing
retain the entry currency. The event page already displays each individual amount
with `efeCurrency`. Summary/budget totals and individual records have distinct units;
there is no exchange-rate data or implicit conversion.

## Contract

The existing 24-column CSV `currency` cell identifies the unit of the amount in its
record. Summary and budget rows retain the resolved reporting/event currency.
A `finance_entry` row uses that entry's authoritative `efeCurrency`, together with
its unchanged `efeAmountCents`. Never substitute event currency, invent a fallback,
convert the amount, or silently omit a foreign or void record to make the units match.
All header positions, row order, event metadata, amounts, statuses and existing
precision-admission behavior remain unchanged.

The preceding schema did not explicitly distinguish common amount currency from
event metadata; no external consumer contract was found in the bounded repository
investigation. This successor makes that meaning explicit. It changes only the
finance-entry unit cell, without adding/removing/reordering columns. External
consumer expectations remain unknown; source/evidence review is required before
publication.

## Counterexample and verification

A USD event has a valid summary of 30 cents plus a EUR void entry of 100 cents and
a USD posted entry of 30 cents. This is consistent with the existing real-handler compatibility fixture:
void entries do not contribute to summary totals, but remain individually listed.
The former exporter emitted USD with raw100 for the EUR record, losing its unit.
Foreign unlinked posted amounts alone are not this example: they still contribute
to summary and the separate currency report guard refuses them.

Run the actual CSV helper regressions against the old cell binding, then the
one-cell repair. The foreign row assertion must fail with EUR expected/USD actual;
the USD summary/budget/entry compatibility and existing precision cases must pass.
Preserve raw outputs and distinguish assertion failures from setup/compile errors.
The compatibility test checks the 24-column layout and unchanged raw amount/status
and event metadata. Finite Jest/source-function results are TESTED, not a complete
implementation/refinement proof or a production incident.

## Assumptions and limits

Existing finance DTO authoring/listing currency validation is trusted; the exporter
copies the wire unit as received and is not a general malformed-DTO decoder.
Summary/budget currency resolution, contributor filters, financial range refusal,
numeric precision, query readiness, authorization and CSV escaping retain their
separate existing contracts. No backend/API/schema change, provider call, rate,
record correction, new currency catalogue or multi-query snapshot guarantee is
introduced. Mounted download/HTTP, external spreadsheet consumers, deployed browser
behavior and production exposure are not established by the direct-helper tests.
Historical parent reports remain immutable and retain their original scope.
