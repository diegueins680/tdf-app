# Event-finance browser monetary precision

Requirement: UI-EVENT-FINANCE-PRECISION-001. Assurance: TESTED where an exact-source execution receipt is present; implementation/refinement proof remains UNPROVEN.

## Contract and counterexamples

Financial DTO amounts and CSV columns are integer cents. The existing authoring parser refuses unsafe integers. Backend signed Int64 values can exceed JavaScript safe integers without violating that backend contract. Keep that wire contract; refuse a browser report that cannot represent its amounts.

The actual original API accepted JSON `9007199254740993` as `9007199254740992` and exported that changed cent. Independently, dividing the *safe* `9007199254740991` cents by100 displayed USD `$90,071,992,547,409.90` instead of `.91`. Safe `7036874417766401` displayed `.02` instead of `.01`; negative values are affected too. Two individually authorable safe inputs can yield an unsafe total. Input-only checks therefore do not protect reports.

For each non-null/undefined monetary field x in a finance summary, budget line or entry, admission requires `Number.isSafeInteger(x)`. These DTO fields use the `Cents` suffix; percentages/IDs are not monetary. Read getters validate before returning data. The CSV builder independently validates cached data before producing rows, including independent budget/entry queries when summary is absent. Export failure creates feedback before any download. The mounted page disables CSV until both independent list queries have data and neither has an error, including stale-data refetch failures. Successfully empty lists remain valid. Existing optional values and percentages retain their semantics. Rejected reads follow existing query error states.

For safe signed cents c, format the exact rational c/100 using the selected locale and currency's default fraction digits d. Convert the admitted integer to BigInt. For d>=2 scale its magnitude by10^(d-2); for d<2 round magnitude to the currency unit with integer half-expand arithmetic. Split integer whole/fraction units, preserve negative-zero sign for subunit negative amounts, and use Intl formatToParts only for presentation. USD/EUR retain2 digits, JPY retains0 and KWD retains3. Localized fraction digits replace the formatted fraction without a floating-point major-unit conversion. Null/undefined prices remain Gratis; unrepresentable numeric prices show Monto no disponible.

## Scope and verification limits

Regression tests exercise actual getters, CSV construction and formatting, including the original counterexamples, signed extrema, unsafe values, optional values, percentages, locale and ISO fraction conventions. A bounded small-cent differential check compares normal Intl formatting; it is testing, not a proof. Negative controls must restore the original floating division and remove each admission boundary to demonstrate detection. Existing UI quality CI discovers these Jest files automatically. Canonical traceability and exact-source receipts bind evidence to source fingerprints/SHAs; historical reports are not relabeled current.

No backend schema, monetary wire, production records, provider calls, new retry behavior, settlement semantics or multi-query snapshot guarantees are introduced. An upstream producer that already rounded an amount *into* the safe range cannot be detected by this boundary. Other application money formatters remain outside this correction. Full browser/locale coverage, physical device acceptance, financial currency attribution and implementation refinement remain separate obligations.
