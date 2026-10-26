# Provider/domain admission qualification

Current reconciliation uses protected main2e01bbe1263e8761fa2692bd951f828c01142449, after the protected merge of621. Native317 and earlier assurance results retain their cb777/1bca identities. Changes to MusicRelease and SocialEventsHandlers and the strengthened empty-discovery gate require fresh compiled current-head CI. See current-main-integration.json; no whole-backend equality is claimed for this new composition.

Clean financial source: `cb7773790f15099933b92b184cfb50d7cf20c8aa`. All 223
registered migrations apply and replay; the first 222 objects/SQL/introduction
ancestors are unchanged from the ledger candidate. Provider migration introduction
`456be073c944dd3d76eb9e3c2169fe7fbb4041ef` remains an ancestor.

The canonical runner executes 224 SQL admission cases, 16 witnessed two-session
races at READ COMMITTED/REPEATABLE READ, 10 original-guard/reference-removal
controls and 4 fee/tax checks. Compiled discovery and execution agree on 317
provider/refund cases, zero failures and no pending cases. The separate aggregate
has 4,039 examples, zero failures and 21 explicit external-runner pending cases.
Stack 3.7.1 uses LTS24.42/GHC9.10.3; PostgreSQL 16.13, Python 3.13.5,
psycopg2 2.9.11 and Node 24.13.1 are recorded. No provider network, production
database, activation or deployment is involved. Owned fixture databases/clusters
are removed/stopped.

The original bba harness falsely accepts wrong SQLSTATE, wrong guard message and
a changed rollback snapshot under Python -O. Actual extracted-helper controls
reproduce all three; explicit checks reject them under normal and optimized
Python. The canonical SQL matrix now runs optimized. These helper controls
validate harness detection and are distinct from real PostgreSQL behavior.

The first 223-schema compiled run at 6566630f156fa35b9bea683d393162c24a34ab6a
failed 3 of 316 cases. Preserve that failing receipt: an unpaid fixture never
reached the intended paid-order409 fence, and two late captures omitted ticket
fulfillment before testing review admission. Corrected setup retains409 and
PaymentReview expectations, adds a distinct exact400 unpaid-order control, and
uses the real finalizePaidTicketOrder operation with every guard retained.

`integration.json` admits only identical financial/backend inputs from the
release-journal composition, preserving the original qualification identity.
A separate report accidentally overlapped that metadata merge; the freshness
checker rejected it and marked all claims stale. That attempt is failure
evidence, never a successful proof result. Fresh clean report/CI remains separate.

Reproduce on a clean checkout with the recorded Stack/Python dependencies:
`sh scripts/test-provider-retry-runtime.sh` (default owned Docker PostgreSQL16),
`python3 scripts/test-provider-domain-harness.py`,
`python3 -O scripts/test-provider-domain-harness.py`,
`node --test scripts/__tests__/catalog-list-audit.test.mjs`, and
`npm run quality:repo`. The native replay driver and raw logs retain this
machine's private temporary paths; use a newly owned empty database named
tdf_provider_retry_test and a free loopback port for native replay.

This is TESTED finite SQL/application behavior, not a universal proof, bounded
model-checking result or complete implementation equivalence. PR #621 must merge
first; current-head hosted checks, independent review and normal merge ancestry
remain mandatory. Keep guards across application rollback.

Fresh integrated assurance: clean `1bca8b216d5cf1f41b053ca5c02c189760fe6eaf` report and repository quality pass. `assurance-artifacts.tar.gz` preserves raw report/SMT inputs and repository log. All144 complete requirements and288 implementation/refinement obligations remain UNPROVEN, separate from11 conditional source-derived SMT claims. No new independent CPC replay or mathematical proof of provider admission is claimed. `scorecard.json` preserves these scopes separately from the native317 execution atcb777.
