# TDF authoritative specification and conformance

Status: active reconciliation on consolidated main, 2026-10-04. **System-wide
conformance and delivery are not yet established.** The prior audit remains in
[history-2026-09-20.md](history-2026-09-20.md), with its original limitations.
Historical green results are not current-candidate evidence.

## Canonical package

- [mobile-query-isolation.md](mobile-query-isolation.md): shipped Mobile per-occurrence query cache isolation and actual-screen late-response regression.

- [session-query-isolation.md](session-query-isolation.md): web account-switch cache isolation, request epochs and bounded negative controls.

- [drive-replay.md](drive-replay.md): actor/request-bound Drive retry admission and provider concurrency exclusions.

- [meta-webhook-admission.md](meta-webhook-admission.md): fail-closed key configuration and exact-body authentication for all supported aliases.

- [webhook-log-privacy.md](webhook-log-privacy.md): private provider payload exclusion from operational receipt logs.

- [catalog-reorder.md](catalog-reorder.md): atomic administrative reordering, revision conflicts and bounded negative controls.

- [ddex-validation.md](ddex-validation.md): canonical validation, atomic failure, shipped Mobile reference compatibility and explicit unavailable responses.
- [api-availability.json](api-availability.json): explicit local-only API deferrals; the compiled gate rejects unexpected mounting or missing undeferred declarations.
- [requirements.json](requirements.json): stable normative obligations, contracts,
  actors, guards, effects, failure/recovery boundaries, implementation, tests,
  models, confidence and unresolved questions. Historical status is separate.
- [inventory.json](inventory.json): generated source discovery, candidate clauses,
  model configurations, exact Mobile pin and implementation fingerprints.
  Discovery does not make a clause an approved product requirement.
- [traceability.json](traceability.json): generated bidirectional mappings,
  unmapped implementation, untested requirements, critical formal gaps, domain
  state machines, resolved OpenAPI operations and feature capability predicates.
- [authority-decisions.json](authority-decisions.json): competing sources and their
  explicit resolution. [research.json](research.json) records primary sources,
  applicability and adoption rationale separately from verification evidence.
- [cors-boundary.md](cors-boundary.md): credentialed production browser origins,
  startup guards, runtime mutation controls and coordinated configuration rollout.
- [dependency-security.md](dependency-security.md): known-version regression floors, scan evidence and unresolved runtime/build-tool exposure.
- [routed-read-boundaries.md](routed-read-boundaries.md): radio presence authentication and separate public/managed trial subject visibility.
- [compiled-api-boundary.md](compiled-api-boundary.md): compiler-derived route discovery, OpenAPI correspondence and unresolved handler/codec boundaries.
- [private-upload-persistence.md](private-upload-persistence.md): private attachment mount admission, legacy-file preservation and remaining coordinated recovery obligations.
- [messaging.md](messaging.md): current-session/consent authority, rejected mutation rollback, bounded model and delivery/idempotency exclusions.
- Domain contracts retain their existing source locations. This index references
  them instead of maintaining another handwritten copy of their transitions.
- [worker-completion.md](worker-completion.md): artist worker failure evidence, canonical target and scoped bounded completion checks.
- [payment-retry.md](payment-retry.md): serialized retry admission, immutable evidence, bounded mutation controls and retirement of the conflicting non-runtime checkout table.
- [payment-arithmetic.md](payment-arithmetic.md), [legacy-escrow.md](legacy-escrow.md)
  and [identity-recovery.md](identity-recovery.md) define narrow verification
  boundaries. `formal/event-operations` and `formal/social` retain scoped
  TLA+/PlusCal/Alloy models and negative controls.

## Authority and conflict resolution

1. Explicit approved product decisions, with their activation scope and recorded
   supersession, govern intended behavior. The current user instruction authorizes
   this audit, repairs, review, merge and eligible deployment; it does not activate
   experimental providers or authorize verification charges.
2. Applicable accepted ADRs and approved domain contracts refine that intent. A
   later source supersedes an earlier one only when its scope and decision actually
   conflict; a newer date or the word “canonical” alone is insufficient.
3. API, data and formal contracts specify boundaries under those decisions. A
   passing formal model cannot override a product requirement. Refinement from
   model to implementation is a separate obligation.
4. Implementation and tests establish observed behavior and executable evidence.
   They do not turn a bug into product policy. Source hashes establish freshness,
   not correctness or independent approval.
5. General documentation is explanatory. `specs.yaml` is historical v1 discovery
   material, not the current deployment, pricing, identity or platform contract.
   Dated audit receipts describe their recorded revision only.

Conflicts lacking an applicable decision remain `SPECIFICATION AMBIGUITY` or
`PARTIAL`. Do not silently choose whichever source is easiest to satisfy. Full
paths identify ADRs: the two historical ADR-0105 filenames are distinct decisions.

## Verified starting baseline

The clean isolated branch starts at root `b07f67c3ebe2022d5f70e94578944b6401616696`
and Mobile `ede1f2a0ccf75f3794c7892f291e1751b279d19e`. The shared dirty checkout
was not used as the candidate. Read-only baseline and source hashes are in
[evidence/2026-10-04-baseline/baseline.json](evidence/2026-10-04-baseline/baseline.json).

The user initially chose on 2026-10-04 to retain the shipped Mobile lineage.
Mobile `main` at `d2fd9399126e3b456c310497fb7d9edba09f981d` contains additional
payment flows/contracts absent from this root baseline. Compatible Mobile fixes
were restricted to that shipped pin at this checkpoint. The subsequent explicit
decision below supersedes that restriction for the newly shipped revision only.

During reconciliation, root main advanced to
`fdac8e76523befee1603f49f6c7cf7d00762931b` through payment consolidation PR #414.
That checkpoint preserved the original Mobile lineage and combined all166 main
migrations with three audit migrations, for169 entries. Its generated API types
alone did not establish native payment-flow parity. Earlier receipts remain
evidence for their recorded trees, not later combined candidates.

Root main then advanced through PR#479 to
`5efd7ff31d55c1eb5b66b2f7710ea4ef87328727`, shipping Mobile
`147ec8bc4ad4150eaab1ad410aa1803822075697`. The product owner explicitly chose
**Use newly shipped Mobile pin**. The audit preserves that consolidated root and
Mobile lineage, including its already shipped tester/payment functionality; it
does not import unrelated later Mobile-main work. The compatibility revision
`ff10746207ba5b57428560db64fb255d7417a481` descends from147ec8bc and carries
the reviewed dependency, feature-registry and generated API repairs. These
ancestry facts do not establish every native payment workflow's conformance.
The additive booking-calendar repair brought that checkpoint to170 entries.
The subsequent DDEX legacy-nullability compatibility repair adds entry171.
AUTHORITY-020 records this baseline supersession; the original starting SHA
and its evidence remain unchanged.

The next consolidated checkpoint is root
`259148d2af3b511dd7d011534e024014c90dfb15` (PR#481). It updates Mobile
distribution evidence while retaining the same147ec8bc shipped gitlink. Those
changes are preserved in this audit; distribution receipts do not establish
application conformance.

The frontend observation at the fdac8e checkpoint identified that root SHA; it
is historical observation, not a claim about the latest frontend deployment.
Public `/version` and
container inspection independently identify production backend
`645f56fcc44f81609fbfd0e03d683b40376ce77a`, version `0.1.0.0`, on Hetzner.
PostgreSQL is 17.8 on the verified `tdf_production_postgres_data` volume.
There are 159 applied migrations versus 160 in the starting manifest; the unapplied
entry is `2026-10-03_discovery_ownership_metadata_boundary`. Recorded checksums
match expanded SQL or an explicitly accepted compatible checksum. Raw-file hashes
must not replace the release runner's include-expanded checksums.

Runtime boolean environment flags are retained in the baseline. Absent variables
and database-managed capabilities still require effective-default and database
verification; this is not a complete feature-enablement claim. No production write,
provider activation or financial transaction was performed for this baseline.

GitHub requires `quality`, `api-contracts`, `production-migrations`, a current
approving review, an up-to-date base and resolved conversations. Admin enforcement
is active. Agent review does not substitute for the required GitHub approval.
No bypass is part of this audit.

## Product and implementation boundaries

TDF includes Haskell/Servant/PostgreSQL, React web and the exact pinned Expo Mobile
repository. Cloudflare preview functions and operational jobs are source-inventoried.
Optional `streaming` and `tidal-agent` deployment status still needs verification.

The [identity lifecycle contract](identity-lifecycle.md) governs anonymous artist
claim denial and atomic credential/session transitions, with two bounded models
and six negative controls. Current runtime and deployment evidence remains required.

Identity is a principal acting as a Party; Party, artist-profile and domain-resource
IDs are distinct. The generated capability predicates are UI discovery policy,
**not backend authorization**. Ownership, scoped grants, RACI, bilateral consent,
session revocation and strict-admin checks need endpoint-level denial evidence.

Money uses currency-qualified integer minor units. Capture, refund, fulfillment,
custody, settlement and payout are separate facts. Server-verifiable evidence and
immutable commercial bindings govern financial transitions. Browser returns are
not payment evidence. Existing source-derived SMT checks cover a narrow Int64
boundary, not every provider, SQL transaction or money path.

Pinned Mobile uses in-memory query caching and disables automatic mutation retry.
Its booking adapter awaits an HTTP result. No durable offline booking queue was
found in that inspected path. Offline booking success must not be advertised.
Local preferences and experiment persistence are not booking acceptance guarantees.

Private-asset ADR requirements, current local-volume media, Google Drive adapters,
privacy/consent and deletion need per-content reconciliation. Do not claim universal
private storage, durable deletion or decentralized ownership from one adapter.

Production is currently Hetzner; the checked-in Fly release command is not
necessarily the current production procedure. `ops/hetzner` contains portable and
cutover configuration. Routine releases must preserve provenance, migration
history, identity recovery floors, database ownership and verified backups.
Canonical routine-release correspondence remains a delivery obligation; never
release to a historical host to obtain a green receipt.

## Published product promises

The discovery inventory includes every shipped HTML page under `tdf-hq-ui/public`,
including privacy, deletion, account terms and Mobile support pages. Their source
hashes are material implementation fingerprints and changes enter CI admission.
Public availability is provenance, not proof of product approval or fulfillment.
The social-message deletion page promises completion within30days; the Mobile
page describes identity verification, a30day target and retention exceptions.
No operational deletion runbook or completion ledger was found in this source
review. Their location has been requested from the owner. Do not replace those
commitments merely to match absent automation, or infer completed deletion from
a signed webhook/tombstone. Per-content execution, exceptions, backups and
operator evidence remain unresolved. Static text is indexed as a source, not
automatically promoted into verified requirement clauses.

## Authored domain requirement declarations

The discovery inventory additionally indexes explicitly identified Markdown-table
clauses from domain contracts. Authored IDs such as `TC01`, `FH-01` and `PROFILE-01`
remain in `declaredId`; a source-qualified discovery ID remains stable across
wording and line-number edits. Source hashes, line locations, original columns and
every repeated declaration are retained. Different statements under one source ID
are surfaced for reconciliation, not overwritten or automatically called a logical
contradiction. Fenced examples, test-result tables and requirement ranges are not
new declarations. Archived documents remain historical.

These are **unreviewed source declarations**, separate from the explicitly
reviewed cross-system requirement register. Their neighboring implementation/test claims
are provenance, not current-head evidence. Extraction does not promote historical
status, roadmap scope or a claimed PASS to current authority. Inline prose, implicit
code rules and semantic reconciliation remain open; this index is not exhaustive.

## Reproduction

```sh
git submodule update --init --checkout --recursive
python3 -m pip install -r formal/system/requirements-verification.txt
python3 scripts/specification-inventory.py
python3 scripts/specification-conformance.py
python3 scripts/specification-inventory.py --check
python3 scripts/specification-conformance.py --check
PYTHONDONTWRITEBYTECODE=1 python3 scripts/test-specification-inventory.py
PYTHONDONTWRITEBYTECODE=1 python3 scripts/test-specification-conformance.py
npm run generate:api
node scripts/check-generated-api.mjs
node --test scripts/__tests__/generated-api-conformance.test.mjs
```

Mobile is mandatory. Client checks compare committed bytes inside each repository
and reject a wrong checkout, including staged generated drift. Source changes
require reviewed regeneration. Formal CI checks both inventories on every PR/main.

Use pinned Java/TLC/Alloy from `formal/event-operations/README.md`:

```sh
node scripts/verify-system-evidence.mjs --output /tmp/NEW-UNUSED-RUN-DIRECTORY
python3 scripts/verify-payment-arithmetic.py
python3 scripts/test-payment-arithmetic-verifier.py
```

Bind logs to command, full revision, source/tool hashes, exit status and negative
controls. Revalidate after source edits. Never admit timeout, incomplete execution,
stale fingerprints or handwritten PASS assertions. Bounded safety/liveness applies
only under each model's stated bounds, fairness, assumptions and exclusions.
Whole-system refinement remains open.

Studio booking scope, resource projection, bounded models and remaining exclusions: [bookings.md](bookings.md).

## Admission of changed implementation surfaces

CI runs `python3 scripts/check-new-specification-surfaces.py --base <full-base-SHA>`
on immutable Git trees. Every added, modified or type-changed material implementation or test file
must have a source fingerprint and a bidirectional relationship to the canonical
requirement register. Renames are evaluated as additions; changes to the Mobile
pin are compared inside the actual repository between both committed gitlinks.
Missing Mobile history, a mismatched checkout, symlink source, missing mapping or
stale source hash fails admission. Unit and real temporary-Git controls exercise
omission, forged mappings, rename bypass and wrong Mobile checkout rejection.

This gate stops new orphan files and edits to existing orphan files. Unchanged
unmapped debt remains listed in traceability. The gate does not establish semantic
correctness of a plausible mapping or replace implementation review. Deleted
paths remain subject to the inventory and requirement orphan checks.
[fixture-routing.md](fixture-routing.md) records the separate test database routing
boundary and the ownership guarantees it does not provide.
