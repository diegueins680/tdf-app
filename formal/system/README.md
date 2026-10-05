# TDF authoritative specification and conformance

Status: active reconciliation on consolidated main, 2026-10-04. **System-wide
conformance and delivery are not yet established.** The prior audit remains in
[history-2026-09-20.md](history-2026-09-20.md), with its original limitations.
Historical green results are not current-candidate evidence.

## Canonical package

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
- [messaging.md](messaging.md): current-session/consent authority, rejected mutation rollback, bounded model and delivery/idempotency exclusions.
- Domain contracts retain their existing source locations. This index references
  them instead of maintaining another handwritten copy of their transitions.
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

The user explicitly chose on 2026-10-04 to retain the shipped Mobile lineage.
Mobile `main` at `d2fd9399126e3b456c310497fb7d9edba09f981d` contains additional
payment flows/contracts absent from this root baseline. Compatible Mobile fixes
must descend from the shipped pin; merging that newer Mobile line is out of scope.

During reconciliation, root main advanced to
`fdac8e76523befee1603f49f6c7cf7d00762931b` through payment consolidation PR #414.
The audit integrates that root revision while preserving the explicit Mobile
lineage decision. Mobile receives generated API types and feature metadata for
the consolidated backend; native provider payment flows are not inferred from
those types. The provider-return feature remains a technical web callback with
no native destination. The combined manifest retains all 166 entries from that
main and appends the three audit migrations, for 169 entries. Earlier receipts
remain evidence for their recorded trees, not this combined candidate.

The public frontend bundle identifies that root SHA. Public `/version` and
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
