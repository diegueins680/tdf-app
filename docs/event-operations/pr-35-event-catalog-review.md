# PR 35 — individual canonical event-operations catalog decisions

## Scope and dependency

Branch `chore/event-operations-catalog-review`, based on draft #413 at
`cbbb3da9f20c2314c73a5114b8c0aa943647054e`. Continues the user-approved CI plan.
Only review metadata/documentation changes; no application, migration, production
manifest, formal model, scanner, CI exclusion, API, generated client or mobile
gitlink changes. The [review contract](catalog-review-contract.md) preceded edits.

Sixteen current fingerprints were inspected against their source and consumers.
Each gets its own justification and risk. Existing decisions remain byte-for-byte
equivalent as parsed objects, with no deletions, refreshed IDs or duplicate IDs.

| Source / reviewed candidate | Classification and authority |
| --- | --- |
| `src/api/eventOperations.ts`: EventOperations methods | Technical function dispatch, not business options |
| `EventRaciEditor.tsx`: Phase | Technical transient editor state; existing RaciWebEditor contract |
| Foundation SQL: canonical lifecycle states | Security/system registry; persisted lifecycle transition policy and checked state domain |
| Foundation SQL: transition authority | Security/system registry; interpreted separation-of-duties discriminants |
| Foundation SQL: ownership/coproduction kind | Security/system registry; scoped persisted relationships |
| Foundation SQL: grant scope codes | Security/system registry; fail-closed contextual capabilities |
| Foundation SQL: grant resource kinds | Security/system registry; target identity boundaries |
| Foundation SQL: command outcomes | Security/system registry; immutable command receipts |
| Foundation SQL: audit outcomes | Security/system registry; override justification and immutable evidence |
| Foundation SQL: attendance modes | Governed reference domain persisted by session, not a new event-type catalog |
| Foundation SQL: visibility | Security/system registry; privacy scopes |
| Foundation SQL: RACI roles | Security/system registry; exact-accountability/responsibility semantics |
| Foundation SQL: override kinds | Security/system registry; version-bound exception evidence, not activation of overrides |
| `DatabaseBoundary.hs`: DatabaseFailure | Technical sanitized exception variants, not business status |
| `Types.hs`: EventTaskStatus | Governed reference consumer of canonical activity statuses |
| `Types.hs`: EventRaciRole | Typed consumer of the canonical security/responsibility registry |

Exact IDs are the 16 new entries in `docs/catalog-persistence/catalog-list-decisions.json`.
Classifying a constraint does not provide registry administration, assert arbitrary
extensions are safe, or resolve the remaining catalog/UX/domain architecture gaps.

## Verification and environment correction

The first local audit was incomplete: this worktree's mobile submodule was empty,
giving 176 unreviewed / 103 stale decisions. Those counts did not justify deleting
anything. `git submodule update --init tdf-mobile` checked out the exact existing
gitlink `53569fc4baa842a6882235d9a12c4ee68c44ff24`; the root reference and mobile
source were not edited. A full baseline rerun then matched hosted evidence:
**179 unreviewed / 9 stale**, exit 1 as expected.

Commands used for the complete baseline and post-review report (distinct output
files `catalog-complete.json` and `catalog-reviewed.json`):

```sh
node scripts/catalog-list-audit.mjs \
  --decisions docs/catalog-persistence/catalog-list-decisions.json \
  --fail-on-unreviewed \
  --output /private/tmp/tdf-raci-webkit.HwYQyR/catalog-reviewed.json
node --test scripts/__tests__/catalog-list-audit.test.mjs
git diff --check
```

The scanner regression test passed (one test). Additional executed Node assertions
compared all existing decisions against the exact parent, required 16 additions,
unique IDs, explicit reviewed flags, substantial per-entry justifications, and
membership in the full baseline's unreviewed fingerprints; all passed.
The complete post-review scan returned **163 unreviewed / 9 stale**, exit 1.
Independent Node assertions verified unchanged discovered fingerprint IDs and
exactly 16 changed decisions. The gate correctly remains red; no finding was
excluded or suppressed. Root/mobile status confirmed the gitlink remained intact.

No new TLC/Alloy execution, browser, backend build, database or production test is
claimed for metadata-only review. PR 34 records its separate browser evidence.

## Remaining work and rollback

All other unreviewed entries and all nine stale decisions remain deliberate red
gate findings, not waived exceptions. Continue individual review of API/schema
consumers, storefront/reputation/payment registries, source fixtures and the
migration manifest; map each obsolete decision to inspected source history before
retirement. Do not mechanically approve heuristic recommendations.

Rollback removes only the 16 added decisions and documentation. No data, migration,
security grant or accepted task history changes. No merge, deployment, production
activation or real-money operation was performed. This does not complete the
catalog program or the end-to-end event-operations mission.
