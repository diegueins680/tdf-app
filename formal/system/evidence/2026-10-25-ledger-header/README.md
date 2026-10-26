# Posted ledger header qualification

Qualified implementation: `e57c8d1404e4a6f7b587e05c5ddbb2072520431c`.
Observed main: `917806ff9c8d455a01bfb8a29c80d6ff353e0e0b`.
Migration introduction: `5cc26ad46c958a029ec4539c60cc1ca77fec1d01`.
No production operation was performed. All database fixtures are synthetic,
newly created loopback databases. The owned databases were dropped and clusters
stopped. The original 220 registry entries and SQL bodies remain unchanged.

## Claims and results

- Full 221-migration apply/replay and actual rendered startup verification pass.
- Ordinary-role regressions reject 16 changes across all eight protected columns
  in posted and reversed fixtures; valid draft/identical writes remain possible.
- Four installed startup mutations reject; exact function, trigger and receipt
  snapshots restore. Removing the header guard admits the original violation.
- The SQL snapshot portfolio contains 30 positive cases and seven mutation
  controls. Three ordinary header writers wait behind posting, witnessed by
  pg_blocking_pids: RC rejects with 23514; RR/SERIALIZABLE reject with 40001.
  Disabling only the new header guard admits a changed source at RC.
- Legacy upgrade/reversal, existing namespace controls (also Python -O), and ten
  installed namespace startup mutations pass. The compiled ledger group runs ten
  examples with no failures or pending cases.
- That Stack artifact was built from F9, retained in receipt.json with its SHA256.
  The driver independently verifies that every Haskell source, test, Cabal and
  Stack input matches the qualified candidate. It is not reported as a new build.
  Hosted CI must build and execute the current candidate independently.
- TLC 1.7.2 checks one transaction, two values and two queued writers: 48 distinct
  states, 138 generated. The guard-removal configuration exits 12 on the named
  PostedHeaderFrozen invariant. No SQL refinement or liveness proof is claimed.
- Repository quality passes after installing the unchanged locked Node packages.
  Explicit external-runner skips are retained, not counted as SQL verification.

The earlier differential receipt uses frozen parent fa5 with 220 migrations and
an earlier 14-field regression. Its baseline fails on the accepted transaction_type
change. The same additive SQL then rejects those attempts and its controls pass.
Its dirty state and exact uncommitted hashes are retained; it is not retargeted
as a clean qualification of the later 16-field regression. See the independent
clean candidate receipt for that result. Three failed preparation attempts are
classified separately in failed-preparations.json and never counted as passes.

## Reproduce

Use the repository's canonical Stack toolchain and pinned formal JAR checksums.
The two included Python drivers record the exact local commands and environment;
their local paths/ports describe this machine. For a portable CI-style replay,
create a new owned disposable `_test` database on loopback and export its URL as
TDF_MUSIC_REHEARSAL_DATABASE_URL and TDF_IDENTITY_HTTP_DATABASE_URL. Do not reuse
an existing or production database or inherit libpq routing overrides.

```sh
node scripts/test-music-integration-rehearsal.mjs
# Build with Stack from tdf-hq before the canonical compiled runner.
(cd tdf-hq && stack build --test --no-run-tests)
bash scripts/test-ledger-posting.sh
npm ci --ignore-scripts
npm run quality:repo
# Full formal lane; JAVA_BIN/TLA2TOOLS_JAR/ALLOY_JAR are the pinned tools.
bash scripts/verify-event-operations-formal.sh
```

TLC may also run just LedgerHeader.cfg and LedgerHeaderMutable.cfg from
formal/event-operations using the exact commands in tlc-receipt.json. A merely
nonzero negative result is insufficient: it must name PostedHeaderFrozen.

artifact-hashes.json preserves raw and compressed log hashes. receipt.json and
tlc-receipt.json bind exact commits, tools and source hashes. scorecard.json is
a dated source snapshot, not a live CI/approval claim. Complete requirements
remain PARTIAL and complete implementation/refinement obligations UNPROVEN.
Owner/DDL bypass, arbitrary installed bodies, production correspondence,
historical attribution repair and provider exactly-once behavior remain open.
Keep the guard through rollback; use a normal merge preserving SQL introduction
ancestry and require the existing production migration/deployment approvals.

Fresh assurance-report.tar.gz retains the source-bound SMT producers and replay,
all serialized inputs and the complete report. Its metrics record 141 complete
requirements and 282 implementation/refinement obligations UNPROVEN, alongside
eleven distinct conditional source-derived SMT obligations. These levels must
not be combined. No new independent CPC check is claimed for this execution.
