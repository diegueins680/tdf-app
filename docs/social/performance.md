# Synthetic query qualification — 2026-09-14

Environment: shared Intel macOS development host, Docker PostgreSQL 16-alpine,
concurrent development processes and memory pressure. No production data.
`TDF_SOCIAL_BENCHMARK=1 bash scripts/social/test-postgres.sh` reproduces the fixture,
checks SQL hashes, exercises migration/races/model cases, then reports plans/timings.

Fixture: 110,000 additional synthetic accounts, 100,000 additional directed
relationships, a source with degree 10,000, a target with degree 90,000, and a
10,000-profile explicitly discoverable pool. Includes zero/one-degree accounts.
The feed fixture has only a few posts; its empty-page timing does **not** establish
feed scalability under a large post volume. No background worker lag was measured.

| Implementation | Observed result | Interpretation |
|---|---|---|
| Original full Discover scan | exceeded 5,000ms statement timeout | failed the predeclared 200ms query target |
| Daily sample capped at 200, nested policy calls | sparse single query ~1,338ms | reduced work, still failed target |
| Same sample with batched authority joins and pure relationship projection | 20 samples: hub p50 40.897ms / p95 59.176ms; sparse p50 43.856ms / p95 69.776ms | passed local synthetic p95 target |
| Following empty result | 20 samples: p50 10.797ms / p95 16.374ms | passed this narrow case only |

Evidence: `evidence/postgres-percentiles.txt` (final SQL hashes, correctness results,
plans, relation storage bytes, degree counts and measurements). Earlier logs retain
failed experiments. Single-query timing is not p95; the final percentiles use 20
samples each. Host contention changed between runs, so these are development
measurements, not a controlled production speedup estimate. There is no evidence
that a graph database would improve TDF's current workload or operating cost.

The selected baseline bounds authorization work and preserves exclusions before
returning candidates/reasons. It samples 200 public opt-in profiles per day and can
miss relevant candidates outside that sample. It does not optimize popularity.
Additional acceptance remains blocked: large-post-volume feeds, realistic degree
and permission distributions, many concurrent writers, client HTTP latency,
full-schema storage/maintenance cost, notification lag, and product-value outcomes.
