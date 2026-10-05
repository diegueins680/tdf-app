# Artist enrichment completion: OPS-ENRICH-001

The normative requirement and transition fields live in `requirements.json`.
The external worker is `scripts/artist-enrichment.mjs`, scheduled by
`.github/workflows/artist-enrichment-daily.yml`. Its separate internal discovery
worker, provider adapter side effects and all other background jobs are not
certified by this scoped contract.

## Resolved discrepancies

The current implementation could record one failed item, stay below its early-stop
threshold and then persist `completed` and return successfully. A synthetic actual
`runPipeline` test reproduces the old false success, then verifies the repaired
failed run, retained error count, report and rejected result. The stop threshold
controls additional work; it cannot waive a recorded item failure. The CLI maps
rejected execution to a nonzero exit.

The workflow and CLI default both target verified canonical
`https://api.tdfrecords.net`, replacing the obsolete Fly destination. Explicit API
base overrides remain available for isolated tests/alternate environments; no
claim is made that any arbitrary configured target is production. No workflow
was dispatched for this repair. The existing schedule and publication opt-in are
preserved; tests reject accidental automatic publication/media ingestion flags.
The active runbook delegates deployment and recovery to the canonical Hetzner
contract, whose guarded release executor remains an open obligation.

## Bounded model

`WorkerCompletionEvidence` represents two items in one already authorized batch,
each settling succeeded or failed, followed by outcome selection, acknowledged
backend persistence and a successful return. Actions are atomic abstract local
steps. The acknowledgment is assumed to correspond to the intended authenticated
backend update. The model does not prove the transport, backend authorization,
database commit durability, provider outcomes, checkpoint file atomicity, startup
failures, early-stop mechanics, crashes or lease recovery.

Safety requires that a completed outcome contains no failed items and that a
successful return has acknowledged completed evidence. No fairness is assumed;
all actions can stutter, so no eventual completion is claimed. The bound is two
items and one run, not arbitrary batch size or multiple-worker refinement.

Two controlled mutations respectively ignore failed items and allow return before
acknowledgment. TLC must reject each with its named invariant. The actual worker
test corroborates the first mapping; the second abstracts the awaited final API
update and is not a network crash/retry test. Full worker delivery semantics and
external exactly-once behavior remain outside this result.
