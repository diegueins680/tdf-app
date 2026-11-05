# Synthetic worker completion and first-failure observations

Requirement: `SYS-WORKER-COMPLETION-001`. This is a verification contract,
separate from `MEDIA-MUSIC-ASSET-001`'s lease, promotion and publication safety.

## Established behavior and counterexample

The existing runtime runner accepts asynchronous workers on ChildProcess close
and fails completion after30seconds. Hosted run40831374480/job123306367783
fails with only `Worker did not stop within 30s` after the DDEX retry scenario,
before reporting the sibling-job scenario. It loses the already captured output
and does not record whether the shell exited. The ticket PostgreSQL/date lane
is not reached; earlier Hspec and local financial passes have separate scopes.

The same sibling timeout was documented in `docs/music-releases/preview-ranges.md`.
Its underlying cause remains UNKNOWN. A single unchanged local sibling scenario
passed under read-only owned process/SQL sampling; that does not explain or
qualify the hosted failure. The first local probe failed its role precondition
before any scenario and is retained separately.

Node24 [ChildProcess documentation](https://nodejs.org/download/release/v24.13.1/docs/api/child_process.html#event-close)
distinguishes process exit from stream closure. A real synthetic parent can exit
successfully while its descendant retains stdout/stderr: the parent's exit has
occurred but close remains pending. A live child and this exited parent require
different investigation despite receiving the same old generic watchdog error.
The new tests construct both cases using owned Node children; no production
worker shutdown cause is inferred from this independent fixture.

## Contract

Attach the observer synchronously after spawning the synthetic child with piped
stdout/stderr. Preserve the existing full captured output and close result,
including nonzero status/signal and rejected spawn errors. Default completion
deadline remains30000ms. Process exit alone, stream destruction, deadline
extension or a later successful attempt cannot substitute for close.

On timeout, reject with one snapshot taken before caller cleanup: PID, monotonic
start/spawn/exit/close/observation timestamps, exit code/signal, observed spawn
error, stream end/close/destroy flags, total output character counts and the last
at most4000characters of each stream. This is bounded diagnostic context, not a
redaction guarantee. The runner scrubs ambient credentials and uses synthetic
output only. No environment or command arguments are added to the snapshot.

`exited-awaiting-close` means an exit event was observed without close. It does
not identify a descendant, SQL blocker or descriptor holder. `awaiting-exit`
means no exit was observed; it does not prove the process is runnable. Close
does not establish that every detached descendant is gone. Timer/close races
retain their actual first result rather than turning a deadline failure green.

The observer's early rejection handler prevents an unhandled spawn-error promise
while readiness is awaited; the original promise still rejects when consumed.
Existing caller outcome assertions, worker SQL, default heartbeat/lease timing,
signals, cleanup and the product worker itself remain unchanged.

## Verification obligations and controls

The local real-process suite exercises normal full-output completion, the actual
30000ms default timer arming, an early spawn error awaited later, a live-child
timeout with bounded tails, and an exited parent retaining inherited output.
The held-pipe fixture cannot complete until its owned descendant is terminated.
Readiness/exit waits are explicitly bounded inside the test deadline. The fixture
reports its descendant PID before readiness and owns a TERM cleanup handler;
on the tested macOS/Linux hosts its separately owned process group also permits
cleanup before the PID handshake. Cleanup has bounded TERM/KILL escalation;
a missing-readiness case verifies bounded rejection and reaping.

Independent review found the original test draft's unbounded readiness waits and
late PID announcement; that draft's local passes did not establish failure-path
cleanup. Its blocking review is retained. The corrected harness adds explicit
wait bounds, early ownership and an executable missing-readiness failure case.

Single-guard mutations must be rejected: accepting exit instead of close,
omitting the exit observation, or discarding timeout snapshot context. The
actual music concurrency scenarios must also execute on their own disposable
database with the changed runner, preserving the existing publication/lease/
cancellation assertions and30second completion deadline. Root repository quality
runs the no-database lifecycle suite without requiring the slower music backend.

Assurance is TESTED only by source-bound passing receipts. No model, mathematical
proof, universal liveness guarantee, provider compatibility or hosted qualification
is supplied by these unit controls. New hosted first-failure data, independent
exact-head review, all required CI contexts and protected FIFO remain necessary.
No production data, deployed worker, payment rule, schema or dependency changes.
