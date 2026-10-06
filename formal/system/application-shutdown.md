# Application startup and shutdown

`OPS-SHUTDOWN-001` governs the backend supervisor. Docker stop needs an installed
signal handler when the application is PID1; the previously tested immutable image
continued until Docker killed it with exit137. Recovery correctly rejected that
as a clean capture. This repair affects newly built applications. It does not make
the old production image safely stoppable or resolve the first deployment barrier.

## Contract and state machine

Handlers for SIGTERM and SIGINT are installed before configuration preparation.
Requests coalesce through one latch. Preparation, initialization and Warp are
supervised tasks; unexpected return and fatal errors propagate to Main.

- Preparing -> serving/initializing: preparation succeeds, listener and setup run
  concurrently; startup responses retain the existing `starting` contract.
- Preparing -> stopping: cancel and join preparation before clean completion.
- Serving/initializing -> stopping: first dispatch initialization cancellation,
  then atomically close startup admission and obtain the listener stop callback.
  Cancellation comes first because an admitted worker starter can be blocked in
  a synchronous database check while holding the admission gate.
- Stopping -> clean: initialization cancellation/completion is observed and Warp
  returns after its accepted requests drain. A listener registered after accepted
  stop is closed immediately. No subsequent readiness publication or worker start
  is admitted. Already admitted work can execute until process termination.
- Stopping -> failed: the outer 30-second budget expires or a non-cancellation
  startup/server error occurs. Main exits nonzero; deadline expiry is never clean.
- Fatal preparation/startup/server failure -> failed: remaining supervised tasks
  receive cancellation; the exception remains observable by Main.

The stop deadline includes admission, cancellation and drainage. Warp 3.4.9's
internal graceful timeout discards the timeout outcome, so it is deliberately
`Nothing`; the supervisor owns the deadline and its classification. A cancellation
sender runs separately because `throwTo` can block behind uninterruptible foreign
calls. The deadline can classify that case as failure; it is not a guarantee that
an arbitrary foreign call or finalizer is interruptible.

Database connection retries catch synchronous failures only. An asynchronous
cancellation must escape to the supervisor. The course reminder catch follows the
same rule. Fatal errors are never transformed into a successful startup.

## Scope and exclusions

Detached workers are process-owned and are **not joined or drained** by this
supervisor. Clean exit establishes initialization completion/cancellation and Warp
request drain, not provider-effect atomicity, all-worker completion, continuous
writer exclusion, or a durable database backup. Existing idempotency/reconciliation
and coordinated-release contracts remain necessary. Concurrent external signal
handler replacement, uncatchable signals, kernel/process crashes and hostile native
code are excluded. Process scheduling and final process exit are environmental
assumptions, not real-time guarantees from `System.Timeout`.

## Executable evidence

`TDF.ShutdownSpec` checks blocked preparation, initialization, late listener
registration, repeated requests, deadline classification, failure propagation,
publication denial, actual retry cancellation, a real held Warp HTTP request and
cancellation while a worker admission gate is held.

`python3 scripts/verify-shutdown.py` runs that implementation and five controlled
source mutations: leave a late listener open; report deadline success; admit late
publication; cancel after taking the gate; swallow startup cancellation. Every
mutant must fail its named behavior, not merely fail to compile. Four isolated
children exercise actual SIGTERM/SIGINT during preparation and serving. This runs
in the backend quality gate with exact source fingerprints.

The Build Image `original-recovery` job additionally requires the exact built
backend's Docker stop to produce exit0 in its isolated synthetic PG17 fixture,
before recovering the original database and API. Its explicit
`TDF_TEST_REQUIRE_CLEAN_APPLICATION_STOP=1` admission must not be weakened to
accept the historical exit137 behavior. A green host test is not this image test.

## Bounded model

`ApplicationShutdown.tla` abstracts one process, one preparation/startup task,
one listener, an accepted-stop flag, at most two in-flight requests and terminal
clean/failed outcomes. Atomic actions represent admission/registration ordering;
Haskell scheduling, exceptions, sockets, OS handlers and actual time are outside
the abstraction. No fairness or liveness claim is made; a timeout is an abstract
failure transition and can occur before drain. Four mutants separately remove
late-listener closure, publication admission, request-drain and startup-join
guards. Safety invariants require each of these boundaries. Passing TLC is bounded
model evidence, not a refinement proof of the implementation or a whole-system
shutdown guarantee.

## Legacy image counterexample

`python3 scripts/test-legacy-shutdown-linux.py` reuses the exclusively owned empty
Linux-host fixture with its explicit machine-id acknowledgement. It pins the
legacy production image digest `38e6264b82db2d81a5b51c3a78740b6a305538b4cdae8d53ced067ccbb1e8fe0`.
It first accepts a synthetic administrative room insert, then holds a second real
HTTP insert at a PostgreSQL advisory-lock trigger and requests SIGINT on that
exact fixture container. The test requires a lost HTTP response, exit255, retained
committed data and rejection by the unchanged production capture validator. It
records whether the interrupted insert committed; it does not infer rollback
from the client's disconnected socket. No product/customer data or provider
credentials are used. The fixture removes only its owned resources.

This is a counterexample to interpreting legacy process termination as HTTP
drain. It does not authorize exit255 in a production recovery, establish every
transaction's outcome, test interrupted uploads or reconcile provider effects.
The new supervisor and the separate image clean-stop gate remain required.
