# HTTP shutdown and startup supervision

`DEPLOY-HTTP-SHUTDOWN-001` owns this boundary. A current-image isolated rehearsal
at `7ab3444` observed TERM taking ten seconds and requiring SIGKILL (exit137),
and INT exiting255. Restart preserved synthetic data. Those observations neither
qualify graceful shutdown nor justify extending the obsolete legacy-image exception.

After Warp installs its listener-close action, TERM and INT invoke it once across
both signal types. It stops listener admission and gives accepted HTTP work Warp's
five-second cooperative graceful-shutdown interval. Repeated signals do not invoke
a stale close action again. SIGKILL is still the force-stop mechanism. Previous
process signal handlers are restored when the scoped server finishes.

Startup and HTTP serving are supervised together: successful startup keeps serving;
startup failure cancels serving and propagates failure; listener/server failure
cancels unfinished startup and propagates failure; normal server completion cancels
unfinished startup. Startup no longer needs to finish database retries before an
already-listening server can finish stopping. Uninterruptible foreign calls can
still delay Haskell cancellation. There is no hard global process deadline.

The Haskell supervisor tests cover all four outcomes. The extracted startup retry
boundary uses `safe-exceptions` so asynchronous cancellation is never retried;
four additional cases cover recovery, exhaustion and cancellation during an
attempt or retry pause. The subprocess waits for a retry-attempt marker before
stopping the process, so a plain sleep cannot conceal swallowed cancellation. The source-bound subprocess
runner uses actual loopback HTTP, SIGTERM and SIGINT: an accepted held request
finishes, a new connection is refused during the drain, a stuck request cannot
fabricate success, unfinished startup is cancelled, and startup/bind failures exit
nonzero. Three actual source mutations independently remove listener closure, the request
grace interval or cancellation propagation in the retry boundary and fail named
assertions. No production data or provider
is involved. Run with the Stack-selected toolchain:

```sh
STACK_ROOT=/absolute/stack/root python3 scripts/verify-http-shutdown.py
npm run quality:backend
```

The zero-grace control explicitly waits for its premature process exit before
observing the socket. This makes that failure deterministic across OS scheduling.
The verifier records listener refusal while the accepted request is still held,
then checks the complete accepted response before asserting that recorded ordering.
An early exit therefore fails the request-drain assertion, while a listener left
open fails the listener-order assertion. Both checks remain required for the
intended implementation. The receipt fingerprints the verifier as well as its
Haskell sources.

The runner's temporary port handoff can fail as fixture setup; this is not evidence
of a shutdown violation. The local subprocess is not container PID1 qualification.
The exact release image must separately exercise the same signals before rollout.

`formal/event-operations/HttpShutdown.tla` abstracts startup/server completion,
listener admission, accepted/completed/aborted requests, timeout and supervision.
The stop action abstracts completion of the listener-close callback, not the instant
a signal is delivered; repeated signals stutter. Two request identities and two
abstract timer ticks bound the checked state space;
two ticks represent expiration, not seconds or an OS scheduling proof. Safety
properties reject admission after stop, premature abortion without failure,
success after observed startup/server failure, uncancelled pending startup on
return, and inconsistent request accounting. Under weak fairness of stop, timer,
drain completion and supervision, the process eventually returns. This assumes
an eventual stop request, cooperative cancellation and a progressing clock. It
does not require successful request completion: work exceeding the grace interval
may be aborted. Three intentionally invalid configurations violate the named
listener, early-abort and swallowed-failure invariants. The implementation-to-model
correspondence is reviewed and tested, not formally proved.

Background worker forks, database writer fencing, in-flight provider effects,
WebSocket client recovery, HTTP/2 wire behavior, load-balancer removal, pre-listener
signal handling, backup coordination, immutable image admission and production
rollout remain separate obligations. Stopping HTTP does not prove they are drained.

## Immutable candidate image gate

The canonical `Build Image` workflow publishes a commit tag after required tests,
then qualifies the resulting immutable digest on a fresh Linux runner. Until the
qualification job succeeds, that workflow is not green. The gate supports the
current canonical `diegueins680/tdf-hq` repository; a foreign repository override
cannot qualify for this production target.

The new candidate runner retains the fixed historical diagnostic and its original
188-migration target. It requires the current checkout SHA, the tested executable
artifact, the digest, matching baked identity, byte-identical canonical migration
SQL, the exact manifest count and the expected backend as `/proc/1/exe`. Caller
Docker routing, Node options, application secrets and database routing cannot enter
the isolated fixture. The producer workflow, artifact service, Docker engine and
kernel remain trusted; hashes do not prove the entire supply chain secure.

Both TERM and INT must stop an idle application with exit0 within seven observed
seconds and restart with committed synthetic data preserved. Each signal also
stops a real public missing-course lookup while its SQL statement is witnessed
waiting behind an exclusive fixture lock. Explicit listener refusal is witnessed while PID1 remains alive before the lock
is released during the grace interval; the accepted request must finish with its expected404 and complete `Curso no
encontrado` body before successful process exit. Header-only or truncated bodies,
conflicting/incorrect Content-Length and transfer coding cannot qualify. The HTTP client lives in the database container's namespace so
application PID1 exit cannot kill the observer prematurely. These two read cases
do not establish graceful completion of every mutation, HTTP2, WebSocket, worker
or external effect. `activeRequestDrainVerified` refers only to these cases.

The database has no external network, public ports or host mounts. Only nonce-owned
labelled containers/volumes are removed; partial failures and cleanup results are
retained. Qualification never changes production or grants release authorization.
Eleven deterministic candidate controls and the eleven shared boundary controls run
in the repository gate; actual Linux execution remains a separate required image
job. A passing control suite is not a passing image execution.
