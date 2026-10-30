# Synthetic container cleanup failure evidence

The hosted private-upload runner must refuse to report success when cleanup
cannot be confirmed. An unexpected nonzero `docker inspect` response must retain
its operation, generated synthetic container name, return code, bounded stdout
and stderr, truncation flags, and the synthetic cases that actually completed.
Each captured stream is limited to 2000 characters before JSON serialization.
The runner owns at most five synthetic cases; it does not enumerate host-wide
containers, inspect private upload contents, or include inherited environment.
An earlier scenario exception must remain in Python's displayed exception chain
if cleanup also fails. Capturing more evidence must not convert a refusal into
success.

This correction preserves successful inspection and cleanup,
foreign-label refusal before removal, removal by verified ID, image, commands,
socket, timeouts, hosted-runner gate, and container isolation. It adds no retry,
skip, daemon repair, or production behavior. Malformed successful inspect JSON,
remove failures, and subprocess timeouts retain their existing failure behavior;
this contract does not claim new structured diagnostics for those other paths.

The original PR677 hosted run 41009219715, job 123911806221, failed with only
`Could not confirm synthetic container cleanup`. That establishes lost response
context, not the actual Docker response or cause. The primary scenario outcome,
daemon condition and cleanup state remain unknown from that old log. No Docker
message case change or production storage failure can be established from that
original log.

A later actual run, PR679 CI41014521942/job123928715883, retained all five
completed cases and the exact inspect response: exit1, stdout `[]`, and stderr
`error: no such object: <the exact generated name>`. The case-sensitive
`No such` substring check rejected this ordinary absence response. Conversely,
that old check could accept a daemon connection error containing `No such file`.

The absence classifier now requires exit1, empty/`[]` stdout and an exact
case-insensitive `error: no such object: <the inspected nonce name>` diagnostic.
It admits the captured new spelling and the prior Docker spelling, but refuses
wrong names, extra messages, another exit code, a nonempty object result and
daemon/transport errors. Successful inspect still checks the nonce label before
removing by ID. Unknown responses still retain their bounded diagnostics and fail.
This changes the absence classifier deliberately; it does not prove that every
Docker version or concurrent daemon state uses these responses.

`scripts/test-persistent-uploads-container-diagnostics.py` executes the actual
runner's `main` with mocked subprocess boundaries and owned temporary files.
Every Docker command and Git revision query is intercepted. These are simulated
tests, not Linux container integration, actual daemon cleanup, production upload
durability or deployment qualification. The stdlib suite runs from the normal
repository-quality lane. Source fault controls must detect discarded diagnostic
context, omitted ownership refusal, acceptance of unknown inspect failures, and
suppression of the primary exception chain. Actual hosted container execution
remains a separate obligation. The twelve-test suite includes four recognized
absence variants and seven nearby refusals in addition to the existing cases.
The old classifier fails the captured lowercase-response controls and accepts
the injected daemon-error control. The revised classifier passes the suite.
The original PR677 response remains unknown; the later captured failure has a
demonstrated classification cause. Fresh hosted execution is still required.
