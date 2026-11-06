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

The absence classifier requires exit1, empty/`[]` stdout and the
case-insensitive fixed prefix `error: no such object: ` followed by the exact,
case-sensitive inspected nonce name. Existing outer whitespace stripping remains.
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
remains a separate obligation. The original twelve-test suite includes four recognized
absence variants and seven nearby refusals in addition to the existing cases.
The old classifier fails the captured lowercase-response controls and accepts
the injected daemon-error control. The revised classifier passes the suite.
The original PR677 response remains unknown; the later captured failure has a
demonstrated classification cause. Fresh hosted execution is still required.

## Exact name binding follow-up

Published head `b99e3543c02294179858282a326fc4912eb80002` described both an exact
inspected nonce name and a case-insensitive whole diagnostic. Its whole-message
casefold also accepted a case-different name. That wording did not distinguish
diagnostic-prefix capitalization from object identity. This refinement makes
that distinction explicit, preserving b99's recognized Docker spellings,
empty-output compatibility, whitespace behavior, isolation and cleanup guards.

Docker names admit uppercase characters under the tagged
[name grammar](https://github.com/moby/moby/blob/v28.5.1/daemon/names/names.go#L6).
The tagged [lookup](https://github.com/moby/moby/blob/v28.5.1/daemon/container.go#L180-L188)
passes the name unchanged, and the
[name index](https://github.com/moby/moby/blob/v28.5.1/container/view.go#L471-L492)
uses exact name bytes. The Docker CLI
[missing-object format](https://github.com/docker/cli/blob/v29.0.0/cli/command/system/inspect.go#L281)
echoes the requested reference unchanged. A diagnostic about an uppercase name
therefore is not evidence about the requested lowercase name.

The added thirteenth test executes the actual main path with a structured mocked
response naming `NAME.upper()`. The b99 baseline accepts it, so the expected
refusal assertion fails. An additive exact suffix check refuses it; removing
only that check restores the assertion failure. The unchanged twelve cases still
pass, including empty/`[]` output, old/new prefixes, unknown-response refusal,
bounded diagnostics, primary exception context and foreign-container refusal.

This is a finite simulated response-binding counterexample. No actual hosted
wrong-case response, cleanup leak or production incident has been observed.
Correct tagged CLI behavior echoes the requested name; the regression checks
the verifier's handling of misbound evidence. The already corrected lowercase
classification failure is a separate historical issue. No stricter stdout,
whitespace or general Docker-version policy is introduced. Current-head hosted
qualification, complete refinement and production durability remain separate.
