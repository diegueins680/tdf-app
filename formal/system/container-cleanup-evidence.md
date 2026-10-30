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

This correction preserves the exact prior success and absence classification,
foreign-label refusal before removal, removal by verified ID, image, commands,
socket, timeouts, hosted-runner gate, and container isolation. It adds no retry,
skip, daemon repair, or production behavior. Malformed successful inspect JSON,
remove failures, and subprocess timeouts retain their existing failure behavior;
this contract does not claim new structured diagnostics for those other paths.

The original PR677 hosted run 41009219715, job 123911806221, failed with only
`Could not confirm synthetic container cleanup`. That establishes lost response
context, not the actual Docker response or cause. The primary scenario outcome,
daemon condition and cleanup state remain unknown from that old log. No Docker
message case change or production storage failure has been established.

`scripts/test-persistent-uploads-container-diagnostics.py` executes the actual
runner's `main` with mocked subprocess boundaries and owned temporary files.
Every Docker command and Git revision query is intercepted. These are simulated
tests, not Linux container integration, actual daemon cleanup, production upload
durability or deployment qualification. The stdlib suite runs from the normal
repository-quality lane. Source fault controls must detect discarded diagnostic
context, omitted ownership refusal, acceptance of unknown inspect failures, and
suppression of the primary exception chain. Actual hosted container execution
remains a separate obligation; the original cause is unresolved.
