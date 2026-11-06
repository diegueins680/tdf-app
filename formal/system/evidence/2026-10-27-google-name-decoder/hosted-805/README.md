# Hosted Google default-name regression evidence

This receipt records **TESTED** examples for AUTH-ENTRY-002. It adds no complete
requirement or implementation/refinement proof and changes no application code.

The backend actually checked out `80578a61d3c1086039f3ff9e103031c56b0515fe`
in [job 121991132659](https://github.com/diegueins680/tdf-app/actions/runs/40444046735/job/121991132659).
Its entire Git tree `3734b78af47d20f286d54a0a3c3e7de49b9c6496` equals source
candidate `d56b337bec10299484aba457a6874cee22f970db` and the later main-integrated
candidate `985be1ca49908260d57ca608e1faf278115ed10c`. Both exact hosted checkout
commits below are retained as ancestors of this evidence branch for reproduction.

| Execution | Observed result | Scope |
| --- | --- | --- |
| Full Hspec aggregate | 4,058 examples, 0 failures, 21 pending | Pending external runners are not counted as executed |
| Explicit Google privacy group | 15 examples, 0 failures, 0 pending | Compiles/imports real TDF.ServerAuth, then selects the group with --fail-on=empty |
| Separate exact-source probe | 19 examples, 0 failures, 0 pending | JSON decoder, validator, name selector and four existing helper examples |
| Restored email fallback | 19 examples, 8 failures | Deliberate mutation detected; full restored ServerAuth matches the original defect |

The successful build/test **step** finished at 03:43:02Z on 2026-10-27.
The surrounding backend job and workflow were later **cancelled**. This receipt
never classifies that whole job/workflow as passed, nor establishes completion
of its remaining HTTP/database/concurrency runners. Python 3.11.2, Stack 3.3.1,
GHC 9.10.3, Hspec 2.11.17, Aeson 2.2.5.0, Text 2.1.3 and Time 1.12.2 are read
from the actual hosted probe, not inferred from image labels.

`native.json` binds its eight inputs; `backend-inputs.json` binds 451 backend
source/test/build files. All 1,363 inputs of the original assurance receipt still
match, using exact pinned Mobile Git blobs for the uninitialized evidence-only
checkout. No new solver execution is claimed here. Local and hosted tool versions
remain separately identified.

## Browser failure retained

The first persona run reported 593 passed, one failed and 22 skipped. Its WebKit
cart lifecycle test exhausted the unchanged 30-second total test budget at the
last close action. The trace shows 71.626 ms between starting that action and
starting timeout teardown. This establishes cumulative deadline exhaustion;
it does not establish a persistent close-button malfunction or resolve the
underlying timing variance. The trace, screenshot, error context and original
70,100,372-byte artifact SHA256 are retained.

A later [persona job 122001043138](https://github.com/diegueins680/tdf-app/actions/runs/40446969997/job/122001043138)
checked out `f2723ec0867d8f3a4a6cf003f94db16c26efcd29`, with the same entire Git
tree. Its full invocation passed 594 cases with 22 skips, followed by separate
10- and 25-case invocations. Node 22.23.3, Playwright 1.59.1 and WebKit 26.4 are
reported in both browser logs. No source, timeout, retry rule or assertion changed
between these executions. The original failure remains evidence, not a pass.

## Reproduce and limits

Fetch this evidence branch, check out the recorded backend execution commit,
and use the pinned Stack project and disposable test services:

```sh
REQUIRE_STACK=1 STACK_ROOT=/path/to/stack bash scripts/quality-backend.sh
STACK_ROOT=/path/to/stack python3 scripts/verify-google-account-name.py
```

Use the workflow's pinned Playwright container and commands to reproduce browser
runs. Source hashes and raw logs are included; artifact hashes are checked in
`qualification.json`. Extraction/caller correspondence, compilers/libraries and
execution infrastructure remain trusted. Synthetic JSON tests do not execute
Google HTTP or account persistence. No provider approval, historical-name repair,
production correspondence, complete refinement or arbitrary-schedule proof is
claimed. All 150 full requirements remain UNPROVEN in the source assurance report.

Publication on this separate evidence branch preserves the reviewed application
head. PR #646 still requires its current CI, latest-main compatibility, resolved
conversations and canonical FIFO admission before normal integration. No merge
or deployment is authorized merely by this receipt.
