# Serial UI test worker recycling

Requirement: `SYS-UI-TEST-WORKER-001`.

The selected UI quality job must finish the selected Jest suite without retaining
every file's execution heap in a single in-band process. On source
`b5609fdb1bc3bd9c5978d6b3ef81b8e80742c3f0`, hosted job `125064289764` aborted
with exit 134 and a fatal JavaScript heap allocation failure near 4 GB before
producing a complete test summary. This is a confirmed execution failure; it
does not identify the underlying source of retained memory.

The default workspace test command must use one worker slot and Jest's supported
idle-memory recycling. It must preserve test discovery, assertions, timeouts,
failure propagation and the rest of the UI quality sequence. A failing assertion
must not become a passing process result or prevent the other selected fixture
files from reporting their results.

The configured idle heap budget is 1 GiB. This is an operational budget for the
existing runner, not a uniquely correct business value. Jest checks memory after
a file finishes and recycles an over-budget worker before reusing its slot.
One slot preserves serial scheduling; removing forced `--runInBand` permits
recycling. See the pinned-major
[Jest configuration contract](https://jestjs.io/docs/30.0/configuration#workeridlememorylimit-numberstring).

Under the documented Jest behavior, when a completed file's idle memory exceeds
the configured limit, its process must not execute the next file. This is a
conditional implementation contract, not a theorem about the operating system,
the parent process, arbitrary files, garbage collection or total resident memory.
An individual file can exceed the budget before its boundary. An in-file fatal
error, a worker startup error or exhausted retries must still fail qualification.
Explicit developer debugging overrides such as `--runInBand` are outside the
default CI command contract and cannot count as recycling evidence.

Regression checks invoke the actual workspace command with dependency-free
CommonJS fixtures and a controlled 1 KiB threshold. They require three distinct
execution PIDs through worker slot 1, all three file results and no pending tests.
A negative control forces the previous in-band behavior and observes one PID.
A second negative fixture contains a real failing assertion: all three files must
report results and the process must exit with failure. The scaled fixture tests
recycling and failure preservation without allocating a large heap.

Assurance is `TESTED` for these controlled fixtures. Full-suite and hosted Node 22
qualification require separate revision-bound evidence. No application test,
assertion or timeout is removed or relaxed. This correction does not claim that
application, transformer or V8 memory retention has been eliminated.
