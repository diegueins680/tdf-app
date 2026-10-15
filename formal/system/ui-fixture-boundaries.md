# UI fixture lifecycle boundaries

`SYS-UI-FIXTURE-001` applies to the selector and notification regression harnesses.
It does not establish full implementation or authorization refinement.

The selector clock advances through its real 300 ms debounce, including an assertion
that no request occurs at 299 ms. Each case resets mock implementations and queued
responses and restores real timers after unmount. Existing selection, failed-read,
pagination, deduplication, request-abort and late-response assertions remain.

A new-tab page event occurs before document loading completes. The notification
fixture awaits the same load boundary used by page.goto before its existing redirect
assertion. It retains the eight-second application assertion, real tab-opening
interaction, owned profile content, original-tab URL and no unrelated mutations.

Keep failed full-suite evidence even when isolated repetitions pass. Local fixture
success does not establish production authentication or authorize release.
