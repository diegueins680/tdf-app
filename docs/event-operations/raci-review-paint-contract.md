# RACI review paint contract — before implementation

Parent #411, exact `f2f01194090a241777c451d06cb0848af7aa759a`.
The user approved the CI investigation/fix plan with Continue. `gh-fix-ci` guides
Actions diagnosis; its unavailable auxiliary plan skill is replaced by the written
plan. No domain, permission, command, schema, migration or production change.

Hosted Linux WebKit reported one failed light-theme RACI journey: foreground and
background contrast as low as 2.36:1, against the existing 4.5:1 assertion. 108
journeys passed and 12 were skipped by existing platform conditions. The downloaded
trace contains a 225ms opacity transition on the dialog container; inline opacity
1 is a target, not proof of computed/composited readiness. Cancel focus after
onEntered also does not measure that state. The later failure screenshot appears
opaque. These observations motivate a synchronization hypothesis, not a proven
Linux engine diagnosis. Three unchanged macOS WebKit repetitions passed.

| ID | Executable obligation |
| --- | --- |
| RP01 | Before the one document-wide axe scan, require the review and its ancestors to be fully opaque/visible and its subtree/ancestors to have no running, pending or paused animations/transitions. Use the existing bounded expectation timeout, not fixed sleeps, disabled motion, or retries of axe. |
| RP02 | Preserve before/ready computed-style diagnostics including opacity, foreground/background, transition and animation state. A permanently translucent review must fail readiness, not be scanned repeatedly until a favorable result. |
| RP03 | A controlled synthetic finite fade is not ready while paused halfway; becomes ready only at its endpoint. An opaque but low-contrast element still fails the unchanged axe scan. |
| RP04 | Keep light/dark, keyboard cancel focus, explicit confirmation, exact uncertain retry, foreign-request blocking, full-document serious/critical axe checks and the existing low-contrast negative control. |
| RP05 | Do not claim the hosted root cause fixed until hosted evidence confirms the corrected journey. No style or domain change is justified by the local non-reproduction. |

This is an executable browser synchronization contract, not a new authorization
state machine. Unchanged bounded RaciWebEditor/TaskView models and server checks
remain the domain authority; no new TLC/Alloy execution is claimed for this test-only
increment. Implementation must fail closed if paint never becomes ready. Rollback
reverts only helper/tests/documentation and retains all original accessibility
assertions. Catalog governance is separate and cannot be waived to make CI green.
