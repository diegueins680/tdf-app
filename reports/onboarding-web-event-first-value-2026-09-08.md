# Onboarding web event first value — 2026-09-08

## Outcome

Saving a public event from an authenticated web directory result now advances the existing `event_saved` onboarding milestone after the favorite API succeeds. The completion call uses the Party-scoped, retry-safe onboarding helper, so a transient completion failure does not undo the saved event and can be replayed by the existing pending-first-value recovery path.

Event removal and non-event directory favorites do not trigger this milestone. The server remains authoritative: its existing completion endpoint verifies that the current Party owns a qualifying, visible event favorite created inside the onboarding window before accepting `event_saved` evidence.

## Verification

- Focused directory Jest passed three times on the evolving and final diff: 1 suite, 6 tests on the final run.
- The event test holds the favorite request pending and proves onboarding is not called before success; after resolution it verifies Party `42` submits `event_saved` and the Party-scoped cache contains the event.
- Negative coverage verifies event removal and a retried profile save do not submit `event_saved`.
- Web TypeScript passed.
- ESLint passed with 0 errors; the repository's 102 existing warnings remain outside this slice.
- The catalog-list audit passed with no new decision. An initial standalone test object was correctly detected as an unreviewed candidate and was refactored into a derived fixture before the final passing scan.
- Repository quality passed, including formal, release, CI-selection, visual-artifact, and persona-program checks.
- The production UI build passed; Vite transformed 12,416 modules and the initial-JavaScript budget remained within its limit at 5 preloads and 412,505 gzip bytes.
- The complete UI run passed the changed directory suite. Under severe local contention, an unrelated course-sorting test exceeded its hard 5-second timeout and destabilized React's shared `act` state, causing cascading unrelated failures; the run was stopped. The course test passed unchanged with a command-line 30-second timeout, and the two cascaded suites passed independently (Intern Task Detail: 7/7; Operations Control Center: 1/1).
- `git diff --check` passed.

## Scope

This slice changes no API path, database schema, authorization rule, experiment state, or release flag. It reuses the favorite persistence added by the parent change and the existing onboarding completion/retry boundary.

No production mutation, customer communication, merge, deployment, or experiment activation was performed.
