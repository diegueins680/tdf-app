# Web onboarding live recovery — 2026-09-08

## Outcome

An authenticated web session no longer needs a reload to retry queued onboarding intent or first-value completion after connectivity returns. The application shell replays both Party-scoped queues when it mounts and whenever the browser emits `online`.

Reconnect signals are coalesced while recovery for the same Party is already in flight, preventing duplicate requests from rapid or repeated browser events. An account change can start recovery for the new Party without allowing the older request to clear its state; the existing Party-ownership guards remain responsible for callback attribution. The listener is removed when the authenticated shell effect is replaced or unmounted.

## Runtime coverage

Shell tests verify:

- authenticated mount replays both onboarding queues for the current Party;
- reconnect replays both queues without a page reload;
- repeated reconnect events do not duplicate in-flight requests; and
- unmount removes the reconnect listener.

The queue helpers' existing tests continue to cover Party isolation, account-switch races, stale responses, invalid stored values, idempotent completion, and the rule that authentication tokens are never stored.

## Verification

- Focused `AppShell` Jest: 1 suite, 11 tests passed.
- Web TypeScript passed.
- Strict ESLint passed for both changed TypeScript/TSX files.
- Strict catalog-list audit passed with no decision change.
- Repository quality passed, including formal, release, CI-selection, and persona-program checks.
- The first full UI run reproduced the known unrelated `CourseRegistrationsAdminPage` timing/overlapping-`act()` cascade. That initial failure contaminated three later untouched suites; the diagnostic totals were 4 failed and 185 passed suites, with 192 failed and 1,592 passed tests. The changed `AppShell` suite passed in that run.
- All four untouched suites then passed in fresh processes: `CourseRegistrationsAdminPage` 526/526, `MarketplaceOrdersPage` 58/58, `PublicBookingPage` 11/11, and `InternshipsPage` 15/15.
- A clean full UI rerun passed all 189 suites and 1,784 tests. Lint completed with 0 errors and 102 existing warnings; TypeScript and the production build passed; the initial-JavaScript budget remained within its limit at 5 preloads and 412,198 gzip bytes.
- `git diff --check` passed.

## Scope

This slice changes no API, database schema, role, module, permission, analytics taxonomy, experiment assignment, or release flag. It adds browser lifecycle coordination around the existing Party-scoped queues and server-authoritative operations. It performs no real authentication, production mutation, customer communication, merge, deployment, or experiment activation.
