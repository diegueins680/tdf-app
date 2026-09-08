# Web onboarding-intent recovery — 2026-09-08

## Outcome

Web existing-account login no longer drops a selected product intent when the authenticated intent update is temporarily unavailable. Password and Google login now synchronously queue the validated intent under a Party-scoped browser key before starting the server update. Only the intent protocol value is stored; the transient login token is passed to the immediate request but is never written to browser storage.

The authenticated application shell retries pending intent with its current cookie-backed session. A response that finishes after an account switch cannot clear the initiating Party's queue. A newer choice cannot be erased by an older in-flight request, another Party's queue remains untouched, and invalid stored values are removed without reaching the API. The server remains authoritative: it validates the closed product-intent protocol and upserts intent using only the authenticated Party identity.

New-account password and Google signup remain unchanged because they already persist intent atomically inside account creation. Intent remains personalization data and never grants a role, module, permission, or feature flag.

## Runtime coverage

Focused recovery and shell tests verify:

- immediate synchronization receives the transient login token while browser storage does not;
- transport failure retains exactly the Party-scoped intent and replay clears it after acknowledgement;
- replay uses the current authenticated shell without retaining credentials;
- a late response after account switch preserves the initiating Party's pending intent;
- an older response cannot clear a newer queued choice;
- Party queues remain isolated;
- invalid intent data is removed without an API request; and
- missing or malformed Party identity cannot write or call the API.

## Verification

- Focused web Jest: 4 suites, 35 tests passed.
- Web TypeScript passed.
- Strict ESLint passed for all changed TypeScript and TSX files.
- Strict catalog-list audit passed with no new decision entry.
- Repository quality passed, including formal, release, CI-selection, and persona-program checks.
- The first full UI run reproduced the known `CourseRegistrationsAdminPage` order/timing cascade: its 168 failures were the only failures while the other 188 suites and 1,614 tests passed. The unchanged admin suite then passed 526/526 in isolation, and the unchanged full UI rerun passed all 189 suites and 1,782 tests.
- The successful full UI rerun also passed TypeScript and the production build; lint completed with 0 errors and 102 existing warnings, and the initial-JavaScript budget remained within its limit at 5 preloads and 412,175 gzip bytes.
- `git diff --check` passed.

## Scope

This slice changes no endpoint, database schema, role, module, permission, experiment assignment, analytics taxonomy, or release flag. The browser queue coordinates retry only; it is not onboarding authority and contains no password, API token, email, profile content, or security grant. No real authentication, cross-browser/device run, customer communication, merge, production deployment, mutation, or experiment activation is included.
