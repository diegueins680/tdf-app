# Web first-value recovery — 2026-09-07

## Outcome

Successful web first-value actions no longer lose their onboarding completion handshake when the completion request is temporarily unavailable. Before requesting authoritative completion, the shared web helper stores only the allowlisted first-value label under a Party-scoped browser key. Any authoritative response clears that exact pending label; a transport failure retains it for a later retry.

The authenticated application shell replays a pending value only for its current Party. A callback that finishes after an account switch cannot clear the initiating Party's queue or emit first-value/completion analytics into the next account. Invalid stored values are removed without reaching the API, and queues belonging to other Parties remain untouched. Completion analytics still require `newlyCompleted: true`, preserving the existing one-shot server authority.

## Runtime coverage

Focused helper and shell tests verify:

- a failed completion handshake retains the initiating Party's allowlisted value;
- the authenticated shell invokes recovery for its current Party;
- a successful replay clears the queue and emits the shared analytics pair once;
- an idempotent `newlyCompleted: false` response clears the queue without duplicate analytics;
- an account switch during the request preserves the original Party's retry and suppresses analytics;
- another Party's queue is not read or removed; and
- invalid stored data is deleted without an API request.

The existing Artist profile, Fan Hub, and access-request consumer suites remain green alongside the shared recovery coverage.

## Verification

- Focused web Jest: 5 suites, 29 tests passed.
- Web TypeScript passed.
- Strict ESLint passed for all changed TypeScript and TSX files.
- Strict catalog-list audit passed with no new decision entry.
- Repository quality passed, including formal, release, CI-selection, and persona-program checks.
- Full UI quality passed: lint completed with 0 errors and 102 existing warnings; 188 suites and 1,776 tests passed; TypeScript and the production build passed; the initial-JavaScript budget remained within its limit at 5 preloads and 412,158 gzip bytes.
- `git diff --check` passed.

## Scope

This slice changes no endpoint, database schema, role, module, permission, experiment assignment, analytics taxonomy, or release flag. The browser queue is retry coordination rather than completion authority: the authenticated server still validates Party-bound domain evidence and eligibility. If the server commits completion but its response is lost, a replay can return `newlyCompleted: false`; the queue converges, but client analytics intentionally do not invent the missed conversion. No real authentication, cross-device browser run, customer communication, merge, production deployment, mutation, or experiment activation is included.
