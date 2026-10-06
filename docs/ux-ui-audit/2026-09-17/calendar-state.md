# Calendar return and authoritative state — UX-260917-043/044

The runtime-schema repair (#444) made the existing journey testable. A controlled
rejected OAuth exchange in the old production bundle dispatched 26–30 requests in
700 ms and retained the one-use code in the address bar. An unrelated local marker
also claimed saved credentials while the actual isolated config endpoint returned
null. These are confirmed audit defects, not hypothetical provider outages.

The successor consumes/removes the callback code before queued dispatch, retains
input after failure and requires explicit retry. Busy refs reject immediate duplicate
clicks. The microtask lets the mutation observer mount through React StrictMode's
setup/cleanup cycle; the first test version exposed a detached observer leaving the
retry button pending and was fixed, not classified as a passing run.

Session changes remount the form and partition cached data. Callback effects check
both the mounted instance and current session object, including logout before React
renders and A→B→A. Config queries identify the selected calendar, clear confirmation
on error, and cannot replace a newly persisted exchange receipt with an older query.
Sync timestamps come from the server; the current editable date range is no longer
presented as the historical synchronization range. Preference cleanup explicitly
keeps the server connection, and config refresh no longer claims Google verification.
No new revocation endpoint, permission or external account operation is introduced.

Fourteen component tests pass, including storage denial, StrictMode failure/retry,
session changes, query/exchange races and concurrent sync. TypeScript, lint and the
production build pass (365624 gzip bytes; five preloads, below the existing budget).
See `formal/event-operations/README.md` for the executable model's exact bounds,
properties, three meaningful counterexamples and fairness assumptions. The initial
model stopped at an expected terminal state; quiescent deadlock checking is now
explicitly disabled without removing the safety/liveness checks. Full pinned TLC
1.7.2 / TLC2 2.17 and Alloy 6.2.0 checks pass using Temurin 21.0.12.1+1.

The browser uses real isolated signup/session/config/events and intercepts only the
synthetic rejected token exchange. It does not perform or certify Google consent,
provider token exchange, revocation, or real event synchronization. Production and
manual assistive-technology checks remain distinct release/coverage gates.

Nine final browser cases pass across Chromium/Firefox/WebKit at 320/768/1280px:
one automatic request plus one deliberate retry, URL code removed, no false saved
connection, zero axe violations, page errors or horizontal overflow. The first
successor preview used a default same-origin API base against an `/api`-only proxy
and received HTML at signup. It is excluded; the qualified production bundle uses
explicit local `VITE_API_BASE=/api` (365633 gzip bytes). No production signup occurred.
See [versioned receipt](evidence/calendar-state-verification.json) and the before/after
phone screenshots. This increment depends on #444's additive schema; merge it only
after that dependency and the exact-head checks/review pass.
