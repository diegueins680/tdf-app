# Task browser fixture isolation contract

This follow-up changes the synthetic browser harness, not feature code, authorization,
domain models or the API. TV-01–06 and the previously checked `TaskView`/`TaskRead` models
remain applicable; no new formal transition or stronger model-checking claim is introduced.

Before changing interception, require:

- Only an explicit HTTP loopback base URL is accepted; missing/remote bases fail closed.
- Every foreign HTTP origin remains intercepted and blocked, including URLs whose paths
  resemble Vite modules and origins resembling the local host.
- Every canonical API request remains intercepted, including session, task, event and
  profile requests, unknown endpoints, queries and non-GET commands.
- Only the exact local Vite `/src/` and `/node_modules/` namespaces bypass the test-runner
  callback. They already reached the same local development server via `route.continue`;
  no production API lives in these namespaces. Other requests retain the existing handler.
- There is no dependency prewarming, synthetic authentication injected into application
  state, skipped request assertion, retry, changed timeout, or disabled StrictMode.

Executable checks must cover exact origins/ports, hostile lookalikes, query strings, both
local module namespaces and all API families used by the task privacy boundary. Browser
checks must retain malformed-response, invalid-selector, exact refresh/retry count,
no-parent-read, locale, keyboard and axe assertions. Browser results are fixture integration,
not server/database E2E or evidence of production startup latency.

Baseline PR 373 traces show 407 requests per single navigation (1,206 for three selector
navigations). The task request completes in 38–78 ms in the final failed desktop traces;
its module starts at 9.034–10.936 seconds. Playwright's installed dispatcher sends matching
requests to the test runner's callback; a serialized RegExp allows unmatched module traffic
to continue in the driver. This is a testable overhead hypothesis, not yet a measured fix.

Rollback only the fixture matcher/import and its checks; no data or application rollback.
