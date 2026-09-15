# GitHub delivery checkpoint — 2026-09-14

Remote `main` was verified through GitHub API and Git SSH at the audited baseline
`17a33eca11d585d84435af85340beece9b51d14e`. Four new branches were pushed successfully and
four PRs were created and then read back with their draft state, head SHA and base verified:

| PR | Base | Verified implementation head |
|---|---|---|
| [336: formal foundation](https://github.com/diegueins680/tdf-app/pull/336) | `main` | `cde0e806b6a5365e1e9f5b4052b74e38ea1aa232` |
| [337: typed lifecycle API](https://github.com/diegueins680/tdf-app/pull/337) | `feat/event-operations-formal-foundation` | `b6ba963623861d1e2804b18adb46e6e4f5f5be80` |
| [338: atomic logistics](https://github.com/diegueins680/tdf-app/pull/338) | `feat/event-operations-api-foundation` | `f500b62c453e7da8981bc66138418fa59a586565` |
| [339: task commit invariants](https://github.com/diegueins680/tdf-app/pull/339) | `feat/event-logistics-transaction-hardening` | `a6cd9d1892ddafda06eab73a4e71389e802a5f7a` |

This document and PR-description clarifications are a subsequent documentation-only checkpoint;
the implementation head above is the one with the recorded local test evidence.

## Hosted-check snapshot, not a green-CI claim

At the first read-back, PR 336's formal `verify` and `postgres-foundation` passed. PR 337's
`verify`, `postgres-foundation` and `postgres-api` passed. Other required Actions jobs were
running or queued, including PR 339's new `postgres-task-commit` job. Preview checks already
reported failures: Vercel on PRs 336/339 and Cloudflare Pages on PR 338. These failures are not
waived or classified as unrelated without evidence. Current status must be read from each PR.

GitHub's Vercel status reports deployment failure and points to
[the PR 339 preview](https://vercel.com/diego-saas-projects/tdf-app-tdf-hq-ui/6nDDywUQSch8XQYCBdSuyvqUXtkA).
The Cloudflare check reports only “Build failed” and links to
[the PR 338 build logs](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/11af7b6d-eac7-4296-b2bf-646927173f0f).
Underlying provider logs were not inspected; the root causes are not yet established.

No merge, review approval, branch-protection bypass, production deployment, production migration,
credential change, provider activation, or real-money action was performed. Repository integrations
automatically started their normal PR preview checks; no manual deployment was initiated.

## Activation and next implementation gates

- Correct receipt replay to reauthorize current access; test revoked/expired grants and stale
  offline commands. Database exception-log redaction also needs explicit security tests.
- Complete task/RACI contextual authorization, time-window and membership-removal behavior,
  audit-preserving command APIs and user-visible database-conflict handling before exposing the
  sidecar. The task commit correction does not implement those missing controls.
- Resolve hosted-check failures and finish the remaining phase-3/4 work, then the later phases
  listed in the delivery plan. The overall end-to-end definition of done is not met.
