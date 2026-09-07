## Summary

- add Party-bound signup, intent, eligibility, completion, and first-value persistence
- expose authenticated onboarding APIs and regenerate web/mobile contracts
- switch web and mobile completion analytics to the atomic `newlyCompleted` result
- require Party-bound, in-window server evidence before accepting `artist_followed` or `access_requested` completion
- resume artist-follow, directory contact, and supported product intent safely after authentication
- retain mobile intent for retry after transient sync failure and expose the supported internships path
- isolate mobile saved events by authenticated Party, preserve ambiguous legacy/corrupt data, and surface storage failures without false success analytics
- add production migration guards, a disposable PostgreSQL rehearsal, and a consolidated continuity audit

The parent onboarding PRs have merged, so this root draft and mobile draft PR #40 now target `main` directly.

## Verification

- catalog list audit: passed
- production-release tests: 49/49 passed
- CI-pipeline tests: 16/16 passed
- onboarding migration forward/idempotency/constraints/cascade/non-destructive rollback/reapply: passed on disposable PostgreSQL 16
- focused web Jest: 5 suites, 27 tests passed on the second-batch tree
- web TypeScript and scoped changed-file lint: passed
- web production build and initial-bundle check: passed; 12,415 modules, 5 preloads / 412,162 gzip bytes, with Vite's existing large-chunk warning
- web lint: 0 errors and 102 existing warnings; direct full-source `eslint --quiet` passed
- the current 184-module backend test target compiled and linked; another process then reacquired Stack's lock before test launch, so the blocked wrapper was stopped and the freshly linked executable passed onboarding 3/3 plus access-request evidence 1/1, including missing, cross-Party, pre-signup, future, valid, and repeated-claim cases
- mobile focused saved-event/component Jest: 2 suites, 9 tests passed
- mobile final full Jest after saved-event containment: 66 suites, 340 tests passed with `REQUIRE_MOBILE_WORKSPACE=1`; typecheck and full lint passed
- regenerated web/mobile API clients match byte-for-byte

The artist-follow and access-request evidence SQLite cases compiled and passed locally via the freshly linked test executable. The interrupted Stack wrapper is not counted as a passing command; CI must reproduce the result on the published head before review completion.

A full web Jest attempt before this continuation's final reconciliation reproduced the unrelated `CourseRegistrationsAdminPage` timeout/overlapping-`act()` cascade from the stacked baseline and an unrelated `PromoCodeField` failure. The run was stopped after those failures; the touched onboarding suites were rerun separately and passed 12/12. No test was disabled or weakened.

## Risks and rollback

- artist-follow and access-request completion are now server-evidence-backed; event saves and reactions remain client-observed, so the paused experiment must remain paused until every included action and account-bound exposure semantics are authoritative
- mobile saved events remain device-local even though onboarding completion is now account-durable; their on-device cache is Party-scoped, corrupt data is preserved, the unowned legacy key is quarantined, and remote convergence/import remain deferred
- directory contact has safe routing/helper/build coverage but no authenticated browser-component run
- no new browser, native-device, real OAuth, staging email, payment, production database, or representative analytics validation was performed in this batch
- application rollback is non-destructive: the additive onboarding table and its account history remain in place; no destructive down migration is included

No merge or deployment is requested by this draft. No production data, real transaction, customer communication, or experiment activation occurred.

## Evidence

See `reports/onboarding-continuity-audit-2026-09-06.md` for the capability matrix, coverage matrix, finding IDs, implementation inventory, exact limitations, and next-batch acceptance criteria.
