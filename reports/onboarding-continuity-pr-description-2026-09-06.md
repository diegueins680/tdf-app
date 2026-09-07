## Summary

- add Party-bound signup, intent, eligibility, completion, and first-value persistence
- expose authenticated onboarding APIs and regenerate web/mobile contracts
- switch web and mobile completion analytics to the atomic `newlyCompleted` result
- require Party-bound, in-window server evidence before accepting `artist_followed` completion
- resume artist-follow, directory contact, and supported product intent safely after authentication
- retain mobile intent for retry after transient sync failure and expose the supported internships path
- add production migration guards, a disposable PostgreSQL rehearsal, and a consolidated continuity audit

This draft is stacked on onboarding audit PR #238. Mobile runtime changes are in draft TDF-mobile PR #40, stacked on mobile PR #39.

## Verification

- catalog list audit: passed
- production-release tests: 49/49 passed
- CI-pipeline tests: 16/16 passed
- onboarding migration forward/idempotency/constraints/cascade/non-destructive rollback/reapply: passed on disposable PostgreSQL 16
- focused web Jest: 5 suites, 27 tests passed on the second-batch tree
- web TypeScript and scoped changed-file lint: passed
- web production build and initial-bundle check: passed; 12,415 modules, 5 preloads / 412,162 gzip bytes, with Vite's existing large-chunk warning
- web lint: 0 errors and 102 existing warnings; direct full-source `eslint --quiet` passed
- clean post-reconciliation Stack build linked the backend and test executables; focused onboarding Hspec passed 3 examples / 0 failures, including missing-evidence rejection and Party-bound in-window follow acceptance
- mobile final full Jest after retry/routing changes: 66 suites, 336 tests passed with `REQUIRE_MOBILE_WORKSPACE=1`; typecheck and scoped lint passed
- regenerated web/mobile API clients match byte-for-byte

The artist-evidence SQLite case compiled and passed locally in the final post-reconciliation backend build. CI must still reproduce that result on the published head before review completion.

A full web Jest attempt before this continuation's final reconciliation reproduced the unrelated `CourseRegistrationsAdminPage` timeout/overlapping-`act()` cascade from the stacked baseline and an unrelated `PromoCodeField` failure. The run was stopped after those failures; the touched onboarding suites were rerun separately and passed 12/12. No test was disabled or weakened.

## Risks and rollback

- artist-follow completion is now server-evidence-backed; access requests, event saves, and reactions remain client-observed, so the paused experiment must remain paused until every included action and account-bound exposure semantics are authoritative
- mobile saved events remain device-local even though onboarding completion is now account-durable
- directory contact has safe routing/helper/build coverage but no authenticated browser-component run
- no new browser, native-device, real OAuth, staging email, payment, production database, or representative analytics validation was performed in this batch
- application rollback is non-destructive: the additive onboarding table and its account history remain in place; no destructive down migration is included

No merge or deployment is requested by this draft. No production data, real transaction, customer communication, or experiment activation occurred.

## Evidence

See `reports/onboarding-continuity-audit-2026-09-06.md` for the capability matrix, coverage matrix, finding IDs, implementation inventory, exact limitations, and next-batch acceptance criteria.
