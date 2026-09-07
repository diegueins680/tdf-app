## Summary

- add Party-bound signup, intent, eligibility, completion, and first-value persistence
- expose authenticated onboarding APIs and regenerate web/mobile contracts
- switch web and mobile completion analytics to the atomic `newlyCompleted` result
- resume artist-follow and supported product intent safely after authentication
- add production migration guards, a disposable PostgreSQL rehearsal, and a consolidated continuity audit

This draft is stacked on onboarding audit PR #238. Mobile runtime changes are in draft TDF-mobile PR #40, stacked on mobile PR #39.

## Verification

- catalog list audit: passed
- production-release tests: 49/49 passed
- CI-pipeline tests: 16/16 passed
- onboarding migration forward/idempotency/constraints/cascade/non-destructive rollback/reapply: passed on disposable PostgreSQL 16
- focused web Jest: 3 suites, 12 tests passed on the reconciled tree
- web TypeScript: passed on the reconciled tree
- web production build and initial-bundle check: passed; 5 preloads / 412,160 gzip bytes, with Vite's existing large-chunk warning
- web lint: 0 errors and 102 existing warnings; direct full-source `eslint --quiet` passed
- focused backend onboarding Hspec before stacked-base reconciliation: 3 examples, 0 failures; the final rerun remained queued behind an unrelated shared Stack build lock and produced no test result
- mobile final full Jest after stacked-base reconciliation: 66 suites, 331 tests passed; required typecheck and lint passed

A full web Jest attempt before this continuation's final reconciliation reproduced the unrelated `CourseRegistrationsAdminPage` timeout/overlapping-`act()` cascade from the stacked baseline and an unrelated `PromoCodeField` failure. The run was stopped after those failures; the touched onboarding suites were rerun separately and passed 12/12. No test was disabled or weakened.

## Risks and rollback

- onboarding completion is idempotent but still records a client-observed action; the paused experiment must remain paused until server-observed action and account-bound exposure semantics exist
- mobile saved events remain device-local even though onboarding completion is now account-durable
- no browser, native-device, real OAuth, staging email, payment, production database, or representative analytics validation was performed in this batch
- application rollback is non-destructive: the additive onboarding table and its account history remain in place; no destructive down migration is included

No merge or deployment is requested by this draft. No production data, real transaction, customer communication, or experiment activation occurred.

## Evidence

See `reports/onboarding-continuity-audit-2026-09-06.md` for the capability matrix, coverage matrix, finding IDs, implementation inventory, exact limitations, and next-batch acceptance criteria.
