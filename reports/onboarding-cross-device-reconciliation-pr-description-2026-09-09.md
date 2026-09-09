# Draft PR: Reconcile onboarding across devices from server evidence

## Problem

A real first useful action could succeed on device A while its separate onboarding handshake was lost. Same-device mobile retry was already durable, but device B had no local action label and could not reconcile the Party's existing server evidence. The legacy completion endpoint also persisted whichever evidenced category the client submitted first, so later requests could misattribute an earlier action.

## Changes

- add authenticated `POST /session/onboarding/reconcile` with no request body or client-selected Party/action
- infer the earliest Party-bound supported action inside the signup window across artist follow, access request, event save, and moment reaction evidence
- document deterministic exact-time tie order without using intent as authority
- route legacy `/session/onboarding/complete` through the same server inference so evidence wins over a hint or explicit exit
- retain the atomic incomplete-to-complete compare-and-set and existing response shape
- reconcile web onboarding after authenticated cookie bootstrap or bearer-token login
- reconcile mobile on authenticated hydration, reconnect, and foreground
- coalesce overlapping same-session mobile triggers and serialize a direct completion behind in-flight reconciliation
- discard stale Party-A results after a Party-B switch
- clear mobile retry metadata only when `completedAt` proves durable completion
- emit onboarding analytics only for `newlyCompleted=true` and the canonical non-null server-returned value
- prevent the paused onboarding experiment from claiming conversion when another action was actually first
- update the canonical OpenAPI contract and regenerate web/mobile clients with the mobile workspace required

## Evidence and tests

- canonical API generation with `REQUIRE_MOBILE_WORKSPACE=1`: passed for web and mobile
- backend exact cross-device Hspec: 1 example / 0 failures
- backend onboarding Hspec match: 3 examples / 0 failures
- backend evidence Hspec match: 21 examples / 0 failures
- final full mobile Jest: 67 suites / 393 tests
- final mobile typecheck and zero-warning lint: passed
- mobile production-profile release check: passed, including five release assets, typecheck/lint, identity/config validation, and public Expo config
- focused web API/analytics/session reconciliation Jest: 4 suites / 19 tests
- web TypeScript: passed after correcting one strict-null issue found by the first run
- web production build and focused zero-warning lint: passed
- full serial web Jest: 185/186 suites and 1,593/1,761 tests passed; all 168 failures were confined to unchanged `CourseRegistrationsAdminPage.test.tsx` after its first timeout
- repository quality and strict catalog audit: passed; detailed component results and advisory findings are recorded in `reports/onboarding-cross-device-reconciliation-audit-2026-09-09.md`
- initial multi-word Hspec match attempts were command-shape failures and ran zero examples; they are not presented as product test failures or passes
- no native-device, PostgreSQL/staging, production, or field-analytics validation was performed

## Security, privacy, and authority

The endpoint derives Party identity only from the authenticated session and accepts no body. It cannot assign roles/modules, accept product intent as permission, or disclose domain evidence. Clients retain existing auth binding/generation guards and do not store or emit tokens, emails, phone numbers, comments, captions, object IDs, or other free text.

## Risk and rollback

Risk is concentrated in evidence-query cost and completion/analytics timing. The event-save audit path currently decodes all matching Party save timestamps to preserve SQLite/PostgreSQL correctness; this should be bounded after real query-plan evidence. Analytics remains intentionally at-most-once: a response lost after commit can undercount without duplicating events. No schema migration or new dependency is included.

Rollback consists of the focused root commit and mobile submodule commit. Existing clients remain compatible because `/complete` and the response schema are preserved; new clients against an old server fail the new reconciliation POST closed.

## Remaining gaps

- real PostgreSQL handler/query-plan validation
- synthetic two-device staging walkthrough and analytics ingestion inspection
- iOS/Android offline, foreground, account-switch, enlarged-text, and screen-reader checks
- durable deduplicated analytics receipt if lossless measurement is required
- stacked-branch reconciliation with current root/mobile `main` before merge
- next broader audit batch: service booking and checkout journeys

Detailed evidence: `reports/onboarding-cross-device-reconciliation-audit-2026-09-09.md`.

Root and mobile branch: `feature/onboarding-cross-device-reconciliation-20260909`.

Local implementation commits: root `08dacddc0f6eab7e50e94dff3272df491380c6a9`; mobile `f487de478939b3c19a62b5f54aa876d96a4eb32c`.

GitHub authentication was invalid at handoff, so these commits were not pushed and no draft PR URL exists. Publish the mobile commit before the root submodule pointer. The stack is behind current `origin/main` in both repositories and must be reconciled and fully revalidated before merge. Do not merge or deploy this stacked batch independently of its base.
