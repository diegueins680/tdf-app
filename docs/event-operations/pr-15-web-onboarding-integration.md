# PR 15 — canonical web onboarding compatibility

Base: draft PR 357, `feat/event-task-read-api`, commit
`881882e15cd1b2400cc2e0452ea50683dea675de`.
Branch: `fix/web-onboarding-reconciliation-integration`. Draft only.

## Scope and sequence

The [WO-01–05 contract](web-onboarding-integration-contract.md) was written first.
It audits the obsolete local-marker imports left behind by the canonical server-evidence
onboarding migration. Removing test mocks that invented those exports reproduces both
ES-module loading failures. Provider tests independently reproduce three missing reconnect
behaviors. The new finite TLA+ model, three precise mutation controls and the complete
existing TLC/Alloy runner passed **before** production implementation changes.

- Remove the two obsolete imports and signup-marker calls; do not recreate a second local
  completion authority or stub exports merely to satisfy TypeScript.
- Keep `SessionProvider` as the first-value reconciliation owner. Reconnect uses the existing
  authenticated, bodyless endpoint; in-flight requests coalesce within the effect lifetime.
  Dispatch and result consumption check cancellation, generation and Party. Cleanup removes
  the listener; old effects cannot clear a new effect's independent in-flight request.
- Preserve Shell's Party-scoped pending-intent retry and coalescing. First-value reconnect
  assertions move to the actual provider rather than an obsolete mocked Shell helper.
- Keep consent/versioned terms, Google `accountCreated`, password signup, login, analytics
  and navigation. Tests assert signup itself does not claim first-value completion.
- Update the existing personal-data test's incomplete session API mock with real canonical
  export names. Its privacy assertions are retained; no test is skipped or weakened.

No backend/API/schema/migration/generated-client/mobile/feature-flag/provider changes.
No rendered pixels or locale strings change. Mocked component tests are not browser E2E,
real signup, external analytics ingestion, or accessibility conformance evidence.

## Executed checks

Commands run from the root unless otherwise stated:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
npm run typecheck:ui
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/session/SessionContext.reconciliation.test.tsx src/pages/LoginPage.test.tsx src/routes/AppShell.test.tsx src/session/onboardingIntentRecovery.test.ts src/analytics/onboardingProgress.test.ts src/api/session.test.ts src/api/eventOperations.test.ts
./node_modules/.bin/eslint tdf-hq-ui/src/pages/LoginPage.tsx tdf-hq-ui/src/pages/LoginPage.test.tsx tdf-hq-ui/src/routes/AppShell.tsx tdf-hq-ui/src/routes/AppShell.test.tsx tdf-hq-ui/src/session/SessionContext.tsx tdf-hq-ui/src/session/SessionContext.reconciliation.test.tsx tdf-hq-ui/src/session/SessionProvider.personalData.test.tsx --max-warnings=0
```

- Complete formal runner: exit 0. New model: 883 generated / 179 distinct states, depth 7;
  three expected invariant violations detected. Total: 23 expected negative controls,
  two SAT Alloy scenarios and 11 UNSAT assertions, within the documented finite bounds.
- Whole-web application typecheck: exit 0 after implementation. The earlier run returned
  exactly the two missing-export errors from PR 14; it was started before production edits.
  No compiler configuration or test exclusion changed.
- Initial seven-suite regression: 69 tests passed. Expanded nine-suite run: 76 passed,
  one new password assertion failed because React Query also supplies a mutation-context
  argument. The test now checks the exact consent payload plus that context; product
  behavior was not altered to accommodate the fixture.
- Focused ESLint: exit 0. Independent whole-web gate failures are recorded below;
  no failed or incomplete check is a pass.

Final focused regression:

```sh
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/session/SessionContext.test.ts src/session/SessionProvider.personalData.test.tsx src/pages/LoginPage.test.tsx src/session/SessionContext.reconciliation.test.tsx src/routes/AppShell.test.tsx src/session/onboardingIntentRecovery.test.ts src/analytics/onboardingProgress.test.ts src/api/session.test.ts src/api/eventOperations.test.ts
npm run quality:repo
```

- Nine suites / **77 tests passed**, exit 0, after the password fixture correction.
- Repository quality passed, exit 0: 8 internship audit, 42 loop, 4 heuristic-audit,
  61 release-safety, 23 CI-scope, 2 visual-artifact metadata and 3 persona-program tests.
  The heuristic audit reported 9,554 findings, 0 critical, 0 errors and 355 warnings;
  this is distinct from the actual TLC/Alloy execution above. Artifact metadata tests
  do not establish that new browser screenshots were taken.
- Shell syntax, whitespace and all 93 local documentation links pass. Backend, generated
  API, mobile pointer, TypeScript configurations and Jest configuration are unchanged
  (verified with an exact base diff). Node 24.8.0 / npm 11.6.0 were used locally.

### Whole-web gates remain blocked

```sh
npm run test --workspace=tdf-hq-ui
npm run lint --workspace=tdf-hq-ui
npm run build --workspace=tdf-hq-ui
```

- Full Jest: reported `CourseRegistrationsAdminPage.test.tsx` failures after 81.415 s,
  including overlapping `act()` warnings and empty-render assertions. It then produced
  no new suite results across repeated polls while consuming CPU. A one-second native
  sample showed promise microtasks/string/regexp work, not a conclusive JavaScript root
  cause. Only this invocation's known Jest PID was interrupted with SIGINT; exit **130**.
  This is a **failed, incomplete** full run, with no aggregate passing count claimed.
  The course source/test are unchanged from the base. The prior cross-device audit also
  reports course-suite failures; that history is not a fresh baseline reproduction.
- Whole-web ESLint: exit **1**, three errors in unchanged files:
  `ArtistPublicPage.intent.test.ts:6` (`no-unsafe-argument`),
  `ArtistPublicPage.tsx:193` and `FanHubPage.tsx:412` (`no-floating-promises`). Source
  inspection finds the intent test imports absent artist-resume helpers, while the other
  two sites still call the now-async canonical completion helper without handling its
  Promise. Fixing artist-follow continuity needs its own bounded UI/contract regression;
  no fake helper, suppression or test deletion is introduced here.
- Build: exit **1**. TypeScript and Vite bundling completed (12,440 modules; Vite 7.3.6),
  but the existing bundle gate rejected **420,604 gzip bytes** versus **419,840** allowed
  (764 bytes over). The >500 kB chunk advisory also remains. The size limit and chunk
  configuration are untouched. The original base cannot build because of its broken
  imports, so this report does not claim that the exact byte overage predates this repair.

The aggregate UI quality gate therefore remains red. Neither the 77 focused passing tests
nor the repaired application typecheck substitutes for these blockers. No production
build/deploy or complete web compatibility claim is made.

### Backend compilation checkpoint from PR 14

`stack test tdf-hq --fast --no-run-tests --no-terminal` in `tdf-hq/` finished with exit 0
on the published base commit above, linking the test executable and installing `tdf-hq-exe`.
This was the stable-source rerun started after PR 357 publication, not the earlier build
started before its adapter edits. Cabal still warns about missing `other-modules` entries.
It is **compile-only**, not a backend test-suite pass. This branch changes no Haskell source.

## Rollback, limitations and next dependency

Revert this bounded commit to undo it; doing so restores the known broken imports. There
is no data migration to reverse. Existing durable onboarding progress is never rewritten
from local signup markers. No grants, roles, sessions, financial state or evidence rules
are changed server-side.

The model abstracts effective effect/session lifetimes, including cleanup; it is not a
proof of transport identity, server authorization or cross-tab synchronization. In-flight
coalescing is local to one effect, not global across tabs or StrictMode effect remounts.
Hung requests have no new timeout; a later session/effect can retry. Lost winning server
responses can undercount analytics under the unchanged at-most-once receipt contract.
See the contract for the complete environmental assumptions.

The event task API still exposes only the scoped minimal read projection; complete task
commands, event workspace UX, logistics/templates, discovery/hiring, financial integration,
offline workflows and mobile remain unfinished. Mobile workspace is still uninitialized
at the unchanged pointer. No screenshots, production migrations, real charges/refunds/
payouts, provider activation, merge or deployment are performed.
