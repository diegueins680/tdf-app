# PR 16 — canonical artist-follow continuity

Base: draft [PR 359](https://github.com/diegueins680/tdf-app/pull/359),
`fix/web-onboarding-reconciliation-integration`, commit
`827d745629f8cdbaee4429ded3689d3bf520a09d`.
Branch: `fix/artist-follow-onboarding-continuity`. Draft only.

## Scope, dependency and order

The [AF-01–05 contract](artist-follow-continuity-contract.md) documents evidence of the
lost artist-return flow, preserved tests/translations and canonical Fans/onboarding APIs.
After correcting the component test's incomplete mock boundary, the baseline still failed
four behavior tests and both URL helper exports were absent. This is a compatibility
repair in the shared discovery/profile domain, not event hiring or a second event system.
The new finite model and the complete pinned TLC/Alloy runner passed **before feature code**.

- Restore validated local signup return and explicit post-login follow consent.
- Fail closed until session and follow state are known; allow lookup retry without a write.
- Freeze artist/action/session/route at dispatch, coalesce clicks and suppress stale receipts.
- Preserve unrelated query/hash on cleanup; retain intent and show localized recovery on failure.
- Use canonical server-evidence completion, with a current-context predicate before and after
  its asynchronous receipt. Handle the existing FanHub call's floating Promise without
  claiming to repair FanHub's separate onboarding state machine.
- Add pure adversarial tests, rendered interaction tests and isolated browser fixtures.

No backend, schema, migration, generated API, mobile pointer, provider, credential or feature
flag changes. Rollback reverts this bounded change; it does not rewrite persisted progress
or delete follows. Server authorization remains authoritative. Existing club auto-follow
behavior needs separate consent review, as described in the contract.

## Formal verification

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
```

Exit 0. `ArtistFollowConsent`: **791 generated / 341 distinct states, depth 7**;
three expected invariant-violation controls detected (missing click, unknown follow state,
stale context). Complete runner: **26 expected negative controls**, two SAT Alloy scenarios,
11 UNSAT assertions. Existing models/assertions retain their documented bounds. The new
model has two Parties plus logout, two targets, three context generations and one command;
it makes no network-liveness, real-server authorization or universal-proof claim.

## Executed verification and diagnostic history

```sh
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/ArtistPublicPage.component.test.tsx src/pages/ArtistPublicPage.intent.test.ts src/analytics/onboardingProgress.test.ts
npm run test:e2e:web -- --config=playwright.artist-follow.config.mjs
```

The first serial focused run passed **42 tests / three suites**, after correcting a test
syntax typo. Earlier concurrent runs had one already-followed readiness timeout while
lint/typecheck saturated this checkout; both heavy runs were explicitly interrupted
(exit 130), not counted as successful. The test now explicitly waits for known follow state;
no assertion, production guard, test timeout or lint rule was removed or relaxed.

The first browser run had two mobile passes and two desktop startup timeouts. Navigation
now waits for DOMContentLoaded, while readiness/consent assertions and their limits remain.
The expanded run had **7 passes / 1 desktop readiness timeout**; the failure snapshot shows
the correct confirmation button after the wait expired. These failures remain part of the
record even if a later rerun passes. Browser tests use synthetic local API interception,
block nonlocal requests and do not exercise real signup, server persistence or providers.

Desktop guest and mobile confirmed screenshots from the expanded run were generated and
visually inspected. The artist controls are visible; the existing global radio bar overlays
part of the mobile footer. This is not whole-app accessibility conformance. Axe checks only
the rendered synthetic confirmed state for serious/critical violations, not every route or
all WCAG criteria. Browser artifacts are local, ignored and not uploaded with this PR.

## Final verification

```sh
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/ArtistPublicPage.component.test.tsx src/pages/ArtistPublicPage.intent.test.ts src/analytics/onboardingProgress.test.ts src/session/SessionContext.test.ts src/session/SessionProvider.personalData.test.tsx src/pages/LoginPage.test.tsx src/session/SessionContext.reconciliation.test.tsx src/routes/AppShell.test.tsx src/session/onboardingIntentRecovery.test.ts src/api/session.test.ts src/api/eventOperations.test.ts
```

**118 tests / 11 suites passed**, exit 0, 22.246 s, including the new delayed profile-switch
and leave/return cases. The final generation guard prevents context ABA revival while
allowing query-only resume cleanup. Node 24.8.0 / npm 11.6.0 used locally.

The first complete serial whole-UI lint and typecheck runs exited 0. They began before
the final profile-generation regression edit; final-source checks are recorded separately.
All 100 relative Markdown links in the event-operation documentation/formal directories,
shell syntax, browser test/config JavaScript syntax and whitespace checks passed.

The final isolated browser rerun passed **8/8**, exit 0, 45.0 s, with no retries or
timeout changes. Both desktop and phone covered local guest return, keyboard consent,
failure/retry, already-followed cleanup and English guest copy. Confirmed-state axe results
contained no serious/critical violations in either viewport. The desktop confirmed
screenshot was also inspected. The earlier failures above are not erased by this pass;
startup timing still deserves CI observation. Tablet, Firefox and WebKit are not verified
by this local two-project run; the default CI configuration discovers the test normally.

Final-source verification command (sequential, not concurrent):

```sh
npm run typecheck:ui && npm run lint --workspace=tdf-hq-ui && npm run quality:repo
```

The complete final-source command exited **0**: whole-UI typecheck, whole-UI lint and
repository quality passed. Repository quality ran 143 tests (8 internship audit, 42 loop,
4 heuristic audit, 61 release safety, 23 CI scope, 2 visual metadata, 3 persona program).
The heuristic scan reported 9,586 findings, zero critical/errors and 355 warnings; it is
not TLC/Alloy verification. Generated internship fixtures remained unchanged. Tests that
exercise Git pushes, release commands or CI use disposable local fixtures; those output
lines are not actual application branch merges or deployments.

Full build, full UI Jest, backend tests and mobile tests are **not rerun by this command**;
prior checkpoint results are not relabeled as current-branch passes.

## Outstanding product and environment gates

The unchanged FanHub onboarding baseline has **five failing tests**, including eligibility,
loading accessibility and recovery behavior. Full UI Jest was not green at the preceding
checkpoint (course suite failed and the full run was interrupted). The preceding build
also exceeded the unchanged bundle budget; these are separate gates, not covered by a
focused passing artist suite. No full-repository CI or production readiness is claimed.

Mobile remains uninitialized and unchanged. Session changes after a request reaches the
server cannot undo it; client fences suppress stale UI/analytics, not remote side effects.
Cross-tab identity, automatic club relationships and full backend follow concurrency need
their own checks. Complete task commands/workspaces, logistics/templates, event-linked
hiring/contracts/payments, collaboration/offline and mobile work remain unfinished.

No merge, deployment, production migration, real charge/refund/payout or provider activation.
