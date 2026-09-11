# TDF onboarding-first continuation: reaction recovery and event-state localization

Execution date: 2026-09-08 (America/Guayaquil)

Root baseline: `57004ccacc02d083cd030ff3d797897a537daad4` (`feature/event-moment-privacy-20260908-reviewed`)

Mobile baseline: `3409aeb692ee754525567412ec053b6ae18c2458` (`feature/event-moment-privacy-20260908-reviewed`)

Implementation branch in both repositories: `feature/onboarding-recovery-localization-20260908-reviewed`

Published mobile commits: implementation `bbd42e8978cdcefaa430e95f7acf0eae42d059ec`; final accessibility head `3b7bf3ed28fc7d95ec9fac84bddc0fa5ba5ceafc`

## Outcome

This bounded continuation removes three observed recovery barriers around first useful event actions. A failed reaction in the optional new-user gate no longer disappears without explanation; a reaction that exists only on the device is no longer mistaken for an account-synchronized action; and the Event detail control that said “Retry saved events” is no longer disabled in the exact state where retry is required.

The same batch moves the shared moment card and saved-event state/action copy into typed Spanish and English catalogs selected from the existing persisted `UserSettingsProvider` locale. English moment cards now use the catalog's English reaction names even when the emergency catalog's generic `name` field is Spanish. The paused experiment remains paused, its eligibility and assignment are unchanged, and completion still requires a remotely acknowledged active reaction.

This is not a claim that every Event detail, Events, or Profile string is localized. Their saved-event controls, status, import, error, and recovery copy are covered; unrelated screen copy still follows older inline-Spanish patterns and remains a documented localization backlog.

## Capability and safety matrix

| Capability | Status | Evidence from this run | Consequence |
|---|---|---|---|
| Root repository read/write | Available | Clean baseline was branched from `57004c`; report and submodule pointer can be committed on the feature branch. | Focused root handoff is possible without touching the default branch. |
| Mobile repository read/write | Available | Clean baseline was branched from `3409ae`; final head `3b7bf3e` was created and pushed. | Mobile changes are independently reviewable. |
| Worktree isolation | Available | Work occurred in `/private/tmp/tdf-event-save-publish-20260907`, not the user's primary checkout. | Existing work in the primary checkout was not overwritten. |
| Mobile submodule | Available | Real submodule worktree, branch, tests, commit, and remote push succeeded. | The parent pointer can reference a published mobile object. |
| Runtime/package managers | Available | Node `v24.8.0`, npm `11.6.0`, local Expo/TypeScript/Jest/ESLint dependencies; release check and Expo Doctor executed. | Static, rendered-component, and package validation are available. |
| Backend/database | Partial; not exercised by this batch | The root baseline already contains the previously verified server evidence/privacy implementation. No backend/API/database file changed here and no backend suite was rerun for this UI-only delta. | No new backend integration claim; prior baseline evidence is not relabeled as a current run. |
| Browser tooling | Available but not applicable to the changed production surface | Playwright `1.59.1` is installed; no web reaction UI exists and no web source changed in this batch. | No browser screenshot or web runtime claim is made. |
| Android tooling/device | Partial | `adb devices` succeeded outside the sandbox and returned no attached device. | Android device, touch, TalkBack, keyboard, orientation, and enlarged-text testing were not possible. |
| iOS tooling/device | Partial | iOS 18.3 simulators were enumerated and an iPhone 16 Pro was booted. The first build stopped at CocoaPods because the shell was ASCII-8BIT; a UTF-8 retry installed CocoaPods and compiled native dependencies, but did not finish within the bounded window and was terminated. The simulator was shut down. | Native compile/install and VoiceOver validation remain inconclusive, not passed. Generated Pod/project changes from the attempt were removed before commit. |
| Screenshots | Unavailable for the affected authenticated/paused-experiment states | No controlled signed-in synthetic account was available and the optional experiment remains paused. | No fabricated before/after image is provided. Rendered Jest trees are test evidence, not screenshots. |
| Test runners | Available | Focused Jest, full Jest, TypeScript, zero-warning ESLint, release validation, asset checks, Expo config, Expo Doctor, and diff checks ran. | Automated regression evidence is available; mocks remain distinct from native/backend integration. |
| Network access | Partial | Mobile branch push and CocoaPods retrieval succeeded. | GitHub handoff is possible; this does not imply staging or production availability. |
| Local/staging configuration | Partial | Public Expo config resolves development API/upload targets to `http://localhost:8080`; release validation separately confirmed the production-profile endpoints without calling them. No staging credentials or synthetic account were present. | No production write, transaction, message, deployment, or authenticated staging walkthrough occurred. |
| Synthetic fixtures/accounts | Partial | Jest fixtures cover authenticated Party `42`, remote acknowledgement, local fallback, failure, retry, saved-state error, and ES/EN locale. No real OAuth, recovery email, or two-device account was used. | Deterministic UI/state behavior is covered; external identity and delivery are unverified. |
| Analytics access | Unavailable | Tests explicitly logged that PostHog was disabled because `EXPO_PUBLIC_POSTHOG_KEY` was unset. | No funnel baseline, field completion rate, or uplift is claimed. |
| GitHub authentication | Available after recheck | Root doctor transiently warned that auth was invalid; `gh auth status` then succeeded from the keyring, and the mobile branch push completed. | Push/PR handoff may proceed. The transient doctor warning is preserved in verification evidence. |
| Deployment safety | Available | Mobile workflows were inspected: validation and Datadog run only for `main`/PRs targeting `main`; EAS builds require manual dispatch and `start_eas_build=true`. | Feature-branch push does not deploy or submit a build. No workflow was manually dispatched. |

## Scope and methodology

The audit combined source and contract inspection, a cognitive walkthrough of a new authenticated user's first reaction, failure injection in rendered React Native tests, saved-event state inspection across Events/Event detail/Profile, localization review against the active settings provider, accessibility-state inspection, and full regression execution.

This is expert assessment using synthetic fixtures, not user research. No participant, interview, quotation, analytics result, conversion improvement, or production observation is invented.

The coherent implementation boundary is first-action recovery plus the copy directly needed to understand saved-event and moment status. It does not reactivate the experiment, redesign navigation, change authorization, alter event eligibility, modify prices/policies, add an analytics SDK, change the API, or deploy.

## Coverage matrix

| Route/screen | Actual role/capability context | Device classes | Important states inspected | Method | Status |
|---|---|---|---|---|---|
| `NewUserOnboardingGate` | Authenticated Party; only a server-active own reaction can complete first value | iOS/Android source and RN renderer | ineligible/control, loading, offline, feed error/retry, empty, moment success, remote success, local-only, failed reaction, retry, exit | Source, mocked repository, rendered Jest | Implemented and automated-verified; native runtime unverified; experiment remains paused |
| `EventMomentCard` | Viewer may react; connect/comment callbacks are caller-controlled | iOS/Android source and RN renderer | ES/EN labels, emergency reaction names, selected/disabled state, image/video accessible names, zero/plural summary, failed mutation | Source and rendered Jest | Implemented and automated-verified |
| `/eventDetail` saved control | Anonymous visitor or authenticated Party | iOS/Android source and RN renderer | sign-in continuation, unknown/loading, server/cache, save/remove, mutation error, saved-query error/retry | Source, focused Jest, full Jest | Retry dead end fixed and automated-verified; real API/device unverified |
| `/eventDetail` moment feed | Authenticated Party for remote mutation | iOS/Android source and RN renderer | localized shared card, reaction failure alert, publish/local/comment states inspected | Source, focused/full Jest | Shared card and reaction error localized; broader composer/feed copy deferred |
| `/(tabs)/events` saved controls | Anonymous visitor or authenticated Party | iOS/Android source | sign-in, cache, saved-query failure/retry, pending-import corruption, import prompt/in-progress/partial/success, save/remove | Source, TypeScript, lint, full Jest collection | Implemented; no dedicated screen renderer/native walkthrough in this batch |
| `/userProfile` saved tab | Anonymous visitor or authenticated Party | iOS/Android source | signed out, loading, cache, read error/retry, empty, partial detail failure/retry, list, remove failure | Source, TypeScript, lint, full Jest collection | Implemented; direct rendered/native test deferred |
| Web event discovery/detail | Public visitor/authenticated Party | Responsive web | No changed surface in this batch | Route/source inventory from baseline | Unchanged; not re-run |
| Server favorite/reaction evidence | Authenticated Party and server authorization | API | No contract/data change in this batch | Baseline report/code comparison | Unchanged; current backend execution not performed |

## Historical finding revalidation

| Historical finding | Current classification | Evidence |
|---|---|---|
| 2026-08-20: onboarding gate can dead-end on unavailable content/errors | Partially resolved | Distinct loading/offline/error/empty states remain; feed error now has in-place retry and reaction failures have explicit recovery. Dependence on eligible moment content remains, and the experiment stays paused. |
| 2026-09-07: saved Event detail displays retry but error state is not reliably actionable | Resolved in implementation | Source showed `isError` in the button's `disabled` expression despite an error-refetch handler. The expression now keeps only the error retry state enabled; focused test exercises the handler. |
| 2026-09-07: English setting produces Spanish saved-event states | Partially resolved/superseded | Saved action, loading, cache, error, import, empty, partial-detail, success, and accessibility strings now use typed ES/EN copy. Unrelated Events/Event detail/Profile shell copy remains inline Spanish. |
| 2026-09-08: failed/local-only onboarding reaction has no truthful visible recovery | Resolved in implementation | Gate now distinguishes remote success, local-only state, and failure; only remote active state can call completion; both non-success states announce and retain an explicit retry. |
| 2026-09-08: moment card mixes English and Spanish | Resolved for the shared card | English rendered test covers heading badge, Connect, English catalog reaction label, media name, summary, comment field, and Send. |
| 2026-09-08: experiment measurement contract invalid | Still paused/not reactivated | No flag, cohort, assignment, window, exposure, or completion-definition change was made. Historical experiment results remain unsuitable for decision-making. |

## Findings and implementation status

### ONB-REC-06 — failed first reaction was silent

- Journey/role: newly authenticated Party in the optional single-feature onboarding gate.
- Reproduction: make `toggleMomentFeedReaction` reject, then press a reaction.
- Expected: explain that no account change occurred and let the person retry without leaving the task.
- Baseline actual: `EventMomentCard` caught the rejection because the parent was expected to own error display, while the gate owned no error state. Nothing visible changed.
- Evidence: component/gate source and a failure-injected renderer test.
- Severity/confidence: high/high. The promoted first action could fail with no recovery or truthful status.
- Cause: error ownership contract existed in comments but not in the gate implementation.
- Remedy/effort/dependencies: gate-owned recovery state and idempotent retry using the existing desired-state reaction API; small-medium, no API change.
- Acceptance: failure announces that nothing changed, completion is not requested, retry remains available, successful retry can complete once.
- Status: implemented and automated-verified.

### ONB-REC-07 — local-only reaction looked like a completed account action

- Journey/role: authenticated new Party during connectivity fallback.
- Reproduction: return `source: local` from the reaction repository.
- Expected: disclose device-only state, retain retry, and withhold onboarding completion.
- Baseline actual: completion was correctly withheld, but the gate gave no reason or recovery instruction.
- Evidence: repository result contract, gate source, rendered local-fallback test.
- Severity/confidence: high/high. The user can believe onboarding succeeded while durable server evidence is absent.
- Cause: boolean conversion guard existed without a user-facing state model.
- Remedy/effort/dependencies: explicit `local-only` recovery state and repeatable desired-state retry; small.
- Acceptance: local-only copy is announced, retry is visible, and completion analytics remain absent.
- Status: implemented and automated-verified.

### SAVE-REC-05 — Event detail's visible retry was disabled

- Journey/role: authenticated customer/fan saving a public event.
- Reproduction: make `saved-event-ids` enter `isError`; observe “Retry saved events,” then attempt to press it.
- Expected: the retry label invokes `refetch`.
- Baseline actual: `isError` also set `disabled=true`, so the handler's recovery branch was unreachable.
- Evidence: source expression and focused rendered regression test.
- Severity/confidence: high/high. A revenue-adjacent return/save action dead-ended after a transient read failure.
- Cause: display state and disabled-state conditions contradicted the handler.
- Remedy/effort/dependencies: keep unknown/loading/mutation states disabled but enable the explicit error retry; small.
- Acceptance: accessible name describes retry, disabled state is false, press calls refetch, no save mutation runs from unknown state.
- Status: implemented and automated-verified.

### ONB-REC-08 — feed error asked users to recover elsewhere

- Journey/role: eligible new Party loading the focused moment feed.
- Reproduction: fail the event or moment query.
- Expected: recover in context or exit intentionally.
- Baseline actual: copy said retry was possible “from there,” but only the exit CTA existed.
- Evidence: gate source and rendered feed-error retry test.
- Severity/confidence: medium/high.
- Cause: error state reused the persistent footer without a query retry action.
- Remedy/effort/dependencies: localized in-place retry that refetches the event list and, when safe, the selected moment feed; small.
- Acceptance: retry button is named, available, and invokes only valid queries.
- Status: implemented and automated-verified.

### L10N-05 — shared moment card ignored the selected language

- Journey/role: Spanish- or English-preferring event participant.
- Reproduction: render the card with English locale and emergency reaction catalog.
- Expected: one coherent language and localized reaction name.
- Baseline actual: “Top moment” appeared beside Spanish publishing/connect/media/comment/summary copy, and the emergency `name` field defaulted to Spanish.
- Evidence: source inventory and ES/EN rendered assertions.
- Severity/confidence: medium/high.
- Cause: reusable component had inline bilingual fragments and did not receive locale.
- Remedy/effort/dependencies: typed shared copy plus explicit `nameEs`/`nameEn` selection; small-medium.
- Acceptance: visible strings and accessible names change coherently with locale without a second translation runtime.
- Status: implemented and automated-verified.

### L10N-06 — saved-event recovery/status copy bypassed the active locale

- Journey/role: English-preferring visitor/customer across Events, Event detail, and Profile saved tab.
- Reproduction: select English, then enter loading/error/cache/import/empty/remove states.
- Expected: saved-event status and recovery controls remain English, with correct plurals and accessible names.
- Baseline actual: these states were hard-coded Spanish even though the persisted locale was available.
- Evidence: source inventory, EventCard English renderer, compilation/lint across all consumers.
- Severity/confidence: medium/high.
- Cause: saved-event continuity was implemented before its copy was moved into the typed locale catalog.
- Remedy/effort/dependencies: shared `savedEventCopy` selected through the existing locale; medium.
- Acceptance: targeted saved-event states have ES/EN equivalents; raw corruption/server text is not used as localized UI; import partial counts remain truthful.
- Status: implemented for saved-event state/action copy; full surrounding screen localization deferred.

## Design-system, copy, and interaction rationale

The change reuses existing buttons, notice containers, theme colors on saved-event screens, user settings, and the established typed catalog approach. It does not create a parallel component library or translation runtime.

Reaction recovery uses a compact warning card because it is tied to the promoted action and must remain visible while the feed remains usable. The primary event-exploration exit remains available, so optional onboarding is not a gate. Retry repeats an idempotent desired state rather than toggling blindly.

Saved-event errors now use stable localized guidance instead of displaying arbitrary lower-layer error messages. Partial import copy reports imported and total counts and explicitly says the device copy was retained. Pending-import corruption is disclosed without deleting or exposing the raw storage parser text.

## Accessibility verification

Implemented and inspected:

- reaction failure/local-only text uses `accessibilityRole="alert"` and assertive live-region semantics;
- feed and reaction retry controls have visible text, accessible names, disabled/busy state, and at least 44-point minimum height;
- the saved Event detail retry exposes a retry-specific accessible name and is operable in its error state;
- moment Connect/Send controls now expose button roles, names, and disabled state;
- comment input and localized media actions expose accessible names;
- reaction controls retain selected/disabled state and practical 44-point sizing;
- no icon replaces an understandable label.

Not verified: VoiceOver, TalkBack, switch control, physical touch dimensions, focus order on a real device, virtual-keyboard overlap, orientation, safe areas, reduced motion, or enlarged dynamic type. This report does not claim WCAG 2.2 AA or native accessibility conformance.

## Performance and analytics

No new network request is added during the normal success path. Retry requests occur only after an explicit error/local-only action. The existing saved-event detail N+1 hydration remains deferred.

No before/after startup, interaction, request-waterfall, or field measurement was performed. Web LCP/INP/CLS targets are unchanged and not applicable evidence for this mobile-only delta. No performance improvement is claimed.

No analytics event or SDK changed. `experiment_converted`, `first_value_completed`, and `onboarding_completed` remain downstream of server-confirmed active reaction plus authoritative completion. Failure and local-only tests confirm no completion request. PostHog/dashboard access was unavailable, so the funnel is “not yet measured.” No password, token, email, caption, comment, or free text was added to analytics.

## Verification results

| Check | Actual result | Interpretation |
|---|---|---|
| `git diff --check` | Passed before commit | No whitespace errors in intended diff. |
| Focused Jest after initial implementation | 6 suites, 42 tests passed | Reaction success/failure/local behavior, gate states, EventCard localization, Event detail moments/persistence, and saved-event storage. |
| Focused Jest after retry-dead-end correction | 4 suites, 28 tests passed | Includes enabled Event detail saved-query retry and final gate/card behavior. |
| Final full mobile Jest | 67 suites, 375 tests passed | Entire configured test collection ran; not an empty collection. PostHog was deliberately disabled in the test environment. |
| `npm run typecheck` | Passed | TypeScript emitted no errors. |
| `npm run lint` | Passed | ESLint completed with zero warnings allowed. |
| `npm run release:check` | Passed | Five release assets, lint, typecheck, production-profile release validation, and public Expo config completed. This did not build, submit, or deploy. |
| `npm run doctor` | 17/17 checks passed | Expo Doctor reported no issues; its configured app-config sync check remains disabled by repository policy. |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor` | 14 OK, 4 warnings, 0 errors | Real mobile workspace detected. Warnings: missing current/prior daily notes, intentionally dirty worktree, and transient GitHub auth failure. Auth subsequently rechecked successfully. |
| Android discovery | `adb devices` returned no devices | Tool works; Android runtime unverified. |
| iOS native attempt | Inconclusive | Simulator available. First attempt failed before build on non-UTF-8 CocoaPods environment. UTF-8 retry installed Pods and compiled many dependencies but was terminated after the bounded window before a result. No pass claimed. |

No backend suite, browser E2E, screenshot comparison, staging API, real OAuth, recovery email delivery, payment, notification, CI, or production validation ran for this batch.

## Changed files

Mobile commits `bbd42e8978cdcefaa430e95f7acf0eae42d059ec` and `3b7bf3ed28fc7d95ec9fac84bddc0fa5ba5ceafc`:

- `src/localization/eventExperienceCopy.ts`
- `src/localization/onboardingCopy.ts`
- `src/components/EventMomentCard.tsx`
- `src/components/EventCard.tsx`
- `src/experiments/NewUserOnboardingGate.tsx`
- `app/(tabs)/events.tsx`
- `app/eventDetail.tsx`
- `app/userProfile.tsx`
- `__tests__/NewUserOnboardingGate.test.tsx`
- `__tests__/EventMomentCardReaction.test.tsx`
- `__tests__/EventCardTickets.test.tsx`
- `__tests__/EventDetailMoments.test.tsx`
- `__tests__/EventDetailLiveBroadcastLifecycle.test.tsx`

Root handoff changes:

- `tdf-mobile` published submodule pointer
- `reports/onboarding-recovery-localization-audit-2026-09-08.md`
- `reports/onboarding-recovery-localization-pr-description-2026-09-08.md`

Screenshot artifacts: none for this batch, for the capability reasons above.

## Deferred work and acceptance criteria

1. **Durable completion-handshake retry after a successful domain action.** Impact: the reaction can be on the server while a later completion request fails; the current recovery state covers the reaction write, not a failed completion handshake. Dependency: persisted session-bound pending completion or server-side reconciliation. Acceptance: reconnect/relaunch completes from server evidence once without repeating analytics or crossing accounts. Priority: highest next onboarding batch.
2. **Complete Event/Events/Profile ES/EN localization.** Impact: saved-event and shared-moment states are coherent, but surrounding navigation, event metadata, RSVP, composer, live, regional settings, and ticket labels can still mix languages. Dependency: extend the same catalog; no new runtime. Acceptance: task walkthroughs in both locales contain no unintended language switch, including plurals, dates, alerts, and accessibility names. Priority: high.
3. **Direct Events/Profile saved-state renderer coverage.** Impact: those consumers compile and pass the full collection but do not have dedicated state-matrix renderer tests. Dependency: stable query/provider mocks. Acceptance: signed-out/loading/cache/error/import/partial/remove cases pass in ES and EN, with retry assertions. Priority: high QA.
4. **Native controlled-account walkthrough.** Impact: touch, assistive technology, keyboard, orientation, safe-area, relaunch, and account continuity remain unproven. Dependency: installable simulator/device build, local or staging backend, synthetic customer account, paused-experiment-safe fixture. Acceptance: record actual device/OS/build/account fixture and results without production writes. Priority: high validation.
5. **Saved-event detail batching.** Impact: one request per saved event increases latency and partial-failure probability. Dependency: bounded public-summary API with explicit unavailable IDs and cache semantics. Acceptance: one bounded request hydrates the visible page and measured request count falls under comparable fixtures. Priority: medium-high performance.
6. **Web moment-reaction journey.** Impact: web users still cannot use this first-value action. Dependency: privacy-minimized contract already exists, public product placement, accessible ES/EN UI, auth continuation, and state coverage. Acceptance: real server acknowledgement, no external redirects, keyboard/pointer/touch support, and loading/offline/error/empty/success tests. Priority: medium pending product placement.
7. **Broader audit runtime coverage.** Public discovery, booking/purchase, education, community, and internal operations remain represented in earlier consolidated matrices; this batch did not rerun every route. Acceptance: controlled task walkthroughs across actual roles, desktop/mobile classes, and transactional failure states without production mutation.

## Brief usability-test script (not executed)

- Stuart: choose English, enter the optional focused moment experience with a synthetic new account, explain what will count as completion, simulate a failed reaction, retry, then exit to Events without feeling trapped.
- Staff: inspect the same synthetic Party's server reaction/completion evidence and verify that local-only/failure states did not record completion or duplicate analytics.
- Artist: open a public event, review moment labels in Spanish and English, react, disconnect before acknowledgement, and explain the recovery state.
- Customer/fan: arrive from an event link, attempt Save while signed out, authenticate and return, interrupt saved-list loading, use the now-enabled retry, then find/remove the event from Profile on a second controlled device.

For every session, record build/commit, locale, device/OS, account fixture, starting URL, observed errors, whether the task completed, and assistive settings. Do not contact participants or claim a session occurred without separate authorization.

## Handoff and rollback

The mobile feature branch is published. The parent pointer must reference that published commit before its own push. Draft PRs should be stacked on the preceding event-moment privacy branches so this bounded delta remains reviewable. Feature pushes do not trigger production deployment; no release-readiness workflow was dispatched.

Rollback is code-only: revert the mobile implementation commit and the parent pointer/report commit. There is no migration, data rewrite, flag change, new dependency, price/policy change, or irreversible state transition.
