# TDF onboarding-first audit: event and moment privacy

Execution date: 2026-09-08 (America/Guayaquil)

Reviewed worktree: `/private/tmp/tdf-event-save-publish-20260907`

Root baseline: `a18a15e92568af136fbfba7416a1c8e738dfd401` (`feature/moment-reaction-evidence-20260907-reviewed`)

Mobile baseline: `bc02f52d0cc9bc31d66afcfd0e6f1c28d14466d8` (`feature/moment-reaction-evidence-20260907-reviewed`)

Implementation branches: `feature/event-moment-privacy-20260908-reviewed` in the root and mobile repositories

Root implementation commit: `39c4cabf8dd6399eb2ad7e9c957d7b5472c61580`

Mobile implementation commit: `e6cec2cdb25a7ae4339f8486042f6e121272233c`

Mobile contract-wording head: `3409aeb692ee754525567412ec053b6ae18c2458`

Publication status: both implementation commits, the regenerated contract clarification, and the report creation commit are published; stacked draft pull requests are open.

## Executive outcome

This continuation implements the two highest-priority privacy items deferred by the preceding onboarding evidence-integrity batch.

First, locally authored events now use the same fail-closed public-visibility contract as imported events. An ordinary authenticated Party can see an event only when it owns the event or the event has valid metadata that does not mark it private and an active lifecycle state with the persisted `public-listable` capability. Strict administrators retain internal visibility. This prevents private and planning-stage event details and their nested routes from becoming an authenticated-user discovery surface while preserving a creator's access to unfinished work.

Second, event-moment reaction responses no longer enumerate other reactors' Party identifiers or activity timestamps. One response row is retained for each stored reaction so existing clients can continue deriving counts; only the caller's own row carries Party identity and time. The mobile mapper already supported anonymous count placeholders, and its regression suite now locks that behavior.

These changes protect the first useful reaction action and repeat event work without adding a new gate, changing permissions, or breaking count behavior. No conversion, retention, accessibility, revenue, or performance uplift is claimed. No visual surface changed, so no before/after screenshot was manufactured.

## 1. Capability and safety matrix

| Capability | Status | Evidence from an actual check | Consequence |
| --- | --- | --- | --- |
| Repository read/write | Available | The isolated worktree accepted focused backend, contract, generated-client, mobile-test, and report changes. | Safe implementation and documentation were possible. |
| Branch, commit, and worktree inspection | Available | `git rev-parse`, `git status`, `git diff`, and `git submodule status` established the baselines above. Both repositories were clean before branching. | Task commits are separable from the user's changing primary checkout. |
| Unrelated uncommitted work | Isolated | Work remained in `/private/tmp/tdf-event-save-publish-20260907`; only explicitly listed files were staged. | No unrelated primary-checkout change is included. |
| Mobile submodule | Available | Required generation ran with `REQUIRE_MOBILE_WORKSPACE=1`; the mobile commit was published before the parent pointer. | A missing workspace could not silently pass and the parent does not reference an unavailable commit. |
| Node/npm | Available | Node `v24.8.0`, npm `11.6.0`; TypeScript, Jest, ESLint, Vite, and OpenAPI generation executed. | Web/mobile static, unit, contract, and build checks were available. |
| Backend runtime/toolchain | Available | Stack `3.7.1` with GHC `9.10.3` recompiled changed sources and linked the 184-module test target. | Focused and full backend verification executed against the current source. |
| Backend/database | Partial | Synthetic in-memory SQLite schemas exercised list/detail/nested-route visibility and reaction serialization. No migration was required. | The invariants are locally verified; PostgreSQL query planning/concurrency and staging integration remain unverified. |
| Browser and screenshot tooling | Available but unused for this batch | Playwright `1.59.1` is installed; earlier real artifacts remain under `artifacts/ux-audit-2026-09-05/`. | No new visual claim or screenshot is made because this batch changes server response/privacy behavior only. |
| Android tooling/device | Partial | `adb devices` executed and reported no attached devices. | No Android launch, TalkBack, orientation, keyboard, safe-area, or OS storage test is claimed. |
| iOS tooling/device | Partial | `xcrun simctl list devices available` found iOS 18.3 simulators, all shut down. | Simulator capability exists, but no iOS build, launch, VoiceOver, enlarged-text, or orientation session ran. |
| Test runners | Available | Hspec, Jest, TypeScript, ESLint, Vite, catalog audit, workflow/release tests, and the repository doctor executed. | Exact results and limitations are recorded in section 9. |
| Network and GitHub authentication | Partial but publication-capable | With stale `GH_TOKEN`-family variables removed, `gh auth status` confirmed the keyring account with SSH Git operations. Pushes and draft-PR creation succeeded. | Publication used the keyring path; no credential value is stored here. |
| Local/staging configuration | Partial | Example/local configurations and runner configs exist. Values were not printed; no staging runtime was exercised. | OAuth, recovery email, payment, media, and communications integrations remain unverified. |
| Synthetic accounts and fixtures | Partial | Synthetic owner, ordinary Party, strict administrator, public/private/planning events, moments, reaction types, and reactions exercised identity/state boundaries. | Deterministic authorization behavior is covered without live data. |
| Analytics access | Source/test only | Existing analytics and evidence contracts were inspected; no configured field dashboard or representative RUM data was used. | Funnel and performance baselines remain “not yet measured.” |
| Push/PR automation safety | Available for feature review | Root image publication is limited to `main`/manual/workflow-call; mobile EAS builds require manual dispatch and `start_eas_build=true`. Feature pushes opened validation/preview work only. | Draft PRs were safe to create. No merge, release, transaction, or production deployment was performed. |

`REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor` finished with 14 OK, 4 warnings, and 0 errors after implementation. The warnings accurately describe the dirty feature worktree, memory notes intentionally absent from the isolated worktree, and the stale environment token; keyring authentication was independently verified.

## 2. Method and prioritization

The continuation combined product/UX journey analysis, information architecture, API and authorization contract review, privacy/security threat modeling, mobile compatibility review, canonical OpenAPI generation, synthetic task-based tests, full regression tests, accessibility/performance scope checks, and release-automation review. These are coordinated disciplinary perspectives, not invented independent reviewers or research participants.

Priority followed user harm, task frequency, business relevance, confidence, reversibility, dependency risk, and effort. The event visibility issue was selected first because it exposed nominally private operational and community content. Reaction minimization followed because reactions are an implemented onboarding first value and clients did not need other users' stable identifiers.

The batch does not reactivate the paused onboarding experiment, redesign navigation, introduce a component library, change a role or permission assignment, alter event workflow semantics, change prices/policies, add a dependency, or deploy anything.

## 3. Onboarding and authorization analysis

Onboarding is still treated as arrival through a useful public context, authentication only when required, intent continuity, an authorized first action, and durable completion. Previous batches established server-verified first values for artist follow, event save, access request, and moment reaction; session-bound mobile completion; and Party-scoped local fallback data. Those protections remain green in the current full suites.

This batch strengthens that journey at two boundaries:

- A creator returning to a private or planning event can still find its own work, including nested moment routes.
- Another ordinary authenticated Party receives the same generic not-found response for nonexistent and hidden events, instead of gaining list/detail/nested-route visibility.
- A person reacting to a public moment still sees total reaction counts and its own selected state, without receiving unrelated Party activity.
- Product intent is not used as a permission. Ownership comes from the stored organizer Party and administrative bypass remains the existing strict-admin capability.

The most important remaining onboarding risks are not a new welcome-screen design: durable retry of a completion handshake after a successful domain action, mixed inline ES/EN recovery copy, missing web moment-reaction UI, and unperformed real OAuth/recovery/session-expiration/device walkthroughs. The experiment stays paused and historical conversion results remain unsuitable until their measurement contract is valid.

## 4. Coverage matrix

Source inventory is not treated as runtime coverage.

| Route/screen/system | Actual access | Device class | Important states | Inspection and verification | Status |
| --- | --- | --- | --- | --- | --- |
| `GET /social-events/events` | Authenticated Party; strict admin bypass | API/web/mobile | public, private metadata, planning state, owner, admin, malformed metadata | Handler/SQL review plus synthetic Hspec list task | Implemented and locally verified; staging PostgreSQL untested |
| `GET /social-events/events/{eventId}` | Authenticated Party; owner/public/admin rules | API/web/mobile | public, hidden, owner, admin, missing | Handler review and route-level Hspec helper | Implemented and locally verified |
| Event nested routes, including moments | Authenticated Party after parent-event visibility | API/mobile | hidden parent, owner parent, empty moments | Handler call-site inventory and Hspec moment-list task | Representative moment route verified; every nested route not individually invoked |
| Event update authorization | Owner/claim contract after visibility | API/internal | public non-owner, hidden non-owner, owner | Existing 403 regression corrected to use a canonical public fixture; full group passed | Verified locally |
| Moment list/create/react responses | Authenticated caller on visible event | API/mobile | own reaction, other reactions, unrelated viewer, count preservation | DTO/mapper inspection, Hspec and mobile Jest | Implemented and locally verified |
| Mobile event moment mapper | Authenticated/guest mapping layer | iOS/Android source | nullable Party ID, several anonymous rows, caller row | Focused and full Jest | Automated-verified; native runtime unverified |
| Canonical OpenAPI and generated clients | Web/mobile engineers | Cross-platform | nullable/read-only caller identity/time, list visibility description | Required generation and identical SHA-1 comparison | Verified locally |
| Prior onboarding gate and four first values | Eligible authenticated Party | Web/mobile/API | eligibility, action evidence, optional exit, account replacement | Previous implementation plus current full regressions | Retained and automated-verified; live auth/device untested |
| Public discovery, authentication/recovery, profiles, community, services, bookings, commerce, education, internal management | Roles/modules documented by preceding consolidated audits | Web/mobile | representative loading/empty/error/success/permission states | Current full automated suites plus prior coverage reports | No new runtime walkthrough in this batch; comprehensive runtime coverage is not claimed |

Explicitly uninspected or untested in this batch: production/staging state, PostgreSQL query plans and concurrency, anonymous public discovery behavior, real OAuth and password recovery, real email/payment/media/communications, browser screen reader and manual keyboard/reflow, Android/iOS runtime, VoiceOver/TalkBack, safe areas, virtual keyboards, orientation, enlarged text, physical touch targets, true offline/reconnect, two-device identity changes, representative analytics, field p75 Web Vitals, and production deployment.

## 5. Historical finding revalidation

Historical reports were preserved rather than rewritten: `reports/ux-ui-audit-2026-08-05.md`, `reports/onboarding-ux-audit-2026-08-20.md`, `UX_AUDIT_REPORT.html`, `reports/onboarding-first-ux-audit-2026-09-05.md`, `reports/onboarding-continuity-audit-2026-09-06.md`, `reports/onboarding-first-action-evidence-2026-09-07.md`, `reports/event-save-continuity-audit-2026-09-07.md`, and `reports/onboarding-evidence-integrity-audit-2026-09-08.md`.

| Historical topic | Classification on 2026-09-08 | Evidence |
| --- | --- | --- |
| Client-asserted onboarding completion | Resolved earlier and retained | All 2,476 backend examples and client suites pass; this batch does not loosen evidence predicates. |
| Local moment cross-account leakage | Resolved earlier and retained | Mobile full suite, including Party-scoped repository tests, remains green. |
| Private authored-event route visibility (preceding report item 2) | Resolved in this implementation | Ordinary list/detail/moment access now fails closed; owner/admin access remains. |
| Reaction response Party-ID enumeration (preceding report item 3) | Resolved in this implementation | Server returns caller-only identity/time and mobile retains anonymous counts. |
| Imported-event malformed/private visibility | Retained and superseded by one broader rule | The canonical metadata predicate now protects imported and local list rows; focused social-event group passes. |
| Paused onboarding experiment | Still paused/deferred | No flag or cohort behavior was reactivated. |
| Mixed hard-coded mobile language | Still present/deferred | No user-visible string was added in this nonvisual batch. |
| Web moment-reaction action | Still absent/deferred | No production web wrapper/UI was found or invented. |
| Representative field performance and funnel data | Not verifiable | No analytics/RUM access or valid field sample was available. |

## 6. Findings and disposition

### EVT-VIS-05 — private locally authored events bypassed the public visibility contract

- Journey and role: ordinary authenticated customer/fan discovering or directly opening a locally authored event; event creator returning to a draft.
- Reproduction: create local events with `isPublic=false` or a lifecycle state without `public-listable`; list/open them as a different ordinary Party in the baseline.
- Expected versus actual: expected only owner/authorized staff access; baseline visibility guard checked only whether an imported event was hidden, so local rows were listed and nested routes were accessible.
- Evidence: baseline `requireEventVisibleToUser`, `selectVisibleSocialEvents`, external-reference predicate, and synthetic route/list tasks.
- Severity/confidence/impact: high confidentiality and workflow-integrity risk, high confidence. Exposure is observed in code/tests; production incidence and affected record count are unknown.
- Likely cause: visibility evolved around imported discovery and treated absence of an external reference as implicitly visible.
- Remedy/effort/dependencies: one owner-aware rule for all events, requiring valid metadata that does not mark the event private plus the persisted lifecycle capability for non-owners. Medium; existing tables/capabilities, no migration.
- Acceptance criteria: public/on-sale event visible; private and planning event hidden from an ordinary non-owner; owner sees all own states; strict admin sees internal states; hidden direct/nested routes return generic 404; malformed/missing state fails closed.
- Status: implemented and locally verified.

### MOM-PRIV-03 — moment responses enumerated every reactor Party and timestamp

- Journey and role: any authenticated Party reading or reacting to a visible event moment.
- Reproduction: store reactions from Parties 2 and 3, then load the moment as Party 2 or unrelated Party 4 in the baseline.
- Expected versus actual: expected aggregate counts plus the caller's state; baseline returned stable Party identifiers and timestamps for every reactor.
- Evidence: baseline `EventMomentReactionDTO`, reaction mapper, mobile count/state mapper, and synthetic two-reactor responses.
- Severity/confidence/impact: high privacy/data-minimization risk, high confidence. Structural disclosure is verified; production reach and downstream use are unknown.
- Likely cause: the persistence row was projected directly into a transport DTO even though client behavior needed only array cardinality and self state.
- Remedy/effort/dependencies: retain one row per stored reaction but null identity/time for non-caller rows; document and regenerate the canonical contract. Small-medium; no migration or mobile production-source refactor.
- Acceptance criteria: each stored reaction contributes one count row; only caller row carries caller Party/time; unrelated viewer receives no Party/time; mobile counts remain correct and never reconstruct other IDs.
- Status: implemented and locally verified.

## 7. Implementation rationale, design system, copy, and accessibility

The existing architecture is retained. Authorization remains server-side and Party-derived. The implementation reuses canonical `isPublic` metadata, lifecycle definitions/states/capabilities, the strict-admin predicate, and current mobile anonymous reaction keys. No browser storage substitute or fake success path was added.

The canonical OpenAPI file was changed first and both generated TypeScript clients were regenerated with the documented root command. `emrPartyId` is now optional/nullable/read-only; `emrCreatedAt` is caller-only and nullable. The response remains cardinality-compatible to avoid a broad client rewrite while improving data minimization.

There is no visual design-system, layout, copy, localization, animation, drag-and-drop, or media-control change in this batch. Therefore no WCAG 2.2 AA or native accessibility conformance claim is made. Existing automated regressions protect behavior, but manual focus, screen-reader, zoom/reflow, reduced-motion, and touch checks remain required for a future visible implementation.

## 8. Performance and measurement

No before/after runtime performance measurement was performed. The list query adds a correlated capability existence check for non-admin views; correctness is verified, but PostgreSQL `EXPLAIN ANALYZE`, request waterfalls, p75 LCP/INP/CLS, and representative latency are not available. The target field thresholds remain LCP at or below 2.5 seconds, INP at or below 200 milliseconds, and CLS at or below 0.1.

No analytics event or SDK was added. Reaction completion semantics from the prior batch remain Party/session/evidence-bound. This response-minimization change sends less identifying data to clients and adds no password, token, email, caption, comment, or free text to analytics. Funnel impact is not yet measured.

## 9. Verification record

| Executed check | Result | Limitation |
| --- | --- | --- |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api` | Passed for web and the real mobile submodule | Contract generation, not transport runtime |
| Generated-client SHA-1 | Both `bd2ab1663403c1900d2cf735a9df82faffad4b67` | Byte equality only |
| Focused Hspec private-event test | 1 example / 0 failures | Synthetic SQLite |
| Focused Hspec reaction-privacy test | 1 example / 0 failures | DTO structure, not live HTTP JSON |
| First affected social-event group run | 20 examples / 1 failure | Correctly exposed an old organizer-auth fixture with no public lifecycle state; not counted as a pass |
| Final affected social-event group | 20 examples / 0 failures | Synthetic SQLite; changed sources recompiled |
| Full freshly linked Hspec executable | 2,476 examples / 0 failures in 4.2493 seconds | Local/synthetic dependencies; not staging/PostgreSQL concurrency |
| Mobile focused mapper suite | 1 suite / 23 tests passed | Mocked API mapping |
| Mobile full Jest | 67 suites / 369 tests passed in 18.595 seconds | Mocked React Native/local dependencies |
| Mobile TypeScript and ESLint | Both passed; ESLint allows zero warnings | Static checks; no native launch |
| Web TypeScript | Passed | Generated contract compile only |
| Web full Jest | 185 suites / 1,757 tests passed in 254.255 seconds | JSDOM/mocked dependencies; known React/MUI console warnings remain |
| Web ESLint | Exit 0 with 0 errors and 102 pre-existing warnings | Warnings remain outside this generated-only web diff |
| Web production build | Passed; 12,415 modules; 5 initial preloads / 412,191 gzip bytes | Existing greater-than-500-kB chunk warning; lab build only |
| `npm run audit:catalog-lists` | Passed; output written to `/tmp/tdf-catalog-list-audit.json` | Static governance audit; temporary artifact |
| `npm run test:ci-pipeline` | 16/16 passed | Automation contract, not hosted execution |
| `npm run test:production-release` | 49/49 passed | Safety invariants; no release/deploy |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run ai:doctor` | 14 OK, 4 warnings, 0 errors | Expected isolated-worktree and stale-environment-token warnings |
| Root and mobile `git diff --check` | Passed before commits | Whitespace only |
| Root draft-PR checks at implementation commit | In progress when this report was drafted | Final hosted state must be checked and reported separately |
| Mobile draft-PR checks | No checks reported because the stacked base is not `main` | Exact local mobile results above are the available evidence |

The initial Stack attempt inside the filesystem sandbox could not take the global Stack cache lock and did not run tests. It was rerun with explicit cache access. No skipped or empty collection is described as passed. No test was disabled, weakened, or removed. Mocked checks are not described as staging, native, external-service, or production validation.

## 10. Changed files and artifacts

Root/backend/contract:

- `tdf-hq/src/TDF/DTO/SocialEventsDTO.hs`
- `tdf-hq/src/TDF/Server/SocialEventsHandlers.hs`
- `tdf-hq/test/TDF/Social/FollowHandlerSpec.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq-ui/src/api/generated/types.ts`
- `tdf-mobile` submodule pointer

Mobile submodule:

- `src/api/generated/types.ts`
- `__tests__/socialApiMapperSanitization.test.ts`

Documentation:

- `reports/event-moment-privacy-audit-2026-09-08.md`

No new screenshot was captured because no visible UI changed. Earlier real screenshots remain under `artifacts/ux-audit-2026-09-05/` and are not presented as proof of this batch. The catalog audit created the temporary artifact `/tmp/tdf-catalog-list-audit.json`.

## 11. Deferred work and acceptance criteria

1. **Concurrent reaction exclusivity.** Impact: simultaneous different-type activation requests can still challenge the per-type uniqueness model. Dependency: compatible database invariant/migration and production-schema rehearsal. Acceptance: exactly one current reaction per Party/moment under concurrent writes without duplicate evidence or destructive retry. Status: deferred.
2. **Comment and author identity privacy contract.** Impact: moment/comment author Party identifiers may be necessary for attribution/moderation but were not justified or minimized in this batch. Dependency: product, moderation, abuse-reporting, and compatibility review. Acceptance: each exposed identifier has a documented user need and authorization rule; unnecessary stable IDs are replaced by safe public identity projections. Status: proposed.
3. **Durable completion-handshake retry.** Impact: a domain action can succeed while a later onboarding completion request fails. Dependency: Party-scoped durable outbox/retry design. Acceptance: relaunch-safe retry without duplicating actions/events and without crossing accounts. Status: deferred.
4. **Web moment reaction UI.** Impact: web users cannot use this first value. Dependency: public-moment product decision, accessible controls, ES/EN copy, and current privacy-minimized contract. Acceptance: keyboard, non-drag pointer, and touch operation with loading/error/offline/success and real auth continuation. Status: deferred.
5. **ES/EN recovery and saved-event/moment copy.** Impact: surrounding mobile journeys still contain mixed inline strings. Dependency: existing localization catalog. Acceptance: one coherent language across arrival/auth/action/recovery, Ecuadorian defaults, and valid international inputs. Status: next visible UX batch.
6. **PostgreSQL visibility performance and staging integration.** Impact: the new existence predicate is correct locally but its real query plan and deployed capability data are unknown. Dependency: staging database/read-only fixtures. Acceptance: representative `EXPLAIN ANALYZE`, list/detail/nested route tasks, owner/admin checks, and acceptable constrained-network response time. Status: blocked by missing staging exercise in this run.
7. **Native/browser accessibility runtime.** Impact: focus, screen-reader, reflow, touch, safe-area, keyboard, orientation, and enlarged-text behavior remain unverified. Dependency: running web fixtures and native device/simulator sessions. Acceptance: documented manual WCAG 2.2 AA/native checks plus automation, without scanner-only conformance claims. Status: deferred.
8. **Field performance and funnels.** Impact: p75 targets and abandonment/completion rates remain unknown. Dependency: consent-respecting analytics/RUM and a stable measurement contract. Acceptance: separate field/lab reporting for signup, auth failures, first action, completion, booking/purchase, LCP, INP, and CLS. Status: not yet measured.
9. **Broader runtime coverage.** Impact: public discovery, recovery, profiles, services, bookings, commerce, education, and internal operations have source and automated coverage but not equivalent current task walkthroughs. Dependency: stable local/staging fixtures and role-specific synthetic accounts. Acceptance: execute the existing coverage matrix across highest-risk states and record inaccessible areas. Status: deferred; this focused continuation is not mislabeled as a complete runtime audit.

The next highest-value coherent batch is user-visible ES/EN saved-event and moment recovery/status behavior, paired with a real browser/native accessibility walkthrough. PostgreSQL visibility-plan verification should precede rollout of this server batch.

## 12. Task-based usability script (not conducted)

- Stuart: create a synthetic account from an event link, react, interrupt the response, retry, refresh, and explain whether the action and onboarding completion are trustworthy.
- Customer/fan: open one public event and attempt direct links to a private and planning event; confirm no hidden title, moment, or identifier leaks, then react and explain the count/self state.
- Artist/organizer: create public, private, and planning fixtures; sign out/in; confirm every own event is recoverable while another ordinary Party cannot access private work.
- Staff: use a strict-admin fixture to inspect all three states, then repeat as an ordinary Party and distinguish 403 ownership from generic hidden-route 404 behavior.
- Accessibility/recovery: navigate visible reaction controls by keyboard, pointer without dragging, touch, and screen reader; enlarge text, rotate, interrupt the network, and verify status/error recovery.

No participant was contacted and no task result, quotation, rate, or approval is claimed.

## 13. Branch and draft-PR handoff

- Root branch: `feature/event-moment-privacy-20260908-reviewed`
- Mobile branch: `feature/event-moment-privacy-20260908-reviewed`
- Root implementation commit: `39c4cabf8dd6399eb2ad7e9c957d7b5472c61580`
- Mobile implementation commit: `e6cec2cdb25a7ae4339f8486042f6e121272233c`
- Mobile PR head after generated contract wording: `3409aeb692ee754525567412ec053b6ae18c2458`
- Root draft PR: [tdf-app #272](https://github.com/diegueins680/tdf-app/pull/272)
- Mobile draft PR: [TDF-mobile #48](https://github.com/diegueins680/TDF-mobile/pull/48)
- Stacked base in both repositories: `feature/moment-reaction-evidence-20260907-reviewed`
- Report creation commit: `0e7afc17598d8f18a8f1cbe12740d111f94fa1d6`
- No merge, production transaction, customer communication, or production deployment was performed. Repository integrations may create automatic feature previews; those are not production validation.
