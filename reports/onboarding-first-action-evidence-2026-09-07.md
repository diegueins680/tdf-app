# TDF onboarding first-action evidence and saved-event safety continuation

Execution date: 2026-09-07 (America/Guayaquil)

Working branch: `feature/onboarding-continuity-20260906`

Root continuation baseline: local reconciliation commit `e0900a3b9357366dbd0f0f43e5861e3b1a8ac3d4`; published branch baseline `d3c94cb25a34dc04e750ed12a4be52af039c327b`

Mobile continuation baseline: `9c399116005535ac8188ab2e6fc7f2b70eeabbe8`

Primary consolidated audit: `reports/onboarding-continuity-audit-2026-09-06.md`

Method: coordinated product, UX, accessibility, localization, frontend, mobile, backend, API-contract, analytics, security, and QA review using checked-out source, synthetic fixtures, generated contracts, and executable local checks. This continuation contains expert assessment and automated test evidence only. It does not claim user research, field analytics, production validation, a native-device run, or a new browser session.

## Outcome

Two onboarding and continuity problems were confirmed and addressed in this batch:

1. A client could label onboarding complete with `access_requested` even when the authenticated Party had not submitted an access request. Completion now requires a Party-bound request created between authoritative signup and the completion request. Missing, cross-account, pre-signup, and future evidence fail closed; a valid request completes exactly once.
2. Mobile saved events used one global AsyncStorage key and swallowed persistence errors. A second account on the same device could therefore see the first account's saved-event interests, and the UI could report success and emit analytics when nothing was stored. Saved-event state is now namespaced to the authenticated Party, account identity is part of React Query keys and mutations, write failures surface as errors, malformed data is preserved for recovery, and the unowned legacy key is quarantined rather than silently assigned or deleted.

The mobile containment is intentionally honest about scope: saved events remain local to one device until the existing account-scoped backend favorite capability is hardened and integrated. The UI no longer calls that state a profile-level save.

## Capability and safety delta

| Capability | Status | Actual evidence | Consequence |
| --- | --- | --- | --- |
| Isolated repository read/write | Available | Changes were made only in `/private/tmp/tdf-onboarding-continuity-20260906`; the original dirty checkout was not used for implementation. | Unrelated user work remains isolated. |
| Branch and baseline inspection | Available | `git rev-parse` and graph inspection recorded the local/published baselines above. The workspace automation had created `e0900a3b9`; its two parents and diff were inspected before work continued. | No automated merge commit was trusted without review. |
| Mobile submodule | Available | Required API generation executed with `REQUIRE_MOBILE_WORKSPACE=1`; web and mobile generated clients compared byte-for-byte equal. Root commit `248403e67` points to the published mobile head. | The root/mobile contract and submodule pointer remain aligned. |
| Node/npm/OpenAPI/TypeScript | Available | Repository scripts executed with installed `openapi-typescript 7.10.1`; web typecheck completed successfully. | Contract and web compile validation are available locally. |
| Backend/Stack | Available with concurrent-workspace interference | The current 184-module test target compiled and linked. Another pre-existing process then reacquired Stack's directory lock before the wrapper launched tests, so the blocked wrapper was stopped and the freshly linked Hspec executable was run directly. The onboarding match passed 3/3 and the access-request evidence match passed 1/1. | Compilation and focused behavior are verified locally; the interrupted Stack wrapper is not reported as a passing command. Exact-head hosted CI remains required after the root push. |
| Database | Partial | No schema change was required in this continuation; the prior disposable PostgreSQL migration evidence remains current for the onboarding table. The new evidence test uses a synthetic in-memory SQLite access-request table. | No staging or production database was touched. |
| Browser/device/screenshots | Available but unused | Existing browser artifacts remain under `artifacts/ux-audit-2026-09-05/`; no browser, simulator, physical device, or new screenshot was launched. | UI source/tests are not described as runtime visual proof. |
| Test runners | Available | Node release/pipeline checks, TypeScript, catalog audit, Jest, and Stack are installed. Exact executed results are listed below. | Skipped and blocked execution remains explicit. |
| Network/GitHub | Available | The mobile continuation is published at `1e07ae92d46677b42b14f33c59766b3dea84e830`; Mobile Validate and Datadog passed. Root draft PR inspection succeeded after one transient API retry. | Root publication and its fresh exact-head CI must still be confirmed at handoff. |
| Local/staging/synthetic accounts | Partial | Local dependencies and synthetic unit fixtures are available; no controlled staging identity, OAuth provider, email sink, or payment fixture was exercised here. | Authentication/provider/transaction delivery remains unverified. |
| Analytics | Source access only | Event semantics and call sites were inspected; no PostHog dashboard or representative field data was accessed. | No conversion uplift or field completion rate is claimed. |
| Push/deploy safety | Available for feature-branch review | Existing root/mobile workflows were inspected; the parent onboarding PRs and the YouTube-catalog base are already on `main`, and their additive manifest conflict was resolved by retaining both entries. No production deploy, transaction, or customer communication occurred. | Push only the feature branch and leave production deployment outside this task. |

No credential value was printed or stored in the report.

## Coverage matrix

| Route/screen/system | Roles | Device | States inspected | Method | Status |
| --- | --- | --- | --- | --- | --- |
| `POST /session/onboarding/complete` | Authenticated new Party | All clients | Missing signup, missing evidence, other Party, pre-signup, future evidence, in-window evidence, repeated completion | Haskell source and synthetic Hspec case | Implemented; backend compiled and focused local execution passed |
| `GET/POST /access-requests` | Authenticated requester | Web/mobile/API | Own list, submission, unauthenticated, duplicate/already-authorized | Servant/API/DTO source, canonical OpenAPI, generated clients | Contract verified; live integration not rerun |
| `GET /access-requests/review` | Authorized reviewer | Web/API | Status filter, forbidden reviewer | Server authorization source and OpenAPI | Documented; runtime untested in this continuation |
| `PATCH /access-requests/{requestId}/decision` | Authorized non-self reviewer | Web/API | Approve/reject, self-review, stale request, missing request | Server authorization/source and OpenAPI | Documented; runtime untested in this continuation |
| `PATCH /access-requests/{requestId}/cancel` | Owning requester | Web/API | Owner, other Party/missing indistinguishability, non-pending request | Server authorization/source and OpenAPI | Documented; runtime untested in this continuation |
| Mobile Events tab | Authenticated customer/fan | iOS/Android source | Account switch, read failure, write failure, saved/all scopes, retry, stale mutation completion | Source, Party-scoped storage tests, TypeScript/Jest | Implemented; native runtime pending |
| Mobile Event detail | Authenticated customer/fan | iOS/Android source | Party unavailable, storage unavailable, retry, save/remove success/failure | Source and compile/test checks | Implemented; native runtime pending |
| Mobile profile saved tab | Authenticated customer/fan | iOS/Android source | Party switch, empty, loading, read error/retry, remove failure | Source and compile/test checks | Implemented; native runtime pending |
| Legacy `tdf-saved-event-ids` data | Unknown prior account | Same mobile install | Account A to B switch, legacy values present | Storage source/unit fixture | Quarantined and preserved; explicit provenance-safe import deferred |
| Web generated contract | Web roles using access requests | Responsive web source | Five access-request operations and evidence descriptions | Canonical generation, TypeScript, byte comparison | Verified locally |
| Mobile generated contract | Mobile roles using access requests | iOS/Android source | Same five operations | Required workspace generation and byte comparison | Verified locally and published in mobile head `1e07ae92d` |
| Other public discovery, booking, commerce, education, and internal routes | Roles/devices in primary report | Web/mobile | Existing coverage states | Prior consolidated audit only | Not rerun; no broadened claim |

Explicitly untested here: real multi-account device switching, iOS/Android persistence behavior, app upgrade with a legacy key, VoiceOver/TalkBack, virtual keyboard, orientation, safe areas, real OAuth, staging API, email delivery, payments, browser performance, field p75 metrics, and production deployment.

## Historical finding revalidation

Historical reports remain preserved and were used as inputs rather than rewritten.

| Historical/current topic | Classification on 2026-09-07 | Evidence |
| --- | --- | --- |
| Client-observed `access_requested` onboarding completion | Resolved and locally verified | Party/time-window query plus missing/cross-Party/pre-signup/valid/repeat Hspec cases passed after the optimized backend/test build. |
| Canonical OpenAPI omitted access-request routes | Resolved | Four paths/five operations added; YAML structure, generation, equality, and web typecheck passed. |
| Saved events are device-local | Still present, narrowed and truthfully represented | Storage remains local but is now Party-scoped; UI copy names the device scope. |
| Saved events leak across accounts on one device | Newly confirmed regression/omission; resolved in implementation | Baseline global key and account-switch cache behavior; new separate Party keys and cache identities. |
| Saved-event write failures look successful | Newly confirmed; resolved in implementation | Baseline swallowed exceptions versus propagated storage errors and caller error states. |
| `event_saved` is authoritative onboarding evidence | Still not true | Backend still treats this value as client-observed; remote saved-event evidence is deferred. |
| Experiment assignment/exposure is cross-device authoritative | Still present/deferred | Experiment remains paused; no reactivation or measurement claim. |

## Findings and disposition

### ONB-EVID-02 — access-request completion was not evidence-backed

- Journey/role: new authenticated user requesting an authorized path to a governed feature.
- Reproduction: create eligible onboarding progress, omit any feature-access request, then call completion with `firstValue=access_requested`.
- Expected: progress remains pending unless this Party submitted a real request during the eligibility window.
- Baseline actual: any allowlisted client claim could complete onboarding.
- Evidence: `TDF.ServerAuth`, `FeatureAccessRequest` model/server creation path, completion contract, synthetic Hspec fixture.
- Severity/confidence: high measurement and journey-integrity risk; high confidence. Observed impact is false completion state; business/conversion impact is not measured.
- Cause/remedy/effort: compatibility fallback lacked domain evidence; add a qualified Party/time predicate without changing authorization or approval policy. Small.
- Dependencies: existing durable access-request table and authenticated Party identity.
- Acceptance: missing, other-Party, pre-signup, and future evidence fail; valid in-window evidence completes once; approval status does not rewrite the historical fact that a request was submitted.
- Status: implemented; OpenAPI/source verified; optimized backend/test build and focused local execution passed. Exact-head hosted CI remains pending until root publication.

### MOB-PRIV-01 — saved-event interests crossed account boundaries

- Journey/role: two authenticated customers/fans using the same device.
- Reproduction: account A saves an event, signs out, then account B signs in and opens saved events.
- Expected: B never receives A's saved-event interests.
- Baseline actual: both accounts read `tdf-saved-event-ids`; clearing React Query did not clear or rebind that key.
- Evidence: `savedEvents.ts`, `AuthProvider` account switching, three mobile callers, and storage unit fixtures.
- Severity/confidence: high privacy/correctness risk; high confidence. No production incidence count is available.
- Cause/remedy/effort: device-global storage key with no owner; require a valid Party ID and use `tdf-saved-event-ids:party:{id}` throughout. Small-medium.
- Dependencies: authenticated Party identity; no backend/schema dependency for containment.
- Acceptance: separate accounts read distinct keys; missing/invalid Party fails before storage; stale mutations cannot emit analytics into the new account; legacy unowned data is not imported.
- Status: implemented; focused/full mobile verification recorded below when complete.

### MOB-STATE-03 — storage failures produced false success

- Journey/role: authenticated mobile event customer/fan with unavailable/corrupt device storage.
- Reproduction: make AsyncStorage `setItem`, `removeItem`, or `getItem` reject, then save/remove/open saved events.
- Expected: no success copy or completion analytics; show recoverable error; retain malformed raw data.
- Baseline actual: reads returned an empty list and writes were swallowed, while UI and analytics treated the mutation as successful.
- Evidence: storage source, Events/Event detail/profile callers, rejection/corruption fixtures.
- Severity/confidence: high data-integrity and measurement risk; high confidence.
- Cause/remedy/effort: best-effort persistence was incorrectly treated as domain success; propagate errors, add query/mutation recovery, and emit analytics only after successful persistence. Small-medium.
- Dependencies: AsyncStorage and existing React Query error handling.
- Acceptance: read/write/remove errors surface; retry is available; corrupt non-array/JSON data remains unchanged; no success analytics on failed persistence.
- Status: implemented; native OS failure behavior remains unverified.

### API-DOC-02 — governed access-request runtime lacked canonical paths

- Journey/role: requesters, reviewers, and generated-client consumers.
- Reproduction: inspect Servant `AccessRequestsAPI` and compare canonical OpenAPI paths.
- Expected: canonical contract documents every shipped operation, authentication method, input, output, and authorization-sensitive error.
- Baseline actual: schemas existed but the four runtime paths were absent.
- Evidence: `TDF.API`, `TDF.Server`, DTOs, OpenAPI diff, generated operation types.
- Severity/confidence: medium-high integration risk; high confidence.
- Remedy/effort: add four paths/five operations without changing runtime behavior. Small.
- Acceptance: YAML parses, operations generate identically for both clients, and web/mobile compile.
- Status: implemented; generation and web compile verified, mobile compile result recorded below when complete.

### MOB-SYNC-01 — saved events still do not follow the account across devices

- Observed impact: a successful save is available only on the current installation. Estimated repeat-engagement impact is plausible but unmeasured.
- Existing capability: account-scoped `directory_favorite(account_party_id,target_kind,target_id,created_at)` supports event favorites and should be reused rather than duplicated.
- Deferral reason: its current API needs canonical event/existence/visibility validation, filtered reads, and OpenAPI/runtime response alignment before a remote-only cutover is safe. A partial migration could lose offline writes.
- Acceptance for the next batch: server-acknowledged desired-state PUT/DELETE, saved-event filtered reads, Party-scoped cache/outbox, explicit account-bound legacy import, offline retry, two-device convergence, and authoritative `event_saved` evidence.
- Status: deferred after immediate privacy/integrity containment.

## Implementation rationale and system behavior

The access-request predicate verifies submission history, not current request status. Approved, rejected, cancelled, or expired requests still prove the first useful action occurred; none of those states grants a role by itself. The change does not allow product intent to self-assign permissions and does not alter review, cancellation, price, legal, or business policy.

The mobile fix requires authenticated Party identity because the current event surfaces are authenticated. It does not invent a guest owner, hash an auth token into a key, or silently attach the ambiguous legacy list to whichever account signs in first. Quarantine avoids cross-account disclosure and destructive migration. An explicit provenance-safe import experience remains a backlog item.

React Query keys include Party identity, and mutation variables retain the initiating Party. If an account changes while a write is in flight, the old Party's scoped cache may be invalidated but the new Party receives no haptic, success analytics, or first-value event from that completion.

## Design system, copy, localization, and accessibility

No component library, brand, ranking formula, role, or navigation architecture changed. Existing React Native controls and TDF styling are reused.

New Spanish states distinguish unavailable saved storage from an empty collection, provide a visible retry, and describe successful persistence as “en este dispositivo para esta cuenta.” Error text is exposed in the existing accessible error/live-region patterns where present. Buttons preserve visible labels rather than replacing them with ambiguous icons.

These touched mobile screens still use hard-coded Spanish as part of a broader pre-existing localization gap; no isolated parallel translation mechanism was added. English catalog parity remains deferred to the existing mobile localization migration rather than introducing another source of truth.

No VoiceOver/TalkBack, switch control, enlarged-text, touch-target measurement, contrast, safe-area, orientation, or virtual-keyboard runtime was performed. No WCAG/native conformance claim is made.

## Performance and measurement

Party namespacing adds no network request and only changes the AsyncStorage key. No before/after performance run was warranted or performed. The existing N-by-one fetch of saved event IDs remains a documented performance/reliability issue for the remote-sync batch.

`access_requested` can now be counted as completion only after server evidence. Mobile save/change and first-value analytics execute only after a successful local write, and an account switch suppresses stale-account emission. `event_saved` is still client-observed by the backend and must not be reported as fully authoritative.

No representative field data, p75 LCP/INP/CLS, funnel baseline, or uplift was measured.

## Verification record

| Executed check | Result | Scope limitation |
| --- | --- | --- |
| `npm run generate:api:ui` | Passed; `openapi-typescript 7.10.1` generated the web client | Contract generation, not runtime integration |
| `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api:mobile` | Passed; generated the real submodule client | Workspace/client generation, not native runtime |
| Generated client `cmp -s` | Passed; byte-for-byte equal | Does not compile consumers by itself |
| Ruby YAML/operation structure check | Passed; four paths/five operations present | Structural contract check only |
| `npm run typecheck:ui` | Passed | Web compile only |
| `npm run test:production-release` | 49/49 passed | Release invariants; no deployment |
| `npm run test:ci-pipeline` | 16/16 passed | CI scope invariants |
| `npm run audit:catalog-lists` | Passed | Static catalog governance |
| Mobile saved-event focused Jest | 2 suites / 9 tests passed | Mocked AsyncStorage and rendered React Native components, not native OS storage |
| Required mobile TypeScript | Passed | Static compile, not native runtime |
| Full mobile ESLint | Passed with `--max-warnings=0` | Static lint, not runtime behavior |
| Required full mobile Jest | 66 suites / 340 tests passed | Full JavaScript collection with mocked/local dependencies, not native device/backend integration |
| Web production build | Passed; Vite transformed 12,415 modules and the initial-bundle check reported 5 preloads / 412,162 gzip bytes | Production bundle creation, not deployment; existing chunk-size warning remains |
| Backend compile/link | Current 184-module test target compiled and linked. The Stack wrapper was stopped with exit 130 only after a different process reacquired the directory lock before test launch. | Compilation/link succeeded; the wrapper is not counted as a passing `stack test` command |
| Freshly linked Hspec executable: `--match onboarding` | 3 examples / 0 failures in 0.0124 seconds | Focused onboarding logic; not full backend suite or staging |
| Freshly linked Hspec executable: access-request evidence match | 1 example / 0 failures in 0.0108 seconds | Missing, other-Party, pre-signup, future, valid in-window, and repeated evidence paths in one synthetic SQLite example |
| Mobile exact-head GitHub checks | Mobile Validate and Datadog Synthetic checks passed at `1e07ae92d46677b42b14f33c59766b3dea84e830` | Remote PR checks; not App Store/EAS release or production validation |
| `git diff --check` | Passed at review snapshots | Whitespace only |

No test was disabled, skipped, or weakened to obtain these results. Interrupted Stack wrappers are not counted as passing tests; only the completed compile/link and directly executed focused matches are reported.

## Changed files in this continuation

Root/backend/contract:

- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq-ui/src/api/generated/types.ts`

Mobile submodule:

- `src/lib/savedEvents.ts`
- `__tests__/savedEvents.test.ts`
- `app/(tabs)/events.tsx`
- `app/eventDetail.tsx`
- `app/userProfile.tsx`
- `src/api/generated/types.ts`

Documentation:

- this report

No new screenshot was captured. Existing real local artifacts remain under `artifacts/ux-audit-2026-09-05/`.

## Remaining risks and next batch

1. Harden and integrate the existing `directory_favorite` event path; add remote desired-state sync, offline outbox/reconciliation, explicit legacy import, two-device/account-switch tests, and authoritative `event_saved` completion evidence.
2. Make `moment_reaction` evidence server-authoritative and retry onboarding completion after a domain action succeeds but the completion handshake fails.
3. Require exact-head hosted backend CI after the root push before treating the locally verified access-request evidence change as integration-ready.
4. Run synthetic multi-account upgrade tests on iOS and Android, including storage denial/corruption and app relaunch.
5. Move new and existing mobile journey copy through the supported ES/EN localization catalog.
6. Keep the onboarding experiment paused until assignment, identity, exposure, completion definition, and eligibility windows are all server-authoritative.
7. Preserve the broader audit backlog for browser accessibility, mobile native accessibility, LCP, deep links, booking/commerce runtime coverage, and internal workflow efficiency.

## Usability script delta (not conducted)

- Customer/fan: sign in as account A, save an event, sign out, sign in as B, confirm A's interest is absent, then return to A and confirm its scoped local state remains.
- Customer/fan recovery: deny or corrupt local storage, attempt a save, explain the result, retry after recovery, and verify no premature success.
- Artist/professional applicant: create a new account, submit one governed access request, refresh/relaunch, and explain the difference between “request submitted” and “access granted.”
- Staff reviewer: review and decide the synthetic request, then confirm the decision did not assign a role automatically.

No participant was contacted and no session result or quotation is claimed.

## Branch and PR handoff

- Root draft PR: https://github.com/diegueins680/tdf-app/pull/241
- Mobile draft PR: https://github.com/diegueins680/TDF-mobile/pull/40
- Mobile continuation head `1e07ae92d46677b42b14f33c59766b3dea84e830` is pushed and its native repository checks pass. Root functional commits `bfd39889e`, `d8d66e4c7`, and `248403e67` plus base merge `0a047a70e` precede this documentation closure; the final root push, exact-head CI, and PR merge state must be confirmed at handoff.
- No merge or deployment was performed.
