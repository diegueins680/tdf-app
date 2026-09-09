# TDF onboarding completion recovery audit and implementation — 2026-09-08

## Outcome

This bounded onboarding-first batch fixes the highest-priority deferred failure in the 2026-09-08 recovery audit: a real first useful action could succeed on the server while the separate onboarding-completion handshake failed, leaving no durable retry after relaunch.

Mobile now stores an allowlisted first-value label under the authenticated Party only, retries the idempotent server handshake on authenticated bootstrap, foreground, and offline-to-online recovery, and clears the marker only after an authoritative terminal response. Tokens, emails, event or moment identifiers, captions, comments, and permission-like values are never stored. Auth-session assertions surround storage, network, cleanup, state, and analytics boundaries so account B cannot consume account A's marker or result.

The backend now accepts a delayed handshake when the Party's durable evidence was created within the 24-hour signup window. For newly accepted completions, it stores the evidence timestamp as `firstValueCompletedAt` and the handshake timestamp as `completedAt`; existing completed rows are not rewritten and may retain the earlier handshake-time meaning. A late explicit optional exit remains rejected, as does missing, pre-signup, future, other-Party, or post-window evidence.

The paused `single-feature-onboarding-v1` experiment remains paused. There are no pricing, cancellation, role, permission, payment, legal, deployment, or production-data changes.

## Capability and safety matrix

| Capability | Status | Evidence from this execution | Consequence |
|---|---|---|---|
| Repository read/write | Available | Read source/instructions and wrote only the isolated worktree | Implementation and documentation were possible without touching the user's primary checkout |
| Dedicated branches | Available | Root and mobile branch `feature/onboarding-completion-retry-20260908-reviewed` created from clean published baselines | Changes can be reviewed independently and stacked on the preceding onboarding PRs |
| Baseline/worktree safety | Available | Root baseline `1a65fa9fb619025f1eeaa41de8ef24500df379d8`; mobile baseline `3b7bf3ed28fc7d95ec9fac84bddc0fa5ba5ceafc`; both clean before branching | No unrelated edits were absorbed |
| Mobile submodule | Available | Real `tdf-mobile` workspace present at the baseline above; required checks use `REQUIRE_MOBILE_WORKSPACE=1` | Mobile cannot silently skip as a missing workspace |
| Node/npm and TypeScript | Available | Mobile and web type checks executed successfully; generated both clients from the canonical OpenAPI file | Client contract and TypeScript validation are covered |
| Backend/DB | Available for local tests | Stack project and SQLite-backed Hspec suite available; no schema migration is required | Backend timestamp invariants can be compiled and exercised without production data |
| Browser/screenshots | Available, not used | Playwright 1.59.1 is installed; this batch changes no rendered UI | No before/after screenshot was fabricated; visual regression is not material to this state-recovery change |
| Native device runtime | Partial | Android tooling reported no connected devices; Xcode listed available but shut-down iOS 18.3 simulators; no native build was run | Native persistence behavior is automated-test verified, not device-runtime verified |
| Test runners | Available | Jest, TypeScript, ESLint, API generation, Stack/Hspec, Expo release checks, and repository doctor are available | Automated regression validation is possible |
| Network | Partial/available with approval | Sandboxed Stack metadata access failed; approved access downloaded the pinned toolchain/snapshot | Dependency fetching was possible; no application request targeted production |
| Local/staging configuration | Partial | Local test configuration and no-op analytics are available; no synthetic authenticated staging fixture was provided | Source, mock, and local DB paths are covered; staging journey remains unverified |
| Synthetic accounts | Unavailable | No synthetic external account credentials were present | No live signup/action/relaunch walkthrough was performed |
| Analytics/dashboard access | Unavailable | PostHog reports disabled when `EXPO_PUBLIC_POSTHOG_KEY` is unset | Event semantics are code/test verified; field delivery and funnel baselines remain not yet measured |
| GitHub authentication/PR | Available | Direct keyring-backed `gh auth status` and `gh api user` succeeded after excluding stale injected token variables; the mobile branch was pushed and draft PR #54 is open | Root push/PR can follow the local backend result; the repository doctor warning is an environment-variable conflict, not absence of keyring authentication |
| Production access | Not used | No deploy, real transaction, communication, or production mutation command was run | This report makes no production-validation claim |

Session instructions read: root `AGENTS.md`, `SOUL.md`, `USER.md`, `MEMORY.md`, `AI_WORKFLOW.md`, `CONTRIBUTING.md`, backend `AGENTS.md`, and mobile `AGENTS.md`. The requested daily memory files for 2026-09-07 and 2026-09-08 were absent. Existing scripts were inspected before execution.

## Method and baseline

This continuation coordinated product strategy, UX/IA, mobile/backend/web engineering, security/privacy, analytics, performance, accessibility, and QA perspectives. Evidence combines source/contract inspection, a cognitive walkthrough of action-success/handshake-failure/relaunch, strict Party/session invariants, generated-client comparison, Jest state tests, and local database tests. No user interviews, analytics uplift, conversion rate, or production result is claimed.

The source baseline already had server-authoritative evidence for artist follows, access requests, event saves, and moment-reaction additions. Every production mobile caller invoked completion after its server-confirmed action and supplied the initiating token. The remaining gap was the swallowed completion failure in `markFirstValueCompleted` and the absence of any recovery lifecycle in `FirstRunProvider`.

## Coverage matrix

| Journey/surface | Role | Device/state coverage | Inspection | Status |
|---|---|---|---|---|
| Mobile artist follow | Authenticated customer/fan Party | Successful follow; failed completion; same-Party retry; account replacement | Source, focused Jest, full mobile Jest, typecheck | Implemented and automated-verified; native runtime unverified |
| Mobile access request | Authenticated Party lacking an eligible capability | Persisted request followed by completion failure/retry | Source, shared helper tests, full mobile Jest | Implemented and automated-verified; screen-specific recovery UI not added |
| Mobile event save, list/detail | Authenticated customer/fan Party | Server save, offline/5xx handshake, relaunch/reconnect retry, stale session | Source, shared helper/provider tests, existing screen suites, full mobile Jest | Implemented and automated-verified; staging unverified |
| Mobile event-moment reaction, detail/gate | Authenticated Party | Remote active addition only; local/remove excluded by prior tests; completion recovery | Source, gate/repository tests, full mobile Jest | Implemented and automated-verified; experiment remains paused |
| First-run provider lifecycle | New and returning authenticated Parties | Bootstrap, offline fail-closed, reconnect, foreground, completed record cleanup, stale GET race, A→B change | Focused rendered tests | Implemented and automated-verified, including direct foreground trigger coverage |
| Backend completion | Authenticated new Party | Missing/valid/repeated evidence; delayed in-window evidence; post-window evidence; late explicit exit | Source, OpenAPI, local SQLite Hspec | Implemented and automated-verified |
| Web completion consumer | Authenticated external user | Existing behavior; generated contract comments only | Source, generated client, focused web Jest, typecheck | Contract verified; no rendered web change |
| Cross-device recovery | Same Party on a different device | Server state survives; pending label does not transfer before a successful handshake | Architecture assessment | Deferred; see risks |
| iOS/Android native persistence | Authenticated Party | Relaunch/foreground storage on actual native runtime | Not executed | Unverified |
| Production field behavior | Real users | Network loss and analytics delivery | Not inspected | Blocked by credentials/authorization |

## Revalidated historical findings

| Historical source/finding | Current classification | Evidence |
|---|---|---|
| `reports/onboarding-recovery-localization-audit-2026-09-08.md`: durable completion-handshake retry is the highest next batch | **Resolved and automated-verified in this batch** | Party-keyed marker, provider replay lifecycle, server late-reconciliation rule, mobile Jest, and SQLite Hspec |
| `reports/onboarding-evidence-integrity-audit-2026-09-08.md`: completion and eligibility must remain auth-session bound | **Still resolved and extended** | Existing capture/assert primitives now guard persisted retry reads/writes/requests/cleanup |
| `reports/onboarding-ux-audit-2026-08-20.md`: device-local flags misclassified returning users | **Superseded for eligibility/completion authority** | Server remains authoritative; local marker is only a retry hint and cannot complete or grant anything |
| Existing experiment-measurement limitations | **Still present/deferred** | Experiment remains disabled; assignment/exposure are outside this batch |

Historical reports were preserved unchanged.

## Findings and implementation status

### OCR-01 — A successful first action lost its completion recovery

- Journey/role: all four mobile first-value paths; authenticated new Party.
- Reproduction at baseline: allow the domain mutation to succeed, make `POST /session/onboarding/complete` fail, then relaunch. The helper returned `false`, callers emitted no completion, and no retry metadata existed.
- Expected: retain the real domain success and retry completion idempotently for the same Party/session.
- Actual impact: onboarding could remain incomplete or reappear even though the useful action already existed. Observed behavior; conversion impact not measured.
- Severity/confidence: high / high.
- Cause: completion exceptions were intentionally swallowed without a durable recovery handoff.
- Remedy: strict Party-keyed, allowlisted AsyncStorage record; retry on bootstrap/foreground/reconnect; server response owns cleanup.
- Effort/dependencies: medium; existing AsyncStorage, auth binding, network provider, and idempotent endpoint.
- Acceptance: offline/5xx retains marker; same Party replays; another Party cannot read/send/delete it; storage failure does not falsify the domain action; terminal response clears it; no token or content is stored.
- Status: implemented and automated-verified on mobile.

### OCR-02 — The server rejected a valid action when its retry arrived after the eligibility window

- Journey/role: any first-value action near the 24-hour boundary; authenticated new Party.
- Reproduction at baseline: create valid Party-bound evidence before the deadline, submit completion just after the deadline.
- Expected: eligibility belongs to the action evidence time; a delayed idempotent handshake must reconcile it.
- Actual: the handler rejected on handshake time before inspecting evidence. Observed contract defect; frequency not measured.
- Severity/confidence: high / high.
- Cause: `isOnboardingEligible now` guarded every completion request and `firstValueCompletedAt` used handshake time.
- Remedy: for supplied first values, find the earliest evidence in `[signupAt, min(now, signupAt + 24h)]`; store evidence time separately; retain the current time rule for explicit exits.
- Acceptance: valid in-window evidence completes after expiry once; timestamp is preserved; repeated call is false; late explicit exit and out-of-window evidence remain false.
- Status: implemented and automated-verified against SQLite; PostgreSQL staging remains unverified.

### OCR-03 — An older eligibility read could reopen onboarding after a newer completion

- Journey/role: authenticated Party completing an action while bootstrap GET is pending.
- Reproduction at baseline: delay `GET /session/onboarding`, complete onboarding, then resolve the GET with the earlier eligible state.
- Expected: newer completion wins.
- Actual: the older effect could overwrite local state. Observed source race; runtime frequency not measured.
- Severity/confidence: medium / high.
- Remedy: generation-bound provider state; a completion invalidates older reads and makes cohort state ready.
- Status: implemented and rendered-test verified.

### OCR-04 — Client analytics delivery remains at-most-once

- Journey/role: completion response lost after the server commits, or analytics SDK delivery failure.
- Impact: a retry returns `newlyCompleted=false`, correctly avoiding duplicates but potentially undercounting completion. No user-facing or authority error.
- Severity/confidence: low user harm, medium measurement harm / high.
- Remedy: if lossless measurement becomes required, introduce a server completion receipt/stable insert identifier and a privacy-reviewed delivery contract.
- Dependencies: backend/API/analytics contract and dashboard access.
- Acceptance: retry can deliver one deduplicated completion receipt without email, token, free text, or cross-session attribution.
- Status: deferred; current implementation favors truthful no-duplicate events.

### OCR-05 — Pending recovery does not move to a different device

- Journey/role: same Party performs the action on device A, loses the handshake, then signs in only on device B.
- Impact: server evidence exists, but device B does not know which allowlisted value to submit. The account remains incomplete until device A retries or the eligibility window expires. Estimated rare edge case; not measured.
- Severity/confidence: medium / high.
- Remedy: a separate server reconciliation operation or atomic completion in each domain transaction; do not overload explicit exit semantics.
- Dependencies: API design, multi-client generation, analytics receipt semantics, and broader domain-handler changes.
- Acceptance: device B reconciles server evidence without client assertion, duplicate analytics, or mutation via GET.
- Status: deferred to the next backend onboarding contract batch.

## Implementation rationale and invariants

- The marker is a recovery hint, never eligibility or permission authority.
- Only canonical API-generated first-value values parse; malformed or permission-like values are discarded without a request.
- The key is namespaced by normalized positive Party ID. Code never enumerates another Party's keys.
- The token is required to enqueue or retry. `captureAuthSession`, request abort signals, and `assertAuthSession` prevent stale responses from mutating the new session.
- Storage failure does not turn an already-acknowledged domain action into a fake failure; the immediate authoritative request still runs.
- Network failure leaves the marker. A terminal server response clears it best-effort. Cleanup failure can cause a harmless idempotent retry.
- `newlyCompleted=true` remains the only completion-analytics signal. A retry that observes an already-completed record clears state without inventing an event.
- No timer or background supervisor was introduced. Existing connectivity changes and app foreground events trigger a bounded pass.
- The network provider moved above the first-run provider so connectivity can trigger recovery; visual network behavior is unchanged.
- API response shape and database schema are unchanged. Existing completed rows are intentionally not backfilled, so their `firstValueCompletedAt` can retain the legacy handshake-time meaning. Generated outputs were produced from `tdf-hq/docs/openapi/api.yaml`, not hand-edited.

## Accessibility, localization, performance, and privacy

This is a state-integrity batch with no new rendered controls or copy, so no WCAG claim or screenshot is appropriate. Existing visible onboarding/reaction recovery remains ES/EN and accessible from the preceding batch.

Mobile performance impact is bounded to the existing progress GET plus at most one pending completion POST for the current Party on a trigger. No media fetch, dependency, polling loop, or render-heavy component was added. The event-save evidence path loads matching `favorite.saved` timestamps for one Party and filters decoded `UTCTime` values, avoiding incompatible SQLite/PostgreSQL textual timestamp ordering. That correctness-first query is on a one-time completion path, but its row count is not yet measured and should be observed before adding a database-specific optimization. Field LCP/INP/CLS and p75 data remain unavailable and are not inferred from laboratory tests.

Persisted JSON contains only `{version: 1, value: <allowlisted label>}` under a Party-scoped key. Analytics properties remain `platform`, `reason`, and the allowlisted `value`; no password, token, email, phone, domain object ID, caption, comment, or other free text is collected.

## Verification

Final commands/results are recorded factually after execution:

- Initial focused mobile recovery set: **pass**, 2 suites / 24 tests; a subsequent direct foreground-trigger test passed in the provider suite, 13/13.
- Focused mobile onboarding surface set before the added foreground assertion: **pass**, 5 suites / 47 tests.
- Final serial `npm test -- --runInBand` in `tdf-mobile`: **pass**, 67 suites / 386 tests; PostHog explicitly disabled because its public key is unset. A prior full run under concurrent lint had one unrelated 5-second event-detail timeout (66 suites / 385 tests passed); the timed-out suite then passed alone, 3/3, before the clean full rerun.
- Final `npm run typecheck` in `tdf-mobile`: **pass**.
- `npm run lint` in `tdf-mobile`: **pass**, zero warnings.
- Focused web session/onboarding/routing Jest: **pass**, 3 suites / 22 tests; Testomatio had no token/pipes.
- Final `npm run typecheck` in `tdf-hq-ui`: **pass**.
- Full web lint: **unverified**. Two runs were intentionally interrupted after extended resource starvation and no diagnostics; the generated file is ignored by the configured linter (a direct invocation returned 0 errors / 1 ignored-file warning). The earlier completed web typecheck and focused Jest are the applicable local web signals for this generated-comment-only change.
- `REQUIRE_MOBILE_WORKSPACE=1 npm run generate:api`: **pass**, web and mobile clients regenerated.
- Initial backend launch: **not a test result**. It compiled the test target, then Hspec rejected a split multi-word `--match` argument before running examples.
- Corrected backend `--match=reconciles`: **pass**, 3 examples / 0 failures, including the new late-reconciliation boundary test.
- Backend `--match=onboarding`: **pass**, 3 examples / 0 failures.
- First backend `--match=evidence`: **failed**, 19/20 examples passed; it exposed mixed SQLite timestamp encodings in the new event-save SQL range. The code was corrected before commit.
- Exact event-save `--match=server-validated`: **pass**, 1 example / 0 failures after the correction.
- Final backend `--match=evidence`: **pass**, 20 examples / 0 failures, including Party binding, in-window evidence, post-window rejection, idempotency, and all onboarding first-value evidence categories. Stack also emitted pre-existing dependency-bound and Cabal `other-modules` warnings.
- `npm run release:check` in `tdf-mobile`: **pass**; 5 required release assets, lint, typecheck, production-profile release validation, and Expo config checks completed.
- `npm run ai:doctor`: **exit 0**, 14 OK / 4 warnings / 0 errors. Warnings: two absent daily memory notes, the expected task worktree changes, and stale injected GitHub token variables; direct keyring-backed GitHub authentication succeeded separately.
- `git diff --check`: **pass** in both repositories after generation; final repetition follows the root commit.
- GitHub: mobile branch pushed and draft PR [TDF-mobile#54](https://github.com/diegueins680/TDF-mobile/pull/54) is open, clean, and has no configured check rollup for its stacked base. Root hosted checks await the focused root commit and draft PR recorded in the handoff update.

These are local/mock/SQLite checks unless explicitly described otherwise. They do not prove native-device storage, staging services, production delivery, or real analytics ingestion.

## Changed files

Root/backend/web:

- `tdf-hq/src/TDF/ServerAuth.hs`
- `tdf-hq/test/TDF/ServerSpec.hs`
- `tdf-hq/docs/openapi/api.yaml`
- `tdf-hq-ui/src/api/generated/types.ts`
- this report and the ready-to-use PR description
- mobile submodule pointer after its published commit

Mobile:

- `src/lib/onboardingIntent.ts`
- `src/providers/FirstRunProvider.tsx`
- `src/providers/AppProviders.tsx`
- `src/api/generated/types.ts`
- `__tests__/onboardingIntent.test.ts`
- `__tests__/FirstRunProvider.test.tsx`

No screenshot artifacts were created because no rendered pixels changed.

## Remaining risks and next batch

1. **Server-side cross-device reconciliation.** Impact and acceptance are in OCR-05; highest-priority remaining onboarding integrity work.
2. **Full surrounding Events/Profile ES/EN coverage.** Several broader screens remain mixed-language outside the shared cards and saved-state slice; acceptance remains a coherent locale after live switching.
3. **Direct Events/Profile state renderer tests.** Existing full Jest covers imports and shared components, but direct loading/cache/empty/error/success render coverage remains incomplete.
4. **Controlled native walkthrough.** Use synthetic accounts to perform action → lost handshake → kill/relaunch → reconnect on iOS and Android, including A→B account switching and enlarged text.
5. **Saved-event bulk hydration.** Replace the remaining N+1 event/venue reads with a bounded server summary contract after measuring a real baseline.
6. **Web moment-reaction journey and broader platform runtime coverage.** Requires an intentional accessible product placement, not a hidden control added for test parity.
7. **Observe event-save evidence query cardinality.** The portable completion query decodes one Party's matching audit timestamps before applying the eligibility window. If production telemetry shows large row counts or latency, add an indexed/normalized server query with PostgreSQL and SQLite contract coverage.

## Handoff

Root and mobile branch: `feature/onboarding-completion-retry-20260908-reviewed`.

Published mobile commits:

- `f4b37cfea1ab7fa3e01a61c8f61eafb67e0ed76b` — recovery implementation and tests
- `a726acd292fc31aa16808a34fa31846bffa3cde1` — generated contract clarification
- `c790ab586ac324c31bc43e8899de36e9f93803e6` — direct foreground-trigger regression test

Mobile draft PR: [TDF-mobile#54](https://github.com/diegueins680/TDF-mobile/pull/54), stacked on `feature/onboarding-recovery-localization-20260908-reviewed`; GitHub reports the draft merge state clean and no configured status-check rollup for this stacked base.

The focused root commit, root draft PR, and final hosted-check state are appended in a documentation-only handoff commit after the initial root push. Workflow inspection confirmed that feature-branch pushes do not execute the root Docker publish job, which is gated to `main`; mobile's validation workflows are gated to pull requests targeting `main`. Nothing in this batch was merged or deployed.
