# PR 17 — account-bound FanHub onboarding recovery

Base: draft [PR 364](https://github.com/diegueins680/tdf-app/pull/364),
`fix/artist-follow-onboarding-continuity`, commit
`1299f9c63a325e17dff0bfe1f048ff52f7bb8b15`.
Branch: `fix/fanhub-authoritative-onboarding`. Draft only; no merge/deploy.

## Scope, sequence and compatibility

The [FH-01–06 contract](fanhub-onboarding-contract.md) audits the five original failing
FanHub tests, global localStorage marker and canonical server support for explicit empty
exit. The new TLA+ model, four negative controls and full TLC/Alloy regression passed
**before feature implementation**. During model design the pending counter was refined
to two generation-bound slots so an old lifetime cannot block a new one; the final
model is the one checked. No discovered counterexample is waived.

`useFanHubOnboarding` uses existing React Query and session/API helpers. Session object,
mode and mounted-lifetime fences isolate pending reads and exit results, including
same-Party rotation and A→B→A. Keys contain only Party, instance and generation, never
credentials. Optional bearer arguments preserve existing cookie call signatures. Runtime
Zod decoding uses generated DTO types; a 2xx or `newlyCompleted` boolean alone cannot
hide guidance. A validated terminal acknowledgement cannot be reopened by an older GET.

Only explicit eligible close/retry submits `{}`. Saving remains visible; failure restores
guidance and offers retry without exposing raw errors. Guests and manager tips use only
ephemeral presentation state, reset with context. The old global key is ignored, not
deleted or converted to authoritative account progress. Existing genre-filter storage
and unrelated hub flows remain intact. New state copy supports Spanish/English and the
previous unnamed progress indicators now have accessible names.

No backend, OpenAPI schema, generated client, database migration, feature flag, mobile
pointer, provider or money changes. Revert this bounded commit to roll back; no data
migration or deletion is required. The separate hub follow/profile mutations do not gain
the exit flow's context fences. Exiting emits no fabricated first-value analytics; it
does not promise lossless analytics delivery when server evidence wins during that exit.

## Exact formal verification

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar bash scripts/verify-event-operations-formal.sh
```

Exit 0. New model: **1,249 generated / 215 distinct states, depth 12**. All four named
mutation violations detected. Full runner: **30 expected negative controls**, two SAT
Alloy scenarios and 11 UNSAT assertions within their documented bounds. Three context
generations and two request slots do not constitute an unbounded proof. No fairness or
eventual network response is assumed; hung requests remain visibly pending.

The unchanged `ReservationRace.tla` emits a PlusCal translation-checksum warning. TLC
checked the committed TLA+ block, not a freshly regenerated PlusCal translation; this
run does not certify translator/source equivalence. Regeneration/checksum hardening is
a separate formal-tooling follow-up, not silently described as verified here. The new
FanHub model is handwritten TLA+ and has no generated translation block.

## Tests and diagnostic history

Unmodified baseline: five failures, exit 1 (10.334 s). After implementation, all five
original behaviors plus nine API tests passed (14/14); fixtures now use generated
`completedAt`/progress shapes instead of an obsolete `completed` field or incomplete
receipt. No original behavior test or accessibility assertion was removed.

Expanded tests first reported 137/140 passed: three new test-fixture problems were a
storage assertion that accidentally forbade unrelated genre persistence, a pending-state
assertion before React Query notification, and a manager without its required module.
The next run had 138/140 passes: manager still lacked the actual registry-required
`Admin` module, and the unchanged artist already-followed cleanup test timed out. The
fixture now supplies the existing required module; access controls were not changed.
Focused FanHub/API then passed 31/31. All failures remain recorded, not counted as passes.

Final expanded regression, including late-GET and inconsistent-signup-payload tests:

```sh
npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/pages/FanHubPage.onboarding.test.tsx src/pages/ArtistPublicPage.component.test.tsx src/pages/ArtistPublicPage.intent.test.ts src/analytics/onboardingProgress.test.ts src/session/SessionContext.test.ts src/session/SessionProvider.personalData.test.tsx src/pages/LoginPage.test.tsx src/session/SessionContext.reconciliation.test.tsx src/routes/AppShell.test.tsx src/session/onboardingIntentRecovery.test.ts src/api/session.test.ts src/api/eventOperations.test.ts
```

**142 tests / 12 suites passed**, exit 0, 26.407 s. These are rendered/mock API and
executable client tests, not actual backend HTTP concurrency or real signup tests.

Browser command:

```sh
npm run test:e2e:web -- --config=playwright.fanhub.config.mjs
```

**8/8 passed**, exit 0, 42.9 s, Chromium desktop and phone, no retries. Synthetic HTTP
fixtures cover read recovery, explicit keyboard exit/failure/retry, malformed 2xx receipts,
already-complete state, reload after terminal confirmation and English guest dismissal.
Both recovery-state axe checks found zero serious/critical violations. The generated
desktop/phone recovery screenshots were visually inspected; the recovery message and
controls remain visible in both layouts. This is not universal WCAG conformance or native
mobile E2E. Local HTTP fixtures block foreign HTTP requests and never call real payment,
signup or completion services. Artifacts are ignored local files, not published evidence.
The isolated config reuses PR 16's local-only Vite setup and selects only these two
projects; default CI discovers the test with its normal wider project configuration.

Final-source checks run sequentially to avoid the earlier host contention:

```sh
npm run typecheck:ui && npm run lint --workspace=tdf-hq-ui && npm run quality:repo
```

Whole-UI typecheck passed on final feature source. JavaScript/browser-config syntax,
formal runner shell syntax, whitespace and all 107 local documentation links passed.
An exact base diff confirms backend, generated API, compiler/Jest configurations and
mobile pointer are unchanged; mobile remains uninitialized at `53569fc4baa842a6882235d9a12c4ee68c44ff24`.

The whole-UI lint produced no diagnostics for over eleven minutes while consuming CPU.
A one-second native sample of this invocation's PID 1159 showed JavaScript/microtask
activity and a 2.5 GB physical footprint (2.6 GB peak), not a conclusive JavaScript root
cause. Only that verified local ESLint process was interrupted with SIGINT; the chained
command exited **130**. This is an **incomplete lint run, not a pass**; repository quality
had not yet started because of the `&&` sequence. No lint rule or CI gate was weakened.

Separate scoped verification commands:

```sh
./node_modules/.bin/eslint tdf-hq-ui/src/api/session.ts tdf-hq-ui/src/api/session.test.ts tdf-hq-ui/src/features/fans/useFanHubOnboarding.ts tdf-hq-ui/src/pages/FanHubPage.tsx tdf-hq-ui/src/pages/FanHubPage.onboarding.test.tsx tdf-hq-ui/src/i18n/locales/en.ts tdf-hq-ui/src/i18n/locales/es.ts --max-warnings=0
npm run quality:repo
```

Both separate commands exited **0**. Focused ESLint covered every changed TypeScript file
without rule overrides. Repository quality passed 143 tests: 8 internship audit, 42 loop,
4 heuristic audit, 61 release safety, 23 CI scope, 2 visual metadata and 3 persona program.
The tracked-source heuristic reported 9,598 findings, zero critical/errors and 355
warnings; it is distinct from real TLC/Alloy verification. Generated internship fixtures
remained unchanged. Fixture logs mentioning local Git pushes, merges or releases are
disposable test operations, not application merges or deployments. These passes do not
convert the interrupted whole-UI lint into a green global gate.

## Remaining gates

Complete event workspace/tasks/logistics/hiring/payments/collaboration/offline and native
mobile remain unfinished. The prior full UI Jest course-suite failures and bundle-budget
failure are not resolved by this bounded repair. Full UI Jest/build/backend/migrations
and mobile checks are not implicitly rerun by the focused command. The club auto-follow
consent risk recorded in PR 16 is unchanged. No server-side identity/revocation or
cross-tab cookie guarantee is added. No production operation or provider activation.
