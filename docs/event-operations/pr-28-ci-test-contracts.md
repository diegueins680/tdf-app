# PR 28 — directory continuity and RSVP CI compatibility

## Scope and evidence before changes

CI compatibility follow-up to draft PR #395, head
`7cdd712568311975407cf0b7cb2ee2081a0c0414`, on branch
`test/event-rsvp-ci-contracts`. Initially test-only; unblocking the directory
suite exposed a lost UI contact-return integration (see CT04 below). Restore
that integration only. No migration, authorization, formal model, payment,
deployment or production configuration changes.

`gh-fix-ci` guided the read-only Actions/log/artifact diagnosis. Its auxiliary
`plan` skill was unavailable; the plan is recorded here. The user's explicit
authorization to proceed autonomously with safe reversible work governs this
reversible correction; external Vercel/Cloudflare checks were not investigated.

Compared exact-head hosted runs for #395 and parent #388:

- [#395 persona job](https://github.com/diegueins680/tdf-app/actions/runs/35055457307/job/104664640171)
  and [#388 persona job](https://github.com/diegueins680/tdf-app/actions/runs/35049400368/job/104646357765)
  both report 97 passed, 10 skipped, one failure at
  `event-rsvp-sharing.spec.mjs:158`. The expected button `Ya tengo una cuenta`
  does not exist. Downloaded #395 screenshot and accessibility snapshot show
  the open `Crear cuenta` dialog with `Cancelar` and disabled `Crear e ingresar`.
  The application source confirms those controls; this is not a timeout diagnosis.
- [#388 UI job](https://github.com/diegueins680/tdf-app/actions/runs/35049400368/job/104646357773)
  reports 209 passing suites and one import failure: `client` mock missing
  `del`. The real profile page now imports RSVP controls/feed, which import
  `SocialEventsAPI`. The contact suite did not mock this added domain boundary.
  #395 skipped UI quality by path scope; that is not evidence this failure disappeared.
- Catalog audit reports 179 unreviewed candidates and nine stale decisions on
  both heads, with identical stale IDs. This does not establish equality of every
  candidate. Domain-by-domain review is separate; do not blindly regenerate or
  exempt decisions to obtain green CI.

## Executable contracts and plan

| Contract | Test change | Required preservation |
| --- | --- | --- |
| CT01 | Assert current accessible signup controls and consent-disabled submission | Keep double activation, exact return URL, one RSVP write, exact body, profile/share/withdrawal and axe checks |
| CT02 | Mock the RSVP domain feed explicitly in directory contact tests | Render the real feed; assert exact directory slug; keep contact target, permission, cancellation and accessibility assertions |
| CT03 | Block unhandled foreign traffic and unconfigured API requests in the touched browser suite | Only fictional route fixtures may succeed; no real signup, RSVP, payment or provider calls |
| CT04 | Restore existing target-bound profile contact navigation using `directoryContactRouting` | Guests retain the exact internal profile target through login; only an authenticated exact-target resume shows review/cancel actions; navigation never sends a message or grants permission |

CT04 was specified before the application repair. Local execution after the
mock fix exposed three existing assertions failing: guest return URL, explicit
authenticated continuation and cancellation. Historical commit `cf971dadc`
contains the missing integration; the current page no longer calls the retained,
unit-tested helpers. This is a behavioral regression, not a reason to change those
three expectations. Keep current RSVP/feed functionality while restoring the
narrow contact block, not the historical whole file.

CT04 inputs are loaded profile ID, current session, route path/search and user
click. Preconditions: successful existing directory read; `kind=profile` for
the specialized return. Guards: current session plus exact loaded target for
resume; wrong/empty target or absent session cannot show resume controls.
Effects: ordinary internal links only, unchanged protected composer destination;
cancel removes resume state by returning to the profile path. No automatic
navigation, message, API write, onboarding receipt or authorization transition.
Non-profile and loading/error paths retain their behavior. The existing server
and protected composer remain responsible for authentication and authorization.

These presentation-only transitions use executable contracts and adversarial
tests, not a claim that `WebOnboardingRecovery` proves directory contact routing.
Existing high-risk formal models/checks remain enabled with unchanged finite
scopes; no new high-risk state machine is introduced here.

## Verification

Executed on 2026-09-15, America/Guayaquil:

- Reproduced the original Jest import failure locally (zero tests could load).
  After repairing the mock boundary, two tests passed and three existing
  continuity assertions failed. Those assertions were preserved and drove CT04.
- Before editing application behavior, reran the full pinned command documented
  in [PR 27](pr-27-raci-real-browser.md): 23 positive TLC configurations, 53 named
  negative controls, 13 PlusCal integrity tests, two SAT Alloy scenarios and 13
  UNSAT assertions passed, exit zero. Finite bounds/abstractions are unchanged;
  no universal proof or new directory-specific model claim.
- Final focused Jest run: **43 passed, zero failed/skipped, five suites**, including
  all seven directory tests (guest/mismatched targets, explicit continuation,
  cancellation, feed scope and ES/EN) plus URL, RSVP component and API regressions.
- Final Chromium desktop run: **3 passed, zero failed/skipped/flaky/retries**,
  28.9 seconds. Complete synthetic RSVP flow, private/cancelled visibility and
  fixture isolation all passed. Existing axe assertions found no serious/critical
  violations on event/profile pages. Both final screenshots were generated and
  visually inspected; screenshots now use test artifact paths, not tracked docs.
- Whole-web TypeScript and focused ESLint passed after the repair; JavaScript
  syntax and `git diff --check` passed. JSON reports were independently read to
  verify counts. This is not a claim that the full 210-suite UI job ran locally.

Focused commands from the repository root:

```sh
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/pages/DirectoryPublicDetailPage.test.tsx \
  src/utils/directoryContactRouting.test.ts src/utils/loginRouting.test.ts \
  src/components/events/EventRsvpControls.test.tsx src/api/socialEvents.test.ts
npm run typecheck --workspace=tdf-hq-ui
./node_modules/.bin/eslint tdf-hq-ui/src/pages/DirectoryPublicDetailPage.tsx \
  tdf-hq-ui/src/pages/DirectoryPublicDetailPage.test.tsx --max-warnings=0
```

The browser command was `playwright test e2e/web/event-rsvp-sharing.spec.mjs
--project=chromium-desktop`, run by an owned programmatic Vite server with
`envFile:false`, `envPrefix:[]`, explicit empty API/analytics/Google-client defines,
loopback `127.0.0.1:4173`, `strictPort:true`, and `CI=''` for reuse of that owned
listener. It used the repository Vite config, closed the listener in `finally`,
and propagated the child exit code. No deployment `.env` or live API was used.
Local JSON/screenshots reside under the disposable diagnostic directory
`/private/tmp/tdf-pr395-ci-evidence.ibCQPb/`; they are not committed credentials
or production captures. Hosted CI continues using its existing Playwright job.

Hosted #395 RACI/browser compilation and backend checks remained in progress
at the final parent checkpoint. #395 stays open/draft on its original head, no
auto-merge; the child does not rewrite it. The parent persona job still reports
the original failure because these fixes are on the child branch. The catalog
failure is still open; no catalog decision was changed or gate bypassed.
External failures remain uninvestigated, outside this skill's scope:
[Vercel](https://vercel.com/diego-saas-projects/tdf-app-tdf-hq-ui/AtDECVzJsZVG8jm2LeXMMh8JQWQ6)
and [Cloudflare Pages](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/56bb4d1d-34a3-4426-85ae-3d6692cf553f).

## Rollback and remaining limitations

Revert only this follow-up's narrow page integration, tests and documentation;
no persisted data changes. Existing server/composer access checks are unchanged.
This synthetic browser test does not replace #395's real session/API/PostgreSQL
integration and does not prove real signup, deployment, native mobile or payment
behavior. Catalog governance, external deployment checks and overall mission
completion remain open. No CI gate, retry count, timeout, skip or authorization
assertion may be weakened by this correction.
