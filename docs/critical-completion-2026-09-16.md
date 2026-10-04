# Critical completion audit — 2026-09-16

Final acceptance recorded on 2026-09-17: C01–C04 are complete. The deployed
backend revision is `9f0da14de84919f8fba391fa752b7df7412a600d`.

## Baseline and access

Initial remote main: `16a4eef7477eb160feb20210406f70adb58be814`. The original
checkout is on `17a33eca11d585d84435af85340beece9b51d14e` with substantial unrelated
tracked and untracked work. All work for this audit uses an isolated worktree.
Architecture: Haskell/Servant/PostgreSQL API, React/Vite web, Expo mobile submodule.
Stack is the authoritative backend toolchain. Repository, formal, contracts,
PostgreSQL, web/mobile and persona-browser gates are defined in the workflows.

GitHub files, issues, PRs, review threads, checks and Actions are accessible.
Main requires an approving review and resolved conversations. Stacked PRs use
the asynchronous merge API; no administrator override is permitted. Fly access
works through the existing securely loaded credential. Production is `tdf-hq`
with database `tdf-hq-db`; web main deploys automatically to Cloudflare/Vercel.
Backend uses `scripts/production-release.mjs`, immutable images, migration
preflight, backup, canary, health/version verification and guarded rollback.

Observed initial production: two started API machines at image digest
`sha256:a480d2cd98ab4475277bbc2362e8f0ca4202b044bc80f23d4fdc8269c36d55ba`;
`/version` reports `954e995f1cd08f363afe2b0e34ccf02dcd80ea8c`; `/health` reports
both status and database OK. Read-only SQL: 90 registered migrations, zero
release leases, zero provider inbox rows, one cancelled checkout. PayPal webhook
intake and event discovery/auto-publication are enabled; refunds and Datafast
remain disabled. Preserve these states during any release.

The managed browser was initially unavailable because its profile was already
in use. Its existing legitimate Admin session was subsequently accessible via
CDP; the final Customer test used a separate context and normal signup/login.
No shared browser was terminated and no session credentials were fabricated. The host had a recent ENOSPC error in another release's final
record write; subsequent live checks establish health, not that record's success.

## Confirmed critical scope and final status

| ID | Evidence and impact / severity rationale | Components / dependencies | Observable acceptance and validation | Status |
|---|---|---|---|---|
| C01 | Main invitation list returns all event invitations to any viewer; create/update lack organizer/recipient authorization. P0: exposed private invitation messages and unauthorized mutation, confirmed by source and existing regression suite; no claim of observed exploitation. | SocialEventsHandlers, invitation client, existing PR #336. No new production schema needed for these handler checks. | Organizer/admin can manage; recipient sees only own invitations and can only accept/decline; outsider, transfer and message edits denied; race checks retain current authority. Exact-head CI/HTTP/PostgreSQL and post-deploy authorized read-only smoke. | Complete via #336 and the final release. Admin sees 78 event 86 invitations; a newly created Customer sees none; anonymous access returns HTTP 401. See final acceptance below. |
| C02 | Main create/update writes logistics activity separately from dependency/assignment replacement; a rejected relation can leave partial state. P1: inconsistent operational state. | Same handler, transaction and retained-edge helper; reuse #336. Foundation opt-in features stay inactive. | Activity/status/version/relations roll back together on rejection; retained prerequisite identity survives updates. Existing actual-PostgreSQL regression, foundation concurrency/rollback and full backend CI; deployed revision and non-destructive smoke. | Complete via #336 and the final release. PostgreSQL rollback/retained-relation checks pass; deployed logistics read returns HTTP 200 with the expected shape. |
| C03 | Both existing commerce worker loops render arbitrary exceptions after character substitution; diagnostic writes are outside their exception boundary. P1: credential/personal-data disclosure risk and stopped payment/expiry processing on sink failure. No actual secret exposure asserted. | ProviderEventWorker, MerchReservationWorker; reuse the relevant implementation/tests from #393 without its unrelated provider stack. | Output independent of exception text; one tick even when a sink fails; next iteration survives; asynchronous cancellation propagates; fixed JSON/counters retained. Hspec/QuickCheck, full backend, repository/formal gates, production health and aggregate queue checks. | Complete via #418 and the final release. 17 focused regressions, full backend suite and CI passed; runtime configuration, health and worker signals verified. |
| C04 | Historical migration order overwrote one privacy boundary; the release gate rejected the schema and old-image restarts failed. P1: safe delivery/recovery dependency discovered during rollout. | Directory view, migration manifest, release schema gate and regression suite; #419, required by C01–C03. | Both privacy predicates survive either historical order; 103 migrations; healthy compatible fleet; original operational gates restored. | Complete via #419. Local/CI migration, retry, projection and restart checks passed; production schema, privacy counts, revision and restored flags verified. |

The initial audit confirmed C01–C03; C04 was added as a necessary release dependency. This is an evidence-based initial
scope, not a claim of exhaustive security certification. Necessary release and
test dependencies belong to these tasks; unrelated new features do not.

## Hypotheses and deferred work recorded at the initial audit

- Reviewed all open PR metadata/dependencies and the two open issues (#128,
  #130), current main workflows, source TODOs, deployment configuration, prior
  requirement/audit documents, invitation/logistics paths and commerce workers.
  Open PR status alone is not missing-work evidence: main already contains the
  social session and DM-write fixes through #400 despite their open stack PRs.
- The payment expansion stack (#331 through #414) includes unqualified providers,
  refunds and accounting changes. Its disabled capabilities and sandbox/merchant
  gates are not evidence of a current revenue outage. Do not activate them as
  part of this security patch. The remaining worker in #393 belongs to that stack.
- Social activation/read-boundary work remains deliberately gated; existing
  requirements prohibit enabling the new model with unsafe legacy readers.
- Artist self-service #406 has passing checks and an approval but an unresolved
  Google-claim preservation review. It is not required to repair C01–C03.
- DDEX placeholders and extensive uncommitted music work are not proof of a
  live essential-service failure. Preserve the existing distribution gates and
  other contributors' work. Service-package deliverable serialization TODOs and
  onboarding experiments are lower priority without stronger impact evidence.
- Main Instagram lifecycle check failed while messaging, image, catalog,
  synthetic and installation checks passed. No widespread core outage is
  established; credential lifecycle follow-up remains separate.

## Delivery record

- #336 head `00f796e74da9e9ec9417ead93980b8a8e71d5fcc`: all 19 reported checks
  passed, including backend, UI/mobile, PostgreSQL foundation, formal, contracts,
  migrations and persona browser. Current approval verified; zero unresolved
  review threads. [CI](https://github.com/diegueins680/tdf-app/actions/runs/35144444939),
  [formal/PostgreSQL](https://github.com/diegueins680/tdf-app/actions/runs/35144444855).
- Ordinary GraphQL merge refused the native stack; the documented async API
  accepted the exact head with default protection handling and returned merged
  revision `d6244296925e20ccb6b2261290204415201843f3`.
  [Merged PR](https://github.com/diegueins680/tdf-app/pull/336).
- Post-merge [image and validation pipeline](https://github.com/diegueins680/tdf-app/actions/runs/35152191100)
  completed successfully. The later combined release and its final checks are
  recorded below; skipped scope-specific jobs are not counted as executed tests.

## Worker patch provenance and recovery

The narrow patch reuses #393 (`ed064a7549f93bdd9239c5781706400c6fad5aa7`),
original author `continuous-improvement-loop[bot]`. Only the diagnostic boundary
of the two workers already present on main and their applicable tests are
ported. Provider routing, payment state, SQL, API contracts, clocks, leases and
retry policy stay intact. #393 remains responsible for its future reconciliation
worker; integrate this patch normally when that stack reaches main.

No migration or feature flag is introduced. Prefer rolling forward if a defect
is found: reverting the diagnostics reinstates the disclosure/liveness risk.
Do not inject provider failures or real financial mutations in production just
to exercise logging; isolated synthetic exceptions provide that evidence.

Local verification of the narrow patch: Stack production/test build succeeded;
full backend **2,564 examples, zero failures**; focused logging **17 examples,
zero failures**, including 200 generated exception-text inputs. Repository
quality (including formal, release and CI-policy tests) passed with new source
staged so the tracked-file formal scan includes it. Catalog audit completed with
the existing decision policy. The first targeted Stack invocation compiled but
failed because of an incorrectly quoted test selector; it was not a test pass.
The correctly invoked focused binary and subsequent full Stack command passed.

The initial read-only production preflight for the merge reached database checks and rejected
the enabled discovery flags and not-yet-published image. No migration or runtime
configuration was changed at that stage. Machine proxy probes timed out and used the release
tool's public fallback; public health/version independently passed. Plan mode
passed. The subsequent guarded maintenance window preserved the original
discovery settings, as recorded in the final acceptance.

## Release dependency discovered on 2026-09-17 (C04)

C04 is P1 and required for C01–C03: the production ledger had already applied
`2026-09-09_music_directory_suppressed_event_privacy`, but had not applied
`2026-09-07_directory_event_visibility_and_favorite_evidence`. Applying the
missing older migration overwrote the tombstone predicate; replaying the later
migration instead drops metadata privacy. A fresh database alone cannot detect
this update-order interaction. The guarded release rejected the resulting view
before changing either API image. All 102 migrations had committed; the release
lease was released. A read-only count found zero currently exposed suppressed
events, which is not proof that the weakened boundary is safe.

The maintenance restoration restarted the old image and revealed another
compatibility dependency: the new RSVP migration enables four `rsvp` lifecycle
capabilities which the old binary rejects. Both machines temporarily failed to
serve the initialized application. Recovery reapplied the previously deployed
privacy migration after checking its exact ledger checksum, then disabled only
those four newly added capabilities. Original images, discovery flags and
PayPal intake were preserved. `/health` subsequently returned database/status OK.

The forward migration composes both predicates, keeps existing migration bytes
immutable, and preserves source rows and stale cached documents. The release
schema gate now requires both boundaries. The automatic production-schema test
reproduces each overwritten view, requires rejection, applies the repair twice,
and checks public events, private metadata, suppressed imports, dependent venue
and cached search projections with rollback-only synthetic data. Local PostgreSQL
passed all 103 migrations, both repair orders and backend boot/restart idempotence.

Deployment acceptance: reviewed merged revision; 103 ledger entries; both privacy
predicates present; no ineligible public rows; both machines healthy on the exact
new digest; invitation/logistics read-only smoke. Keep the four RSVP capabilities
disabled while any old binary can be restarted. Restore their original enabled
state only after both machines run the compatible revision, and verify it. A
rollback to `954e995` must disable these four capabilities first; never delete
migration ledger rows, weaken schema checks or restore a less private view.

Recovery also restored the old `ck_commerce_refund_provider` definition: the
previous image compares its exact SQL definition during startup and rejects the
new provider values even while those providers are disabled. The transaction
validated the narrower constraint against all existing rows; no refund records
were changed. Both replicas subsequently returned health OK, with the old
revision confirmed. Before deploying the new binary, pause discovery while the
old-compatible schema is still present, then restore the already-reviewed
canonical constraint (including `placetopay` and `payphone`) for the new schema
gate. If a rollout fails, restore the old constraint before restarting the old
image. This is a schema compatibility step, not provider activation. PayPal
intake stays enabled; refund and Datafast gates stay disabled.

## Final production acceptance — 2026-09-17

- [#418](https://github.com/diegueins680/tdf-app/pull/418) merged as
  `6f77a7eec93d96de36840a13235adff9ad13c917`; [exact-head CI](https://github.com/diegueins680/tdf-app/actions/runs/35153190429) passed.
- [#419](https://github.com/diegueins680/tdf-app/pull/419) merged as
  `9f0da14de84919f8fba391fa752b7df7412a600d` after current approval, passing
  [CI](https://github.com/diegueins680/tdf-app/actions/runs/35235466407) and resolved
  review threads. A merge commit preserved the migration introduction ancestry.
- The [merged-revision checks and image publication](https://github.com/diegueins680/tdf-app/actions/runs/35238806594)
  passed. `scripts/production-release.mjs` deployed the immutable image to Fly
  `tdf-hq`, first Chicago and then Los Angeles. Both machines reported the exact
  revision and database/status OK after the original flags were restored.
  Maintenance finished at 15:31:33Z. The successful attempt required no rollback.
- Verified amd64 digest:
  `sha256:fd7a015858ff34df984a718097afddae22500c21282bfa4434aafeeff39a594d`.
  Confirmed backup: `vs_LRqNAqabQP5UgA9x9pBJGO`, created at 15:09:32Z.
  The full production schema gate passed with 103 ledger entries, zero release
  leases and zero private/suppressed events in the public projection.
- Discovery/auto-publication and all four RSVP capabilities were restored.
  PayPal webhook intake remained enabled; refund and Datafast gates remained
  disabled. The required worker encryption secret was present. A 100-line log
  sample contained no worker tick-failure or disabled-worker messages; the
  provider inbox was empty. This bounded sample is not a claim of exhaustive
  error-free operation or a substitute for sandbox payment tests.
- A legitimate Admin browser session received HTTP 200 and 78 invitations for event 86;
  the same request without authentication received HTTP 401. Logistics returned HTTP 200
  with the expected event and activity fields. No invitation or payment write
  was used for these production checks.
- At 20:43:56Z, a newly created synthetic account authenticated through the
  normal signup/login endpoints with only the Customer role. In its isolated
  browser context, event 86 returned HTTP 200 and the account was not its organizer;
  invitations returned HTTP 200 with an empty list; `/admin/users` returned HTTP 403.
  The frontend subsequently confirmed this Customer session. The original
  Admin session remained intact. No password, token or private invitation
  message was published. Recipient mutation and concurrency guarantees remain
  backed by isolated PostgreSQL regressions, not claimed production mutations.
- [Vercel Production](https://tdf-app-tdf-hq-g1tc8ji1r-diego-saas-projects.vercel.app)
  and [Cloudflare](https://dash.cloudflare.com/?to=/c07256e78d05ad9a508d0aee82ac577a/pages/view/tdf-app/1d54b89e-9e6d-456d-9da1-1f62c4f4d2bb)
  confirmed web deployments of the same revision. Subsequent Instagram-only
  commit #420 did not change the backend deployed by this delivery.

No critical-scope blocker remains. The temporary outage and rollback constraints
above remain part of the delivery record. These observations describe the
recorded acceptance window; future releases require their own verification.
