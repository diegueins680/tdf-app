# Critical completion audit — 2026-09-16

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

The visible browser is unavailable because its profile is already in use. Do not
terminate that browser, impersonate a user, or manufacture an authenticated
production test. The host had a recent ENOSPC error in another release's final
record write; subsequent live checks establish health, not that record's success.

## Initial confirmed critical scope

| ID | Evidence and impact / severity rationale | Components / dependencies | Observable acceptance and validation | Status |
|---|---|---|---|---|
| C01 | Main invitation list returns all event invitations to any viewer; create/update lack organizer/recipient authorization. P0: exposed private invitation messages and unauthorized mutation, confirmed by source and existing regression suite; no claim of observed exploitation. | SocialEventsHandlers, invitation client, existing PR #336. No new production schema needed for these handler checks. | Organizer/admin can manage; recipient sees only own invitations and can only accept/decline; outsider, transfer and message edits denied; race checks retain current authority. Exact-head CI/HTTP/PostgreSQL and post-deploy authorized read-only smoke. | PR #336 verified and merged as `d6244296925e20ccb6b2261290204415201843f3`; build/deployment pending. |
| C02 | Main create/update writes logistics activity separately from dependency/assignment replacement; a rejected relation can leave partial state. P1: inconsistent operational state. | Same handler, transaction and retained-edge helper; reuse #336. Foundation opt-in features stay inactive. | Activity/status/version/relations roll back together on rejection; retained prerequisite identity survives updates. Existing actual-PostgreSQL regression, foundation concurrency/rollback and full backend CI; deployed revision and non-destructive smoke. | Implemented/verified/merged in #336; deployment pending. |
| C03 | Both existing commerce worker loops render arbitrary exceptions after character substitution; diagnostic writes are outside their exception boundary. P1: credential/personal-data disclosure risk and stopped payment/expiry processing on sink failure. No actual secret exposure asserted. | ProviderEventWorker, MerchReservationWorker; reuse the relevant implementation/tests from #393 without its unrelated provider stack. | Output independent of exception text; one tick even when a sink fails; next iteration survives; asynchronous cancellation propagates; fixed JSON/counters retained. Hspec/QuickCheck, full backend, repository/formal gates, production health and aggregate queue checks. | Narrow implementation and regression verification in progress. |

The initial audit confirms these three tasks. This is an evidence-based initial
scope, not a claim of exhaustive security certification. Necessary release and
test dependencies belong to these tasks; unrelated new features do not.

## Investigated hypotheses and deferred work

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
  is being monitored. Pending checks are not counted as passes.

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

Read-only production preflight for the merge reached database checks and rejected
the enabled discovery flags and not-yet-published image. No migration or runtime
configuration was changed. Machine proxy probes timed out and used the release
tool's public fallback; public health/version independently passed. Plan mode
passed. The release must preserve enabled discovery, using its supported guarded
maintenance window or reviewed preservation tooling, before mutation is allowed.

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
