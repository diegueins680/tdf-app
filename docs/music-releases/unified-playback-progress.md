# Frontend reconciliation in progress

Draft companion [PR716](https://github.com/diegueins680/tdf-app/pull/716) reuses
[PR670](https://github.com/diegueins680/tdf-app/pull/670), retaining backend source
883e12ee3 from [PR712](https://github.com/diegueins680/tdf-app/pull/712) and the
native session controls from [PR706](https://github.com/diegueins680/tdf-app/pull/706).
Native video/audio now share explicit pause, current-source event fences, bounded
recovery and artist navigation with ordinary tracks. Delayed internal pause,
native pause/resume intent, resumed metadata timeout, stale playing timeout and
same-session video-to-audio fallback counterexamples were reproduced before the
fixes. Audio mode never falls back to downloading video. Ready native artist
recordings now participate in the profile queue with exact Party filtering.

The latest held composition passed type checking, two transport/artist suites
with132 tests and three artist suites with41 tests. The track-wide authorization repair then passed three core suites/136 tests, including four controls that failed when format filtering downgraded protected identity. The earlier restored source
64eeb8e64 passed368 suites/4208 tests; that whole-suite result predates native
composition. The native composition subsequently completed375 suites/4311 tests and45 native browser cases across Chromium desktop/phone and Firefox. Those runs predate the track-wide identity repair; the corrected source subsequently passed376 suites/4315 tests and all45 browser cases. Hosted checks and independent review remain pending. Local
browser attempts stopped before test execution on sandbox loopback permission
and then missing jq PATH; both are retained as harness failures.

Backend a5745dbbe passed canonical Stack4308 examples (zero failures,23 pending),
actual HTTP publication/permissions/health/featured withdrawal/restoration and
stable diagnostic pagination, complete251-migration apply/replay/startup,
repository quality and compiled API inspection933 operations/snapshot match.
Hosted run42432110542 passed35 checks, with3 expected skips; the untraced Linux
worker passed23 scenarios. Genuine approval and normal MERGE auto-merge exist,
but protected main has advanced and ordinary FIFO/conflict reconciliation is
still required. Backend implementation/testing/approval is not merge/deployment.

Native Records health/grant identity/provenance correspondence, full catalog
reconciliation, real provider/storage acceptance and physical devices remain
open. Featured editing only offers supported Music/legacy identities. Production
is unchanged: guarded runtime receipt and release/recovery controls remain
unqualified. No rollout flag is activated or deployment protection bypassed.

## Preserved backend-stage checkpoint

# Backend-first music player delivery

This stage supplies server prerequisites for the universal player in [PR #670](https://github.com/diegueins680/tdf-app/pull/670). Against protected main `d0b10b60d93cb379ac57b204e1d4137cd22dc5ba`, frontend runtime is unchanged; only generated web declarations and the existing long campaign browser journey budget change. Its action/assertion bounds are retained; the full edit/retry/reload/import/review flow has a 60-second total budget after the failed WebKit trace reached its final visible button at the 30-second default deadline. Mobile [PR #182](https://github.com/diegueins680/TDF-mobile/pull/182) merged through normal protection; its exact `20daa1b` source pin preserves the merged decoder repair. No feature flags or production configuration are activated.

The backend adds featured preference persistence, current-source legacy authorization, persisted source health, bounded safe worker checks, public suppression/restoration, and paginated owner/team/admin diagnostics and revalidation. The 251-entry migration manifest preserves all 246 protected-main entries and historical SQL bytes and introduction ancestors. Invalid legacy featured sources become unavailable without deleting the saved preference; successful validation restores eligibility.

Before the latest main reconciliation, the backend tree matched full-player source `0dad14c4300bb618908e491c3e15a921389f1e84`. That source passed the canonical Stack build, 4,275 backend examples (zero failures, 23 pending), real HTTP permission/health/pagination/restoration workflows, the full 250-migration rehearsal and 13 safe-probe tests. These historical results retain their execution scope. The latest main merge preserves the Drive OAuth bootstrap repair, Dataweb payment changes and native resolver lock; the combined candidate needs its own qualification. Full-player UI and browser results do not qualify this branch's frontend. Mobile #182 separately passed 685 tests across 99 suites, release checks and offline Metro startup locally, then hosted validation and independent review before protected merge.

Production still reported API `4dac175d` and disabled music-release flags. A read-only diagnostic found the API configured image lacks a registry digest and its Compose-file label differs from the canonical contract. The canonical collector therefore provides no valid receipt. The guarded routine release executor remains unfinished in `ops/hetzner/README.md`. Deployment requires valid runtime identity, coordinated database/assets/uploads backup and writer fencing, compatible recovery, migration rehearsal and authenticated canary/storage qualification. Never restore an old database backup over newer user data.

After this backend stage passes current-head checks and independent review, merge through ordinary admission and qualify its guarded deployment. Hold #670 until its required endpoints and migration policy are deployed; then reconcile its preserved frontend implementation, obtain fresh checks/review, merge and verify production playback. No protection or deployment precondition is waived. Full legacy/canonical/resource provenance reconciliation, real provider credentials, physical devices and real storage acceptance remain outstanding.

The current stage also reuses the original forward archived-Party reference guard and rollback from PR #670, with its introduction ancestry intact (migration 251). Bounded optional worker shutdown diagnostics retain failure results; they are diagnostic instrumentation, not a claimed hang fix. Its preceding Linux baseline failed the unchanged 30-second deadline; current candidate worker qualification is required. Public featured reads now use the same artist visibility policy as legacy grants; an actual HTTP baseline returned a selected legacy ID after profile withdrawal and must pass the new denial/owner-retention/restoration scenario before delivery.

Current candidate qualification supersedes the earlier execution scope where explicitly stated: source `e13eb06b9` passed canonical Stack with 4,301 examples (zero failures, 23 pending), actual HTTP profile withdrawal/owner retention/restoration and publication/territory/health/pagination/entitlement/DDEX workflows, the complete 251-migration rehearsal, actual PostgreSQL identity and source-health checks, 23 local worker scenarios, 42 diagnostic controls, 13 probes, repository quality, generated clients, type checking and build. Its hosted run 42400968363 passed 32 checks but failed the untraced Linux worker and campaign WebKit journey; aggregate quality therefore failed. The new persistent heartbeat stop marker and additional controlled race test have 43 passing local controls; fresh untraced Linux and browser results remain required. Optional diagnostic success never overrides the original failed baseline. Colima cannot start locally because qemu-img is missing, so real Docker/S3 coverage remains unavailable.

The retained native-playback artifact from hosted run 42400968363 has 74 expected results, zero unexpected/flaky/skipped: 14 each for Chromium desktop/phone/tablet, Firefox critical and WebKit critical, plus two each for Android-small and WebKit iPhone. This exercises the stage's main-equivalent frontend, not PR #670's dependent artist-recovery UI. The adjusted campaign flow separately passed all eight Chromium desktop/phone cases locally. Physical devices and real providers remain unverified.

Protected main advanced again through session-window, finance CSV and torrent-worker repairs. This stage normally incorporates `d0b10b60d`; the authentication change means historical whole-backend executions are not current-candidate qualification. Music-specific HTTP/SQL/backend inputs retain their earlier evidence scope, while fresh combined-head hosted backend qualification is required. The persistent-stop worker change passed all 23 untraced local scenarios.


## Configured storage health boundary (2026-11-08)

Untrusted legacy locations remain limited to public HTTPS on port 443, with DNS
validation repeated after redirects and the checked TCP address pinned. Signed
asset probes use only the literal operator `MUSIC_S3_ENDPOINT` HTTPS authority,
its configured port/base path, and `MUSIC_S3_DERIVATIVE_BUCKET`. The health source
view selects stream/preview derivatives; the probe cannot borrow credentials for
another origin or bucket, escape the namespace through decoded traversal, or
follow a signed redirect. Private DNS/loopback are trusted only through that
exact operator configuration. The original hostname and port are retained for
Host/SNI while the resolved TCP peer is pinned; default certificate and hostname
verification remain enabled. An operator must supply valid TLS trust through
normal system/Python configuration; no verification bypass is introduced.

The final fake-connection/resolver harness rejects the original implementation
for a configured private endpoint on port 9000 and passes the bounded repair.
All 22 checks include the 13 original legacy/response/native-sample controls.
They do not demonstrate a real private object-store TLS handshake, deployed
worker identity, provider acceptance or complete legacy provenance. Existing
byte/deadline caps, source-version leases, publication/territory gates and
production activation requirements remain separate.

The concurrent companion update ba709f94f is preserved by normal merge, including backend883e12ee3 configured-storage repair and current-main9df3922fc. The web runtime is byte-identical to completed ef68fbf58 UI/browser inputs; all22 configured-storage probe controls pass locally. Real storage/TLS and current whole-source hosted acceptance are separate.
