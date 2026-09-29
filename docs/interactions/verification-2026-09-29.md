# Interaction verification — 29 September 2026

Implementation is verified in disposable environments. Production remains on the
previous release; no interaction schema migration or activation has been applied
there. This record does not establish store signing, HTTPS app-link association,
physical-device VoiceOver/TalkBack behavior or a completed production rollout.

## Current review follow-up

The historical evidence below remains tied to its stated commits. The new forward
repair migration and moderator-reason clients are undergoing fresh qualification.
All 138 migrations and all seven SQL property suites pass against the restored
production PostgreSQL 17.8 schema; activation/retry/pause/resume preserve the 30
existing notifications. The clone is paused, with no application workers.
Regression coverage now includes the eight-way notification preference truth table,
new mentions after read, aggregate unread refresh, reaction no-ops, edit after policy
changes, permanent legacy follow revocation, protected report reasons and atomic
refusal of authorless legacy conversion. SQL suites and concurrent commands pass;
seven rendered web tests and eight native tests pass, with type/lint validation.
The HTTP suite additionally checks an event containing 60 moments without truncation.

Android 18 was accepted by Play only as a **held unpublished Alpha draft**, now
superseded. Android 19/iOS 26 signed candidates were cancelled before upload for the
moderation follow-up. Alpha 17 remains active. The old image build passed, but the
forward repairs require a new image. No interaction production migration,
activation, store rollout or Cloudflare configuration change has occurred.

## Exact source and artifacts

- Root tested commit: `daf3011fc` (documentation-only evidence follows it).
- Mobile checkout: `371fc64`; only Maestro YAML differs from application revision
  `7f2e88552eca1d22e203beef1e856a36e1631088`.
- Android artifact: successful [run 36487477689](https://github.com/diegueins680/TDF-mobile/actions/runs/36487477689).
- iOS artifact: successful [run 36487477792](https://github.com/diegueins680/TDF-mobile/actions/runs/36487477792), source merge
  `7fc2678ed0fd9ce160c46aeee5adeb3101e8d27a`. Its tree
  `e5192e0c5acb0cfbabbfbb51682186bbd1bceda7` exactly matches application revision
  `7f2e885`. The downloaded app passed strict deep code-signature verification and
  was installed without rebundling or resigning on a dedicated iOS 18.3 simulator.
- Android runs on a disposable Android 35 emulator. Both artifacts use isolated
  test API configuration and test signing; neither is a store release.

## Executed journeys

[Full CI run 36490145023](https://github.com/diegueins680/tdf-app/actions/runs/36490145023)
is green, including the installed Android job and all backend, migration, API,
repository, mobile, web and persona-browser prerequisites. Android evidence is
retained as synthetic screenshots in that run; no fixture credentials are uploaded.
The identical runner completed locally against the unmodified final iOS artifact.

On both platforms the runner drives login, reaction selection, comment creation,
a cold notification open, automatic exact-reply focus, comment editing, parent
body deletion, retained replies, thread collapse and expansion. It then checks the
real API for the parent's tombstone, erased body, surviving reply body and valid
parent relationship. The reply and notification are created by a separate fixture
account through the canonical API and existing notification dispatcher.

Earlier device failures produced concrete fixes: native focus now confirms real
viewport coordinates before announcing, Android uses the existing safe-area
provider, and automation selects the full Android input before replacing text.
The rendered regression rejects a stale offscreen view token and requires a
corrective scroll before announcement. A fresh keyboard tutorial is handled by
the fixture without changing app behavior.

## Other verification

- Full Stack build; 3,539 examples, zero failures, six pre-existing pending cases.
- Native: 86 suites / 523 tests; type, lint and release validation pass. Seven
  rendered interaction regressions and ten release/signing checks are included.
- SQL/model/property tests cover policy, blocking, privacy, parent/root integrity,
  tombstones, moderation, notifications, idempotency and counters; concurrency
  runs cover 80 reaction writes, duplicate creates and reply/delete races.
- Ten real-API web journeys pass in desktop/phone/tablet Chromium, Firefox and
  WebKit. Browser axe reports zero serious/critical findings; rendered tests cover
  bounded pagination, focus behavior and optimistic rollback.
- A fresh PostgreSQL 17.8 production restore passed all 137 reviewed migrations,
  first/repeated activation and pause/resume. All 30 existing notifications were
  retained; exact legacy source/migration counts matched. Live data was unchanged.
- Synthetic 10,000-comment / 2,000-reactor query-plan inspection measured local
  summary/root/reply/context times of 154/44/21/38 ms; these are not production SLOs.

## Remaining release gates

Independent protected review, merge, immutable backend build, fresh encrypted
backup, guarded live migration, signed native releases, public association
configuration and production canary verification remain required. GitHub's stored
iOS profile passed the Associated Domains preflight in run 36493156554; the older
local profile lacks that capability and is not used. App Store Connect login is
pending for build-history/submission verification. Play Console's latest uploaded Android version is 17; version
18 is available as of this check. The last GitHub iOS release used build 25, but
App Store Connect history must be checked before selecting its successor.

Use [the rollout runbook](rollout.md), preserving Trader and its shared Fly database.

## Release-runtime follow-up

Mobile PRs [116](https://github.com/diegueins680/TDF-mobile/pull/116) and
[117](https://github.com/diegueins680/TDF-mobile/pull/117) merged through the normal
workflow after validation. PR 117 separates OTA runtime `1.0.1-interactions.1`
from old binaries lacking Expo Crypto; it changes compatibility metadata and
release qualification, not interaction behavior. The runtime correction is merged commit `5eb05bc`; the root additionally pins
merged verifier follow-up `85bac6e`. Mobile main is now
`6df4bebbaf139fe8996e40e2cb866083405fedd3`.
Both Python guard suites run in Mobile Validate, including negative native/config
runtime mismatch and mixed Android resource-value cases. Ten tests, resolved
production Expo config, type checking, lint and hosted Mobile Validate pass.
Signed candidates from the earlier main were cancelled before distribution.
Their replacements are Android [36493153223](https://github.com/diegueins680/TDF-mobile/actions/runs/36493153223)
(version code 18) and iOS [36493156554](https://github.com/diegueins680/TDF-mobile/actions/runs/36493156554)
(candidate build 26). Android qualification passed; its retained AAB has SHA256
`1d0534d23fc036ec23f16700445204bda67a82169ffea6298a7c110e90954748`.
The download hash, signature, compiled runtime, API and four association routes
were independently verified. No store upload has occurred.
The iOS archive/export succeeded but entitlement parsing failed because codesign
returned human-readable output. Mobile PR [118](https://github.com/diegueins680/TDF-mobile/pull/118)
merged the explicit XML fix after hosted validation. Twelve Python checks pass
locally, including an actual macOS ad-hoc signing/entitlement round trip.
Replacement signed iOS run [36495672565](https://github.com/diegueins680/TDF-mobile/actions/runs/36495672565)
is pending qualification; the failed artifact was never distributed. No OTA update
or channel mapping was changed. See the mobile repository's
`docs/interaction-release-runtime.md` for the compatibility contract.

The first image-preparation run (36494858380) failed GitHub workflow validation:
the reusable native artifact job requested `actions: read` without that grant
from its caller. The image workflow now grants only `contents: read` and
`actions: read` to its required-test job. Sixteen pipeline regressions pass,
including this caller/callee permission contract. This was caught before merge
or deployment; a successful replacement image build remains required.

## Malformed response regression

Image run 36495066879 exposed a real publication crash: its RSVP browser trace
received an HTML fallback as an interaction summary, then reaction rendering
attempted `reduce` on an absent array. The shared web/native API now rejects
malformed summaries and invalid counts before query caching. Eight API cases per
client cover invalid payloads and recovery; 14 focused web and 15 focused native
tests pass. The full web signup/RSVP/profile/share/withdrawal journey now injects
an HTML discussion response and passes with zero page errors (two browser cases).
Mobile PR 119 carries the generated counterpart; root pins `1ae6e43`.

Android 18 was accepted by Play and saved only as an unpublished Alpha draft
(release 6), explicitly marked HOLD. Alpha 17 remains active. Replace this draft
with a newly qualified build containing the response guard before rollout;
version 18 is now consumed. iOS run 36495672565 first failed a GitHub dependency
clone with connection reset; its retry was cancelled before signing so the next
candidate can include the response fix. No iOS upload or tester rollout occurred.
The image retry was also cancelled after identifying the real response defect;
a fresh image build is required from corrected source.

## Encrypted backup and dependency audit

A fresh live dump captured at 2026-09-28T23:19:19Z was encrypted directly into an
off-host archive using checksum-verified age 1.3.2 and the existing dedicated
SSH public recipient. Decryption was verified against the plaintext digest and
streamed into a new isolated PostgreSQL 17 database, with no application workers.
Restore completed at 23:20:09Z with 115 migration entries, 30 notifications and
no interaction installation. Ciphertext SHA256:
`45c8b5002d4921feb3be4a1898fb302d133df2d77f9f13bbfa237422e79dcfb4`.
The protected receipt and decryption identity reference remain outside Git; retain
the identity for recovery. Capture a new backup at actual cutover if writes have
continued. This does not authorize restoring an old snapshot over accepted writes.

CI Safe Install found newly indexed ip-address advisories GHSA-2vr4-cq9g-pvrc
and GHSA-rpw4-54j3-4h4q. The lockfile updates only that transitive package from
10.3.1 to compatible patched 10.7.2; the existing audit policy is unchanged.
Mobile PR119 has merged as ce6f9ecba1675381c65e002a6b5ca3c968413896.
Signed corrected candidates are Android19 run36497188620 and iOS26 run36497191523;
neither has completed qualification or distribution at this checkpoint.

## Publication-authority repair qualification

Imported `social_sync_post` records have no public publication state. The reserved
artist_update target is disabled and its source resolver refuses access even if
its capability flag is accidentally re-enabled. Ten supported publication kinds
remain enabled; imported source content is preserved. This closes a review-found
privacy exposure before any production activation.

The 139-entry bundle passed installation and a second checksum-verified no-op on
the isolated encrypted production restore. The new entity properties passed on
PostgreSQL16 and PostgreSQL17.8, including anonymous/owner denial, capability-toggle
resistance and preservation of the private source caption. The clone retains all
30 notifications and is paused. Live production is unchanged.

Hosted backend job 109187113814 at root3ecbf3a12 passed the full build, unit suite
and every HTTP integration check, including all 60 legacy event moments. The UI
and persona-browser jobs also passed. The only failed gate was the migration
registry's stale catalog-inventory fingerprint, narrowly refreshed and locally
rechecked without changing its technical-constant classification. The later
publication-authority HTTP checks still require qualification on the new head.


## Final artifact qualification and withdrawal follow-up

Root090b5a132 passed all hosted checks, full Android installed E2E run36500706927
and immutable image run36500704280. The unmodified final iOS artifact from1117bca48
(application tree identical to pinned66d6ee809) passed the complete installed
journey locally against the139-migration disposable database; native full runner
completed with authoritative API assertions. Its Mac API binary predates only the
legacy moment batch change independently covered by the final Linux HTTP lane.
Signed iOS26 from native main16278639 passed independent downloaded checksum,
codesign, production API, runtime and associated-domain verification:
SHA256 93c1b145425665145d93456dde65f5318122b7ec7a350ee7e6335a7a96934d86.
No store upload or production deployment occurred.

A subsequent review identified withdrawal after lost club write eligibility.
The third forward repair preserves current source visibility and account guards,
allows only the actor's own removal, and marks new choices nonselectable. Actual
SQL properties cover revoked/restored eligibility, own-slot isolation, repeat
removal and authoritative counters; HTTP and rendered web regressions cover the
same contract. New final CI and140-entry rehearsal must pass before release.
