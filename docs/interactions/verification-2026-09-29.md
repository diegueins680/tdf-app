# Interaction verification — 29 September 2026

Implementation is verified in disposable environments. Production remains on the
previous release; no interaction schema migration or activation has been applied
there. This record does not establish store signing, HTTPS app-link association,
physical-device VoiceOver/TalkBack behavior or a completed production rollout.

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
release qualification, not interaction behavior. The root now pins merged commit
`5eb05bc`; mobile main is `16202bf72f04e85f9f2bf624006eac7c033afa55`.
Both Python guard suites run in Mobile Validate, including negative native/config
runtime mismatch and mixed Android resource-value cases. Ten tests, resolved
production Expo config, type checking, lint and hosted Mobile Validate pass.
Signed candidates from the earlier main were cancelled before distribution.
Their replacements are Android [36493153223](https://github.com/diegueins680/TDF-mobile/actions/runs/36493153223)
(version code 18) and iOS [36493156554](https://github.com/diegueins680/TDF-mobile/actions/runs/36493156554)
(candidate build 26); qualification and publication remain pending. No OTA update
or channel mapping was changed. See the mobile repository's
`docs/interaction-release-runtime.md` for the compatibility contract.

The first image-preparation run (36494858380) failed GitHub workflow validation:
the reusable native artifact job requested `actions: read` without that grant
from its caller. The image workflow now grants only `contents: read` and
`actions: read` to its required-test job. Sixteen pipeline regressions pass,
including this caller/callee permission contract. This was caught before merge
or deployment; a successful replacement image build remains required.
