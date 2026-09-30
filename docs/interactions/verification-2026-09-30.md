# Interaction release evidence — 30 September 2026

The feature is not merged, deployed or activated. This document supersedes
earlier release-status summaries, without reassigning historical test results to
new artifacts.

## Qualified application revision

- Root application/schema revision: `0a5653dc349f9faaf237a521aa50016a2fb1d6d9`.
  [CI 36525453887](https://github.com/diegueins680/tdf-app/actions/runs/36525453887)
  passed, including actual backend HTTP regressions, web journeys, migrations,
  contracts and property checks. The default optional Android job was skipped;
  installed Android has separate evidence below. All 37 known review threads are
  resolved. PR 470 received independent approval on this exact commit on
  30 September; GitHub reports APPROVED/CLEAN.
- [Image build 36525461076](https://github.com/diegueins680/tdf-app/actions/runs/36525461076)
  passed. Registry inspection matched the CI index digest
  `sha256:3e7f5396d9ffa91347578f614ff8356a47bb47e77d721470efff07211fd851d1`.
  This is a pre-merge candidate, not a deployed release.
- The manifest contains 155 migrations. A fresh PostgreSQL 17 database passed
  all seven SQL suites and nine concurrency properties. The encrypted production
  restore accepted the manifest and retains all 30 notifications while paused.
  No live schema was changed. A fresh backup is required at actual cutover.
- The 10,000-comment/2,000-reactor rehearsal measured 18.582 ms for summary,
  6.782 ms for roots, 4.865 ms for replies and 4.824 ms for exact context.
  Synthetic changes were rolled back; these measurements are not production SLOs.

## Native artifact boundary

Root pins `3ee82fe403b358b405568ed5164cf7798eb45e0b`. Native PR 124 merged as
`12a472ecb68e9a9c0bcba81baf0fafa551d59253`. Both have tree
`474467f45df50a4cfd6690d8d84fecee8903ef13`, also matching the simulator artifact's
source `fd6cdcd22915384ab8dbbf9c5a128b40419505fc`.

- [Installed Android 36520750030](https://github.com/diegueins680/tdf-app/actions/runs/36520750030)
  passed reaction, comment, reply notification/deep link, edit, parent deletion
  and retained-reply assertions. The first emulator attempt was obstructed by a
  Pixel Launcher ANR dialog; the fresh-emulator retry passed.
- Native unit/rendered qualification passed 553 tests, type checking and lint.
  This does not substitute for installed-device or screen-reader tests.
- The latest iOS artifact has verified source provenance and strict signature,
  but its installed journey is **not qualified**. Repeated local runs exhausted
  disk space during app reset/reinstallation and diagnostic creation. Earlier
  successful iOS journeys apply only to their earlier artifacts. On 30 September,
  starting the dedicated simulator reduced available space to about 1 GB; it was
  stopped before another test attempt. Free capacity subsequently fell below
  500 MB again, and Chrome displayed its own low-storage warning.
- Signed Android 22 and iOS 29 passed independent receipt/hash, signature,
  production API, runtime and associated-domain configuration checks. Android 22
  remains a saved, held, unpublished Play draft; Alpha 17 remains active. iOS 29
  has not been uploaded. App Store Connect sign-in was verified on 30 September;
  its TestFlight list shows build 25 as the newest listed build. Candidate 29
  is absent. The collapsed upload-history section has not been inspected.
- Physical VoiceOver/TalkBack and installed HTTPS association behavior remain
  unverified. Static signing/configuration checks do not establish those results.

## Deployment gates

Production health was checked again on 30 September and reports database/status
OK. The last inspected live schema remains the 115-entry baseline. Trader and its
shared Fly database remain untouched. Neither Cloudflare production automation
nor association variables have been changed for this release.

Follow the [rollout runbook](rollout.md): preserve the approval, pause only automatic
Cloudflare production deployments, protected merge commit preserving migration
ancestry, exact merged image, fresh encrypted backup and rehearsal, schema/backend
deployment, client and association qualification, then guarded activation and live
canaries. Do not bypass review or publish held native builds before their gates.
