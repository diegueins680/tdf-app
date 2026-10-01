# Interaction verification — 30 September 2026

## Production release (11:27 UTC)

PR [#470](https://github.com/diegueins680/tdf-app/pull/470) received independent
approval on `fd87416e99af3176d107178832f238ef00003be5`, passed required CI and
merged normally as `645f56fcc44f81609fbfd0e03d683b40376ce77a`, preserving both
the reviewed tree and migration introduction ancestry. All post-merge checks
passed, including the [immutable image build](https://github.com/diegueins680/tdf-app/actions/runs/36703425146).

- Hetzner runs that exact revision, amd64 image
  `sha256:38e6264b82db2d81a5b51c3a78740b6a305538b4cdae8d53ced067ccbb1e8fe0`.
  Source, architecture and embedded migration bundle were independently checked.
  `/health` reports database/status OK; the API container has no restart or OOM.
- The reviewed 159-entry migration ledger is applied. Canonical interactions are
  enabled and activated; the release lease is released. Legacy engagement source
  and migrated counts match (all zero). All 77 preexisting notifications survived.
- A fresh final backup was taken with old writers stopped, encrypted off-host,
  and restored into an isolated PostgreSQL 17 database before live migration.
  Backup SHA-256: `282d21898cccb5261cefe5202dacfb586cbc178cbd761741a47eef1d9f7d5436`.
  Exact migration application twice, first/repeated activation and pause/resume
  passed. Restore databases remain paused without application workers.
- Cloudflare production deployment `2e2a984b-f3c9-4391-9cc9-95fa20daac8d`
  serves the same revision. Automatic production builds were restored; the
  temporary release hook was deleted. Existing source/build/env/binding settings
  were preserved. Trader and its shared Fly database were untouched.
- Both verified hosts serve the expected Apple and Android association documents
  as HTTP 200 JSON without redirects, including actual Apple association client
  user agents. This verifies hosted documents, not OS handoff on a physical device.

After activation, recovery means pausing this compatible interaction backend and
fixing forward. Do not downgrade to the old backend, reset activation, reopen
legacy writers, or overwrite new interactions with a stale database restore.

## Final verification and mobile distribution

The [final installed iOS run](https://github.com/diegueins680/tdf-app/actions/runs/36700761570)
passed real HTTP checks and the installed native journey against schema 159.
Its application revision has identical backend/SQL/web/native code to the release;
only catalog-audit metadata differs. The original simulator artifact was verified
and installed unchanged. The matching [installed Android journey](https://github.com/diegueins680/TDF-mobile/actions/runs/36520750030)
also passed. The native source is `3ee82fe403b358b405568ed5164cf7798eb45e0b`,
whose tree matches the signed release merge `12a472ecb68e9a9c0bcba81baf0fafa551d59253`.

All nine SQL property suites and ten concurrency groups passed on PostgreSQL 17,
including publication withdrawal, pause admission, blocking, duplicate requests,
counts, thread integrity and privacy. Nonempty legacy/report migration fixtures
cover data preservation despite empty production legacy engagement. The restored
production benchmark with 10,000 comments and 2,000 reactions measured 18.415 ms
for summary, 6.838 ms for roots, 5.104 ms for replies and 4.655 ms for comment
context; synthetic rows were rolled back. These are measured fixture timings,
not a production latency guarantee.

Production qualification used owned temporary accounts and real HTTP requests:
reaction selection/change/removal/counts, idempotence, comments/replies/mentions,
edit authorization, notification dispatch and exact destinations, owner hide and
restore, deletion tombstones, blocking and unblock. Actual desktop and phone
browser flows passed authoring/edit/delete, reaction reconciliation, exact focus,
disclosure and axe with no serious/critical violations. Fixture bodies were
deleted, its source hidden, and credentials/tokens/roles revoked afterward.

A further active-focus check found a web defect: deleting a leaf comment left
focus on the document body. The small follow-up returns focus to a retained
comment or its own discussion toggle after dialog cleanup, and restores the menu
on cancellation. The regression fails before repair. All 19 focused rendered tests, typecheck,
scoped lint, bundle build and catalog audit pass. Ten real-API browser journeys
pass across Chromium desktop/phone/tablet, Firefox and WebKit, including keyboard
cancellation, leaf deletion, parent tombstone focus and retained replies. An
initial timer-only repair failed WebKit; retained cards now restore focus from
the dialog exit callback. These browser tests use an isolated API/database; the
web fix still needs post-deployment verification. This follow-up does not
change the backend, schema, native source or API contract and requires its own
protected review and web deployment.

As of 11:35 UTC, iOS build 29 is Apple `VALID` and `IN_BETA_TESTING` internally.
Specific tester-group membership cannot be verified with the available submission
key. Android build 22 has been submitted to the existing Alpha closed-test track
and is **in review**, not yet verified live. Neither is claimed as a public-store
release. Physical-device VoiceOver/TalkBack and installed signed HTTPS handoff
remain unverified; simulator/rendered/browser checks do not establish those results.

## Earlier checkpoints (historical, superseded by release evidence above)

The current mobile source `3ee82fe403b358b405568ed5164cf7798eb45e0b`
(tree `474467f45df50a4cfd6690d8d84fecee8903ef13`) passed the complete installed
iOS journey on a dedicated iOS 18.3 simulator. The original artifact from
[build 36519753119](https://github.com/diegueins680/TDF-mobile/actions/runs/36519753119)
passed strict signature and source-tree verification and was installed unchanged.
The runner completed login, reaction, comment creation, cold notification opening,
exact reply focus, editing, parent deletion, retained replies and collapse/expand;
final API assertions confirmed the tombstone and parent relationship.

This local journey used the latest 155 interaction migration schema and the
existing local backend executable. It does not qualify a newly integrated backend
binary. Exact application revision `0a5653dc349f9faaf237a521aa50016a2fb1d6d9`
already passed its hosted Linux HTTP/API suite. [Hosted macOS run 36653896221](https://github.com/diegueins680/tdf-app/actions/runs/36653896221) also passed the complete real HTTP suite and installed iOS journey against that exact backend. The current schema-157 application revision is being qualified by [run 36657712401](https://github.com/diegueins680/tdf-app/actions/runs/36657712401). Android's matching installed
journey passed [run 36520750030](https://github.com/diegueins680/tdf-app/actions/runs/36520750030).

Main advanced with event-ingestion PR #469 after the independent approval of
interaction PR #470. Integration preserves all 116 main registry entries before
the 40 interaction/compatibility migrations, every SQL byte and introduction
commit, and both event and interaction CI/release gates. The resulting 156-entry
manifest requires fresh merged-source checks and protected review.

Production remains on the previous backend. This record does not establish store
publication, physical-device VoiceOver/TalkBack behavior, installed signed HTTPS
association, or completed production activation. Android 22 remains an unpublished
store draft. Apple validation and direct upload of unchanged signed iOS 29 succeeded
on 30 September (delivery `0b1fda64-c760-4b91-9f32-de889b520092`). The queued
Expo submission was confirmed cancelled before direct upload, avoiding duplicate
delivery. Apple API now reports build 29 as `VALID`; tester distribution and store publication remain pending.

A subsequent report-abuse review is fixed by the additive
`2026-09-30_interaction_report_content_version` migration (manifest 157).
The unchanged-content regression failed before the repair and passes afterward.
Focused real HTTP checks confirm that fresh request keys do not reopen unchanged
resolved reports, while actual edits allow one audited reopen. SQL also covers
no-op edits, owner hide/restore, and content changed while awaiting review.
Existing report evidence is preserved. All seven SQL property suites and nine concurrency checks pass on fresh schema 157. A nonempty historical-report migration test proves baseline preservation and safe reapplication after a later edit; 82 release tests also pass. Fresh hosted CI remains required.

The schema-157 revision `f6c00278b` passed full required CI
[36657877447](https://github.com/diegueins680/tdf-app/actions/runs/36657877447).
A later review caught loss of existing club reactions during pre-activation
staging. The repair preserves the existing reaction bar only for an explicit
server pre-activation response, never for ordinary authorization or availability
errors. Rendered tests cover both club publication kinds, existing counts, failed
writes/retry, transition to canonical controls and no fallback on 401/403/404/500.
Actual HTTP assertions cover the marker before activation and its absence during
a converted pause. This backend/web repair requires fresh hosted verification;
the native source and signed artifacts are unchanged.

The fresh encrypted off-host backup from 01:40 UTC restored successfully to an
isolated PostgreSQL 17 database. Schema 156, first/repeated activation, pause and
resume passed; all 77 current notifications remained, and exact legacy
source/migrated engagement counts agreed. No rehearsal workers were started.
The restored copy subsequently accepted schema 157 and remains paused with all 77 notifications intact.

Cloudflare production settings were saved privately before pausing automatic
production deployments. Preview builds, existing variables, build settings and
branch settings were verified unchanged. The documented Apple team ID and both
Android signing certificates are configured for the next deployment. The current
web deployment continues serving; association responses must be verified after
the new web deployment. Restore the saved production-build setting at rollout.

A later publication-boundary regression failed on schema 157: removing a public
record's collection membership still exposed its discussion. Additive migration
158 now requires matching active/published collection membership for recordings,
sessions and releases, including moderator access. Regression properties cover
inactive, draft, mismatched and detached collections, anonymous/authenticated
reads, target registration, deep links, commands and engagement preservation.
All eight property suites and ten concurrency groups passed on fresh PostgreSQL 17, including six publication-withdrawal interleavings across the three record kinds. These exercise both membership deletion and
collection withdrawal while a comment transaction is admitted. Focused actual HTTP checks also pass anonymous/authenticated withdrawal, denied reactions/comments and deep links, and recovery for all three publication kinds. The full HTTP suite includes these cases. This repair leaves the
web/native code and API shapes unchanged; full hosted checks remain required. The encrypted production restore accepted all 158 migrations and repeated application, retained all 77 notifications and remains paused.

The final web destination repair handles a published record beyond the bounded
feed or without a media preview through one permission-enforcing detail lookup.
It preserves publication authority and existing discussion identities when feed
position or preview availability changes. Twelve rendered regressions cover all
three record kinds, missing previews, denial after cached success, retry, no
duplicate ordinary-grid lookup and malformed identifiers. All 36 focused
selection/panel tests pass, as do real-API browser journeys on Chromium desktop, phone and tablet, Firefox
and WebKit across the three beyond-window and two preview-less cases. Keyboard
collapse/expansion and axe checks of the selected publication pass in all five
projects with no serious or critical violations. Type/lint pass.
Backend, native artifacts, schemas and API shapes are unchanged by this web fix.

Two subsequent review regressions were reproduced before repair. Refreshing a
feed across a page boundary now reselects and focuses the linked publication;
unchanged selection still leaves manual pagination under user control. Five
rendered source-kind regressions cover movement in both directions. Classified
discussions now require the same non-null future expiry as their public detail
endpoint. SQL tests cover null/current/past expiry, anonymous/owner/moderator
access, rejected registration/writes/links and preserved engagement on renewal.
Focused real HTTP checks also pass against public detail and interaction routes.

The complete backend/web checks for `0fcad7e00` passed. Hosted iOS run
[36659729464](https://github.com/diegueins680/tdf-app/actions/runs/36659729464)
passed the installed journey; the later schema-158 run passed real HTTP but timed
out while starting the XCTest driver before executing the app flow. The hosted
workflow now allows a bounded five-minute driver startup, following
[Maestro's documented startup setting](https://docs.maestro.dev/maestro-cli/environment-variables).
The unchanged signed native artifact still needs qualification against the final
schema after these repairs.

The expiry repair passes all nine property suites and ten concurrency groups on
PostgreSQL 17. The encrypted restored database accepted and reapplied the exact
159-entry manifest, remains paused, and retains all 77 notifications. The 27
focused web tests, typecheck, scoped lint, catalog audit, 65 release checks and
three specification-inventory checks pass. Production still runs schema 115.
