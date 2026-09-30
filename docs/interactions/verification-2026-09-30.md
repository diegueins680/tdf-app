# Interaction verification — 30 September 2026

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
