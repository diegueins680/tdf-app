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
store draft; signed iOS 29 has been uploaded to EAS Submit and is queued for App Store Connect processing (submission `d2a6ee83-7c1a-406e-979c-3c79e6460b73`). Store publication is not complete.

A subsequent report-abuse review is fixed by the additive
`2026-09-30_interaction_report_content_version` migration (manifest 157).
The unchanged-content regression failed before the repair and passes afterward.
Focused real HTTP checks confirm that fresh request keys do not reopen unchanged
resolved reports, while actual edits allow one audited reopen. SQL also covers
no-op edits, owner hide/restore, and content changed while awaiting review.
Existing report evidence is preserved. All seven SQL property suites and nine concurrency checks pass on fresh schema 157. A nonempty historical-report migration test proves baseline preservation and safe reapplication after a later edit; 82 release tests also pass. Fresh hosted CI remains required.

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
