# Canonical video ingestion rollout

This work extends `social_sync_account`, `record_external_resource`, recordings,
collections and `catalog_backfill_run`. A profile connection grants no ingestion
approval. Only a strict administrator can verify a stable YouTube channel and
record a separate approval reference. The schema enables no source or worker.

Apply `2026-09-20_records_ingestion_runtime.sql` through the checksum-pinned
production manifest, then deploy the exact reviewed image through the guarded
release runner. Do not override a concurrent release hold, independent review,
required checks, or the compatible-recovery requirement. Existing production
`EVENT_DISCOVERY_AUTO_PUBLISH=true` requires separate applicable approval evidence;
the historical research-pilot approval alone does not grant publication authority.

The administration entry is `/configuracion/fuentes-videos`; its API is
`/admin/records-ingestion`. Source creation/enablement, global control and manual
run requests are audited. A verified source's collection must be an existing
published recording collection. Artist/party links use existing canonical IDs;
no name-based identity creation occurs. The official channel is
`UCx9Jpaw_XDrMtIdzWYlU51g`, uploads playlist `UUx9Jpaw_XDrMtIdzWYlU51g`.
Provider credentials remain in server-side `YOUTUBE_API_KEY`.

The background worker is registered in application boot. The default interval
is one hour, configurable from 300 through 86400 seconds in the control row.
Weekly reconciliation uses the most recent Sunday 00:00 UTC identity. UTC slot
calculation is independent of the host timezone. Each invocation commits at most
two pages with their checkpoints. Full runs first traverse uploads, then directly
verify all previously imported source resources in batches of 50. Playlist
omission alone does not withdraw a video. Interrupted/incomplete runs resume before
new slots; catch-up chooses the latest slot instead of replaying a backlog.
Incremental traversal also preserves pagination for upload bursts. It currently
refreshes complete accessible uploads rather than assuming a fixed lookback.

Manual and scheduled calls share a connection-held PostgreSQL advisory lock per
source. Page writes and checkpoint/counters commit in one transaction; source
updates also acquire the same transaction lock and return a conflict while a
run holds it. Changing the linked target cancels incompatible saved runs.
A different execution key returns the pending run to resume. Cancellation
releases the lock; a retry resumes the committed checkpoint. Resource uniqueness is the
existing `(provider_id,resource_kind,external_code)` constraint. The existing run
ledger identity includes source, execution key, mode and dry-run flag. Provider
failures retain checkpoints. Missing metadata is a review/unavailability count,
never evidence that a failed or incomplete playlist deleted content. The worker
reserves a conservative 15 provider units per invocation under a 9000-unit UTC-day
budget; channel verification reserves three units in the same budget and
is limited to one attempt per administrator per minute. No paid quota changes
are made. Sources are ordered by oldest attempt to prevent starvation.

Provider-owned snapshots contain title, description, publication time, duration,
thumbnail variants, channel/video identities and embedding/live status. They do
not invent a recording date or classify Shorts by duration. Valid public new
uploads are published into the chosen canonical recording collection; existing
curated session membership is retained. Existing editorial titles/artwork and
inactive resources are protected. No follower notification or email path is
called by backfill. Non-embeddable videos retain provider links.

Provider metadata older than 30 days is withdrawn in bounded batches even when
ingestion is stopped. Nonpublic metadata is cleared immediately following a
successful direct provider check. Provider-owned recording text, durations and
images and their audit snapshots are cleared, while independent editorial edits
survive. Minimal identity markers allow a later verified public refresh to
restore generated fields. Audit identities and actions remain available.

Before enablement, use an administrator dry run and inspect eligible/review counts.
Then run a full reconciliation with a stable execution key, inspect the public
feed and real images, replay the same key, and run a fresh key to verify unchanged
content. Capture actual scheduled execution separately from manual execution.
The original removed Llama Este Pez IDs must stay unavailable; replacement uploads
`0hYDXQ5hWfo` and `0W4KdgkQD5w` are distinct provider resources, not replacement
thumbnails borrowed for the removed IDs.

Emergency stop: PUT `/admin/records-ingestion/control` with
`{"running":false,"intervalSeconds":3600}`. Per-source disable uses the source
endpoint and works during provider outages. The reverse SQL stops ingestion and
removes its mutation function, retaining imported catalog content and all audits.
Deploy a compatible earlier binary only through the guarded recovery path. Do not
apply an old video-catalog rollback to remove this runtime's imports.

Validation is recorded separately in PR/check evidence. The current work is not
proof of production enablement, persisted real-source backfill, cron execution,
nationwide event coverage, event approval, or mobile-store availability.

## Local evidence (2026-09-27)

- `scripts/test-records-ingestion-runtime.sh`: real PostgreSQL and Haskell
  orchestration passed checkpoint/resume, competing keys, replay, stopped dry
  run, provider outage, direct known-resource reconciliation, concurrent calls,
  and cancellation. Only the provider boundary is synthetic.
- SQL tests passed approval/private/identity constraints, editorial ownership,
  stale responses, nonpublic deletion, public restoration, retention while
  stopped, rollback/reapplication and concurrent canonical upserts.
- Browser regressions passed 10 scenarios across Chromium desktop/phone/tablet,
  Firefox and WebKit, with bounded image fallback, decoded provider placeholders,
  unchanged source links and accurate unavailable states. These use fixtures.
- Haskell scheduling: three examples passed, including 200 QuickCheck cases.
  All four ingestion admin handlers reject staff roles and malformed Admin grants
  before touching the database or provider.
- UI focused tests: 15 passed; type checking and focused lint passed.
- Full Stack-built Hspec suite: 3,539 examples, zero failures, six existing
  pending examples (rerun with permission for the HTTP test to bind locally).
- Release tooling: 82 tests passed. The existing formal-methods audit passed;
  this does not establish a formal proof of the ingestion service.
- Full Stack build passed after local disk exhaustion was resolved. Both the
  executable and test suite linked; the full suite results are recorded above.
- Staging rollout remains blocked by the previously observed Fly HTTP 403 for
  overdue invoices. No rollout, production backfill or scheduled execution is
  established by these local checks.
