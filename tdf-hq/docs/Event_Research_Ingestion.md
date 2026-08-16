# Event research ingestion

This workflow stores evidence-backed web research separately from published social events. It is intended for Ecuador event discovery where many official ticketing sources expose HTML, social posts, queue pages, or other material that the automated structured-feed cron must not scrape.

## Safety boundary

- Every route under `/social-events/event-research` requires strict administrator access.
- Candidate upserts never create or publish a `social_event`.
- Before explicit pilot approval, the database permits at most 20 active pilot candidates across all runs. A discarded candidate frees one slot; no record is deleted.
- Pilot approval is an explicit `POST /social-events/event-research/pilot/approve` with a durable reference. It is append-audited and cannot be silently replaced with a different reference.
- `web` discovery sources are registry entries for manual research only. They require an HTTPS official URL and must remain disabled, so the structured-feed cron never treats HTML as a feed.
- A candidate records its IANA timezone, primary source, evidence list, verification time, confidence, source-owned fields, and the complete normalized payload.

## Idempotent batch flow

1. Read `GET /social-events/event-research/pilot` and the latest runs. Do not start a new unapproved pilot if 20 active candidates already exist.
2. `POST /social-events/event-research/runs` with a stable run key such as `ecuador-events-2026-08-16`. Reusing the key returns the existing run.
3. Process bounded source batches. `PUT /social-events/event-research/candidates` uses `(provider, externalId)` as its unique key.
4. Commit the last confirmed source position with `PUT /social-events/event-research/runs/{runId}`. A retry with the same content creates neither a candidate duplicate nor a duplicate change entry.
5. Close the run as `completed` or `failed`. A completed run cannot be reopened; exact candidate retries remain readable and materially different writes are rejected.
6. Audit the result through `GET /social-events/event-research/changes?run_id=...`.

The content hash excludes run identity, verification timestamp, and evidence consultation timestamps. A later verification of unchanged source content is recorded as `verified`; a changed normalized payload is recorded as `updated`, with before and after values.

## Confidence

`high` is accepted only when the start time, venue, city, direct purchase URL, and an `official_sale` evidence item are present. End times remain optional unless the official source confirms them. `medium` and `low` candidates may preserve incomplete or contradictory information in review, but still require an official HTTPS source and at least one evidence item containing the primary source URL.

## Schema rollout and rollback

Apply `tdf-hq/sql/2026-08-16_event_research_ingestion.sql` before deploying code that uses these routes. The reverse migration is `tdf-hq/sql/2026-08-16_event_research_ingestion.down.sql`. The isolated regression script is:

```sh
./scripts/test-event-research-ingestion-migration.sh
```

It verifies the seven disabled source seeds, the accumulated cap, retry idempotency, discard-and-replace behavior, rollback, and clean reapplication.
