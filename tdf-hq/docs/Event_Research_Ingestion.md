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

## Materialization after pilot approval

`POST /social-events/event-research/candidates/{candidateId}/materialize` is the only supported bridge from a research candidate to a social event. Start a dedicated research run and send its id with `{"erMaterializationRunId":"3","erMaterializationPublish":true}` only for an explicitly approved pilot candidate that is still `high` confidence and `draft`, has official-sale evidence, a confirmed start, venue, city, timezone, event type, and at least one lineup artist. `event_end_unconfirmed` is the sole permitted publication blocker; the event end remains null instead of being inferred. The first write requires the supplied run to remain `running`; checkpoint and close it after each confirmed batch.

The operation locks the pilot and candidate, verifies that the candidate belongs to the approved pilot, resolves or creates provider-linked venue and artist entities, reuses an unambiguous matching event when present, inserts the provider event reference, links the candidate, and appends the `materialized` audit change in one transaction. A newly created event is owned by the authenticated strict administrator that initiated materialization, so later manual corrections remain manageable; reused events keep their existing owner and gain only missing confirmed lineup links. Unpublished materializations store a `materialization_draft:` provider status; discovery refreshes preserve that publication hold and do not rewrite the held event even when global auto-publication is enabled. Provider/external identity and the candidate/event audit key make retries return the existing link. Once linked, replays do not overwrite event fields, so later manual edits are preserved; a later publish request for a still-private linked event returns a conflict instead of silently claiming success. After an administrator makes the event public through the normal event workflow, replaying with `publish=true` releases the provider hold.

Ambiguous entity matches, broken references, unapproved pilots, medium/low confidence, review candidates, unknown blockers, cancellations, postponements, and explicit availability outside supported sale states return a conflict without committing partial entities. Fully sold-out candidates also remain in review until the catalog defines a public sold-out lifecycle state; partially sold-out sales remain `on_sale`. Image metadata is copied only when the candidate payload explicitly marks permission as `confirmed`; price tiers are not synthesized when capacity or tier quantities are unknown.

## Confidence

`high` is accepted only when the start time, venue, city, direct purchase URL, and an `official_sale` evidence item are present. End times remain optional unless the official source confirms them. `medium` and `low` candidates may preserve incomplete or contradictory information in review, but still require an official HTTPS source and at least one evidence item containing the primary source URL.

## Schema rollout and rollback

Apply `tdf-hq/sql/2026-08-16_event_research_ingestion.sql` before deploying code that uses these routes. The reverse migration is `tdf-hq/sql/2026-08-16_event_research_ingestion.down.sql`. The isolated regression script is:

```sh
./scripts/test-event-research-ingestion-migration.sh
```

It verifies the seven disabled source seeds, the accumulated cap, retry idempotency, discard-and-replace behavior, rollback, and clean reapplication.
