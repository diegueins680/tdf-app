# Ecuador event discovery

The backend discovers events for Ecuador cities in the canonical city registry,
prioritizing Quito, while retaining previously supported followed destinations.
Ecuador discovery does not require user subscriptions or change them.
The mobile Events tab still uses followed cities for personalized display and
offers **Explore** for other public events. Registered coverage is not proof
that every Ecuador city or official source has an automated interface.

## Sources

The source registry is stored in `event_discovery_source`. V1 supports:

- Ticketmaster Discovery API (`ticketmaster`);
- Buen Plan Ecuador's public catalogue (`buenplan`);
- venue-owned HTTPS iCalendar feeds (`ical`);
- venue-owned HTTPS JSON feeds (`json`).

Ticketmaster and Buen Plan are seeded by the production migration. A venue feed
must have a unique source key, an HTTPS URL, and one `event_city`. HTML scraping
is deliberately not supported.

Strict administrators can manage these records at
`/configuracion/fuentes-eventos`. As an operational fallback, a verified venue
feed can also be registered with:

```text
GET  /social-events/event-sources
POST /social-events/event-sources
PUT  /social-events/event-sources/:sourceId
```

```sql
INSERT INTO event_discovery_source
  (source_key, name, source_type, feed_url, city_id, enabled, priority,
   consecutive_failures, created_at, updated_at)
SELECT
  'venue-example', 'Venue Example', 'ical',
  'https://venue.example/events.ics', id, TRUE, 400, 0, now(), now()
FROM event_city
WHERE normalized_name = 'quito' AND country_code = 'EC'
ON CONFLICT (source_key) DO UPDATE
SET feed_url = EXCLUDED.feed_url,
    city_id = EXCLUDED.city_id,
    enabled = EXCLUDED.enabled,
    priority = EXCLUDED.priority,
    updated_at = now();
```

The venue JSON contract accepts either an array or `{ "events": [...] }`:

```json
{
  "events": [
    {
      "id": "venue-event-123",
      "title": "Live set",
      "start": "2026-08-10T01:00:00Z",
      "end": "2026-08-10T04:00:00Z",
      "venue": "Venue name",
      "address": "Street 123",
      "ticketUrl": "https://venue.example/tickets/123",
      "imageUrl": "https://venue.example/events/123.jpg",
      "priceCents": 2500,
      "currency": "USD",
      "status": "on_sale",
      "type": "concert",
      "artists": ["Artist name"]
    }
  ]
}
```

Venue feeds reject non-HTTPS/private-looking URLs, do not follow redirects, and
have response-size and timeout limits.

## Canonical events and purchase options

Provider IDs remain the idempotency key within each source. When a new source
resembles an existing event in the same city—using title, start time, venue, and
artists—it attaches a second external reference to the canonical TDF event
instead of publishing a duplicate.

The API returns every active provider reference in `eventSources`, including its
label, URL, price, currency, and status. The mobile event detail displays the
available purchase platforms. Source priority determines which source owns the
canonical title, schedule, image, and default ticket link.

A source may miss an event once without hiding it. After two successful source
runs omit it, that purchase option becomes unavailable. The canonical event
stays public while another active source still supplies it. Past and
out-of-subscription events are removed from the public feed, not deleted.

## City subscriptions

Authenticated clients manage subscriptions through:

```text
GET /social-events/cities?q=&country=
GET /social-events/me/city-subscriptions
PUT /social-events/me/city-subscriptions
GET /social-events/events?scope=subscribed
GET /social-events/events?scope=all
```

The PUT body is:

```json
{
  "eventCities": [
    {
      "eventCityInputName": "Guayaquil",
      "eventCityInputCountryCode": "EC",
      "eventCityInputTimeZone": "America/Guayaquil"
    }
  ]
}
```

Country codes are ISO 3166-1 alpha-2. The list is replaced atomically and is
limited to 20 cities per user. Existing fan/artist profile cities are migrated
once as Ecuador subscriptions for backward compatibility.

## Schedule and multi-machine safety

The existing worker runs shortly after boot for at most the latest missed slot,
then daily at 06:00 America/Guayaquil (11:00 UTC). The hour is configurable via
`EVENT_DISCOVERY_HOUR_LOCAL` (default 6); conversion uses UTC-05:00 explicitly,
never the server local timezone. Sunday is identified from that same timezone
and performs full source reconciliation in the daily slot, without a second
weekly job. The current adapters fetch full bounded source inventories on other
days too; no incremental cursor behavior is claimed for these adapters. Every
enabled source claims its own `(source, scheduled_for)` ledger row. A PostgreSQL
advisory lock prevents concurrent replicas from running the batch, while the
ledger makes restarts idempotent and permits a failed source to be retried.

Ticketmaster requests are rate-limited, paginated, and bounded by configured
lookahead/page limits. Exceeding a page budget or failing any requested city
marks the source run failed: partial results cannot drive missing-item
reconciliation or a misleading success. Buen Plan is independently isolated in the registry so it
can be disabled without affecting Ticketmaster or venue feeds. Source failures
record the last error and consecutive failure count without stopping other
sources.

Per-event persistence failures abort their source run. After fetching, completion
locks and rechecks the enabled source before absence reconciliation, including
empty feeds. Reconciliation, the completed run ledger and the source success
timestamp commit in one transaction; a disabled source or failed final write
cannot leave partial success evidence. The source-row lock is held only during
completion, not across network requests.

## Configuration

```env
EVENT_DISCOVERY_ENABLED=false
EVENT_DISCOVERY_AUTO_PUBLISH=false
EVENT_DISCOVERY_PILOT_LIMIT=20
TICKETMASTER_API_KEY=your-consumer-key
TICKETMASTER_API_BASE=https://app.ticketmaster.com/discovery/v2
EVENT_DISCOVERY_HOUR_LOCAL=6
EVENT_DISCOVERY_LOOKAHEAD_DAYS=90
EVENT_DISCOVERY_MAX_PAGES_PER_CITY=5
EVENT_DISCOVERY_COUNTRY_CODE=
```

`EVENT_DISCOVERY_ENABLED` is the master kill switch and remains false during the
initial production rollout. Ticketmaster can be disabled in the source registry
or left enabled without a key; its failure does not prevent Buen Plan/venue
feeds from running. `EVENT_DISCOVERY_COUNTRY_CODE` remains a legacy default for
the old single-city helper; the discovery registry sends the explicit Ecuador country code.

Buen Plan's endpoint is public but undocumented. Keep its source independently
disableable and review its logs/terms before enabling it in production.

`EVENT_DISCOVERY_AUTO_PUBLISH` defaults to `false`. In this mode provider
records are idempotently created or refreshed as non-public `planning` events.
Research candidates and imported canonical events share the existing cumulative
20-item pilot allowance until its durable approval is recorded. Linked candidates
and multiple provider references count once. Discarded unlinked candidates free
a slot; suppressed imports keep their tombstones. Known records remain refreshable
when capacity is exhausted, and database triggers serialize both entry points.
`EVENT_DISCOVERY_PILOT_LIMIT` can lower the scheduler's limit but cannot raise the
unapproved database cap or bypass it by setting auto-publish.

Automatic publication additionally requires a non-revoked entry for the exact
source in `event_discovery_publication_approval`, with an explicit applicable
reference, actor and approval time. The migration creates no approval. Historical
research-pilot approval is insufficient, and manual `web` sources cannot receive
this authority. Disabling or changing a source revokes its prior approval;
revocation and import share the pilot lock. Approval alone does not make missing
venue, lineup or sale-reference information publishable. Existing explicit
research materialization remains its own guarded administrative workflow.

## Deployment

Production uses `RUN_MIGRATIONS=false`. Before deploying this binary, apply in
manifest order:

```text
tdf-hq/sql/2026-07-12_event_discovery_imports.sql
tdf-hq/sql/2026-07-30_event_city_subscriptions.sql
tdf-hq/sql/2026-09-27_event_ingestion_boundaries.sql
```

Use the current [Hetzner operational contract](../ops/hetzner/README.md).
Its routine guarded release executor remains an open obligation. Historical Fly
preflight/release commands are retired and reject before remote action. Verify
the complete current migration manifest rather than treating the three domain
migrations above as a complete deployment batch.

Prepare the reviewed release/recovery bundle, then follow the canonical
[Hetzner procedure](../ops/hetzner/README.md). Preparation does not deploy:

```bash
npm run release:backend:prepare -- FULL_RELEASE_SHA FULL_RECOVERY_SHA NEW_PRIVATE_DIRECTORY
```

After rollout, verify `/health`, `/version`, the exact release SHA, and
`[Cron][EventDiscovery]` logs before enabling the master switch.


## Schedule cutover and remaining coverage work

This consolidates the existing six-hour loop; do not register another cron.
During a guarded rolling deployment, disable discovery on **all** old replicas
before starting the new binary. After both revisions are verified, set the
approved flags and `EVENT_DISCOVERY_HOUR_LOCAL=6`, then enable the existing worker.
Do not leave an old enabled replica running the six-hour schedule. The source
ledger/advisory lock remains shared; a Sunday run uses the same daily identity.
Verify an actual 11:00 UTC run separately from a manually triggered execution.

The registry includes seven manual research sources: Meet2Go, Passline Ecuador,
BuenPlan Tickets social, Feel The Tickets, TicketShow, On Time Tickets and Output
Concerts. Their existing disabled `web` entries remain manual research paths.
Buen Plan's undocumented endpoint still requires an applicable access/terms review;
this change does not infer permission from its public accessibility. No source is
enabled by the migration.

Remaining acceptance work includes verifying the live registry's Ecuador coverage,
provider permissions, event administration dry-run/manual-run progress controls,
and resumable provider pagination beyond the current bounded inventory fetch.
Those outcomes are not established by this boundary correction. Staging and
production remain subject to billing access, independent review and guarded rollout.

Rollback stops non-web sources and restores the prior research-only trigger while
retaining imported records, pilot decisions and publication approval history.
Re-enable only after verifying the deployed code's controls. The regression script
`scripts/test-event-ingestion-boundaries.sh` covers shared-cap races, duplicate
canonical links, discard semantics, separate approval, revocation and reapplication.

### Editorial ownership on refresh

Newly imported event fields retain the last source value in the existing event
metadata. Refresh compares that value with the current canonical value, preserves
editorial changes and unrelated metadata, and permanently relinquishes an edited
field so later coincidental agreement does not reclaim it. Legacy events without
ownership evidence stay protected; their source references still refresh.
Venue and artist profile ownership is a separate remaining limitation. A failure
to persist an event now fails its source run before absence reconciliation, rather
than logging the error and reporting a successful run.

Buen Plan's ten-page request budget now fails incomplete inventories explicitly.
A truncated prefix cannot mark unseen events missing or count as a successful
reconciliation. Continuing beyond that cap still requires the pending resumable
provider-page implementation and verified source permission.

### Ownership compatibility and reconciliation

`_discoveryOwned` is an internal stored-data object. The stored event decoder recognizes that object before applying the unchanged public-field allowlist; malformed ownership objects, unknown public fields and duplicate top-level keys still fail closed. Public requests cannot supply the namespace, and event responses do not expose it.

Editorial updates and image uploads compare against the row locked for that write. Changed fields permanently lose ingestion ownership; unchanged fields retain their prior source evidence. Metadata edits and lineup replacement commit with the event update. A concurrent ownership, suppression or explicit workflow change is rechecked before writing.

Both provider and subscription reconciliation retain editorial visibility, ticket URLs and workflow choices. Source expiry or disappearance can still hide an event, but returning source data cannot claim an editor-owned or unproven legacy publication flag. Reconciliation advances the snapshot only for fields it still owns. No feature activation or production rollout is introduced.

The forward migration `2026-10-03_discovery_ownership_metadata_boundary.sql`
aligns the anonymous directory predicate with this stored-data boundary. It
changes only the existing function, preserving metadata validation, the composed
suppression view and all source data. Apply/reapply and both historical privacy
migration orders are covered by the production-schema rehearsal. Recovery is
forward-only: preserve the private snapshot and suppression predicates; restoring
the older allowlist would hide otherwise public imported events. If ingestion
must be paused operationally, use its existing source enablement controls and
retain all canonical/editorial data. This change does not activate any source.
