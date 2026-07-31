# Event discovery by subscribed city

The backend imports upcoming events only for cities explicitly followed by active
TDF users. The mobile Events tab uses that scope by default and also offers an
**Explore** mode for all public events already present in TDF.

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

The worker runs shortly after boot and then at UTC six-hour boundaries. Every
enabled source claims its own `(source, scheduled_for)` ledger row. A PostgreSQL
advisory lock prevents concurrent replicas from running the batch, while the
ledger makes restarts idempotent and permits a failed source to be retried.

Ticketmaster requests are rate-limited, paginated, and bounded by configured
lookahead/page limits. Buen Plan is independently isolated in the registry so it
can be disabled without affecting Ticketmaster or venue feeds. Source failures
record the last error and consecutive failure count without stopping other
sources.

## Configuration

```env
EVENT_DISCOVERY_ENABLED=false
TICKETMASTER_API_KEY=your-consumer-key
TICKETMASTER_API_BASE=https://app.ticketmaster.com/discovery/v2
EVENT_DISCOVERY_LOOKAHEAD_DAYS=90
EVENT_DISCOVERY_MAX_PAGES_PER_CITY=5
EVENT_DISCOVERY_COUNTRY_CODE=
```

`EVENT_DISCOVERY_ENABLED` is the master kill switch and remains false during the
initial production rollout. Ticketmaster can be disabled in the source registry
or left enabled without a key; its failure does not prevent Buen Plan/venue
feeds from running. `EVENT_DISCOVERY_COUNTRY_CODE` remains a legacy default for
the old single-city helper; explicit subscriptions always send their own country.

Buen Plan's endpoint is public but undocumented. Keep its source independently
disableable and review its logs/terms before enabling it in production.

## Deployment

Production uses `RUN_MIGRATIONS=false`. Before deploying this binary, apply in
manifest order:

```text
tdf-hq/sql/2026-07-12_event_discovery_imports.sql
tdf-hq/sql/2026-07-30_event_city_subscriptions.sql
```

Use the guarded backend release lane:

```bash
npm run release:backend:plan -- --sha <full-sha>
npm run release:backend:preflight -- --sha <full-sha>
npm run release:backend -- --sha <full-sha> --execute --confirm <full-sha>
```

After rollout, verify `/health`, `/version`, the exact release SHA, and
`[Cron][EventDiscovery]` logs before enabling the master switch.
