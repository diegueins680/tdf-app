# City Event Discovery Cron

The backend can import upcoming events from the [Ticketmaster Discovery API](https://developer.ticketmaster.com/products-and-docs/apis/discovery-api/v2/) only for cities attached to active TDF user accounts.

## What it creates

For each provider event, one database transaction creates or refreshes:

- the real venue, including address, city, country, coordinates, website, state, postal code, and image when supplied;
- Ticketmaster attractions as social-event artist profiles, including genres and images;
- the social event, including dates, description, minimum advertised price, currency, image, status, and external ticket URL;
- venue/event/artist provider references and event-artist relationships.

Imported events use Ticketmaster's external purchase link. The importer does not create TDF ticket tiers because a public price range is not authoritative sellable inventory.

Provider IDs are stored in dedicated reference tables, so retries update the same records instead of deduplicating by title. Cancelled Ticketmaster events are marked cancelled rather than deleted.

## City targeting

The job uses the normalized union of nonblank cities from active users' fan and artist profiles. It trims and case-folds values, rejects unsafe or overlong city strings, and accepts an event only when its returned venue city exactly matches the requested city after normalization.

## Schedule and multi-machine safety

The first run starts shortly after backend boot. Later runs happen once per day at `EVENT_DISCOVERY_HOUR_LOCAL`. The deployment sets `TZ=America/Guayaquil`, so the default hour is 03:00 Ecuador time. A PostgreSQL advisory lock and a persistent per-day run ledger allow only one API replica to import at a time and prevent a restart from duplicating that day's run.

Each run processes at most 500 cities, rotating the starting point daily when there are more. Requests are throttled to four per second, use at most five 100-event pages per city by default, and retry one rate-limited response using the provider's `Retry-After` header.

## Configuration

```env
EVENT_DISCOVERY_ENABLED=true
TICKETMASTER_API_KEY=your-consumer-key
TICKETMASTER_API_BASE=https://app.ticketmaster.com/discovery/v2
EVENT_DISCOVERY_LOOKAHEAD_DAYS=90
EVENT_DISCOVERY_MAX_PAGES_PER_CITY=5
EVENT_DISCOVERY_HOUR_LOCAL=3
EVENT_DISCOVERY_COUNTRY_CODE=EC
```

`EVENT_DISCOVERY_COUNTRY_CODE` defaults to `EC`. User profiles currently store a city but not a country, so the importer intentionally keeps one deployment-wide country scope to avoid matching a same-named city elsewhere.

The API key is never written to application logs. One city or event failure is logged and does not stop other cities from syncing. Past imported events are completed automatically; future imports are cancelled and removed from the public feed when their city no longer has an active user.

## Deployment

When `RUN_MIGRATIONS=false`, apply `tdf-hq/sql/2026-07-12_event_discovery_imports.sql` before enabling the job or deploying the new backend binary.

Useful log tags:

```text
[Cron][EventDiscovery]
```
