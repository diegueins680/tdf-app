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
EVENT_DISCOVERY_ENABLED=false
TICKETMASTER_API_KEY=your-consumer-key
TICKETMASTER_API_BASE=https://app.ticketmaster.com/discovery/v2
EVENT_DISCOVERY_LOOKAHEAD_DAYS=90
EVENT_DISCOVERY_MAX_PAGES_PER_CITY=5
EVENT_DISCOVERY_HOUR_LOCAL=3
EVENT_DISCOVERY_COUNTRY_CODE=
```

Production intentionally defaults `EVENT_DISCOVERY_ENABLED` to `false`. Setting a Ticketmaster secret does not authorize or start imports. Enable the job only as a separate operation after the discovery migration, backend rollout, health/version checks, and log review have succeeded.

`EVENT_DISCOVERY_COUNTRY_CODE` is optional and defaults to no country restriction because active users may live in different countries. The importer still requires the returned venue city to exactly match the requested profile city after normalization. Set a two-letter code only when every user city should be restricted to one deployment-wide country.

The API key is never written to application logs. One city or event failure is logged and does not stop other cities from syncing. Past imported events are completed automatically; future imports are cancelled and removed from the public feed when their city no longer has an active user.

## Deployment

Production keeps `RUN_MIGRATIONS=false`. The new backend also uses provider-reference tables from ordinary event and artist handlers, so `tdf-hq/sql/2026-07-12_event_discovery_imports.sql` must be applied before deploying the binary, even when the cron remains disabled.

Use the guarded backend release lane rather than invoking Fly directly:

```bash
npm run release:backend:plan -- --sha <full-sha>
npm run release:backend:preflight -- --sha <full-sha>
npm run release:backend -- --sha <full-sha> --execute --confirm <full-sha>
```

After rollout, confirm the exact `/version` SHA, a healthy response, direct Machine checks, and clean logs before changing `EVENT_DISCOVERY_ENABLED`. Enabling the job targets the normalized union of all eligible active-user cities; there is currently no production city allowlist or single-city canary setting.

Useful log tags:

```text
[Cron][EventDiscovery]
```
