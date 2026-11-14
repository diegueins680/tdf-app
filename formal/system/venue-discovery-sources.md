# Venue discovery sources for six cities

Requirement `EVT-VENUE-DISCOVERY-001`. Operational detail and the source list
live in [docs/Event_Discovery_Cron.md](../../docs/Event_Discovery_Cron.md).

## Scope

The daily discovery worker (06:00 America/Guayaquil) covers every `event_city`
row whose country is Ecuador or Colombia, plus followed cities. Migration
`2026-11-14_latam_venue_discovery_sources.sql` registers Quito, Guayaquil,
Cuenca, Bogotá, Cali and Medellín and seeds ten enabled sources: two venue
iCalendar feeds, six single-venue schema.org sources and two platform
schema.org sources.

## Rules

1. **Private by default.** A `jsonld` source cannot hold a publication
   approval, so its events are created and refreshed as non-public drafts under
   the existing pilot allowance. This contract changes no publication rule.
2. **Structured data only.** A `jsonld` source reads link targets from its
   index document and schema.org `Event` objects from JSON-LD blocks. It reads
   no other markup.
3. **Bounded and polite.** One index plus at most `maxPages` (≤ 400) event
   pages per run, about one request per second, 20-second timeout, 10 MB per
   document, at most three redirects, each validated as a public HTTPS host.
4. **No silent truncation.** An index with more event links than the budget, a
   failed index, or an event page answering anything other than success,
   `404` or `410` fails the source run. A failed run never marks events missing.
5. **No guessed facts.** An event is imported only with a title, a start
   instant, a venue name and a city. Start instants come from an explicit UTC
   offset or from a local time in a zone without daylight saving
   (`America/Guayaquil`, `America/Bogota`, `America/Lima`, `America/Panama`,
   `UTC`). Date-only values and local times in any other zone are dropped.
6. **City attribution.** A source bound to a city files every event under that
   city. A source without a city imports an event only when its locality or
   street address names a registered discovery city, and the stated country,
   when present, matches.
7. **Date-only publishers are labelled.** When a source declares a
   `dateOnlyLocalTime` marker and an event's local start equals it, the event
   keeps the date and its description starts with “Hora por confirmar: la
   fuente publica solo la fecha.”
8. **Catalog types.** Free-text categories and schema.org types map to
   `concert`, `festival` or `other`; an unknown category never fails a run.
9. **Operator choices persist.** The migration inserts missing rows only. It
   never re-enables, re-points or re-prioritizes an existing source, on first
   application or replay. Rollback disables the ten seeded sources and keeps
   cities, sources, references and run history.

## Not established

- No platform's terms of service were reviewed. `robots.txt` was read for each
  seeded host on 2026-11-14 and allows the paths used.
- No production run has been observed; the live fetch path (redirects,
  compression, rate) is exercised only by manual requests with the worker's
  user agent, not by an automated test.
- Coverage is partial: see “Known gaps” in the operational document.
