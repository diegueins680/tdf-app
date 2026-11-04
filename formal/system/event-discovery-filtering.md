# Event discovery by city and performing artist

The user approved multiple artists with ANY semantics, upcoming events by default,
and a visible option to include past events, following a failed New York city
search in the signed-in events workspace. No production event identity was supplied.

| ID | Rule |
| --- | --- |
| EVT-DISCOVERY-FILTER-001 | Apply authorized event visibility and city, performer, type, state and time restrictions before result pagination; selected performers use OR and independent filter categories use AND. Preserve canonical identities, explicit time controls and coherent page/calendar results. |

For viewer `u`, event `e` and selected artist IDs `A`, inclusion is:

`visible(e,u) AND city(e,q) AND (A={} OR performers(e) intersects A) AND type(e,t) AND state(e,s) AND time(e,clock)`.

The existing suppression, metadata, lifecycle and owner/admin rules remain the
visibility authority. These changes do not grant access to another viewer's data.
An event appears once even if several selected performers belong to its lineup.
Artist profile IDs and Party IDs remain distinct. No matching is inferred from
organizers, invitees, attendance, equal names or equal numeric Party IDs.

## City source and normalization

Production PostgreSQL executes the embedded `social-event-city-match.sql` predicate
before LIMIT/OFFSET. A venue's canonical city wins when linked: match its Spanish
or English name, or an exact alias registered for that city in the cities catalog.
With no canonical city, match the venue's city text. Only if there is no usable
venue location may an unsuppressed external reference supply the city.

Partial name matching uses the existing directory normalizer, with NFC composition,
case folding, supported Latin accent removal and whitespace/punctuation folding.
Alias matching is exact after normalization. New York/NYC/Nueva York equivalence
depends on canonical names and explicit catalog aliases; none is hardcoded.
Canonical city identity, country and subdivision data are preserved. A bare city
name may legitimately return locations with the same name in different countries;
this does not merge those locations or add a country selector.

The previous bounded venue prefilter could omit locations after row 5000. The
public projection searched only its localized display city, and its frontend then
repeated that display-text test, discarding otherwise valid server alias matches.
The new predicate and removal of that redundant client filter address these
reproducible failure classes. They do not establish the cause of the unidentified
production event. SQLite fixtures retain their venue-text compatibility path;
the new canonical SQL is specifically tested on PostgreSQL.

## Existing API compatibility and UI

`GET /social-events/events` retains its route and parameter codecs. `artistId`
accepts either one positive Int64 social Artist ID or a comma-separated list of
at most 50 submitted IDs, deduplicated by canonical numeric identity. A blank
whole filter is unset; blank members, invalid IDs and overflow fail with 400.
Bound parameters in an EXISTS predicate implement the artist union without
expanding event IDs into a parameter list.

`start_after` now means an earliest effective end/start: a known end must be at
or after the supplied instant; without an end, the start must be at or after it.
The public upcoming projection uses the same effective-end rule. This widens the
previous start-only interpretation to include ongoing events. The workspace
supplies the current instant per request unless “Incluir eventos pasados” is on.
Explicit instants retain the existing UTC codec. No stale clock is saved in URLs.

The signed-in workspace stores city, artist IDs, type, state, past inclusion and
page in its URL. Text input is debounced and requests consume cancellation signals.
Query identities include the viewer and all effective filters. Results are pages
of 20 with one lookahead row. Counts explicitly say “en esta página”; no exact
total is invented. The existing maximum offset is 10000; the last accessible page
explains that additional results require narrower filters. The calendar shows the
same page, marks matching dates, and selects a matching day after filtering.
Its explicit day selection does not hide the independent result list.

The anonymous agenda remains an upcoming public projection; shared city matching
is corrected there without exposing authenticated artist management endpoints or
adding anonymous access. Artist autocomplete and past-event browsing are in the
signed-in workspace shown in the report.

Organizers can maintain a canonical lineup from event details, including existing
events and newly created events. The editor reads current event fields immediately
before the existing PUT and omits unrelated metadata/workflow updates. It guards
session changes and unmount. The existing full-event API has no optimistic version
precondition, so another editor can still race non-metadata fields between read
and write; this change does not claim a concurrent-edit guarantee. Historical
unstructured descriptions are not automatically converted into artist links.

## Verification boundaries

- React/API regressions cover selection, query arguments, URL restoration,
  pagination, empty-state time controls, stale responses, localized public city
  display, lineup saves and revoked/unmounted editor continuation.
- A pinned test-only PostgreSQL WASM engine executes the actual embedded city SQL
  and existing directory normalizer. It covers canonical/venue/external precedence,
  ES/EN/alias matching, NFC/accents/whitespace, injection-like input and pagination,
  including a negative control reproducing the old 5000-venue loss. This is not a
  compiled handler, production data, HTTP authorization or concurrency test.
- Hspec adds a canonical-ID parser property and malformed/duplicate/boundary cases;
  execution requires the repository's pinned Stack toolchain.
- The browser fixture tests actual autocomplete controls and mobile overflow with
  synthetic APIs. It does not establish database or physical-device behavior.

No schema migration, production backfill, event publication, merge or deployment
is implied by these tests. Actual compiled backend, hosted browser, current-head
CI/review and release prerequisites remain authoritative.

To reproduce the isolated SQL checks without changing application dependencies:

```sh
npm install --prefix /tmp/tdf-event-search-engine --ignore-scripts --no-audit --no-fund @electric-sql/pglite@0.3.14
TDF_PGLITE_MODULE=/tmp/tdf-event-search-engine/node_modules/@electric-sql/pglite/dist/index.js node scripts/test-event-discovery-postgres.mjs
```
