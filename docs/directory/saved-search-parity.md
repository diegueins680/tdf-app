# DIR-SAVED-SEARCH-001 — Saved alerts use public non-geographic eligibility

Authority: the directory functional specification's saved-search section,
issue #534, the current public search contract, and the product owner's
25 October 2026 instruction to repair existing user journeys.

## User problem and scoped outcome

The existing migration's alert trigger independently matched six filter fields.
The public query also matches service, remote/availability, profile kind, dates,
artist-kind aliases, substrings and catalog aliases. Mobile stores the `all`
sentinel but omits it from public requests; the trigger treated it literally.
Users could receive irrelevant alerts and miss results they asked to follow.

A full213 disposable PostgreSQL reproduction found ten valid-query mismatches:
six false positives and four false negatives. This is evidence of behavior,
not a production-user count or a measured activation/retention uplift.

## Contract

- Public search and internal alerts reuse typed scalar SQL predicates for
  non-geographic eligibility and entity/derived-listing selection. Resolve
  matching alias IDs once per query. Do not construct JSON per candidate row.
- Preserve public identity, ranking weights/scores, order/cursor, facet meaning,
  event/venue visibility and the existing ephemeral public geographic query.
  People-only filters remain inapplicable to events and venues.
- An alert result must still be in the canonical public projection. Private,
  moderated, expired or suppressed content cannot acquire a notification.
  Mixed results suppress derived listings; explicit classified searches retain
  them. `artist` includes artist/band/project, and saved `all` means mixed.
- Keep valid saved JSON, names, IDs and hashes. No history rewrite/backfill,
  dedup-key change, source grant or provider activation is included.
- Decode historical JSON at one guarded boundary. Invalid types, unsafe casts,
  noncanonical UUIDs, unknown filters or unsupported geography yield no-match and do not abort
  another document's index update. Do not silently broaden malformed queries.
- Validate new saved queries before reserving/writing them. Reject unknown or
  nested fields, malformed typed values and control characters. Match date
  acceptance to public UTCTime, not PostgreSQL's relative/ambiguous time grammar.
  Validate control characters and unrepresentable numeric scales before casting
  raw JSON: PostgreSQL jsonb rejects U+0000 and out-of-range numerics before a
  JSONB-accepting SQL helper can run ([PostgreSQL17 JSON types](https://www.postgresql.org/docs/17/datatype-json.html)).
- Never store exact latitude/longitude in a new saved search, even when alerts
  are disabled. Nonempty radius with alerts enabled returns actionable400:
  “Las alertas por radio aún no están disponibles. Quita el radio para guardar
  esta alerta.” Removing the radius permits a normal save. A radius-only search
  may be saved with alerts disabled; no center is inferred or stored.
- Keep one delivery/notification per saved-search/result/version. Same-version
  replay changes nothing; a changed version may match once. Internal intent and
  notification remain transactional; email/push stay disabled.

## Implementation and verification boundary

The additive migration defines two pure inlinable eligibility helpers, a shared
alias lookup and a guarded historical-filter decoder. The existing public SQL
keeps typed inputs foldable and materializes only the alias array. The existing
save endpoint retains its DTO and response shape; no Mobile pin or dependency
change is required because its error UI already shows the server message.

PostgreSQL tests compare against the independently preserved pre-repair public
predicate, execute actual trigger/delivery/notification writes, test malformed
historical JSON, canonical event/venue and derived-listing controls, version
replay, complete migration prefix/replay and exact previous-function rollback.

The existing shipped-SQL runner also checks discovery/privacy, scores/facets
and cursor behavior. Deterministic4000-document plan comparisons check result
and facet equality, scalar-helper inlining, once-per-query aliases and observed
indexed paths. Timing/buffer observations are scoped evidence, not a universal
performance guarantee. The mandatory CI backend lane supplies its compiled
binary for authenticated HTTP save/reload, invalid/radius recovery, public
parity, dedup and account isolation. SQL-only execution is not an HTTP pass.

## Remaining scope and release

#534 remains PARTIAL for geographic alerts. The specification prohibits exact
device coordinates and the old saved radius has no center; supporting an
approximate public center requires an explicit product/privacy contract.
Existing unsupported geographic alerts are skipped, without rewriting rows.
This repair covers changed canonical indexed documents. Public legacy artist
fallback rows without a directory profile do not emit index-trigger updates;
alerts for changes exclusive to that fallback remain outside this repair.
No general erasure of historical payloads, external delivery guarantee, native
physical-device acceptance or mathematical implementation proof is claimed.

Release requires the protected review/CI/admission process and reviewed migration
application. Rollback restores the previous trigger and removes the new helper
functions without deleting user searches, deliveries or notifications. Restore
the compatible previous backend together with rollback: the new public query
requires the new functions. Never roll this schema back under the new binary.
