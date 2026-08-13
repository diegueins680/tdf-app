# Phase 1 inventory summary

Captured on 2026-08-07 from the baselines documented in `README.md`.

## Static inventory

The latest reproducible pass (2026-08-12 UTC, including the candidate implementation) scanned 857
files and produced 527 semantic candidates after coalescing repeated occurrences in the same
source. It found:

- 337 likely dynamic business catalogs.
- 48 likely governed reference datasets.
- 106 likely security/system registries.
- 36 likely technical constants.
- 58 exact duplicate groups.
- 88 normalized spelling, punctuation, case, or accent variant groups.

The deterministic per-candidate decision report classifies all 527 entries with an explicit
migration target or technical-constant justification. The difference from discovery
recommendations comes from explicit rules for parser, rendering, test, generated-client,
migration-history, and automation mechanics. Retained technical candidates include reviewed
strict renderer and administration adapters; each
has a written justification and the
`technical_constant_allowlist` target in `catalog-list-decisions.json`.

The heuristic classification is a review queue, not a final decision. In particular, protocol
discriminants, reducer actions, parser tokens, MIME types, display breakpoints, and test mechanics
must be retained only with explicit technical justifications. Navigation, statuses, payment
options, provider codes, roles, and content structures must not be dismissed as technical merely
because code currently models them as unions or enums.

## Production findings

The read-only production query inspected 195 candidate text columns; 84 currently have values.
Notable migration evidence includes:

- `party_role.role`: 285 assignments spanning 29 role strings. A fresh aggregate read-only
  preflight found two active `Admin` assignments on two distinct parties, but only one party has
  an active credential. The legacy schema cannot prove persisted capability coherence, so the
  second independent emergency-administrator path remains a rollout blocker. See
  `security-emergency-readiness.md`.
- `service_catalog`: 15 rows. Spanish and English labels such as `Clases`/`Classes`,
  `Mezcla`/`Mixing`, `Producción de eventos`/`Event Production`, and
  `Ensayo`/`Rehearsal` are separate rows rather than translations of canonical services.
- `booking.service_type`: four distinct copied labels over four bookings, including
  `Grabación de Banda`, `Grabación de Voz`, `Recording`, and `Rehearsal (DJ)`.
- Genre data is fragmented among `artist_genre.genre`, `fan_profile.favorite_genres`, and
  `radio_stream.genre`. The fan-profile values include comma-separated lists, case variants, and
  `Femenino`, which cannot be deterministically treated as a music genre.
- `input_row.instrument`: 14 distinct microphone/model strings across 21 rows. These values mix
  instrument/input purpose with equipment identity and require reviewed mapping rather than blind
  normalization.
- `pipeline_card.stage`: four values in four rows, while backend and mobile source code define
  several incompatible stage sequences.
- `cms_content`: six versions for exactly three Records slugs (`records-releases`,
  `records-recordings`, and `records-sessions`), all Spanish; three are published and three
  archived.
- Currency values are currently all `USD` in populated business tables, while seven currencies
  are independently allowlisted in backend configuration, web, and mobile.
- Locale preferences are all `es`/`EC` in production, while five locales are independently
  hardcoded across backend configuration, web, and mobile.

## Candidate cutover status for genre consumers

- `artist_genre.genre` remains preserved migration evidence, while social-artist runtime reads and
  writes use `artist_genre_membership.genre_id` exclusively.
- `artist_profile.genres` remains preserved migration evidence and historical read fallback, while
  core artist writes, discovery filters, Label Artists, and Fan Hub use ordered
  `artist_profile_genre_membership.genre_id` values.
- `fan_profile.favorite_genres` remains preserved migration evidence and historical read fallback,
  while fan profile writes and the web editor use ordered
  `fan_profile_genre_membership.genre_id` values.
- `radio_stream.genre` remains preserved migration evidence. Candidate runtime search, upsert, and
  transmission contracts use `genre_id` exclusively, and the web widget loads published genre
  UUIDs and labels from the catalog API. Exact external metadata can resolve to a canonical genre;
  unresolved or ambiguous metadata is withheld in immutable observation/candidate rows instead of
  being guessed or written as a new catalog value.
- `radio_stream.country` remains preserved migration evidence. Runtime search, upsert, transmission,
  and web controls use `country_id`; labels come from the persisted governed reference. Exact
  alpha-2, alpha-3, Spanish-name, or English-name matches are recorded as observations and only a
  unique active, non-deprecated match is converted. The reviewed seed contains 249 identities from
  a dated bilingual UN M49 snapshot plus the explicit ISO 3166/MA Taiwan supplement.
- Production values such as `Femenino` remain withheld. The migration records unresolved evidence
  and does not guess or destructively convert those rows.

## Candidate cutover status for instruments and input lists

- `input_row.instrument` was a misnamed microphone/DI observation column, not an instrument list.
  The reviewed 21-row map now writes musical purpose to `instrument_id` and physical equipment to
  `mic_id`; copied text is cleared and rejected by the new writer trigger.
- The mapping resolves four published instrument identities and 13 active physical assets. The two
  non-exact normalizations are explicitly evidenced: `AKG C414 (HC)` maps to the existing AKG C414
  asset because `HC` was placement metadata, and the left/right KU-100 observations map to the one
  Neumann KU-100 binaural asset.
- Live Session payloads accept `primaryGenreId` and musician `instrumentId` only. The backend
  validates active published entities before any intake write, and the web sources both selectors
  from one public batch.
- Full mapping, rehearsal identifiers, negative tests, rollback behavior, and rollout sequencing
  are recorded in `instrument-input-cutover.md`.

## Candidate cutover status for feedback

- `feedback_category` and `feedback_severity` are specialized dynamic catalogs with stable UUIDs,
  bilingual labels, lifecycle metadata, ordering, replacement, and validated global defaults.
- Public feedback writes require `categoryId` and `severityId`; copied `category`/`severity` fields
  are rejected by strict multipart decoding, backend checks, and database triggers.
- The web loads both selectors and defaults in one public batch. Email presentation resolves the
  persisted Spanish labels; the caller never supplies trusted labels.
- The web catalog index discovers authorized definitions from the database, and the contextual
  feedback editor supports bilingual create/edit drafts, review, approval/rejection, remote search,
  manual order fields, and scoped-default reassignment. Mobile exposes the same strict category and
  severity editors through its persisted-definition index.
- Production inventory observed one category value, `idea`, but did not capture severity. The
  production dry-run must therefore prove the missing relationship and will withhold the row if it
  cannot do so unambiguously. It will never infer the persisted `p2` default.
- Full schema, per-value identities, migration rehearsal, failure gates, and rollout order are in
  `feedback-catalog-cutover.md`.

## Candidate cutover status for social event types

- `social_event.event_type_id` is now the only runtime relationship. Event create, update, list
  filtering, detail/list presentation, and provider imports no longer read or write
  `metadata.eventType` or accept a code/label/slug as identity.
- The persisted `event-types` bootstrap contains 15 bilingual published identities and one
  `social-event/global` default. Backend and database validation require matching catalog/workflow,
  active state, effective dates, publication, and no deprecation.
- A controlled two-row rehearsal resolved `" Fiesta "` and `"concert"` uniquely, passed apply,
  no-op rerun, rollback, reapply, and negative safety tests with two evidence and two mapping rows.
  The sanitized production baseline did not capture these distinct metadata values; a current
  production dry-run must enumerate them before any write.
- The six Domo event/pricing rows remain an explicit outstanding dynamic business structure. They
  require an effective-dated venue quote-profile model referencing event type, service, currency,
  and venue; replacing only the selector would leave frontend pricing authoritative. Full evidence
  and the dependency model are in `social-event-type-cutover.md`.

## Candidate cutover status for social event workflow state

- `social_event.workflow_state_id` is now the only lifecycle relationship. Runtime create uses the
  persisted initial default, update validates persisted transitions, filters compare UUIDs, and
  provider imports resolve UUIDs before writes. `metadata.eventStatus` is rejected.
- The public `social-event-lifecycle` definition contains nine bilingual bootstrap states, one
  initial default, 43 transitions, and persisted `public-listable`/`ticket-purchase` assignments.
  Additional states remain administrable; code retains only parser outputs and deny-by-default
  executable capability identifiers.
- Web uses the public workflow endpoint. Sensitive/internal workflows are not public and sensitive
  administrative reads require `security.read`. Mobile snapshot schema v6 validates a separate
  versioned workflow cache and carries no emergency workflow list.
- A four-row rehearsal passed apply, no-op rerun, exact rollback, reapply, evidence immutability,
  seven database negatives, and three atomic migration-abort cases. Production distinct statuses
  remain unknown and must be enumerated in the fresh dry-run. Full evidence is in
  `social-event-workflow-cutover.md`.

## Candidate cutover status for DDEX references and lifecycle

- Governed `ddex_standard_version`, `ddex_standard_support`, `ddex_message_type`,
  `ddex_vocabulary`, and `ddex_code` rows own reference identities, bilingual metadata,
  provenance, runtime support, ordering, deprecation, and revision. The reviewed snapshot contains
  ERN 4.3.2, RIN 2.1, MEAD 1.1, and DSR architecture 1.4; only the implemented ERN 4.3.2 path is
  enabled for validation/import/export.
- Documents, partner policies, and exports use canonical foreign keys. The backend rejects legacy
  family/version/message/status and `allowedVersions`/`ernVersion` writes; the web reads the one
  governed reference endpoint, and both generated clients derive UUID contracts from OpenAPI.
- A bounded PostgreSQL rehearsal passed dry-run, apply, no-op rerun, exact rollback, reapply,
  immutable evidence, legacy-string/invalid-transition/deactivation/hard-delete negatives, plus
  atomic threshold and unknown-version failures. A fresh candidate startup and a second startup on
  the same schema were healthy and validated all five DDEX reference foreign keys.
- Five sensitive operational workflows now own 23 states for validation, import planning, import
  execution, export, and queued jobs. Four job operations and three import-change operations are
  persisted read-only technical registries. Their typed snapshot carries IDs and bilingual labels;
  operational rows store foreign keys and database guards reject legacy strings or invalid
  transitions.
- The operational PostgreSQL fixture mapped six records, recorded six source rows/five workflow
  mappings/two operation mappings, passed no-op rerun, exact rollback, reapply, and six negative
  integrity checks with a batch size of one.
- Validation reports now reference three persisted results, three severities, and four layers. A
  four-field fixture passed dry-run, batch-size-one apply, no-op rerun, exact rollback, reapply,
  immutable evidence, and seven negative checks. Parser constructors remain an allowlisted
  exhaustive adapter and no longer supply report labels or stored identity.
- Production DDEX distinct values have not been captured in the dated sanitized baseline. A new
  read-only production inventory and zero-ambiguity dry-run remain mandatory before any write.
  Upload storage, preview, conflict resolution, commit, and export rendering return `501`; their
  former fake UI actions and placeholder client modules have been removed. This slice establishes
  safe reference identity, not a completed DDEX engine.

## Candidate cutover status for operational pipelines

- `pipeline_card.service_offering_id` and `pipeline_card.workflow_state_id` are now the only
  runtime relationships. Routes capture workflow UUIDs, writes accept UUIDs, and copied
  `service_kind`/`stage` values are rejected after cutover.
- Six internal definitions persist 35 bilingual states, six scoped initial defaults, 180 direct
  transitions, and 11 explicit service-offering bindings. The former Haskell and mobile stage
  lists and sample boards were removed.
- Web renders workflow tabs and columns from persisted definitions. Mobile obtains definitions and
  cards in one typed, revisioned snapshot request, strictly validates cross-references, and retains
  a last-known-good cache without invented emergency cards.
- Production inventory observed four deterministic values: mixing `Brief`/`Prep` and mastering
  `v1`/`Approved`. The disposable rehearsal mapped those four values with zero unresolved,
  ambiguous, or conflicting identities and passed rerun, rollback, reapply, immutable evidence,
  threshold, unresolved-stage, and conflicting-UUID checks. A current production dry-run remains
  mandatory. Full evidence is in `pipeline-workflow-cutover.md`.

## Ambiguity policy applied

No production value has been merged, corrected, or converted during inventory. Deterministic
evidence supports grouping only when stable IDs, unique normalized codes, explicit aliases, or
multiple conflict-free fields agree. The following are withheld for review unless later evidence
proves identity:

- `Femenino` in favorite genres.
- Free-form compound genre strings such as `ROCK, BLUES, LATIN` and
  `Rock alternativo, hip-hop, trap`.
- Service rows that may be translations versus genuinely different offers.
- Any future input-row microphone/model value not present in the reviewed per-track map; this slice
  never infers an instrument from an equipment label alone.
- Any production feedback row whose severity is null, unknown, conflicting, or not uniquely
  matched; the form default is not evidence about historical intent.
- Any production social-event type that does not uniquely match an active, effective, published
  UUID; the `party` default is not evidence about historical intent.
- Domo's `photo` presentation key until it is reviewed against `photo-session` together with its
  venue quote profile and booking relationship.
- Artist slug normalization variants whose public URL history must be preserved.
