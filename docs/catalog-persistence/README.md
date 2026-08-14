# Persisted catalog program

Status: implementation in progress. This directory records the immutable baseline, architecture,
migration evidence, verification results, and rollout decisions for the repository-wide catalog
persistence cutover.

The production schema gap, authoritative migration order, CI rehearsal, deployment gates, and
rollback boundary are documented in `production-schema-cutover.md`.

## Baselines

- Root repository: `ce0c3bc19e2d9030e871480e9e93790940c9eb12`
- Mobile submodule: `75c1fbb9a61ca80549ede8c7908d013f2a8f4c83`
- Production backend observed on 2026-08-07:
  `fc09aed0899476d3617933e6103d7c00995d1fdd`
- Production Fly releases observed: `2077` (`ord`) and `2078` (`lax`)
- Production database: PostgreSQL 17.2, 184 public base tables

Production inspection is read-only and excludes secrets, credentials, free-form reasons, message
bodies, contact fields, notes, payloads, metadata, URLs, hashes, and external identifiers.

## Reproducible reports

- `scripts/catalog-list-audit.mjs` discovers lists, unions, sum types, option arrays, switches,
  OpenAPI enums, JSON registries, SQL constraints, environment allowlists, consumers, exact
  duplicates, and normalized variants. CI requires an explicit reviewed decision and justification
  for every fingerprint and rejects stale decisions.
- `scripts/production-catalog-inventory.mjs` runs a bounded, anonymized production inventory inside
  `BEGIN TRANSACTION READ ONLY` with statement and lock timeouts.
- `reports/static-list-inventory.json` is the machine-readable static baseline.
- `reports/list-consumer-matrix.csv` is the review-friendly consumer matrix.
- `reports/production-distinct-values.json` is the sanitized production baseline.

Do not treat the current report as proof that all candidates should become database rows. Each
candidate must receive one reviewed classification: dynamic business catalog, governed reference
data, security/system registry, or genuine technical constant. Test fixtures and generated clients
are consumers, not authorities.

## Candidate verification evidence

The current candidate includes normalized Records, typed authored-content CMS metadata, and an
explicit CMS cutover rehearsal.
Against the disposable PostgreSQL 17 integration database, the source inventory found 78 published
items: 67 releases, 6 standalone recordings, and 5 sessions. All 78 mapped with deterministic
provider evidence; ambiguous, unresolved, and rejected counts were zero. Three collection-level
resources also passed validation.

The Records migration was applied twice with the same run code. The second execution created no
duplicate identities, memberships, resources, mappings, or audit events and did not increment a
resource version without a semantic change. The non-destructive rollback ran twice, retained the
normalized graph, provenance, aliases, and audit history, and left the legacy CMS digest unchanged.
The integration also consolidated 67 duplicate `primary-stream`/`primary-audio` junctions from an
earlier candidate without deleting any release or external-resource entity. The assertion ended
with `Records CMS backfill PostgreSQL integration checks passed`.

Candidate startup with `SEED_DB=false` left the three legacy Records CMS rows byte-for-byte stable:
count 3 and digest `e978ebd39aa2d3be541c0a7a2cd98fe7` before and after. The public typed feed then returned
67 releases, 6 recordings, and 5 sessions at revision 246 with no repeated release resources. Its
ETag conditional path returned 304 for `"catalog-246"`. After rebuilding the seed with the
canonical `primary-audio` relationship, two consecutive `SEED_DB=true` starts converged at revision
247: 31 catalog definitions, 2 authored contents, 67 releases, 6 collection recordings, 5 sessions,
3 collections, zero `primary-stream` relationships, and zero duplicate release/resource pairs. The
second start did not change the feed revision or recreate the repaired junctions.

The typed Label Projects backfill selected two fixture notes, preserved one supplied UUID,
generated one deterministic UUID with source provenance, and reported zero ambiguous or rejected
rows. Apply, no-op rerun, rollback, and no-op rollback each passed twice. Candidate startup repaired
an earlier accidental `(active, updated_at)` uniqueness constraint into the intended non-unique
lookup index. Authorized CRUD returned versions 1 and 2, a stale update returned 409, soft deletion
returned 204, and a repeated deletion returned 410; anonymous access returned 401.

The general catalog backfill now maps services by exact normalized label plus matching service kind
rather than positional integer IDs, because seed order is not stable across installations. The
PostgreSQL integration repaired deliberately retained bad positional assignments, verified all
nine fresh-install source rows against their expected canonical service, then proved a no-op rerun
by hashing IDs, rates, pricing models, and optimistic versions. General catalog rollback and the
security-registry integration both passed. The scripts now accept both upgrade schemas that retain
`party_role` and clean schemas where that legacy table never existed; the adapter is temporary and
never recreates a legacy writer. Exact legacy tax basis points create governed references with
source/version metadata, conflicting code/rate identities abort the transaction, and all service
pricing, tax, and currency relationships finish on foreign keys with copied codes cleared. On
2026-08-11 both final paths passed dry-run, apply, no-op apply, rollback, and no-op rollback: the
clean path covered 19 mappings and zero legacy roles, while the upgrade path covered 57 mappings
including 26 legacy role rows. Both retained their emergency-administrator counts.

The genre cutover now covers social artist profiles, core artist profiles, and fan favorite
genres. All three write contracts accept only ordered genre UUIDs, validate active published rows
inside the backend transaction, reject duplicate or unknown IDs, and expose localized labels only
as presentation data. PostgreSQL upgrade rehearsal created and validated the core-artist and fan
membership tables, their genre foreign keys, indexes, and active/publication triggers. A synthetic
legacy fan value `Rock, Soul` backfilled to `{rock,soul}` at positions `{0,1}`; the HTTP contract
then accepted a canonical UUID write and rejected the copied-label field, an unknown UUID, and a
duplicate UUID with 400 responses and no partial rows. Upgrade rerun retained 21 genres and the
same 10 core-artist memberships with zero duplicates. A separate clean database produced 21
genres, 10 core-artist memberships, no legacy genre writes, valid constraints, and a healthy API.

The candidate Radio cutover now uses canonical `country_reference` and `genre` UUIDs. Stream
search, stream upsert, and transmission creation no longer accept copied country codes/names,
genre labels, or slugs; responses resolve Spanish presentation labels exclusively from persisted
rows. CSV country/genre and ICY genre metadata remain untrusted external observations: exact unique
active matches may populate `radio_stream.country_id` or `genre_id`, while unresolved or ambiguous
values are preserved in immutable, domain-specific observation and candidate tables for review.
The former three frontend-curated stations are idempotent database bootstrap rows with country
foreign keys, and the web widget loads stations, countries, and genres from the APIs. Retained
`radio_stream.country` and `radio_stream.genre` columns are migration evidence only.

Radio browser-broadcast auto-stop policy is no longer the frontend array
`[0,30,60,90,120,180]` or the hardcoded default `120`. It is a specialized
`radio_auto_stop_option` catalog with bilingual labels/descriptions, an explicit validated
`duration_minutes`, normal catalog lifecycle fields, and a global `catalog_scoped_default`
relationship. New installations seed six published options and one 120-minute default without
overwriting a later administrative choice. Database triggers reject out-of-range durations, a
second active default, a default pointing outside the active published catalog, deactivation of
the current default, and hard deletion. The authenticated typed endpoint returns the catalog ID,
cache revision, canonical option UUIDs, durations, localized presentation labels, and exactly one
default. The Radio widget stores the selected option ID, derives behavior from its persisted
duration, and refuses to start a browser transmission when policy cannot be validated. A new
`/configuracion/catalogos/radio-auto-stop` interface creates typed drafts and supports submit,
approve, reject, revision comparison context, and default reassignment through the existing
audited publication workflow.

Application appearance is now the specialized public `appearance_mode_option` catalog instead of
independent `system`/`light`/`dark` option arrays. Persisted rows own bilingual labels,
descriptions, ordering, publication state, availability, UUID identity, and the single global
`catalog_scoped_default`; the closed codes remain only as exhaustive renderer discriminants. Web
and mobile persist the selected UUID plus its code, migrate legacy code-only storage, and fall back
to the published default when an ID becomes inactive or disappears. Startup requires exactly the
three code-recognized rows and one active published default, while database constraints reject
unknown codes, competing defaults, default deactivation, and hard deletion. Web exposes a central
`/configuracion/catalogos` index and a typed Appearance draft/review interface. Mobile snapshot
schema v4 introduced appearance in the boot batch; schema v5 added event types; schema v6 added the
persisted social-event workflow; schema v7 added event-moment reaction types; and current schema v8
adds the separate Fan Club content-reaction catalog. It keeps a last-known-good snapshot and uses a
marked emergency set only while upgrading an older cache or when no valid cache exists. An older
snapshot clears its ETags and must obtain missing data before catalog-dependent writes are enabled.

The authenticated web and mobile applications now populate their central Catalogs surfaces from
protected `catalog_definition` rows rather than treating a compiled menu as the definition list.
They provide locale-aware discovery for every authorized definition. Strict native editors cover
Appearance, Radio auto-stop, Feedback categories, Feedback severities, event-moment reaction types,
and Fan Club content-reaction types: they use canonical
item/revision UUIDs, remote item search, bilingual fields, scoped defaults, optimistic base
versions, and the same draft/submit/approve/reject endpoints. Unsupported `entity_kind` values deny
writes and stay read-only until a typed editor exists. Backend capabilities and distinct-approver
rules remain the authority; client visibility never grants access.

The governed country foundation is generated deterministically from the official bilingual UN M49
snapshot dated 2026-08-11. It validates 248 UN identities per language, cross-checks alpha-2,
alpha-3, and numeric codes, and adds the separately reviewed ISO 3166/MA `TW`/`TWN`/`158`
supplement because UN M49 omits it. The resulting 249-row snapshot records source version and sync
date; semantic upserts increment versions only when source values change and deactivate stale
snapshot rows instead of deleting them. Two consecutive disposable-database starts converged at
249 active rows and an unchanged aggregate version of 251.

The unused authenticated `/countries` endpoint and its two-field `CountryDTO` were removed after a
repository-wide caller search found no backend, web, mobile, generated-client, integration, or test
consumer. New installations no longer create the duplicate legacy `country` table. Existing
installations retain that table untouched as rollback/migration evidence; it has no runtime model,
route, or write path and is not treated as authoritative. Country selection is served only through
the typed `countries` catalog backed by `country_reference`.

User locale preferences now accept only canonical `localeId`, `currencyId`, and optional
`countryId` values. The backend validates active, non-deprecated references plus deployment
enablement for locale and currency, stores UUIDs, and clears all three copied codes on every new
write. Responses resolve locale, ISO currency, and ISO alpha-2 codes from the referenced rows as
read-only presentation data. The deterministic backfill records every original code as immutable
mapping evidence, rejects ID/code conflicts, writes the foreign keys, and clears the copied
columns; rollback restores the evidence and previous writer shape.

Backend `SUPPORTED_LOCALES` and `SUPPORTED_CURRENCIES` now restrict persisted deployment
enablement rows rather than acting as an independent catalog. Startup requires an exact match and
one persisted default, while semantic upserts leave revisions and timestamps unchanged when the
configuration is unchanged. Public locale/currency pages expose that default as a scoped canonical
UUID, and their enablement versions participate in both page and batch ETags so a deployment-policy
change invalidates caches. The web application removed its locale/currency allowlist environment
variables and loads both choices and their defaults from one public catalog batch. Mobile stores the UUIDs in its
last-known-good settings, reconciles older code-only settings against the versioned snapshot, and
never sends bundled emergency IDs to the backend. Country selection remains offline-searchable
and UUID-based. The obsolete `supported_currencies` table has no runtime model or writer; existing
installations retain it only as migration/rollback evidence, while new installations use
`currency_reference` plus `deployment_currency_enablement` exclusively.

The instrument/input-list cutover now separates musical purpose from physical equipment. Input
rows store a published `instrument_id` plus an active microphone/DI `asset` foreign key; Live
Session intake and musician writes store `primary_genre_id` and `instrument_id`. Copied strings are
rejected by strict multipart parsing, backend validation, and database triggers. The reviewed
21-row migration maps four instrument identities and 13 physical assets, preserves per-row source
evidence, aborts on missing or ambiguous candidates, and has a guarded idempotent rollback. The
complete mapping and rehearsal evidence are in `instrument-input-cutover.md`.

Feedback category and severity are now specialized persisted catalogs rather than frontend arrays
and backend string allowlists. Public writes require canonical UUIDs, the web batches both
localized selectors and their scoped defaults, notification labels come from persisted rows, and
database triggers reject copied strings or inactive/unpublished references. The guarded migration
records each original value, aborts on null/unknown/ambiguous identity, is idempotent, and restores
only unchanged canonical targets. The production inventory's lone `idea` row has no captured
severity evidence, so production remains fail-closed pending a current dry-run. Full identities,
rehearsal results, and rollout order are in `feedback-catalog-cutover.md`.

Social event types now use `social_event.event_type_id` throughout create, update, filtering,
presentation, and external discovery imports. Web and mobile batch published bilingual options and
the persisted `social-event/global` default; mobile snapshot schema v6 fails closed for creation
while only emergency or incomplete legacy data is available. The backend and database reject missing, inactive,
ineffective, unpublished, deprecated, unknown, or copied metadata values. The guarded two-row
rehearsal mapped `" Fiesta "` and `"concert"`, proved a no-op rerun and exact rollback, and retained
per-row evidence. Domo's six combined type/pricing rows are intentionally still reported: they
require a specialized venue quote-profile model, not just substitution of the global selector.
Full contracts, evidence, and rollout boundary are in `social-event-type-cutover.md`.

Social event lifecycle state now uses `social_event.workflow_state_id`. The persisted public
workflow owns bilingual state labels, the initial default, allowed transitions, behavior
capability assignment, ordering, and cache revision. `metadata.eventStatus` is migration evidence
only and new writes reject copied status codes, labels, and slugs. Web reads the permission-safe
public workflow endpoint; mobile snapshot schema v6 stores and validates a versioned workflow
snapshot with no invented emergency states. Full mapping, authorization, offline, rollback, and
rehearsal evidence is in `social-event-workflow-cutover.md`.

Event-moment reactions now use `event_moment_reaction.reaction_type_id` throughout backend and
mobile writes. The specialized public catalog owns UUIDs, bilingual names/descriptions, symbols,
order, lifecycle, and replacement metadata; the API rejects legacy strings and resolves
presentation metadata from the referenced row. Web and mobile provide strict catalog editors,
while mobile snapshot schema v7 keeps a last-known-good reaction page and keys offline selections
by UUID. A three-row PostgreSQL fixture passed dry-run, apply, no-op rerun, exact rollback,
reapply, evidence immutability, and negative integrity guards. Full contract, mapping, and rollout
evidence is in `event-moment-reaction-cutover.md`.

Operational Kanban stages now use persisted workflow and state UUIDs. Six internal workflow
definitions own 35 bilingual stages, six initial defaults, 180 allowed direct transitions, and 11
explicit service-offering bindings. The Haskell stage module and mobile demo boards were removed;
backend, web, OpenAPI, and mobile now use `service_offering_id` and `workflow_state_id`. Mobile
loads one revisioned snapshot request and retains only a strictly validated last-known-good cache.
The four-row PostgreSQL rehearsal passed dry-run/apply/no-op rerun/exact rollback/reapply and three
atomic safety failures. Full identity mapping, offline behavior, startup gate, and rollout order
are documented in `pipeline-workflow-cutover.md`.

The DDEX reference and operational cutover now persists official standard/version provenance
separately from runtime support. Documents reference `ddex_standard_version`,
`ddex_message_type`, and the sensitive `ddex-document-lifecycle` state by foreign key; partners use the ordered
`ddex_partner_standard_version` junction and exports accept `exportStandardVersionId`. The strict
OpenAPI contract and both generated clients contain no version-string write field. A disposable
PostgreSQL 16 rehearsal covered one document, one export, and a partner with two repeated legacy
version values: dry-run mapped all values deterministically, apply in batches of one recorded four
source rows/four catalog mappings/one workflow mapping, rerun was a no-op, rollback restored the
exact duplicate array and legacy fields, and reapply restored canonical references. Negative
threshold and unknown-version runs aborted atomically. Two candidate startups (fresh and existing
schema) returned healthy, retained four standards/one executable message/12 lifecycle states, and
validated five explicit DDEX foreign keys. Full evidence and remaining runtime limits are in
`ddex-reference-cutover.md`.

Five additional sensitive DDEX workflows persist 23 validation/import/export/job states. Jobs and
import changes reference four and three specialized operation rows respectively. A six-record
PostgreSQL rehearsal passed deterministic dry-run, batch-size-one apply, no-op rerun, exact
rollback, reapply, immutable evidence, and legacy-string/transition/deactivation/delete negatives.
The permission-protected reference snapshot returns IDs and bilingual metadata for every workflow
and operation; fake web actions and unused placeholder backend modules were removed.

Validation reports also use canonical registries: three results, three severities, and four
layers. The contract returns UUIDs, stable codes, and bilingual labels instead of serialized
Haskell constructor names. A four-field rehearsal passed dry-run, batch-size-one apply, no-op
rerun, exact rollback, reapply, immutable evidence, and seven negative integrity checks.

Directed verification after the latest changes:

- Hardcoded-list CI audit: passed across 872 files and 522 candidates with no unreviewed or stale
  decisions. The generated JSON inventory and CSV consumer matrix now both include those reviewed
  decisions rather than an intermediate unreviewed scan.
- Records PostgreSQL forward/rerun/rollback integration: passed.
- Label project-note PostgreSQL dry-run/forward/rerun/rollback integration: passed twice.
- General catalog per-value mapping/idempotency/rollback integration: clean-install and
  legacy-upgrade paths both passed, including exact tax-reference provenance and copied-code
  removal.
- Security registry PostgreSQL integrity integration: passed.
- Latest event-moment reaction checks: three legacy values mapped with zero unresolved or
  ambiguous identities; dry-run/apply/no-op rerun/exact rollback/reapply plus four negative guards
  passed on disposable PostgreSQL 16. The backend fast build and focused strict request-contract
  example, web/mobile typechecks, regenerated clients, 3 focused web tests, and 24 focused mobile
  tests passed. A first Hspec invocation used a hyphenated matcher and selected zero examples; the
  corrected quoted matcher selected and passed the intended example.
- Latest Fan Club reaction checks: the polymorphic target/string table was replaced by typed post
  and memory foreign-key tables plus the separate five-item `content_reaction_type` catalog. A
  two-row disposable PostgreSQL fixture passed dry-run, apply, no-op rerun, exact rollback,
  reapply, and unknown-ID/referenced-deactivation/hard-delete/evidence-deletion guards. Backend fast build,
  strict request parsing, web typecheck and 7 focused tests, mobile lint/typecheck and 21 focused
  tests passed; generated OpenAPI clients are byte-identical. Full evidence is in
  `content-reaction-cutover.md`.
- Backend Records contract and validation tests: 5/5 passed; Haskell fast build passed.
- Latest genre contract checks: Haskell executable and test suite compiled; 15 focused artist/fan
  examples passed. Web genre/API/editor checks passed 17 focused tests, both generated clients were
  regenerated, and backend/web/mobile typechecks passed.
- Latest Radio/country checks: the full 132-module backend executable compiled and 18 focused Radio
  examples passed across strict contracts and explicit country/genre removal. The governed snapshot
  tests passed 2/2 and the deterministic generator `--check` verified all 249 identities. The
  regenerated web client and Radio API passed 14 focused tests; web and mobile TypeScript
  typechecks passed. The disposable PostgreSQL integration applied the general backfill twice and
  rolled it back twice with 65 mappings, zero unresolved/ambiguous/rejected rows, stable country and
  genre observation counts, and explicit negative checks for unknown/inactive UUIDs and attempted
  observation deletion. The rerun held the exact same 65 mappings, including locale, currency, and
  country mappings for a stable preference fixture.
- Latest Radio auto-stop checks: the candidate migrated and seeded six specialized options with
  one active 120-minute global default, and an authenticated smoke request returned all six in a
  typed revision-1 envelope. The PostgreSQL integration again held exactly 65 mappings across
  apply/rerun and passed both rollback runs; it additionally rejected a second default, an
  out-of-range duration, default deactivation, and hard deletion. The focused Haskell draft
  contract passed 2/2 examples, the web policy/API tests passed 7/7, the generated-client plus
  Radio API set passed 15/15, and web/mobile typechecks passed. Both OpenAPI clients were
  regenerated. The first Haskell test invocation with a spaced `--match` and the first accented
  `Char8` fixture were invocation/fixture errors; the corrected ASCII fixture and `--match=auto-stop`
  run passed.
- Latest appearance checks: the final backend executable compiled and the strict typed Haskell
  draft contract passed 2/2 examples. Three focused web suites passed 5/5 tests, including legacy
  storage migration, UUID persistence, inactive-choice fallback, and draft construction; two
  focused mobile suites passed 8/8 tests, including v2/v3 snapshot upgrade, last-known-good
  behavior, emergency fallback, and rejection of unknown/inactive cached modes. Web and mobile
  typechecks passed, and regenerating both OpenAPI clients produced the same
  `8dcf3b6bb300edad40f4d60476bef00b90ed67c4` SHA-1. The disposable PostgreSQL integration passed
  dry-run/apply/rerun/two rollbacks with 65 exact mappings and the appearance constraint negatives.
  Two seeded candidate starts converged on the same three UUIDs, aggregate option version 3, one
  `system` default at version 1, and catalog cache revision 1. Public API smoke returned all three
  rows plus the scoped default, `ETag: "catalog-1"`, and a matching conditional request returned
  304.
- Latest mobile-administration checks: the protected API wrapper, strict form adapters, and generic
  authorization error handling passed 19/19 focused tests across three suites. Negative cases
  reject unsupported entity kinds, appearance creation without an existing UUID, fractional or
  out-of-range Radio durations, and legacy copied selector fields before an API request. Mobile
  TypeScript typecheck, directed ESLint, and `git diff --check` passed. The refreshed hardcoded-list
  audit classifies each renderer/form dispatch as a justified technical boundary while retaining
  persisted definitions as the menu authority. A mistakenly root-scoped `npm test` invocation
  reached the web workspace and failed during Jest configuration on the locally installed
  `@testomatio/reporter` package; the correctly scoped mobile invocation passed and no repository
  or external state was changed by that setup failure.
- Latest regional-preference checks: the strict backend contract accepts locale, currency, and
  country UUIDs and rejects copied-code writes. The backend executable rebuilt successfully and
  its focused strict-JSON contract passed 3/3 examples. Focused web API tests passed 2/2; focused
  mobile API/settings/theme/snapshot tests passed 14/14; both TypeScript typechecks and directed
  ESLint checks passed. The PostgreSQL 17 rehearsal completed dry-run, apply, a no-op rerun,
  preservation of a newer canonical selection, rollback, and no-op rollback with 65 mappings and
  zero unresolved, ambiguous, or rejected rows. Two identical starts left all deployment
  enablement versions and timestamps unchanged; a temporary `es/USD` to `en/EUR` default switch
  and restoration passed the unique-default constraints. A deliberately unbackfilled preference
  made startup exit non-zero, after which the disposable fixture was restored. Public smoke
  returned canonical `es` and `USD` default UUIDs with `ETag: "catalog-18"`; the matching
  conditional request returned 304. Both generated clients match SHA-1
  `8fb72be1b43010dcf011edfa9a69437f522d4cda`.
- Latest instrument/input-list checks: the executable build passed, 33 focused Live Session
  examples and 2 input-list LaTeX examples passed, and 10 focused web multipart/generated-client
  tests, web/mobile typechecks, and directed web lint passed. The newly documented OpenAPI multipart
  endpoint regenerated identical web/mobile clients at SHA-1
  `09fddc9f14a66963ffd231c3553912e404ec552b`. PostgreSQL
  dry-run/apply/no-op rerun/rollback/no-op rollback/reapply mapped
  21/21 input rows with zero unresolved or ambiguous rows, validated three foreign keys, and held
  digest `b193e331daa25871a337dd4e908bf64a`. Four transactional negative cases rejected copied text,
  unknown or inactive instruments, and a missing microphone/DI reference. Two candidate starts
  were healthy and did not change the run/evidence counts or digest; the public genres/instruments
  batch returned UUID items with `ETag: "catalog-2"` and a conditional 304.
- Latest feedback checks: the executable build and 24 focused Haskell examples passed; six focused
  web tests, 10 focused mobile administration/API tests, web/mobile typechecks, directed lint, and
  OpenAPI generation passed. Both generated
  clients match SHA-256 `9b067867b60ae1450bf9ff13ef243a5c91452a44d9e3677d0f72e0aa87bb40fd`.
  PostgreSQL dry-run/apply/no-op rerun/rollback/no-op rollback/reapply mapped one controlled legacy
  row with zero unresolved, ambiguous, or rejected values and retained one run, one evidence row,
  and two field mappings. Eight negative cases covered copied/unknown/missing/inactive references,
  default deactivation, hard deletion, safety-threshold excess, and an unresolved source value.
  Candidate startup and bilingual batch smoke passed with `ETag: "catalog-2"` and conditional 304.
- Latest social-event-type checks: the guarded PostgreSQL dry-run/apply/no-op rerun/rollback/reapply
  mapped two controlled rows with zero unresolved, ambiguous, conflicting, or rejected identities,
  retained one run plus two source and two mapping rows, and rejected missing/copied/unknown/inactive
  identifiers, default deactivation, threshold excess, and an unresolved source atomically. Web API
  and collaborative-event tests passed 15/15 assertions; the focused Haskell event run passed
  114/114 examples including zero-write rejection of an unknown provider type. The mobile focused
  slice passed 72/72 assertions and the complete mobile suite passed 232/232 tests in 44/44 suites,
  with typecheck and lint green. The complete web suite passed 1,533/1,533 tests in 131/131 suites
  and its typecheck passed. Both generated OpenAPI clients are
  byte-identical at SHA-256
  `e5bdd157aee4a5b3b1dc4ccba4253986e0f257e36d4b7ef61552852708ce1d7f`.
  The full web lint completed with zero errors and 89 inherited warnings. The complete backend
  suite passed 2,245/2,245 examples. Candidate startup returned HTTP 200 for health and the
  15-item Spanish event-type batch with its one scoped default and `ETag: "catalog-1"`; a matching
  conditional request returned 304. SQL smoke checks found zero invalid social-event references
  and the integrity trigger installed. Full evidence is recorded in
  `social-event-type-cutover.md`; remaining repository and production release gates are still
  outstanding.
- Latest social-event-workflow checks: a four-row PostgreSQL dry-run/apply/no-op
  rerun/rollback/reapply mapped absent, spelling-alias, punctuation-alias, and importer values with
  four immutable source and four workflow-mapping rows. Hard deletion of evidence and seven
  database invariants were rejected; threshold excess, unresolved status, and UUID/string conflict
  each aborted atomically. The focused Haskell run passed 132/132 examples, web catalog tests
  passed 8/8, mobile workflow snapshot/API tests passed 6/6, and both TypeScript typechecks passed.
  HTTP smoke returned 200 plus ETag, conditional 304, and 404 for sensitive/internal workflows.
  Both generated clients match SHA-256
  `e5bdd157aee4a5b3b1dc4ccba4253986e0f257e36d4b7ef61552852708ce1d7f`.
- Latest operational-pipeline checks: the six-workflow/35-state/11-binding registry passed startup
  validation after a four-card cutover. Dry-run mapped 4/4 with zero unresolved, ambiguous, or
  conflicting identities; apply/rerun/rollback/reapply and immutable evidence passed with batch
  size 2. Threshold, unresolved-stage, and UUID/string-conflict runs each aborted without source
  mutation. The executable and 138-module Haskell test-suite build passed, as did 4/4 focused
  Haskell contracts, 3/3 web adapter tests, 4/4 mobile snapshot/API tests, both TypeScript
  typechecks, and directed web/mobile lint. Persisted read/create/update/delete capabilities are
  enforced independently; local HTTP smoke returned 200 with `pipeline.read` and 403 without it.
  The candidate returned HTTP 200 health after backfill. Generated clients match SHA-256
  `62d30a99cd9cfe4985791bf79e17c4d68d3c6598318057e32623de6385bbb361`.
- Web Records API tests: 2/2 passed; TypeScript typecheck passed.
- The protected OpenAPI contract and both generated clients now include content types, authored
  content, localized workflow states, and typed label project-note CRUD. The former
  `LabelProjectsPage` no longer reads or writes CMS payloads or slugs.
- Current and historical authored-content slugs both resolved the same persisted content UUID in
  the candidate API; legacy Records and Label Projects CMS slugs returned the missing sentinel.
- Directed web CMS/Label/API/access tests: 81/81 passed; web typecheck and lint passed.
- Mobile Records API, offline snapshot, catalog snapshot, and settings integration tests: 16/16
  passed; TypeScript typecheck and full lint passed.
- Earlier whole-candidate checkpoint, before the latest Radio/appearance/event-type slices:
  backend 2,248/2,248 examples. The current complete-suite checkpoint is backend 2,245/2,245,
  web 1,533/1,533 in 131/131 suites, and mobile 232/232 in 44/44 suites. These counts reflect
  intentional removal and replacement of obsolete string contracts, not missing test execution.

This evidence is local candidate evidence, not production rollout evidence. Commands and the full
suite status will be refreshed in the draft pull request before any deployment gate is considered
satisfied.

The next isolated P1 slice is documented in `creator-badge-cutover.md`: creator badge assignments
move from copied text to published `creator_badge_type` UUIDs. Notification types and their typed
destinations remain a separate governed operational cutover.

## Safety state

No production mutation, backup, migration, deployment, or external publication has been performed
for this program. The current candidate test checkpoint is green, but production rollout remains
gated by completion of the remaining catalog cutovers, a final exact-revision full-suite rerun,
production-like restore rehearsal, verified rollback, backup identifier, compatible clients, and
two coherent emergency-administrator paths. Production inspection found two active `Admin`
assignments on distinct parties, but a fresh aggregate read-only preflight found only one party
with an active credential. The legacy schema also cannot prove the complete persisted capability
graph. `security-emergency-readiness.md` records the privacy-safe evidence, reusable preflight,
and exact authorized action required; row counts alone do not satisfy the administrator-lockout
gate.

The release runner now consumes that aggregate preflight from the exact candidate Git blob. It
requires two independently authenticatable parties before migration, repeats the check after
migration, and requires two canonical coherent paths before deploying any API Machine. Database
integrity also rejects removal of the last coherent assignment, critical grant or registry row,
active credential, or emergency role. Runtime capability checks require active modules and
actions. Local verification passed 24/24 release-runner tests, 17/17 focused Haskell security
tests, the canonical and legacy preflight fixtures, and the rolled-back PostgreSQL security
integration.
