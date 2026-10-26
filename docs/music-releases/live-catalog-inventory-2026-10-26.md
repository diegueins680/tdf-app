# Live audio catalog inventory and migration boundary

On **2026-10-26 at 17:47 UTC**, a reviewed read-only collector observed an empty
canonical music catalog and **65 legacy artist-release rows**. Eight legacy rows
contained a nonblank primary reference; none contained a nonblank YouTube
reference. The canonical catalog is not populated merely because legacy release
cards exist. This snapshot does not complete the upload-to-playback mission.

Documentation baseline: protected main
`78fa351b469c1d2031df3232204dd722ac480e3b`. This increment archives evidence and
an import/activation plan. It changes no application, database, role, flag,
processing job, original file, legacy link or publication state.

## Observed counts

| Fixed aggregate | Observed result | Meaning and limit |
| --- | --- | --- |
| Canonical `music_asset`, all fixed role/state buckets and `__other__` | 0 | Database rows only; storage objects were not listed or fetched. |
| Canonical releases / release versions | 0 / 0 | No canonical published release was exercised. |
| Canonical processing jobs / upload sessions | 0 / 0 | No live processing/retry journey was exercised. |
| Named ready, published and structural reference gaps | All 0 | Vacuous in this empty catalog; no nonempty integrity or playback acceptance follows. |
| Legacy `artist_release` rows | 65 | Rows, not necessarily 65 unique recordings, files or rights-cleared releases. |
| Legacy nonblank `spotify_url` / `youtube_url` references | 8 / 0 | Field presence only; no provider classification, availability or ownership was inspected. |

The historical `spotify_url` slot is overloaded: the source upload path in
`FanHubPage` stores a Drive result through `buildReleaseStreamUpdatePayload`.
The eight references therefore cannot be described as eight Spotify tracks,
eight native audio files or eight authorized sources. No individual reference,
artist, owner, title, asset identifier or original was exported or fetched by this
inventory. Existing links remain intact.

All **nine fixed aggregate families** and both inventory-session/capability
preflights succeeded. The eight allowlisted relations had SELECT available and
no unestablished RLS coverage. Consequently **no table-permission blocker was
observed for this scope**. This does not grant access to other sources or justify
changing privileges if a future collection fails.

The [machine-readable report](../../reports/music-catalog-inventory-2026-10-26.json)
and [evidence archive](../../reports/music-catalog-inventory-evidence-20261026/README.md)
retain the complete fixed counters and provenance. Original execution receipt
SHA-256: `57164d673611d3af3eaadda84384b93319bb953e386c167d0ab6064172e59295`.

## Collection and verification boundary

Root reviewed manifest
`cf359cf1f9a704b052e17cf5c2e423e66aa346a440a6de094dff751bd38fb046`
and executed the collector once. The canonical Docker socket, Compose identity,
container images/network/volume, PGDATA and local database socket checks are
reused without modification. Before/after container identity must match; only a
fingerprint leaves the host.

Every new SQL statement begins `BEGIN READ ONLY`, uses
`SET LOCAL ROLE tdf_catalog_inventory`, caps statements at **two seconds**, and
rolls back. Missing SELECT, schema, RLS coverage, query timeouts or invalid output
produce **UNOBSERVED with null counts**, never an invented zero. The sole
privileged exception is the unchanged canonical `verify_storage` fixed boolean
probe under local `postgres`; it checks database/socket/read-only/data-directory
identity and reads no business rows. There is no privileged fallback for the
aggregate queries, grant, DDL or role reconfiguration.

The archive separates **13 fake/pure local tests** from the **actual live SQL
observation**. The tests check wrappers, bounded command construction,
permission/timeout handling, output allowlists and identity drift. They do not
prove production authorization or mathematical correctness. Actual live SQL
parsed and executed for the nine aggregate families in this one observation.
Transactions were sequential, not an atomic platform-wide snapshot.

The portable replay command checks archived hashes, Python/JavaScript syntax,
count relationships and those 13 tests. It performs no SSH, Docker, SQL or storage
request:

```sh
python3 -B reports/music-catalog-inventory-evidence-20261026/verify-archive.py
```

## Coverage still missing

The [source audit](universal-audio-audit-2026-10-25.md) remains the dependency map.
This collection covers canonical music tables and legacy artist-release reference
presence. It does not inventory audio in directory portfolios, fan-club
posts/memories, events, generic Drive uploads, radio, live sessions, podcasts or
other contextual uploads. It does not deduplicate recordings across those
surfaces or establish that they contain no audio.

No object listing or HEAD request was made. Storage-only orphans, original
master preservation, actual MIME/codec/bitrate/loudness, expired URLs and delivery
availability remain unobserved. Database-reference counts do not establish any of
those facts, rights, visibility, search indexing or authorization.

## Phase impact

Production universal playback is **not delivered** by the observed state:
canonical music is empty, the 65 legacy rows have not been reconciled, and the
recorded music flags are intentionally OFF. P0 web-player improvements and
synthetic decoder tests remain separate from an enabled, rights-cleared
production music journey. P1 historical catalog coverage is incomplete; P2 SLO
and physical-device acceptance remain unmeasured here. Zero canonical rows or
zero reference gaps establish no missing-object health or rights conclusion.

## Historical import and reconciliation plan — not executed

1. Build a private, owner-authorized census of the 65 legacy rows and the remaining
   source contexts. Keep individual references/ownership information out of public
   audit artifacts. Preserve stable legacy links and their artist/context
   relationships; do not rewrite or remove them during census.
2. Review uploader/owner authority, rights, intended visibility and source type for
   each proposed import. Provider/context pages remain external references unless
   independently authorized media is supplied. A link or a public card is not a
   rights declaration or permission to download and republish an original.
3. For eligible authorized uploads, use the existing canonical release, recording,
   asset and processing lifecycle. Design an idempotent provenance mapping from
   each legacy context to one canonical recording; share contextual references
   rather than duplicate masters. Imports start **private/unpublished**. No
   automatic import, download, copy, publication or processing job is introduced
   by this documentation increment.
4. Validate original/rendition references and processing readiness independently
   from rights, editorial publication and search visibility. Reconcile database,
   storage and catalog projections with separately approved, bounded tooling.
   Retain originals and user metadata; ambiguous attribution, missing rights or
   unavailable media becomes an explicit review item, not a guessed repair.
5. Re-run a reviewed aggregate audit after an approved cohort and reconcile counts
   against the private provenance ledger. Test duplicate events, retry/idempotency,
   revoked access and rollback before broader import. This observation contains no
   import ledger, backfill implementation or successful migration evidence.

An empty canonical database does not justify deleting storage or legacy rows.
Future import rollback must distinguish newly staged private records from
existing user assets and preserve original masters, attribution and stable links.
No such rollback drill or production mutation occurred here.

## Activation and release gates

A **separate earlier 07:06 UTC runtime snapshot** recorded API commit
`4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`, 213 migration records and all five
`music_releases.*` flags false. The repository baseline has 222 manifest entries;
this is not evidence that those entries were deployed, nor authority to apply a
numeric difference blindly. Compare actual checksums/dependencies and use the
existing approved deployment mechanism before any activation decision.

The catalog observation changed no flags. Keep migration compatibility, backend
and worker readiness, private storage/delivery authorization, owner/rights review,
moderation and operational rollback as explicit prerequisites to a limited
cohort. Publication and discoverability must remain permission-governed even
when authoring or processing becomes available.

A real authorized production upload → processing → publication → search →
selection → delivery → persistent playback journey remains **UNVERIFIED**.
Startup/rebuffer/start-success SLOs, representative long-form audio, physical
mobile devices, background/lock-screen behavior and production rollback also
remain incomplete. Synthetic browser/decoder results from other delivery
increments retain their original scope and do not establish these outcomes.

## Traceability and release scope

The user inventory/migration/privacy requirements lead to the fixed aggregate
queries, reviewed manifest, original execution receipt, archived self-tests and
the migration/activation gaps above. This is operational evidence, not a new
production-logic contract or a theorem. All 145 canonical requirement objects,
1,339 catalog decisions and the 222-entry migration manifest remain exact against
main. The docs-only PR can be reverted without changing production behavior.
