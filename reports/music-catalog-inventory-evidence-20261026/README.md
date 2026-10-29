# Music catalog inventory evidence — 2026-10-26

This archive records one root-reviewed, read-only production observation at
17:47 UTC. It contains aggregate counters and fixed operational provenance;
no raw business rows, titles, owners, asset IDs, object keys, URLs or failure
messages were exported. The exact execution receipt is preserved byte-for-byte
at `observation.json` (SHA-256
`57164d673611d3af3eaadda84384b93319bb953e386c167d0ab6064172e59295`).
See [the inventory and migration assessment](../../docs/music-releases/live-catalog-inventory-2026-10-26.md).

## Evidence boundaries

- `collector/` preserves all reviewed source bytes, SQL, self-tests and the
  original review manifest, SHA-256
  `cf359cf1f9a704b052e17cf5c2e423e66aa346a440a6de094dff751bd38fb046`.
  Its PREPARED/NOT_EXECUTED labels, private preparation paths and original
  README describe the earlier preparation event. They are retained rather than
  relabeled after execution. The later `observation.json` is execution evidence.
- The historical `collector/self-test.log` records 13 fake/pure local tests.
  They do not query production. Separately, the observation records nine live
  aggregate query families and two inventory-session/capability preflights.
- All new SQL uses `BEGIN READ ONLY`, `SET LOCAL ROLE tdf_catalog_inventory`
  and a two-second statement timeout. SELECT/RLS/schema/timeout gaps produce
  UNOBSERVED/null counts, never invented zeros. None occurred for the eight
  fixed relations in this observation.
- The sole privileged exception is the unchanged canonical `verify_storage`
  boolean identity probe under local `postgres`. Its fixed query SHA-256 is
  `0289213686e8bfc6f04d88009c94355bfde458656df14e4f30d1b0672d1f4793`;
  the byte-exact canonical inspector SHA-256 is
  `19794399d49fc0d91653d91e6b3c8e4d2c5abd947927102e2f2f8b7f198df83e`.
  It reads no business data and grants no fallback for catalog permissions.
- `production-baseline-snapshot.json` is a sanitized excerpt of a separate,
  earlier 07:06 UTC runtime inspection: API `4dac175`, 213 migration records
  and all five music flags false. It is not an atomic part of the catalog query
  or proof that the repository's 222-migration main baseline was deployed.
- Zero structural gaps are vacuous with empty canonical asset/release/version
  tables. They establish no nonempty upload, storage integrity, rights,
  indexing, playback, performance or physical-device acceptance.

## Safe local replay

From repository root:

```sh
python3 -B reports/music-catalog-inventory-evidence-20261026/verify-archive.py
```

This validates hashes, Python/JavaScript syntax, archived count relationships
and the reviewed 13 fake/pure tests. It executes no SSH, Docker, SQL, storage
request or production operation. `offline-replay.log` records its execution.

Do not run the archived `prepare-spec.py`, `build-review.py`, `remote-payload.py`
or `launch.mjs` as local replay. They are original operational evidence with
historical private preparation paths. Any future live collection requires fresh
review of its source/manifest, the existing canonical identity guards and the
operator's existing dedicated SSH identity. This archive authorizes no rerun,
role/grant/flag change, migration, processing job, import or publication.
