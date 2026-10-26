# Universal audio release evidence — 2026-10-26

The mission remains **incomplete**. Two focused increments are merged; the canonical production music API was disabled at the last read-only observation, and no real production upload-to-playback journey is established.

## Integrated capabilities

| Requirement | Specification | Implementation and verification | Integration |
| --- | --- | --- | --- |
| MEDIA-MUSIC-PLAYER-001; PLAYER-002/003/005; LIVE-004 | [Player session model](../../formal/system/player-session.md): bounded design exploration, no implementation proof | [PlayerProvider](../../tdf-hq-ui/src/player/PlayerProvider.tsx); source-generation rejection, selected-recording authorization, bounded recovery; scoped native AAC/FLAC browser tests | [PR620](https://github.com/diegueins680/tdf-app/pull/620), merge `3c25a9de56345a8fccea4c1320b0548fed37a334`; [CI40284481007](https://github.com/diegueins680/tdf-app/actions/runs/40284481007) |
| MEDIA-MUSIC-DISCOVERY-001; MEDIA-002 | [Catalog discovery](../../formal/system/music-catalog-discovery.md) | [MusicRelease handler](../../tdf-hq/src/TDF/Server/MusicRelease.hs), [MusicCatalogPage](../../tdf-hq-ui/src/pages/MusicCatalogPage.tsx); authorized track-title search, accent/partial matching and URL pagination; actual SQL and synthetic-service browser journeys | [PR622](https://github.com/diegueins680/tdf-app/pull/622), merge `e05d003669a930d54b4fe7dd71d72d09335c11ae`; [CI40363971868](https://github.com/diegueins680/tdf-app/actions/runs/40363971868) |

PR622's actual merge has parents `78fa351b469c1d2031df3232204dd722ac480e3b` and approved head `5f45677588bd8494226e00fd51d81c134b84bdd6`. Its tree `dabd64ad893e2959fd21226206488121eaad8a24` equals the tested CI merge tree. Main has 222 migration entries; this feature adds no migration.

## Verification boundary and retained failure

CI40363971868 attempt1 failed one existing WebKit phone cart case after 565 passes and 22 declared skips. Its 30-second whole-test budget expired during the final close-cart action; no root renderer/host cause was established. One controlled unchanged-head failed-job repeat passed 566 persona cases, the same 22 skips, 10 default-off checks and 25 discovery checks. All 588 original case identities, timeouts and expected statuses were retained. The cart case took 28,912 ms of its unchanged 30,000 ms budget; that narrow margin remains a reliability risk. No timeout, skip, assertion or product change fixed it. Both attempts are retained in the accompanying evidence packet.

Playback browser evidence distinguishes real decoding from declared mock smoke cases. The [published P0 client receipt](../../reports/universal-audio-release-evidence-20261026/published-client-conditional-results.json) uses synthetic API/media responses; it does not prove production catalog, storage, rights or object delivery. The local compiled-handler build is still running in this checkpoint; no new binary identity or HTTP pass is established. Its later result must retain actual source/binary identity and must not be inferred from CI or SQL extraction.

## Pending capabilities

[PR624](https://github.com/diegueins680/tdf-app/pull/624) safely routes legacy provider/context links and keeps direct-media selection in the global player. [PR626](https://github.com/diegueins680/tdf-app/pull/626) aligns public detail, favorites, playlists and history source projection with delivery authorization. [PR629](https://github.com/diegueins680/tdf-app/pull/629) prevents listener queue/title/position persistence crossing observed account boundaries while preserving same-owner playback. At the recorded snapshot their exact heads are approved and ordinary technical checks passed; protected integration remains pending. Each composed successor needs current-main reconciliation, exact-head CI and fresh review under FIFO. Their candidate implementation and formal documents become main references only after integration.

## Production, historical catalog and performance

The [live inventory](live-catalog-inventory-2026-10-26.md) records nine observed aggregate families: 65 legacy release rows, eight nonempty primary references, and zero canonical assets, releases, versions, upload sessions or processing jobs. The primary field is overloaded; eight references do not establish provider type, playable bytes, availability, ownership or publication rights. Zero structural gaps are vacuous for the empty canonical catalog. No title, owner, object key or raw business row was exported, and no production business data was written.

Separate read-only runtime snapshots at 07:06 and 17:19 UTC observed API `4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`, 213 migration records and all five music flags disabled. These are historical observations, not a fresh state guarantee or proof that main's 222-migration backend is deployed. Existing storage, recovery and deployment gates must be satisfied before activation. Historical references require manual ownership/rights review and a private, idempotent import retaining provenance and stable links; no automatic public backfill is authorized by an empty catalog.

No representative cold/warm startup, search p95, playback-start success rate or rebuffer baseline was measured. No before/after SLO improvement is claimed. Browser functional timeouts and bundle budgets are different measurements. Physical-device background audio, cross-tab policy, complete WCAG 2.2 AA, optional normalization, non-release catalog/search coverage and end-to-end operational alerting remain open.

The [23-criterion acceptance register](../../reports/universal-audio-release-2026-10-26.json) records every AC as partial or unverified with its evidence and limit. None is declared fully met. The next work is protected integration of the reviewed fixes, compiled-handler qualification, safe backend readiness/activation, rights-reviewed historical coverage, and real target-environment listening measurements. This evidence increment changes no application code, schema, role, flag, storage object or original master.
