# Authorized source references in music responses

Status: source controls tested; exact-candidate compiled CI and release evidence pending.

The public release detail and personal favorites, playlists and listening-history
responses previously advertised ready audio renditions that the public delivery
endpoint could deny. A preview-only listener could receive full-rendition IDs;
one track's territory could exclude it while another track kept the release
visible. The delivery endpoint already rejected these references, but advertising
them caused inconsistent source metadata and avoidable authorization failures.

This focused correction applies the existing delivery-policy predicate to each
projected source in the four handlers. Preview/full/territory rules now govern
source metadata through the same authoritative function used before public URL
issuance. Selecting media still requires a fresh authorization request. The
projection issues no credential and does not prove actual bytes, storage
availability, rights, MIME or codec compatibility.

Existing titles, relationships, collection ordering, account predicates and
unavailable saved entries remain intact. `available` retains its current public
version identity meaning; it is not strengthened into a guarantee of playable
media. Other public metadata and existing delivery credential expiry/cache bounds
remain separate obligations.

The branch is `fix/music-source-availability-20261026`, based on protected main
`3af768dacd95c1223975c62da3ce8088bed89b47`. It is an independent focused
main-targeted increment, separate from player/search/provider-link candidates.
The four-predicate patch, runner, dedicated CI invocation and runner-only backend
scope regression are committed as
`ade78b5ab11ba297cf419a96012032ebf289820d`. The receipt binds all five source
inputs, the publication fixture and the actual baseline/candidate/scope logs.

The contract is
[music-source-projection.md](../../formal/system/music-source-projection.md), owned
by `MEDIA-MUSIC-SOURCES-001`. All 142 main requirement objects remain exact;
the new scoped requirement makes 143 total. All 1,330 main catalog decisions are
retained. Inventory, traceability and catalog classifications are checked
separately after this recorded source freeze; their outputs establish structural
coverage and freshness, not a test pass or authorization proof.

The PostgreSQL regression executes actual shipped SQL and delivery-policy
functions using valid published fixtures, then tests preview/full eligibility,
mixed territories, unavailable entries, account isolation and source-policy
agreement. Ten expected baseline agreement checks failed on the original source.
The corrected source passed four SQL surfaces and all four gate-removal controls,
which deliberately disclose a denied full source when the new predicate is
removed. The runner extracts the actual adjacent Haskell binding list and uses
whitelisted expressions to derive fixture parameters; eight omitted/reordered
argument controls pass. This is lexical correspondence, not compiled Haskell
evaluation or actual HTTP binding acceptance. All 17 CI-selection tests passed.

The local execution used Node 24.13.1, PostgreSQL server 16.13 and psql 18.2 with
the runner's relevant real migration subset. The owned random database was
removed and its cluster/listener stopped. These are finite real SQL and lexical
controls, not a full production-schema rehearsal. The handler SHA-256 is
`9b001e5a36c2dde75a8df89e898992815765451f39d6a25cdb167dde9ec74bea`;
the runner SHA-256 is
`be8d5865645e51743add2211a9bce26fc391742ddf8ea83aee2bccadf2b996d0`.
The versioned report is `reports/music-source-projection-2026-10-26.json`.

No migration, data backfill, processing job, master rewrite or infrastructure is
added. Application rollback restores the prior projection behavior, including
its known overly broad source references. Native browser/HTTP/storage acceptance,
full compiled Stack build, current-head hosted checks, independent review and
production deployment are not established by these local controls. The
application rollback is documented; no target-environment rollback drill was
performed. No production business writes are performed.
