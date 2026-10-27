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
by `MEDIA-MUSIC-SOURCES-001`. The original source-baseline metadata retained all
142 main requirement objects plus the scoped requirement, for 143 total. The
earlier fan-club main refresh retained 143 main objects plus the same scoped
requirement, for 144 total. The latest welcome integration below retains 144
main objects plus the scoped requirement, for 145 total. All 1,330 main catalog decisions are
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

## Current-main metadata reconciliation

Source qualification remains tied to actual source commit
`ade78b5ab11ba297cf419a96012032ebf289820d` and its original baseline
`3af768dacd95c1223975c62da3ce8088bed89b47`. Metadata was checkpointed at
`bc819dc938be3abff9861284052e36206ff2c48f`, then normally merged with protected
main `ed9eb9a1600f0a827e002bb2ab64a608f8352b2c`, which includes the fan-club
read-recovery integration.

All 143 current-main requirement objects, including every field of
`COMM-CLUB-001`, are retained exactly. The original `MEDIA-MUSIC-SOURCES-001`
object is appended unchanged, for 144 total. The complete 1,330-decision catalog
file is unchanged from main; no new source-fixture decision was needed. The
five source qualification inputs and pinned Mobile source
`d832bb3057dd03d44c0f35eea05be9f36320429d` remain unchanged.

Only the canonical requirement registry and four generated inventory/catalog
reports conflicted; those artifacts are reconciled from the actual combined
sources. Metadata and strict catalog checks are repeated after the normal merge.
No source SQL, build, HTTP, playback or deployment result is relabeled as a new
execution. The versioned report separates source-baseline evidence from the
current integration baseline and its metadata validation.

## Protected-main welcome integration

The next normal merge advances candidate
`80fd2a676720112166d1245b70cf7dcf03be11a9` to protected main
`a1238fbf6d98062904a4bd80d2f9df35ebc40706`, including the independent
welcome integration. All 144 main requirement objects, including the full
`UX-WELCOME-001` object, are retained exactly. The original scoped
`MEDIA-MUSIC-SOURCES-001` remains unchanged, for 145 total. The complete
1,330-decision catalog file and all five source qualification inputs remain
byte-identical to their preserved authorities. The pinned Mobile source remains
`d832bb3057dd03d44c0f35eea05be9f36320429d`.

Only canonical/generated metadata conflicts are resolved. Inventory, traceability,
source admission and strict catalog JSON/CSV checks qualify this combined metadata.
The actual PostgreSQL receipts stay tied to source baseline `3af768d` and source
commit `ade78b5`; they are not relabeled as execution of this merge. No new SQL,
compiled build, actual HTTP, browser/storage playback or deployment result is
claimed. Exact published-head CI and independent review remain required.


## Protected playback and ledger main reconciliation

This normal merge advances `744e75fcd2d0ec74e988436b2ccbad7c9cea5eaa` to protected main
`78fa351b469c1d2031df3232204dd722ac480e3b`. It preserves all 145 main requirement objects
and unchanged `MEDIA-MUSIC-SOURCES-001`, for 146 total. The inherited production
migration manifest has 222 entries and is unchanged; no migration is applied.

The complete 1,339-decision main catalog file is retained byte-for-byte. The
SQL handler and PostgreSQL runner retain hashes `9b001e5a...` and `be8d5865...`;
the two CI-scope inputs also retain their original bytes. The merged CI workflow
changes legitimately to inherit protected playback and ledger checks, so the
five-input historical receipt is not represented as wholly unchanged. Current
CI-selection and pipeline contracts are executed against that merged workflow.
Original finite PostgreSQL/lexical evidence retains its original source/runtime
identity; no fresh SQL, compiled HTTP or media-byte test is claimed here.

Generated inventory, traceability, strict catalog and source-admission checks
qualify the combined metadata independently of runtime evidence. Fresh exact-head
hosted checks and independent review remain required after publication. No push,
review request, protected-main merge or production deployment is made by this
refresh task.

## Protected catalog-search and Stripe main reconciliation

Local preparation normally merges the previously published
`ac396190351fc8b26279cc1f2611c1eff34ce271` with actual protected main
`9b7aa4648d4158f6da84fbb5e794267cd4953177`. All 146 current main requirement
objects and the unchanged scoped source-projection requirement are retained
(147 total), along with the whole 1,339-decision file and 222-entry migration
manifest. No unapproved listening, credential-cache or other branch is grafted.

The combined handler now has SHA-256
`2cc0c8634833e09dae8f42804cac61fbc2f2f7200bc9bb82e080444ea310118f`.
Its complete `listPublicReleases` function is byte-identical to current main's
recording-title search. Each of the four complete source-projection function
blocks, including its SQL and actual Haskell bindings, is byte-identical to the
qualified `ac396190` version. The runner remains byte-identical. This preserves
the relevant qualified fragments, not the old whole-handler hash.

The merged workflow retains both dedicated PostgreSQL runner steps. Both
runner-only backend selectors and their regression cases remain, while every
other main CI entry is preserved. The merged CI contract suite passed 46 tests
with zero failures under Node 22.14.0. The report records new complete-file hashes,
fragment correspondence and later qualification results separately from the
historical SQL receipts. A fresh owned PostgreSQL run, metadata gates and
committed-source admission are recorded when executed; the subset runner still
does not establish the complete production-schema, compiled HTTP or byte-delivery
boundary. The separate full-schema test follow-up is not imported here.

This is unpublished preparation. The existing remote `ac396190` review remains
untouched. Publication, a new independent review, current-head CI and production
acceptance remain separate release gates; no deployment is performed.

The fresh root-orchestrated PostgreSQL run on 2026-10-26 passed all four
projection groups, four policy-removal mutation controls and eight lexical
argument controls, using a new owned PG16 cluster and Node 22. Its source
fingerprints bind the combined handler above and unchanged runner. All random
databases were removed, the cluster stopped and port55571 verified closed; the
older search cluster was untouched. This executes the selected 14-file SQL
fixture, not the complete 222-entry schema or compiled HTTP/storage boundary.
Metadata verification totals36 passing cases:26 passed initially, while the10
conformance cases could not start until generated inventory conflicts were
resolved and then passed. The initial import failure remains recorded.

## Protected post-legacy main refresh

After #624 merged, this preparation normally merges
`62f9d4f874eed1cd5ac94f60194f289d426c4a94` with actual protected main
`2e01bbe1263e8761fa2692bd951f828c01142449`. All 147 main requirement objects
are preserved exactly, with the unchanged source-projection requirement for
148 total. The whole 1,341-decision main catalog file and 222-entry migration
manifest are retained. The newer legacy-player implementation is inherited from
protected main, not from an unapproved branch.

All five qualified source inputs are byte-identical to the previous candidate,
including the combined handler hash `2cc0c863...`, unchanged PostgreSQL runner
and the CI workflow/selection inputs. The four complete projection functions
and bindings retain their qualified fragments; the public catalog function
remains byte-identical to protected main. No PostgreSQL or Stack run is repeated
or relabeled for this metadata refresh. The earlier fresh owned-PG receipt keeps
its actual execution identity and selected-fixture/lexical scope. Appropriate
metadata, strict catalog, scope and committed-source checks are recorded in the
report and private final receipt.

This baseline is explicitly pinned to the post-#624 main above. A later main
advance requires its own reconciliation. The existing remote `ac396190` and its
review remain untouched until parent-controlled publication; no push, new review
request or production change is performed by this task.

## Protected provider-domain main reconciliation

This normal merge advances local parent
`52aa82ea70ed1e2b3dc26180977d9a0b838dea59` to actual protected main
`c66a65a10a8e5e48f89172d61f6f57fb81697896` (#596). All 148 main requirement
objects, including `PAY-DOMAIN-BINDING-001`, remain exact. The unchanged scoped
`MEDIA-MUSIC-SOURCES-001` object makes 149 total. The entire 1,341-decision main
catalog file, its provider retirement history and the 223-entry migration manifest
remain byte-exact to protected main. No production migration is applied here.

All existing runtime, UI and API source bytes/modes remain unchanged from 52aa.
The release handler retains hash `2cc0c863...`; the four projection functions,
actual bindings and public catalog search therefore retain their exact qualified
fragments. The SQL runner and both CI-selection inputs remain unchanged. The
workflow legitimately inherits main's `python3-psycopg2` build dependency while
retaining both dedicated PostgreSQL steps, so the older five-input workflow
receipt is not represented as wholly byte-identical.

Protected main also adds provider-domain SQL and updates `ProviderRetrySpec`;
whole-backend source identity is consequently not claimed. The earlier source
qualification and selected 14-file PostgreSQL/lexical controls keep their actual
source/runtime identities. No fresh Stack, PostgreSQL, compiled HTTP, browser,
full 223-schema or media-delivery qualification is claimed by this reconciliation.
The later music-search feature remains a separate stack above this parent.

Ordered inventory/conformance generation, metadata/source admission, strict
catalog JSON/CSV, API-client freshness and CI-scope checks qualify the combined
metadata separately. Exact-head hosted checks, current independent review and
protected integration remain required after parent-controlled publication. This
baseline is pinned to c66; later advances require their own reconciliation.
No production, source qualification or other active worktree is changed here.

The merged metadata passed 36 regression cases and 46 CI-selection/pipeline cases on Node 22. Strict catalog JSON/CSV checks passed with 1,341 candidates/rows and no stale/unreviewed decisions; both generated API clients remain fresh at Mobile d832bb3. Source equivalence compares this merged worktree with 52aa, not raw c66: all tracked non-test/non-SQL backend inputs remain exact, while inherited SQL/test changes make whole-backend identity false. Git-derived compilation metadata still differs on a future build; no existing binary is retagged as this candidate. Immutable source admission follows root's normal merge commit.
