# Keep music credentials out of HTTP caches

The music access API issues short-lived bearer URLs for playback, artwork,
downloads, multipart uploads and DDEX packages. A local real-HTTP run of
`5f45677588bd8494226e00fd51d81c134b84bdd6` observed no `Cache-Control` header on
a successfully signed public asset response. This established a missing cache
instruction, not an actual CDN leak: the URL was not fetched and no production
cache was probed.

A narrow WAI boundary now adds `Cache-Control: private, no-store` to the eight
credential operations listed in the
[scoped contract](../../formal/system/music-credential-cache.md), including their
normal errors and GET/HEAD aliases. It replaces conflicting cache directives
and preserves all other headers, JSON and status. Both the direct application
and outer boot path use the same idempotent boundary, preserving the previous
CORS and exception-handling order.

The public catalog, release metadata, favorites, playlists, listening history,
payments and unrelated routes retain their existing behavior. API/handler
signatures, generated clients, rights checks, URL expiry and the 222-entry
production migration manifest are unchanged. No new storage or processing cost
is introduced.

From the repository root, the focused test command is:

```sh
cd tdf-hq
stack --no-terminal exec -- runhaskell -isrc -itest test/MusicCredentialCacheMain.hs
```

The same specification runs in the canonical Hspec aggregate as
`music-unit-credential-cache`. The existing server specification adds actual
application-route parsing controls. The
[execution report](../../reports/music-credential-cache-2026-10-26.json) keeps
baseline, source identities, test outcomes and release limitations separate.
Finite route/response tests do not prove every future endpoint or intermediary.

Release qualification must build a fresh candidate with canonical Stack/GHC
9.10.3 and `--force-dirty`, confirm its actual `/version`, and check successful
and denied credential responses plus unchanged catalog behavior over HTTP.
The historical 5f binary is a baseline/compiler input. The fresh 43b qualification below is separate execution evidence. No production configuration or data has been changed.

There is no migration or backfill. An application rollback requires no data
rewrite, but removing the boundary restores the cache-instruction gap and
therefore needs an explicit release-risk decision. Previously issued grants
remain subject to their original authorization/expiry bounds; this change does
not purge caches or revoke object credentials.

Integration refresh: the source checkpoint `2a90e5d38817833bcfb54fda7ffe68e9d57263d3`
is normally merged with protected main
`9b7aa4648d4158f6da84fbb5e794267cd4953177`. All 146 current main requirement
objects are preserved, with one new scoped requirement (147 total). All eight
qualified implementation/test fingerprints and the public API source remain
unchanged. The 42-example result retains its original execution context; the
full application/HTTP execution was subsequently qualified on frozen43b; current exact-head CI, independent review and production release gates remain pending. The complete
1339-decision file and 222-entry migration manifest retain current main bytes.

## Compiled and HTTP qualification

Frozen source `43b1b4b00715c5564525d7bce1fa838bf6366122` passed the canonical application/test build and all 45 compiled affected cases (42 middleware/router plus 3 actual mkApp TCP parser cases). Its actual `/version` matched 43b; application SHA-256 is `9d87e3d4ec955f82aaed5b0cdb7e9d0a1631b5dfe56a61df7aaaf36eff576537`. The owned PostgreSQL16/full 222-schema HTTP run passed 54 assertions/65 steps, retaining the original 34 functional checks and adding exactly-one private/no-store checks for success, original denials, GET/HEAD spellings and all seven other credential error routes. Unrelated catalog cache behavior remained unchanged. All owned resources were cleaned up.

[Versioned evidence](../../reports/music-credential-cache-evidence-20261026/qualification-summary.json) retains source/binary identities, raw-log hashes and explicit limits. No object was fetched or browser/production journey tested. Review hardened the test client before execution: reject redirects, enforce exact write method/path/payload combinations and finalize PASS only after cleanup. Preparation revisions and safety controls remain archived.

The first hosted specification job correctly refused the changed Cabal manifest. An [independent context review](../../reports/music-credential-cache-formal-context-review-20261026.json) justified two source pins after checking the three module registrations and unchanged semantic dependencies; fresh SMT, regression and independent certificate checks passed. No proof gate or obligation was relaxed. All 147 full implementation/refinement requirements remain UNPROVEN and the formal release decision remains BLOCKED. Later formal/evidence-only edits retain the qualified eight runtime/test source hashes; the 43b execution is not relabeled as a build of a later commit.
