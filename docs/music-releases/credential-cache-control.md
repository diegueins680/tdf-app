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

## Protected post-legacy main composition and original CI failures

This local preparation normally merges the published evidence successor
`7ddfbd73bf08d007b4658b700bb9cbe289a6b480` with actual protected main
`2e01bbe1263e8761fa2692bd951f828c01142449`. All 147 main requirement objects
and the unchanged credential-cache requirement are retained (148 total), along
with the complete 1,341-decision file and 222-entry migration manifest. All eight
qualified runtime/test source files remain byte-identical to `43b1b4b`; its
45 compiled cases and 54 HTTP assertions retain that execution identity. No
Stack, PostgreSQL, SMT or browser run is repeated or relabeled for this refresh.

The [separate original-CI archive](../../reports/music-credential-cache-ci-failures-7ddf-20261026/README.md)
preserves full attempt-1 logs and the original QA receipt for source `7ddfbd73`,
base `9b7aa464`, tested merge `9edec4ee`. Persona testing recorded 565 passes,
22 skips and one failure: WebKit's phone cart test exhausted its whole-test
30-second budget at the final `Cerrar carrito` click. The cause remains
UNVERIFIED. Social's 63 focused tests passed; browser journeys were SKIPPED
after the dependency-install step exhausted the 15-minute job budget following
font-mirror delays. The separately archived root-fetched annotation confirms
15m0s. Original logs, packets and their earlier observations are not rewritten.

Required main composition followed by fresh exact-head CI is not a cart fix.
There is no rerun of `7ddf`, timeout/skip/assertion/workflow modification or new
production action. The reviewed Cabal source pins, append-only proof history
and original assurance evidence remain byte-identical. Their conditional
source-check results keep their original context; global implementation and
refinement remain UNPROVEN and the assurance release decision remains BLOCKED.
Adding the protected-main requirement does not create a new proof.

The integration baseline is explicitly pinned to post-#624 main above; later
main advances need their own reconciliation. Current-head CI, independent review,
protected merge and deployment acceptance remain separate release gates.

The [independent source-preservation review](../../reports/music-credential-cache-source-review-post624-20261026.json) checks all 976 backend blobs and Git modes against the qualified source, alongside the original HTTP inputs, native paths, proof-context files and old evidence packet. This is a read-only source comparison, not a new build or execution.

## Cart CI reliability correction after d765

Original source `d7657f3daf7ee037703a327d840a67e29ac61099` failed persona run
40404888561 on tested merge `7b33eee0`: 579 passed, 22 declared skips and one
WebKit phone cart failure out of 602 cases. The downloaded artifact's actual
SHA256 is `75d6a47be209899ba10ce34b28cce4f25d8b2edc2944c8be46458b2bc34e70b9`;
the extracted trace confirms the final close at original line 172:64 was pending when
the full 30s budget expired. Four synthetic cart API calls returned 200. Rendering
root cause remains UNVERIFIED; this increment does not change product rendering.
The [separate CI correction packet](../../reports/music-credential-cache-cart-ci-20261026/README.md)
retains original results/logs/bounded trace, source assertions and all setup history.
The full 70MB parent ZIP remains private, with actual verified identity recorded.

The test now gives two coherent journeys separate unchanged 30s budgets. The first
performs genuine add/open/close followed by actual reload and count/badge/layout
checks in that same session. The second starts fresh, genuinely adds through UI,
validates POST200/cart identity/quantity, then reloads/removes and checks empty state.
All 44 original assertion/layout-call lines and original nonblank lines remain;
the saved-cart outage/recovery case is unchanged. The full original eight-step
drawer→reload→remove composition is no longer one same-session test. No forceclick,
retry/skip/timeout relaxation, seeded successful persistence or rendering fix is
claimed. Test-scope decomposition is explicit rather than a replay of failed d765.

Root-confirmed stronger-v3 source `c86a9ccf` passed 9/9 across Chromium desktop,
Chromium phone emulation and Firefox, with no skips/flakes/retries. Its exact QA
receipt preserves a small cart-only prelaunch guard plus an eight-file in-run/post
comparison; it does not fabricate an eight-file before-run capture. Local case
durations 3.540–12.043s are functional observations, not SLOs. The earlier09b7 v2
receipt qualified weaker 99f source before the additional first-journey reload and
remains historical. Initial private-config cwd/ENOENT setup ran zero cases; its
API-origin preflight correction occurred before any browser test, not as a product
repair. Local WebKit installation is unsupported on macOS 12.7.6; its failure log
is retained without a bypass. Fresh-candidate hosted CI including WebKit, current
reviews and protected merge requirements remain mandatory, with no waiver.

All 976 backend blobs/modes, the eight qualified cache files and old evidence/proof
packets remain byte-exact to their qualified inputs. The 45 compiled/54 HTTP results
retain source 43b; cache Stack/PostgreSQL/SMT/browser tests are not repeated. The
historical 147-row aggregate assurance and append-only pins/history remain unchanged;
implementation/refinement is still UNPROVEN and formal release BLOCKED. The 148
normative requirements, complete 1341-decision file and 222-entry manifest remain
unchanged. Ordered metadata and exact-head CI/release checks are separate gates.
No production or other active music-search branch is changed by this correction.

The CI-only metadata refresh passed 36 declaration/inventory/conformance/admission regression tests and strict catalog JSON/CSV scans with 1,341 entries and no unreviewed/stale decisions. Normative 148 requirement objects and complete 1,341 decisions/222 migrations remain exact. Freshness, source preservation and immutable post-commit admission are tracked separately in the current report; fresh exact-head hosted CI including WebKit remains required.
