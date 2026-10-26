# Track discovery in universal search — local delivery evidence

## A. Executive summary

`/buscar` previously searched directory entities only. Release catalog search
could match a recording title without identifying the matching track, making a
first-track Play action unsafe. This increment adds an authorized track result
contract and dispatches the exact clicked recording to the existing global player.
The local evidence below qualifies this scoped behavior; it does not complete the
universal-source catalog or establish production playback.

## B. Repository baseline

The branch `feat/universal-music-search-20261026` starts from source-authorization
candidate `52aa82ea70ed1e2b3dc26180977d9a0b838dea59` (#626), which includes protected
main `2e01bbe1263e8761fa2692bd951f828c01142449`. Backend-only checkpoint
`c88e3849f33f52619f786bf7f6ef4d2819b26fab` has completed canonical compilation and compiled HTTP qualification on its original222-entry schema; subsequent main composition remains pending.
Frontend, native tests, formal work and metadata were uncommitted during that
build; their qualified hashes do not make them a complete clean `c88` checkout.

All 148 parent requirement objects remain exact; the new scoped requirement makes
149. Of 1,341 inherited catalog decisions, 1,340 remain active and exact. The old
API-dispatch object is retained exactly in append-only history; its successor and
two finite-model fixtures are three reviewed technical entries, making 1,343
active decisions. The [manual source review](../../reports/music-track-search-evidence-20261026/catalog-source-review.json)
records those choices. The 222-entry production manifest is unchanged. The final
strict catalog scan and metadata integration remain root-owned release checks.

## C. Completed capabilities

The [contract](../../formal/system/music-track-search.md) and
[OpenAPI](../../tdf-hq/docs/openapi/music-search.yaml) describe the new public
`GET /music/search` boundary. It retains the public feature gate and trusted-edge
territory policy. Text matching first narrows current public track/version
contexts inside the database; a correlated source aggregation then applies
`music_public_asset_accessible` for that exact version and recording. Contexts
with no authorized source never reach public result projection, recording
deduplication or pagination. This query structure avoids authorizing every private
or unrelated asset for each query; no timing-noninterference proof or latency
benchmark is claimed. Only minimal deferred asset references and numeric
bitrate/loudness metadata are exposed. Metadata responses use private/no-store.
Existing release/library SQL and the separate credential middleware are unchanged.

`/buscar` has an independent Spanish/English music region with loading, empty,
service-failure, retry and page controls. Draft edits hide old playback actions.
An explicit click dispatches the precise hit and current visible-page queue;
metadata arrival and navigation do not autoplay. Access resolution remains deferred
until selection. The same public-track adapter is reused by the release page.
Directory filters/history and `/inicio` music request behavior remain intact.

## D. Verification

The [machine report](../../reports/music-track-search-2026-10-26.json) maps
`MEDIA-MUSIC-TRACK-SEARCH-001` through design properties, implementation, tests,
pending exact-candidate CI and release gates. The [evidence packet](../../reports/music-track-search-evidence-20261026/README.md)
contains original receipts, losslessly archived raw logs/results, member hashes
and current leaf hashes. Original failed attempts are retained alongside passes.

- Frontend: the full Node22 UI suite passed3,544 tests in 312 suites with zero failures, skips or todo cases. The55-test/six-suite affected selection also passed, plus canonical app type
  checking and lint of ten owned files. These use synthetic API replies, React
  Query, route mounts and event inspection. The 23 API-client search/pagination
  tests passed in root's actual tool stdout (session 87386); no raw log was
  retained, and none is fabricated in the packet.
- SQL: real PostgreSQL 16.13 and shipped query/binding controls passed with 14
  selected historical migrations, the real normalizer and valid publication
  fixtures. A/B selection, private/withdrawn/territorial listening policy, preview
  sources, metadata minimization, deduplication, stable pages and negative controls
  are covered. An initial impossible blank-artist fixture failed the real schema
  constraint; its correction asserts that constraint. Both attempts and separate
  first-attempt cleanup reconciliation remain unchanged. Final owned databases
  were removed and the owned cluster stopped. This is not full222 or HTTP proof.
- Native (historical pre-tag v2): six cases passed across Chromium desktop, Chromium phone web emulation
  and Firefox. Three decoded real generated AAC/FLAC bytes with advancing native
  clocks and route continuity; three received injected 403 responses for all B
  renditions, kept B selected and fetched no media or A fallback. APIs, permissions,
  catalog content and media delivery were injected local fixtures. A and B share
  one synthetic test tone, so identity follows IDs and URLs, not audible song
  recognition. Late A requests were browser-cancelled; this does not prove a stale
  response was delivered. Separate unit races and the design model cover that
  assumption. Initial EPERM setup executed zero cases; an earlier run had three
  passes and three test-evaluator `ReferenceError` failures. Their original results
  remain archived. No local WebKit pass is claimed: installation is unsupported on
  macOS 12.7.6, and the failure log is retained without a bypass.
- Formal design: 9,642 states and 54,454 transitions were explored through 12
  events, with 936 frontier states at that bound. Six mutants yielded replayable
  counterexamples, and ten scenario/replay tests passed. Safety is
  `MODEL_CHECKED_WITH_BOUNDS`. Conditional draining checked every reached state,
  with three additional responses observed under a cap of four. Liveness remains
  `PARTIAL` and implementation refinement `UNPROVEN`; this is not a whole-system
  proof or an implementation-derived theorem.
- Scope/pipeline contracts: 47 tests passed before the subsequent model-registration
  addition; root must recheck the final composition. Hosted exact-candidate checks
  and hosted exact-candidate qualification remain separate gates; the appended c88 build/full222 HTTP results below supersede the packaging-time pending snapshot.

Source-capture history is explicit. The first capture contained zero files and is
retained as setup context. The subsequent 972-file capture occurred during the
active build and omitted one Unicode non-code TeX template. It remains unchanged.
A [supplemental read-only review](../../reports/music-track-search-evidence-20261026/backend-source-review.json)
checks all 973 actual committed backend blobs, raw bytes and executable modes
against `c88` and the current working files. That later review is not relabeled as
a before-build snapshot or successful compilation. The original source-capture and packaging snapshots remain intact; the appended result section records the later executed compiled HTTP packet.

## E. Performance

Playback/search p95 and rebuffering metrics are unmeasured (`null` in the report).
Case durations, test budgets and decoder clock polling verify functional behavior,
not SLO compliance. Representative device/network and large-catalog measurements
remain required before performance claims or release exceptions.

## F. Integration

The new complete candidate is unpublished, unmerged and not deployed. Parent #626
must integrate through protected policy. Final metadata/source admission, strict
catalog scans, canonical checks, hosted exact-head CI and required reviews remain
separate gates. No branch-protection, assertion, timeout or retry safeguard is
weakened by this evidence packaging.

## G. Deployment

No environment was deployed or activated, no production flags were changed, and
no production account, upload or listening journey was tested. Existing backend
feature activation remains separate. The `compiled-http/` evidence subdirectory
now contains the executed c88 qualification; it is not evidence of deployed/current-main execution.

## H. Remaining risks and mitigation

Coverage is limited to canonical music releases. Historical/legacy media, social
and event attachments, universal migration/reconciliation, physical mobile,
background playback, local WebKit, complete WCAG 2.2 AA, production authorization,
CDN/storage delivery, upload-to-playback and production observability remain
unqualified by this increment. Existing global formal release gates remain.

Rollback is an application-source rollback with no migration or master mutation;
its target-environment execution is unverified. Coordinate backend/frontend rollout:
an unavailable search route must show a recoverable music-section failure while
directory search continues. No production data, job, rights declaration or original
master is changed by the local qualification.

## I. Next highest-value action

Checkpoint the executed c88/full222 evidence, then normally compose the refreshed #626 parent and current protected main c66. Reconcile its 223-entry manifest/new requirement and qualify the final complete candidate through fresh metadata, source admission, hosted CI and independent review. Runtime/full223, target rollout, upload/media and SLO acceptance require their own evidence.


## Appended execution qualification

The unchanged backend checkpoint `c88e3849f33f52619f786bf7f6ef4d2819b26fab`
completed its canonical retry with exit 0. The first attempt compiled and linked
but failed install/strip with ENOSPC (exit 1); its original log/marker remain
archived. No source, assertion, timeout or retry-limit change repaired it. The
complete973-file source capture occurred during the active build; the original
empty/972 captures and Unicode-template supplement are retained without a
before-build claim.

The full Node22 UI suite passed **3,544 tests in 312 suites**, with zero failures,
skips or todo cases. Final CI change-scope/pipeline checks passed 47 tests; the
separate narrower 19-test selection stays separate. Ten fresh compiled pure
ContentSpec payload examples also passed; these are compatibility regressions,
not search HTTP or full backend-suite execution.

The actual c88 binary SHA-256 is
`b10f2a0ab3ec8a0c5f42ede90e5bc8bacb80cceafd2cd95042ce9cb7e68cbd8b`.
Its `/version` returned c88. The owned PostgreSQL16/full222-schema qualification
passed **105 assertions and 66 steps**, using only actual GET/HEAD health, version
and music-search requests. It verifies exact B identities, query/limit/offset and
trusted-country bindings, success/handler-error private-no-store, disabled 404,
private/withdrawn/territorial/preview eligibility, whitelist metadata, canonical
deduplication, paging and fresh withdrawal denial. The malformed-integer parser 400
case carries no handler-cache promise. No signup/login, mutating HTTP, source
signing, storage/object download, real upload or production journey was executed.
All owned server/database/cluster cleanup flags are true; frontend/metadata hashes
were unchanged during the run. Individual durations are not SLO measurements.

Compiled declaration review adds only `GET /music/search`:905 existing operation
declarations and two raw mounts remain equivalent after ignoring ordinal shifts.
The expected first snapshot-drift failure is retained; root reviewed and copied
the reflected candidate. This does not resolve existing API conformance debt or
replace final inspector/CI checks after composition.

A later annotation-only native successor adds `@mobile-flow` to two test titles
without changing assertions, timeout, retries or configuration. The retained v3 attempt exited 1: five cases passed and Firefox's happy-path case failed in
afterEach artifact/trace creation with ENOSPC after its functional body. That case
remains failed; unchanged source hashes do not promote it to a pass. The original
pre-tag v2 six-pass receipt remains historical. That failed attempt stays recorded; current-head
CI remains required; no assertion, timeout, skip or retry gate is waived. After root observed more than 2 GiB free space, one identical v4 qualification returned actual exit 0: six local cases passed with no failures, skips or retry. All 20 pre/post source hashes remained exact; no source, assertion, timeout or configuration changed. Hosted discovery now selects 14 executions across seven
projects; discovery is not a WebKit execution result. Local WebKit remains
unsupported on macOS 12.7.6 without a bypass. Original native receipts/archive are
preserved, and current-source hashes are appended separately.

Protected main advanced to `c66a65a10a8e5e48f89172d61f6f57fb81697896` during
qualification, adding `PAY-DOMAIN-BINDING-001` and migration 223. This feature has
not yet integrated that baseline. These runtime results remain c88/2e/full222
evidence; no223-schema execution, current-main qualification, hosted CI, review,
merge, deployment or production acceptance is inferred. Root must checkpoint,
perform the normal merge, reconcile metadata and execute fresh final gates.

See the [appended summary](../../reports/music-track-search-evidence-20261026/post-qualification-summary.json)
and its supplementary archive/member hashes. The original42-member archive and
packaging-time receipts remain byte-for-byte unchanged.
