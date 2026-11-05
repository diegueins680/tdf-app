# Track discovery in universal search — refreshed-parent delivery evidence

## A. Executive summary

`/buscar` now has an authorized recording-search contract and selects the exact
clicked track through the existing persistent player. The implementation addresses
release-first-track substitution and separates music errors/pages from directory
search. Local source-bound qualification exists for the earlier code and schema;
current-parent integration and release gates remain separate.

## B. Repository baseline

Complete feature checkpoint `0dd0c65161da2a23cea7ea4d90659b8c944f031e` is normally
merging refreshed source-availability parent
`a37690f9c0d3f81f340c3a7c1a1f3c4cb273e96c`, which includes protected main
`c66a65a10a8e5e48f89172d61f6f57fb81697896`. All 149 parent requirement objects
remain exact; scoped `MEDIA-MUSIC-TRACK-SEARCH-001` makes 150 total. Its
behavioral clauses remain unchanged; one implementation ownership mapping for
the material OpenAPI YAML is added after the admission gate correctly rejected
that previously unmapped surface. The active
catalog has 1,343 reviewed decisions. It retains main's 223-entry manifest decision
`2634b239375f2ad309ba`, the exact old `6ff395b6131079017c9d` object in main's
reconciliation document, and the exact old API-dispatch decision `56e2b5d3ce52bde290e8`
in this feature's history. Its three reviewed technical successors remain unchanged.
The 223-entry production manifest is byte-exact to the refreshed parent; no
migration is applied by this metadata reconciliation.

Runtime/UI/API/model/native/SQL-runner inputs remain unchanged from checkpoint
0dd. Protected main adds provider SQL and modifies `ProviderRetrySpec`, so current
whole-backend identity is FALSE. Historical backend compilation and HTTP results
remain bound to actual `c88e3849f33f52619f786bf7f6ef4d2819b26fab` and the 222-entry
schema. New current-head compilation and full223 HTTP qualification are pending.
No old binary is retagged; Git-derived version/time identity remains build-specific.

## C. Completed capabilities

The [contract](../../formal/system/music-track-search.md) and
[OpenAPI](../../tdf-hq/docs/openapi/music-search.yaml) define `GET /music/search`.
It retains the public feature and trusted-territory policy, narrows public contexts
by text inside the database, then requires eligible exact-version/recording sources
before result projection, recording deduplication or pagination. Minimal deferred
IDs and numeric bitrate/loudness are returned with private/no-store; no delivery
URL or arbitrary technical metadata is exposed. The existing engine reauthorizes
selection. This is canonical release search, not universal historical/attachment
indexing, constant-time retrieval or a measured latency guarantee.

The independent ES/EN music region in `/buscar` hides obsolete draft-query actions,
distinguishes empty/service errors, supports retry/pages and synchronously selects
the precise clicked recording. It introduces no result-wide signing waterfall,
mount autoplay or second player. Directory filters/history and `/inicio` behavior
remain intact. The shared adapter preserves release/recording/asset identity.

## D. Verification

The [machine report](../../reports/music-track-search-2026-10-26.json) maps the
requirement through design properties, implementation, tests, pending final CI
and release. Its acceptanceScope matrix records all 23 acceptance criteria with
explicit scoped, partial, unmet or unverified status. All 29 actual premerge [evidence packet](../../reports/music-track-search-evidence-20261026/README.md)
leaves and original compressed archives remain byte-exact; historical failures
and their source identities are not rewritten by this refresh.

- Earlier frontend checks passed 55 affected tests, API-client checks passed 23
  in root's tool stdout without a retained raw log, app type checking/lint passed,
  and the full UI suite passed 3,544 tests in 312 suites. Synthetic APIs/media
  doubles remain distinct from actual HTTP and decoder evidence.
- Actual shipped SQL/binding controls passed the selected 14-migration fixture;
  the impossible blank-artist fixture failure and correction remain separate.
- The actual c88 canonical build passed after its retained install/strip ENOSPC
  failure. Its real binary and full222 PostgreSQL metadata-only HTTP run passed
  105 assertions/66 steps. Ten compiled pure payload regressions also passed;
  these do not establish a full backend-suite result or new223 execution.
- Annotation-only native source74b passed six cases locally in v4 after retained
  v3 ENOSPC afterEach failure (five passes, one failed case). Three journeys decode
  real AAC/FLAC and three exercise injected denied-B delivery. APIs, authority,
  content and media delivery are injected local fixtures; A/B share one test tone.
  Identity follows IDs/URLs, not audible recognition. Late A requests were cancelled,
  not proven delivered stale. Actual local configuration discovery selects 14 cases across seven projects,
  including two WebKit iPhone cases. Enumeration is not browser execution or hosted CI. Local WebKit is unsupported without a bypass.
- The independent finite design model checks 9,642 states/54,454 transitions through
  12 events with 936 frontier states, six counterexample mutants and ten replay
  tests. Safety is MODEL_CHECKED_WITH_BOUNDS; conditional completion is bounded,
  liveness PARTIAL and implementation refinement UNPROVEN.

Current-parent ordered metadata, strict catalog, source admission, API-client and
CI-scope checks are recorded separately after this refresh. Existing API availability
and response-status policies need no exemption: the new route is mounted with a
reachable 200 success, not a deferred or deliberately503 handler. The compiler
snapshot adds only GET/music/search to 905 existing operation semantics and two
Raw mounts (ordinal shifts only); final actual-binary comparison remains a gate.

The refreshed-parent metadata passed 36 regression cases, 47 CI-scope/pipeline
cases, strict 1,343-entry JSON/CSV checks, two generated-client freshness checks,
public-route admission and existing availability/status-policy checks. Final
ordered freshness and immutable post-commit admission are recorded separately.
These metadata gates do not promote old runtime evidence to full223/currentHEAD.

## E. Performance

Startup/search p95, rebuffering and representative-device/network SLOs remain
unmeasured (`null`). Functional test budgets, case durations and advancing decoder
clocks are not SLO measurements or benchmark exceptions.

## F. Integration

This complete feature candidate is unpublished, unmerged and not deployed. Its
normal parent merge is pending root commit. Exact-head CI, current independent
review, parent integration and protected merge safeguards remain required.
No failed result, assertion, retry, timeout, skip or API gate is waived.

## G. Deployment

No production flag, account, data, master or provider is changed by this metadata
work. Historical full222 results do not imply current full223 or production
acceptance. Root owns the fresh compiled binary/schema qualification and release
workflow after the exact merge candidate is committed.

## H. Remaining risks

Non-release/historical/social/event sources, private-owner catalog search, actual
object/CDN delivery/revocation, upload-to-playback, physical mobile/background
behavior, hosted WebKit, complete WCAG AA, production observability and SLOs remain
unqualified by this increment. Existing global formal release gates remain.
Application-source rollback needs no music migration or master rewrite; target
rollback execution is unverified. Coordinate backend/frontend rollout so unavailable
music search is recoverable while directory search continues.

## I. Next highest-value action

Commit the reconciled current-parent candidate, capture its exact backend inputs,
run the canonical warm build and actual full223 metadata HTTP qualification, then
complete current-head CI/review before publication and protected integration.

The following section preserves pre-integration execution chronology through0dd;
its statements about a pending c66 integration describe that historical checkpoint.

## Historical execution qualification through checkpoint 0dd

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
