# Privacy-only listener resource disposal

## A. Executive summary

When the listener changed or the application unmounted, the previous audio element was paused but retained its source. This focused fix removes that source during disposal while preserving the existing generation, play-attempt, intent and retry guards. It does not import the search or metadata-readiness changes from PR640.

## B. Repository baseline

At evidence capture, the private working tree started at `3de0406ff241c03a1878810f718b2e587ee4ad8b` and was normally merging `1129112f89d3c00d33b8c86043a6bd197031dcff`. It was uncommitted and had no candidate-specific hosted CI or approval. The runtime change is limited to the existing unmount cleanup: pause the old element, remove its `src` attribute and call `load()`.

Previous delivery reports remain unchanged. Their CI, reviews and source qualifications do not qualify this candidate.

## C. Completed capabilities

Cleanup invalidates old work, cancels retries and resets the detached element once. The new listener receives an empty listening session and can play a new explicit selection. Same-owner continuity, storage envelopes, device preferences, authorization and source selection remain unchanged.

## D. Verification

Two desired tests using the actual App and session providers first failed against the unchanged privacy provider: `A.m4a` remained attached after an owner change and after unmount. The root-owned Chromium native baseline also failed the disposal predicate. These failures remain recorded separately from the corrected results.

The corrected source passes:

- 87 focused tests in 8 suites, scoped lint for the three changed files and app TypeScript checks.
- The full UI suite: 3,578 tests in 311 suites, with no failed or pending tests and actual process exit 0.
- 30 native cases across Chromium desktop, phone web emulation and Firefox, with no skips, retries or flaky results. These comprise 24 decoder fixtures, 3 denial fixtures and 3 explicitly mocked smoke cases. The disposal case passes in all three projects; all 491 captured runtime and specification inputs remain unchanged during the run.
- Whole-project lint and the local production-mode build: app TypeScript, Vite and bundle budget checks pass. The bundle has 5 preloads and 337,618 gzip bytes of initial JavaScript.

The cancellation test permits one deliberate disposal load, then retains its ten-second check for no additional load, play or authorization call. No assertion, timer budget or unrelated test is weakened. The initial full-suite launcher used the wrong hoisted Jest path and ran zero tests; its setup failure is retained separately.

The [machine report](../../reports/player-listener-disposal-2026-10-27.json) records all 23 acceptance criteria with scoped or incomplete status. The [evidence packet](../../reports/player-listener-disposal-evidence-20261027/README.md) retains 47 raw members in a 382,695-byte archive. Root's offline replay reproduces its bytes exactly. Three known credential patterns produce no matches; this is a limited scan, not proof that every possible secret format is absent.

Unit tests use declared API, route and media doubles. Native tests use generated media and real browser clocks and decoders, except the three mocked smoke cases; account, catalog and authorization replies remain synthetic. The [privacy contract](../../formal/system/player-listening-privacy.md) adds the cleanup obligation. No new formal model or refinement proof is claimed. Final canonical metadata and immutable admission were still pending at this evidence snapshot.

## E. Performance

Playback and search p95, rebuffering and representative device/network SLOs remain unmeasured. Passing functional timeouts and bundle budgets do not establish those SLOs.

## F. Integration

At evidence capture, this was private preparation, not a published, reviewed, approved or hosted-CI-qualified candidate. A later commit or publication must retain the exact source correspondence and obtain its own required checks and review. The combined privacy/search experiment remains separate; its results are not relabelled as this privacy-only execution.

## G. Deployment

No production deployment, flag change, real account or content mutation, upload, protected-media transfer or provider operation occurred. Native fixtures used owned loopback servers and generated audio. No schema or original master changed.

## H. Remaining risks

Server authentication, private-byte authorization, CDN revocation, physical devices, OS audio focus, cross-tab coordination, secure erasure and complete WCAG coverage remain outside this qualification. The resource cleanup defect does not establish a production protected-byte disclosure. A rollback must preserve or safely clear owner-scoped listening state; no rollback drill was executed.

## I. Next highest-value action

Finish canonical metadata, freshness and immutable admission, then retain the exact candidate commit. Obtain fresh hosted CI and independent review for the focused change before protected integration. Production acceptance remains separate.
