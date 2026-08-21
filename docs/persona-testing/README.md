# Persona-based quality program

Audit date: 2026-08-21. Baseline: `origin/main` at `560ac9954b655ba8aa719963a2c890110b74a232`; mobile submodule at `e1f9707eb130ec24892b544e0f470d254d4b2176`.

## Executive summary

This repository-native program turns the implemented product into 17 risk-ranked epics, 26 deterministic fictional personas and 78 complete-lifecycle journey specifications. It adds safe opt-in backend seeding, machine-readable scenarios and traceability, web E2E infrastructure, accessibility checks, mobile regression identifiers, CI enforcement and a refreshed feature/permission audit. It ran only in an isolated local worktree. No production or staging data, real provider, real recipient or public surface was touched.

Executed evidence is intentionally narrower than the catalog. The browser program verifies protected login recovery, UI registration, public discovery and public guest-ticket hold slices across Chromium desktop, phone and tablet plus critical Firefox and WebKit auth coverage. The mobile quality run verified 49 suites/256 tests, including the ticket cancellation and idempotency slice. Repository and web quality baselines passed. After an initial disk-capacity block and one decoder-import compile correction, GHC 9.10.3 built the Haskell backend. A disposable PostgreSQL 16 environment then verified two idempotent persona-seed boots, successful PER-02 login/session resolution and database token state. That journey exposed and then verified the fix for server-token replay after logout; the final complete backend suite passed 2,420 examples with 0 failures. Remaining domain/provider journeys are explicit coverage gaps, not implied passes.

Confirmed findings comprise one High web/mobile parity defect, one High application-recovery defect, one High dark-theme contrast problem, one Medium landmark problem, one High undisposed backend-capability gap, one Medium stale-audit problem, one Low non-reproducing mobile timing instability, one Medium untriaged dependency-risk signal, the previously documented High incomplete DDEX capability, one Medium missing web E2E gate, and one High logout-token revocation defect. Safe repository-side fixes were implemented for all but the endpoint-disposition backlog, dependency triage, and DDEX implementation. See [findings](findings.md).

Release readiness remains **conditional / not demonstrated** for complete authenticated and financially authoritative lifecycles. Local PostgreSQL migration/seed/login/logout now has direct evidence, including stale-token denial, but registration verification, expiry/reset/role changes, domain record lifecycles, sandbox provider events, notifications, refunds and reconciliation remain unexecuted. Backend compilation/tests, public discovery, protected-route recovery, web reflow/automated accessibility and mocked mobile ticket cancellation/idempotency are locally green within the stated slices.

This work includes no participant testimony. There were zero human participants, and no SUS, UMUX-Lite, NPS or fabricated sentiment scores were produced. Persona reactions in [quality assessment](quality-assessment.md) are explicitly heuristic hypotheses.

## Evidence vocabulary

- `verified-automated`: the cited command passed for the specifically described slice; it does not expand to uncited lifecycle steps.
- `direct-observation`: an investigator reproduced the behavior in an instrumented local run or source-backed inspection.
- `specified-not-executed`: acceptance criteria exist, but the journey was not run end to end.
- `blocked-environment`: local prerequisites prevented execution.
- `blocked-external`: safe execution requires an authorized sandbox, credential, participant or provider certification.
- `simulated-hypothesis`: a synthetic persona supports heuristic evaluation only; it is not human feedback.

## Deliverable map

- [Product and implementation inventory](product-inventory.md)
- [Risk-based epic inventory](epic-inventory.md)
- [Synthetic persona catalog](persona-catalog.md)
- [Detailed journey specifications](journey-specifications.md)
- [Persona–epic–story traceability](traceability.csv)
- [Machine-readable personas](../../test/personas/personas.json), [scenario blueprints](../../test/personas/scenario-blueprints.json), and [generated scenarios](../../test/personas/scenarios.json)
- [Execution results JSON](execution-results.json) and [execution report](execution-report.md)
- [Findings and sanitized evidence](findings.md)
- [Per-persona heuristic experience summaries](quality-assessment.md)
- [Permission and security matrix](permission-security-matrix.md)
- [Web/mobile parity assessment](web-mobile-parity.md)
- [Prioritized backlog, coverage gaps and release gates](backlog-and-release-readiness.md)
- [Human usability protocol](human-usability-protocol.md)
- [Current feature-discoverability audit packet](../feature-discoverability-audit/2026-08-21/README.md)

## Safeguards

All persona emails use the reserved `persona.test` domain. Seeding is off by default and requires both a non-production runtime and `TDF_ENABLE_SYNTHETIC_PERSONAS=1`; its password comes from a runtime environment variable and must contain at least 16 characters. Synthetic parties are matched only by reserved email/credential, never by display name, so a fictional name cannot attach roles to an unrelated local record. The seed adds no public directory projection and calls no external adapter. Disposable databases are the supported cleanup boundary.

Playwright mocks session, directory, catalog and currency endpoints. Failure-only screenshots, traces and video are written under ignored `artifacts/persona-playwright/` and uploaded by CI for 14 days. No token, credential, payment data or real PII is present in committed evidence.

## Regeneration

```sh
npm run generate:persona-program
npm run test:persona-program
npm run generate:features
npm run audit:features
npm run test:e2e:web
```

The current feature packet can be regenerated for review with `ALLOW_PENDING_FEATURE_DISPOSITIONS=1 npm run generate:feature-audit-reports`. Omitting the opt-in is deliberately a quality gate: generation fails while 40 endpoints lack explicit product/security disposition.
