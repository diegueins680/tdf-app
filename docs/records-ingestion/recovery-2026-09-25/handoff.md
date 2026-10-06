# Video ingestion recovery — 25 September 2026

Two independent Codex sessions are using `/private/tmp/tdf-video-event-20260925`, branch `codex/video-event-ingestion-20260925`. Avoid concurrent edits. The direct-chat recovery session paused edits after confirming that terminal session `01a0d40f-e26a-76a1-a9d8-fd6a943d681f` was actively editing and testing this checkout. User ownership clarification is pending.

## Recovered work

The deleted `/private/tmp/tdf-ingestion-completion-20260921` draft was reconstructed from the 20 September session logs. It includes runtime SQL and rollback, Haskell service/admin routes/scheduler, UI/source controls, OpenAPI, provider metadata, thumbnail candidates, and tests. Restoration was reviewed and limited to file writes; prior deployment, merge, credential and database commands were not replayed. The original branch/commit source is current main `66dae5b8ec22f3f0fe819dfcd9bae5ed727c6b48`. The recovery copy in this directory contains tracked-file patch plus untracked implementation files; it is a draft, not a tested release.

Some transcript-recovery artifacts were repaired: imports, run identity mode, correct advisory unlock, conservative quota reservation, empty-heredoc fixture contamination and the final thumbnail metadata field-access edit. The test runner now falls back to isolated PostgreSQL 16 when Docker is unavailable. Do not revert the concurrently added dry-run behavior in RecordsIngestion.hs.

## Validation and outstanding code work

- UI typecheck passed after client generation. The existing thumbnail/API tests passed 10 examples. The three restored metadata regression tests also passed in a separate invocation (13 focused UI/API examples total).
- Disposable PostgreSQL migration tests passed: catalog/availability, approval/private content, identity, public membership, replay without side effects, editorial preservation, stale response, rollback/reapply, and concurrent one-recording/one-change assertion. Log: `/private/tmp/tdf-ingestion-recovery/database-tests.log`.
- First isolated backend compile found missing imports, now fixed. Second isolated compile of the service and admin handler passed (exit 0); log `/private/tmp/tdf-ingestion-recovery/backend-compile.log`. This is not a whole-app build.
- Recovery is NOT completed ingestion. Review source config/atomic admin audits, cross-key resume, per-source failure isolation, quota accounting, full reconciliation of previously known resources missing from uploads, retention, public association behavior, admin validation, real service/DB integration tests and migration registration before enablement. Original event scope still requires nationwide coverage and shared pilot/approval enforcement. Existing PR #460 owns optional confirmed event-end implementation; it has all-green CI at 648f2d6dc but no review.

## Exact release evidence

- Production still reports backend `592916f2a7fbcd740409acda42e69d1a6bfb128a`; the thumbnail data correction is not deployed.
- Main 66dae5b8e contains the reviewed thumbnail correction, daily event scheduling, provider adapter and required disabled-financial-write safety. Build Image run 35570521684 succeeded.
- `989d0663cc249bcee74331604561628d3435c961` has an approved exact-head review but its registry image is absent. Do not treat this as unavailable recovery generally: tested ancestor `3e5563a2dd1c01d1c7b6f73ac3153681a9290a12` is accepted by the current guarded preflight.
- Preflight with recovery 3e5563a2 completed **ready**, verifying DB/migration/security readiness and recovery compatibility. Main immutable index `sha256:96927caf3dc0538ddbe2c3abab6534a29aa3068bc7162679609aca8d821ee900`; recovery index `sha256:ed842af824c2e1b9de08d608bf94c7f5364691f1903038652661f2055eb9e3fc`.
- Staging deploy of this exact main image through existing Fly config was rejected at release creation with HTTP 403: **account has overdue invoices**. No release was created. Billing action required at https://fly.io/dashboard/diego-saa/billing before any staging/production rollout. Do not retry or bypass release safeguards while billing remains blocked.
- Existing production configuration has EVENT_DISCOVERY_ENABLED=true and EVENT_DISCOVERY_AUTO_PUBLISH=true. Historical research-pilot approval is not new event publication authority. Preserve original task approval boundaries and verify applicable recorded approval before expanding automated publication.

This session did not merge, commit, push, enable new sources, import real data or deploy a new release. It did not change billing or send external messages.
