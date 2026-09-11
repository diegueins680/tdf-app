# Draft PR: make public booking replays authoritative

## Problem

The preceding idempotency implementation checked resource availability before acquiring the per-key lock and reading the durable replay receipt. Once the first booking committed, its own room allocation caused a later identical retry to return `409` instead of the original booking. Changed input under the same key could likewise surface a generic availability conflict instead of the stable idempotency conflict.

The earlier browser test mocked HTTP and could not observe this database-dependent ordering defect.

## Changes

- Move public-booking resource resolution into the key-serialized database transaction.
- Return a matching durable receipt before consulting mutable room availability.
- Retain validation, deterministic resource locking, exclusion-constraint conflict handling, transactional writes, and engineer-notification suppression.
- Add a guarded real HTTP/PostgreSQL concurrency harness that:
  - builds a production-like schema using the reviewed migration path;
  - launches the exact tested backend executable with a clean provider-free environment;
  - observes an advisory-lock waiter for concurrent equal-key requests;
  - verifies post-commit replay and changed-payload conflict semantics;
  - verifies concurrent different-key resource conflict;
  - asserts booking, Party, receipt, resource, allocation, credential, and role cardinality.
- Run the harness in backend CI and ensure future harness-only changes select backend validation.

## Evidence

- Pre-fix real runtime: failed with `Post-commit equal-key replay response: expected '200', got '409'`.
- Final HTTP/PostgreSQL test: passed with equal-key `200/200`, post-commit replay `200`, changed-payload `409`, and overlapping resource `200/409`.
- Focused Hspec: 1 example / 0 failures after production/test executable rebuild.
- CI pipeline contract: 18/18 tests passed.
- Catalog test: 1/1 passed; strict catalog audit passed.
- Shell syntax and diff checks passed.

Detailed evidence and coverage: `reports/public-booking-http-concurrency-audit-2026-09-09.md`.

## Risks and compatibility

- No API, schema, generated client, UI, mobile, dependency, price, policy, permission, role, or payment behavior changes.
- A genuinely new request holds its key transaction while resolving resources; matching replays now skip availability queries. The key lock affects only equal idempotency keys.
- The two-second trigger exists only in the disposable test database.
- The integration script refuses non-test/non-loopback targets and starts the server with `env -i` so inherited provider credentials are unavailable.

## Rollback

Revert this branch's commits. No database rollback is required. Reverting would restore the incorrect post-commit replay ordering, so retain the HTTP regression if the handler implementation is replaced.

## Remaining gaps

- Inventory external clients before enforcing the required header in rollout.
- Exercise the Domo fallback at runtime.
- Add crash-after-commit fault injection and local notification-sink verification.
- Staging, production, analytics, screen-reader, physical-device, and field-performance validation remain out of scope for this batch.

No merge or production deployment is requested.
