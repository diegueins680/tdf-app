# Task/RACI read API contract

This is the typed transport refinement of [TR-01–08](task-read-contract.md), not a new
authorization policy or task lifecycle. The prior SQL migration remains unchanged.

`GET /event-operations/events/{eventId}/tasks/{activityId}` has no request body, actor,
grant or evaluation-time argument. Both captures are positive JavaScript-safe integers
(1–9007199254740991). This initial JSON-number transport fails closed for values outside
that range instead of rounding IDs or versions; a later lossless-ID rollout must be explicit.

| ID | Executable boundary |
|---|---|
| TA-01 | Authenticate using the existing bearer/cookie handler. Derive actor from `AuthedUser`; recheck and lock its current session in the same transaction as `loadTask`. Caller-supplied query parameters cannot select actor or time. |
| TA-02 | Feature disabled returns existing 404 feature_disabled; otherwise SQL NULL yields identical 404 not_found for absent, foreign-event or unreadable tasks. Invalid path captures return 400; invalid/stale session 401; SQL/decoder faults sanitized 503. No raw rows/credentials in errors. |
| TA-03 | Strict nested JSON: eventId/activityId/status/version, optional non-null policy, RACI and accountabilityNeedsAttention. Accept the five statuses already enforced by the legacy logistics handler and exactly four canonical RACI roles. Reject unknown/missing/null/invalid fields, mismatched target IDs, nonpositive/unsafe IDs and versions, duplicate party-role pairs and inconsistent attention. |
| TA-04 | Return no-store cache policy on successful task reads; no ETag or aggregate write token is implied. Activity and policy versions are not an aggregate concurrency token. Reauthorize every network request; do not add a task cache or offline queue. |
| TA-05 | Typed OpenAPI and generated web declarations match the server wire shape. Web client rejects unsafe/nonintegral/nonpositive captures before sending and validates the response at runtime; malformed successful responses become a fixed Spanish error, not an empty success or raw decoder diagnostic. Existing snapshot and transition methods retain their contracts. No fake UI or mobile implementation claim. |

Applicable checked models: `TaskRead` (NoUnauthorizedTask, CoherentTaskProjection,
four negative controls), `TaskReadStructure` (event/task/grantee binding), `SessionFence`
(CurrentBoundSession, six negative controls), `SnapshotRead` (allowlisted errors) and the
remaining unchanged event models. Re-run the pinned suite before implementing this adapter.
The adapter refines read operations without changing model transitions or fairness assumptions.
Strict JSON, exact numeric transport and response headers are executable contracts tested
with Hspec/property tests and real HTTP/PostgreSQL; the finite models do not prove Aeson,
Servant, browser storage, proxies or arbitrary database implementations correct.

Verify typed decoding with adversarial rows; exercise the production authenticated subrouter
against disposable PostgreSQL, including exact grants, assignment-only denial, mismatched
events, grant/token revocation, expiry, fault recovery and after-auth session changes. Apply
task-commit and task-read prerequisites in both local and CI HTTP runners. Keep existing
HTTP/session regression tests intact. No production migration registration or activation.

Rollback the consumer first; leave the harmless read functions or remove them with their
function-only rollback after consumers are quiesced. No domain data migration. Rich field
visibility, titles/dates, membership-removal semantics, history, commands, UI, large-plan
performance and mobile flows remain subsequent dependency-ordered increments.

## Verification matrix

- JSON decoding: valid round trips and generated safe versions; missing/extra nested fields,
  null optional policy, invented statuses/roles, duplicate RACI pairs, mismatched event/task,
  unsafe versions/party IDs and inconsistent attention all reject without retaining payload.
- Wire: exact successful shape and no-store header, cookie/bearer identity, task-scoped access
  without parent-event read, denied siblings/foreign event/assigned-only actors, invalid
  captures, expired/revoked grants, token mutation after authentication and sanitized SQL fault.
- Web: exact path and no-store request, unsafe captures cause zero calls, valid optional-policy
  and protected-task shapes, malformed responses reject generically, transport errors propagate,
  and existing snapshot/transition calls retain their routes and idempotency header.
- SQL migrations are unchanged; the HTTP runners must install task-commit and task-read before
  seeding opted-in tasks. Their safe-environment guards remain mandatory and independently tested.
