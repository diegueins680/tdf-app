# RACI HTTP command contract (before feature implementation)

Depends on private SQL PR 383, exact base `f6ffb2a27b87ed42d4d49289024d5b8a612b94cf`.
No new domain, SQL migration or production activation. The existing canonical command,
event/task grants and transaction-local `withCurrentAuthSession` are reused.

`POST /event-operations/events/{eventId}/tasks/{activityId}/raci/reassign`
requires authenticated bearer/cookie context and UUID `Idempotency-Key`. Exact JSON fields:
`expectedRevision` (canonical positive BIGINT decimal string), `role` (four canonical RACI
values), positive safe-integer `fromPartyId`/`toPartyId` (distinct), nonblank `reason` <=2000
characters and `correlationId` <=200. Unknown/null fields fail validation. No actor, time,
hash or grant is accepted in the body. Transport validation does not normalize accepted text.

RA01: revalidate and lock the current bound session in the same transaction as SQL, validation
and commit. Existing `SessionFence`, `RaciReassignment` and scoped Alloy relations apply.
RA02: parse exactly one nonnull SQL result inside that transaction. Accept only a strict
allowlisted error object or strict success DTO matching event/task/key/role/old/new party
and result revision = expected+2, using exact integer arithmetic. A replay returns the same
historical revision; never compare it with a new GET. No malformed response may commit newly
staged changes. `CommandBoundary` models validation, commit/abort and early/unbound mutations.
RA03: 200 only after successful transaction commit; no-store on success and handler-generated
command errors. Servant parse/authentication errors retain the existing framework behavior.
Malformed response/unknown error/DB failure -> sanitized 503; SQL errors abort the transaction.
Known SQL errors: invalid_request400; not_found/feature_disabled404; forbidden403;
idempotency_conflict/version_conflict/operation_not_ready/assignment_not_replaceable/
assignee_unavailable/assignment_conflict/accountability_not_ready409. Authentication remains401.
No automatic retry, rebase, compensation or fake success after timeout/abort.
RA04: typed web client validates request and exact bound outcome, preserves explicit UUID,
captured bearer and signal, disables caching. Client rejection cannot undo a server commit;
after ambiguous network failure the caller must retry the exact original command/key.
RA05: API/OpenAPI/generated web types remain additive; no UI editor/mobile/offline activation.

Formal bounds: one command, four combinations of shape/target validity, finite phases and
one commit/abort choice; no liveness/fairness or universal/refinement proof. Trusted SQL
provides authorization, exact request hash and atomic mutation/audit/receipt; the new model
checks the application validation/commit ordering only. Existing finite Alloy models reuse
the same relations with no new entities. HTTP fault injection must verify rollback, not only
an error status. Typed/unit/property/client tests enforce exact fields and BIGINT boundaries;
real auth/subrouter/PostgreSQL tests exercise accepted/replayed/stale/forbidden/malformed and
revoked sessions. Activation still requires separately reviewed migration/flag and operational
limits. Generated requests/fixtures are synthetic, not consent or notification delivery.

## Formal verification evidence

Before feature implementation on 2026-09-15, the full pinned suite passed with exit 0:
21 positive TLC configurations, 45 named negative controls, 13 PlusCal integrity tests,
2 SAT Alloy scenarios and 11 UNSAT assertions. `CommandBoundary` generated 14 distinct
states, depth 4; both mutations returned exit 12 with `ValidatedCommit`. Exact command:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

The HTTP transport deliberately rejects whitespace-only reasons (including tabs), a stricter
subset of the private SQL primitive's space-trim check. It preserves accepted text unchanged.
The SQL function and its allowlisted rejection semantics remain trusted: a compromised SQL
function that writes and then fabricates a recognized rejection is outside this boundary model.
Request parsing uses the existing Aeson/Servant JSON parser; no duplicate-JSON-key detection
or new global body/rate limiter is claimed. Client rejection of an ambiguous response does not
roll back a transaction already committed on the server.
