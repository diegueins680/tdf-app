# RACI editor context — specification before implementation

Audit: PR 384 has an authenticated command and opt-in revision read, but the task DTO
intentionally contains neither mutation authority nor assignment windows nor eligible recipients.
An editor must not infer these from a role label, global people search, a cached session or RACI
membership. This increment supplies the prerequisite read context, not a new editor or marketplace.

`GET /event-operations/events/{eventId}/tasks/{activityId}/raci/context?afterPartyId=0`
returns exactly eventId, activityId, aggregateRevision (positive BIGINT decimal text), canManage,
operationReady, replaceableAssignments (existing partyId/role shape), eligiblePartyIds (<=100),
and optional nextAfterPartyId (last returned ID if another page exists). Cursor is a nonnegative
safe integer, default zero; no arbitrary limit, actor or clock. No names, contacts, profiles,
grant details, invitation data, UUIDs or assignment windows are disclosed.

EC01 privacy: require current exact task read; otherwise return opaque absence. Reuse current
session and event authorization fences. Only currently authorized task managers/owners receive
options; read-only users get canManage=false, operationReady=false and empty option arrays.
No global event.manage, coproduction, assignment or stale grant creates mutation authority.
EC02 coherence: lock feature, event authorization, then exact task metadata FOR SHARE. After
waiting, capture one clock and use one statement snapshot for authority, task state, current
RACI/accountability and eligible candidates. No read writes, repair or event task-write fence.
EC03 operationReady: current manage AND event draft/planning AND task planned/confirmed AND
required current A=1/R>=1. Options are empty when not ready. Replaceable source pairs are current
non-revoked unbounded rows only, preserving the command's conservative timed-source policy.
EC04 eligible candidates: current owners or effective event/exact-task read/manage grantees,
rechecked through the canonical task-read predicate; unique ascending safe Party IDs > cursor.
No relationship/grant is created. Membership, consent, scheduling, booking and acceptance are
not inferred. Filtering out source party/duplicate role pairs remains an editor responsibility;
the existing command revalidates all conditions authoritatively at execution.
EC05 pagination: keyset pages of 100 plus one sentinel; nextAfterPartyId iff more matches exist.
Each page is a fresh authorized snapshot, not a cross-page consistent roster or authority token.
Grant expiry/revocation and concurrent writes may invalidate options immediately; expected
revision and fresh command authorization remain mandatory. Stored revision does not advance
merely because time passes. No roster cache or automatic write/rebase on GET.
EC06 transport: strict typed response and target/cursor/uniqueness/order checks; malformed,
foreign or contradictory context becomes sanitized unavailable, never permissive defaults.
Existing task/read/command JSON remains unchanged. No-store responses; optional captured bearer
and signal in the web client. Additive SQL function/rollback only; production manifest and
activation remain unchanged. Removal of the function fails unavailable without fallback.

`RaciEditorContext` abstracts two grant levels plus none, two revisions, three clock instants,
one metadata reader/writer, matching/foreign target and eligible/ineligible candidates. Fresh
authorization, candidate filtering and metadata locking have named negative controls. No new
liveness/fairness or universal/refinement proof. `TaskReadStructure.als` adds manager-gated
recipient relations using its existing effective grants; owners remain abstracted as equivalent
effective task grants. Executable tests cover actual owner relationships, interval boundaries,
pagination, unsupported lifecycle, exact money-free revision transport, no mutation, rollback,
metadata waits, session revocation and strict API/client contracts. User-facing names/eligibility
picker, confirmation/conflict UX and editor activation remain the next dependent increment.

## Completed formal validation (2026-09-15)

Run from the repository root before feature implementation:

```sh
env JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
  bash scripts/verify-event-operations-formal.sh
```

Exit 0: 22 positive TLC configurations, 48 named negative controls, 13 PlusCal integrity
tests, 2 SAT Alloy scenarios and 13 UNSAT assertions. The three new negative controls
required exit 12 and their specific PrivateOptions/EligibleOptions/CoherentContext failures.
An initial unparenthesized latch expression produced an incompletely assigned successor;
the runner correctly rejected that model error. Explicit RHS parentheses fixed all three
latches before the complete successful rerun; no invariant was removed or weakened.

To recover untruncated state counts, the positive model was also rerun sequentially from
`formal/event-operations` with:

```sh
/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
  -XX:+UseParallelGC -jar /private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
  -workers 1 -metadir /private/tmp/tdf-raci-context-positive-20260915 \
  -config RaciEditorContext.cfg RaciEditorContext.tla
```

Exit 0: 3,724 generated / 1,584 distinct states, depth 9. Alloy recipient assertions use
scope 4 and effective-grant abstractions; the existing positive scenario remains task-read
only. These are bounded checks, not an unbounded proof or automatic SQL refinement proof.
See [PR 25 evidence](pr-25-raci-editor-context.md) for executed implementation tests and
remaining deployment, UI, timing, scale and integration limitations.
