# Scoped event-operations catalog review contract

Depends on PR #413 at `cbbb3da9f20c2314c73a5114b8c0aa943647054e`.
The approved CI plan includes individual catalog review. This increment reviews
only the 16 discovered fingerprints in the canonical event-operations foundation,
typed API, database boundary and RACI editor. Source contents, consumers, state
machines and existing SQL checks were inspected before classification.

1. Preserve all source code, migrations and existing decisions. Add a decision
   only for an exact current fingerprint with a source-specific justification.
2. Transport method names, local UI phases and sanitized exception variants are
   execution mechanics, not administrator-editable business lists.
3. Authorization, ownership, visibility, RACI, overrides, audit and guarded event
   lifecycle discriminants are security/system registries with closed executable
   semantics. Persisted policy rows remain authoritative where implemented;
   classifying a check does not create a registry administration API or activate it.
4. Attendance modes and task-status DTOs are governed reference data; typed consumers
   must agree with canonical persisted constraints, not invent a second catalog.
5. Never mark a business list technical merely to obtain a green audit. No bulk
   generated approvals, stale-decision deletion, scanner exclusion or waiver.
6. Verify against the initialized exact mobile gitlink. An incomplete source tree
   cannot justify deleting mobile decisions. Expect the full gate to remain red
   for all other unreviewed/stale candidates and report its exact counts.

No runtime/domain behavior changes, so existing formal models remain unchanged;
this is review metadata, not new TLC/Alloy evidence. The exact fingerprint scanner
and its regression suite are the executable contracts. Rollback removes only
these decisions and review documentation, never persisted application data.
