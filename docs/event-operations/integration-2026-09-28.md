# Event stack integration — 2026-09-28

This candidate reconciles the existing event delivery chain through PR417 with main
`cc244b1f86603055997b51379b297baebfd3e7ce`. It preserves original commits and authors
through normal merges. The separate PR339 successor
`259becd7b3763e084b54da8749b1e6fde8d9d862` is also included: it was not an ancestor
of the stack tip and contains valuable deletion, dependency-approval and RACI guards.
No source PR or branch may be retired until the approved replacement is merged.

## Compatibility decisions

- Keep main's token identity and add the opaque authenticated-session witness needed
  for transaction-time revocation fencing. Synthetic test actors carry both fields.
- Retain main's current invitation/logistics handlers, all-change formal workflow triggers,
  arithmetic verification, evidence admission, lazy analytics loading and onboarding replay.
- Preserve the production migration manifest byte-for-byte. Event adapter migrations remain
  opt-in and unregistered; integration does not authorize feature activation or deployment.
- Generate both client projections from the combined canonical OpenAPI contract. The mobile
  companion must be reviewed and reachable before the parent pin is eligible to merge.
- Keep the original catalog retirement ledger immutable. Explicit successor mappings and
  a separate archive preserve obsolete fingerprints; four new API projections have individual
  source reviews. The stale-decision gate and negative controls remain enforced.

## Defects repaired during integration

The old task-commit adapter overwrote current foundation protections. Its validator now
requires the current dependency snapshot in completion overrides, and its write lock retains
protected-task deletion checks. Rollback retains integrity triggers rather than restoring a
weaker historical schema. New regressions reject stale graph approvals and unsafe deletion
before and after rollback; deliberately invalid historical-fixture installation remains rejected.

Completion decision cases now roll back their individual test state after all assertions,
so expired assignments cannot contaminate later cases. Expiry-race fixtures use attributed
retirement and valid replacement after asserting rejection. Task-read projection tests retain
their transitional-state assertions inside a transaction and add immediate-constraint negative
controls proving future required assignments cannot commit. No database guard is disabled
for ordinary fixtures, no assertion is removed, and no CI job is skipped.

Current validation results and remaining blockers are recorded in the branch-audit report.
Earlier PR delivery records describe their original snapshots; this integration record governs
compatibility and rollback behavior of the combined candidate.
