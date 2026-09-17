# Invitation creation and prerequisite deletion boundaries

Invitation creation now locks the current social event and checks organizer or strict-admin authority in the same database transaction as insertion. PostgreSQL uses the existing event row lock; SQLite acquires its write lock before reading authority. Sender, recipient, pending-status and visibility validation remain. Actual handler tests race both organizer transfer and organizer removal against creation, expect 403 and unchanged invitation counts, then verify an authorized creation still succeeds.

Legacy logistics deletion invokes the opt-in foundation guard before deleting any relation. The guard acquires the same event write fence as policy and dependency changes, and rejects both protected tasks and prerequisites referenced by protected tasks. The direct activity deletion trigger invokes it too. Rejection uses SQLSTATE 23514 and preserves graph, activity and protected-task version. Unprotected deletion remains supported.

The HTTP guard runs only on PostgreSQL with the new helper and active foundation trigger. Foundation-inactive deployments and the documented rollback retain legacy behavior. Apply the additive foundation migration before relying on this guard; there is no automatic production migration or deployment in this audit.

Validation evidence is recorded in the repository audit report: full Stack suite, actual PostgreSQL invitation concurrency, foundation apply/reapply/rollback and prerequisite deletion, and repository quality. No new API/client shape or product workflow was introduced.
