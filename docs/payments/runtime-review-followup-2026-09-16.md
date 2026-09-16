# Payment runtime review follow-up

## CI database boundary

GitHub Actions job104857160527 completed the Stack suite, then failed at `test-payment-audit-runtime.sh: 7: docker: not found`. The backend container already has PostgreSQL clients and a service. CI now creates a dedicated `tdf_payment_audit_test` database and passes its URL to the runner. The runner requires that exact database name and an empty public schema, then applies the fixture transactionally. Without a configured URL, local runs retain the owned Docker fixture. The service-mode test disables Docker for the child runner, executes all three PostgreSQL examples and verifies populated/wrong-name databases are refused.

The initial Stack build/test subshell must not inherit `TDF_PAYMENT_AUDIT_DATABASE_URL`:
`PaymentAuditSpec` uses its presence to enable the PostgreSQL examples, but their
fixture is not loaded until the subsequent mandatory runtime runner. The parent
retains the URL so that runner initializes the fixture and executes the same three
examples with `--fail-on=empty`. Orchestration regressions cover this ordering and
verify that failures from either Stack or the runtime runner remain fatal. This
changes test setup only; no production schema, credentials, or deployment change
is required, and no payment assertion is removed.

The service password is scoped to `createdb`, rather than exported to the entire
build/test step: an inherited `PGPASSWORD` conflicts with the isolated `DB_PASS`
fixtures used by configuration tests. The audit runner authenticates using its
dedicated URL. Hosted run `35123867242` confirmed both setup defects (three missing
table failures and six configuration-fixture conflicts); the aggregate quality
failure was downstream. A workflow regression guards the password scope.

## Financial summary clarity

Every settlement, refund, dispute, seller-balance, payout and reconciliation heading now includes its environment. The regression uses identical provider/currency/status across sandbox and production in all six families. Existing intent, component and commission environment labels remain.

## Verified refund synchronization

The runtime-sync migration adds an AFTER UPDATE trigger for verified `processing` to `succeeded` refunds linked to canonical intents. Existing approval, immutable allocation, provider/environment/merchant and completion-evidence controls run first. The new trigger locks the intent, verifies the binding and remaining captured amount, updates refunded amount/status and writes correlated state history in the same transaction. Repeated success writes do not count twice. Legacy unbound attempts retain their existing behavior; this is forward synchronization, not a silent historical backfill.

PostgreSQL coverage follows actual requested/approved/processing states with distinct approver and immutable line allocations: partial600, replay, rejected refund above canonical capture with full rollback, rejected cross-environment binding, then final1900 producing refunded2500 and exactly two refund history rows. Rollback retains all five capture/refund history rows. The generic failed-intent fallback concern remains blocked pending provider-specific no-charge/resource-finality evidence.

## Migration introduction review

The cited e6ef717 ancestry observation does not apply to current head19af56e44: all five payment `introducedBy` SHAs are ancestors and contain their registered SQL paths. No manifest SHA or ancestry gate was changed to bypass the requirement. Evidence is recorded in the audit's payment-migration-introduction-verification.json.

Executed validation results and failures are recorded in the audit validations.md. No deployment or provider activation was performed.

Final validation: operator UI7, typecheck, targeted ESLint, repository quality, strict JSON/CSV1117, canonical PostgreSQL migration suite and both service/Docker runner modes passed. The complete production-shaped automatic migration rehearsal passed twice/idempotently using CI's pgvector/pgvector:pg17 image and the already validated backend binary. Earlier fixture/setup failures are retained in the audit logs.
