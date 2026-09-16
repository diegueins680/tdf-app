# Payment review repairs

## Follow-up: CI database, environment labels and verified refunds

The backend CI container has PostgreSQL client tooling and a `postgres` service,
but no Docker daemon. Mandatory runtime regressions now create a fresh owned
`*_test` database on that service; a failed creation never deletes an existing
database. Local runs retain owned disposable containers. The CI policy tests
exercise both ownership outcomes rather than skipping database regressions.

All six financial summary families now show their environment in the heading.
The integrated operator UI suite passes ten tests, including otherwise identical
sandbox/production cards. Every currently registered migration introduction was
verified as an ancestor after integrating the concurrent branch history; no
unrelated commit was substituted to bypass release ancestry validation.

The additive `2026-09-16_payment_intent_refund_sync.sql` migration updates bound
canonical intents from verified successful refunds in the same transaction as
the refund write. It records financial changes in immutable state history,
preserves dispute/chargeback states and does not release failed payment intents.
Existing verified bound refunds are reconciled under migration locks; inconsistent
bindings or amounts abort rather than being silently adjusted. Reapplication and
duplicate success updates do not double-count. Legacy unbound refunds retain
their prior behavior. Existing migration checksums are unchanged.

Apply through the reviewed migration lane before relying on canonical refund
totals. Stop refund writers before using the paired rollback, which removes only
the new synchronization trigger/functions and preserves totals and history.
Resuming old writers after rollback can make canonical totals stale; prefer a
forward fix. No production data, provider activation or deployment was changed.

Datafast research: its [official integration guide](https://datafast.docs.oppwa.com/tutorials/integration-guide)
documents failed and successful transactions sharing one checkout ID and a
30-minute expiry. Its [transaction reports](https://datafast.docs.oppwa.com/reporting-transaction)
support querying all transactions by merchant reference. Neither a decline nor
expiry alone proves that an earlier submitted bank transaction cannot complete.
The no-charge fallback finding remains blocked pending authoritative finality
validation; no merchant sandbox transaction has been executed in this audit.

The email/WhatsApp marketplace checkout remains a contact request. It retains
buyer, inventory and idempotency validation and sends the order email only on
creation. It does not create a payment attempt, select bank transfer or overwrite
an existing order's payment state on replay. Coordinating an order is not evidence
of payment; settlement still requires an independently qualified payment flow.
An active online payment prevents switching to contact coordination until its
outcome is resolved, preserving the concurrent repair's payment-state guard.

The canonical attempt boundary now requires Datafast server verification and
PayPal capture capability before creating or authorizing a payment. Availability
discovery alone is insufficient because callers can invoke checkout directly.
Existing environment, contract, credential and marketplace payout gates remain.
No provider is activated by these changes.

Operator payment-intent totals join their owning checkout and group by environment,
status and currency. `cpiEnvironment` is an additive API field, reflected in the
OpenAPI contract, generated clients and independent UI cards. Consumers must not
sum sandbox and production cards together.
Legacy responses without the new field are labeled as an unreported environment,
never silently assumed to be production.

Regression coverage includes routing without completion capabilities, a source
boundary check for the contact handler, the actual overview query against an
isolated SQLite fixture, PostgreSQL environment grouping, and UI
cards with matching currency/status in different environments. The source check
is not a substitute for the actual-handler test. The concurrent repair's
`test-payment-audit-runtime.sh` exercises sale/rental contact replays, pending-online
denial, and the real overview query in disposable PostgreSQL. `quality:backend`
now runs it after building the test binary, so CI cannot silently omit those cases.

The contact/routing and environment-label fixes need no additional schema,
credential change or provider configuration. The refund synchronization described
above separately requires its additive migration.
Deploy backend and generated clients together. An application rollback would
restore the identified defects; prefer a forward fix. Existing payment evidence
and historical orders are not rewritten or deleted.

## Additional review boundaries

The no-charge fallback finding remains **blocked**, not fixed. A local prototype
retired intents on selected decline codes, but review found that a transaction
decline is not sufficient evidence that the enclosing hosted resource is terminal.
That prototype was withdrawn before any parent-branch push. The existing
positive-only synchronization trigger and fail-closed active-intent guard remain.

Required evidence: the configured Datafast/PayPal merchant integration must identify
an authenticated terminal resource outcome (or confirmed cancellation/expiry),
bind it to the immutable checkout and prove that no authorization, capture, pending
attempt or later hosted retry can charge it. The current status parsers lack that
resource-finality contract. Do not resolve the review or merge this PR until it is
implemented and qualified with bound callback/replay tests. No secret value is
needed in chat; provider documentation and a sanitized sandbox trace are sufficient.
The [PayPal failure guide](https://developer.paypal.com/api/handle-payment-failures/)
explicitly permits restarting some declined flows; a decline string alone is not
proof of irreversibility. No live provider qualification was performed here.

`MERCH_BANK_TRANSFER_INSTRUCTIONS` now enables only merchandise routes; other
flows require `COMMERCE_BANK_TRANSFER_INSTRUCTIONS`. Subscription discovery adds
the recurring capability to the existing one-time/completion requirements.
Operator capability labels require verification for the exact account environment.

Amount components now join through their intent to the owning checkout and group
by environment as well as type/source/currency. The additive `cacEnvironment`
contract and generated web/mobile clients mirror `cpiEnvironment`; legacy UI
responses remain visibly unknown, never implicitly production.

The PostgreSQL runtime audit remains mandatory in `quality:backend`; the CI-policy
regression checks that it cannot silently omit its matching tests. No new schema
migration or destructive rollback is needed for the completed environment/routing
fixes; retain existing evidence and prefer a forward application fix.
