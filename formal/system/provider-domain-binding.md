# PlaceToPay and PayPhone checkout admission

This contract covers the `awaiting_payment`/`processing` to `paid` admission
boundary in PostgreSQL for service bookings, event tickets, course registrations
and Domo deposits. It does not authorize provider activation, production
migration application or real-money verification requests.

For a checkout C, admission requires a succeeded payment attempt A and immutable
binding B with:

- A.checkout = C.id; A.environment = B.environment = C.environment;
- A.provider = B.provider; A.merchant_account = B.merchant_account;
- A.amount = B.amount = C.total; A.currency = B.currency = C.currency.

For PlaceToPay, B.resource_type is `session`; for PayPhone it is `sale`.
A.operation is `create`. A durable create operation O must belong to A, with
exact provider, environment, merchant account, resource ID and provider
reference equal to B. A's payment intent must identify C and agree on provider,
amount and currency. The provider reference identifies O; it need not equal
C.domain_order_id. Neither an arbitrary binding nor an unsigned callback alone
satisfies this contract. Authenticated query validation in the application
remains a separate necessary trust boundary.

Legacy Datafast/PayPal/Stripe bindings retain their checkout/capture/payment
resource kinds and the internal order reference comparison. Approved manual
payments retain independent submitter/reviewer checks and manual evidence.
Domo additionally requires an accepted, unexpired date hold. No fulfillment,
policy, immutable evidence, monetary snapshot or ledger guard is removed.

The additive migration replaces the four payment admission functions in one
transaction while excluding checkout writers. Its pure predicate is shared by
those functions. Application rollback retains these guards and all evidence;
use a reviewed forward migration for any later change.

`scripts/load-payment-test-schema.sh` loads the production-shaped baseline and
the complete reviewed manifest, then replays the manifest and verifies the
ledger count. The provider runtime runner uses this schema, without reduced
domain tables or disabling domain triggers. Synthetic domain builders create
real event types/workflows, orders, policies, accepted terms and runtime rows.
The independent PostgreSQL matrix tests both new rails on all four domains,
wrong accounts/environments/references/resource kinds/resource IDs/intents/
currencies/amounts, legacy rails, and unchanged state after rejection. Real
blocking is witnessed at READ COMMITTED and REPEATABLE READ; a serialization
failure is retried in a fresh transaction. Replays preserve paid state and
fulfillment. Reinstalling each original guard reproduces denial of valid new
rails; removing the reference comparison admits a named forbidden case.

These finite tests establish their observed boundaries. They are not a universal
implementation/specification proof, provider sandbox certification, or evidence
that production has received the migration. The compiled provider/refund suite,
current-head CI and independent protected review remain required.
