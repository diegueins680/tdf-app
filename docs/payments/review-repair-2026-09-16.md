# Payment review repairs

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

No schema migration, credential change or provider configuration is required.
Deploy backend and generated clients together. An application rollback would
restore the identified defects; prefer a forward fix. Existing payment evidence
and historical orders are not rewritten or deleted.

## Additional review boundaries

An authenticated, bound decline may now retire its unpaid canonical intent and
all related create/capture attempts in the same transaction. The checkout lock
serializes this with fallback creation; replay cannot change a newer provider's
checkout status, duplicate transition history, or restart the declined provider.
Only PayPal `DECLINED` and Datafast `800.100.151`, `800.100.153`, `800.100.155`
are classified as confirmed no-charge outcomes. These are explicit decline codes
documented in [PayPal capture status](https://developer.paypal.com/sdk/orders/v2/definitions/capture_status/)
and the [OPPWA result-code reference](https://docs.oppwa.com/sites/default/files/eposyaml/payments_api.yaml).
Unknown codes, request/authentication errors, timeouts, missing or mismatched
amount/currency/order/merchant bindings remain blocked pending reconciliation.
Authorized/captured money cannot be retired by this path. This does not infer
that every provider error means no charge and does not rewrite historical failures.
The existing positive-only synchronization trigger remains unchanged.

`MERCH_BANK_TRANSFER_INSTRUCTIONS` now enables only merchandise routes; other
flows require `COMMERCE_BANK_TRANSFER_INSTRUCTIONS`. Subscription discovery adds
the recurring capability to the existing one-time/completion requirements.
Operator capability labels require verification for the exact account environment.

Amount components now join through their intent to the owning checkout and group
by environment as well as type/source/currency. The additive `cacEnvironment`
contract and generated web/mobile clients mirror `cpiEnvironment`; legacy UI
responses remain visibly unknown, never implicitly production.

The PostgreSQL runtime audit now additionally applies the real canonical forward
migrations to a second disposable database and tests declines, ambiguous failures,
binding mismatches, fallback, retired-provider replay, and late-decline replay.
It uses synthetic sandbox account metadata only, never live credentials or
production account activation. No new schema migration or destructive rollback
is needed; retain existing evidence and prefer a forward application fix.
