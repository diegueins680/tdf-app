# Payment review repairs

The email/WhatsApp marketplace checkout remains a contact request. It retains
buyer, inventory and idempotency validation and sends the order email only on
creation. It does not create a payment attempt, select bank transfer or overwrite
an existing order's payment state on replay. Coordinating an order is not evidence
of payment; settlement still requires an independently qualified payment flow.

The canonical attempt boundary now requires Datafast server verification and
PayPal capture capability before creating or authorizing a payment. Availability
discovery alone is insufficient because callers can invoke checkout directly.
Existing environment, contract, credential and marketplace payout gates remain.
No provider is activated by these changes.

Operator payment-intent totals join their owning checkout and group by environment,
status and currency. `cpiEnvironment` is an additive API field, reflected in the
OpenAPI contract, generated clients and independent UI cards. Consumers must not
sum sandbox and production cards together.

Regression coverage includes routing without completion capabilities, a source
boundary check for the contact handler, the actual overview query against an
isolated SQLite fixture, PostgreSQL environment grouping, and UI
cards with matching currency/status in different environments. The source check
is not a substitute for a live HTTP checkout test.

No schema migration, credential change or provider configuration is required.
Deploy backend and generated clients together. An application rollback would
restore the identified defects; prefer a forward fix. Existing payment evidence
and historical orders are not rewritten or deleted.
