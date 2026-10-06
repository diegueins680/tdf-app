# Payment configuration and amount-component boundaries

Manual transfer runtime qualification now receives the requested product flow.
The merch-specific instruction variable qualifies only merchandise; other flows
require generic commerce instructions. Existing provider capability, contract,
environment and enablement checks remain authoritative.

Amount-component summaries now join their canonical intent and checkout and group
by immutable environment as well as component/source/currency. The DTO, OpenAPI,
generated web/mobile contracts and operator cards carry that environment. Older
responses explicitly display an unknown environment. No schema migration, provider
activation, payment movement or new product flow is introduced.

Verification: 2554 full Stack backend examples, five operator UI tests, mobile and
web typechecks, and three actual PostgreSQL regressions passed. The component case
verifies separate production/sandbox groups even when their final totals match.
The configuration tests cover merch-only, all seven non-merch flows, generic and
blank settings. The mobile generated artifact is published in draft TDF-mobile#83
at337d46f25a8b7c7c688aa3657ffb599ecb4c47c7 and matches the web artifact byte for byte.

## Remaining no-charge fallback review

PR331 comment4022349405 remains open. `Checkout.recordPaymentFailure` accepts an
untyped code and is used for provider transport failures, verification mismatches
and status responses. That alone is not authoritative evidence of zero financial
exposure. Retiring every intent marked failed would make ambiguous outcomes eligible
for a second provider and could defeat the existing protection against double charge.

The canonical state machine already has `PaymentFailureConfirmed`, but the legacy
bridge has no typed no-charge evidence tied to a specific provider resource, immutable
binding and terminal resource state. The smallest safe follow-up is to establish
that provider-specific evidence contract, map only qualified terminal results to it,
and atomically transition/audit the intent before cross-provider fallback. Preserve
pending, mismatched, transport-error and potentially authorized outcomes until
reconciled. No blanket failed-attempt-to-failed-intent synchronization was added.

The OPPWA legacy reference distinguishes declines from errors that can retain an
amount reservation; it is supporting context, not current Datafast merchant
qualification: [OPPWA reference](https://docs.oppwa.com/sites/default/files/legacy/PayPipe_Starter_Package.pdf).
[PayPal's capture definition](https://developer.paypal.com/sdk/orders/v2/definitions/capture_status/)
also separates completed, pending, declined and failed states. Current merchant
contract/resource-finality evidence and bound callback/replay tests are required
before declaring a provider-neutral no-charge mapping complete. Staging/provider
qualification was unavailable during this audit; no live provider request was made.
