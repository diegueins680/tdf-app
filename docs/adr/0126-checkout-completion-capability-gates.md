# ADR 0126: Qualify the complete implemented checkout

Date: 2026-09-15. Status: implemented on a dependent draft branch; not activated.

## Context

Upstream payment-core PR #331 added `requireCheckoutCompletion` and corrected
Datafast's historical confirmation operation in commit `be010ac27`. That change
must coexist with the later durable intent, replay, recovery and evidence work.
This branch merges the updated core history and latest observed main rather than
reimplementing or cherry-picking that work without its dependencies.

Database-backed integration tests exposed a remaining gap: PlaceToPay bank and
DeUna routes, and PayPhone wallet routes, were still advertised when their
method-specific `server_verification` capability was merely documented rather
than verified. The application finishes these methods using an authenticated
status query. A create-only qualification is therefore insufficient.

## Decision

The public runtime boundary applies the following minimum requirements before
selecting routes. Caller-supplied requirements are additive and cannot remove
these requirements. Existing account, contract, credential, method, product,
currency, environment, runtime-configuration and feature checks remain in force.

| Implemented method / operation | Required capability evidence |
|---|---|
| Public card checkout | `one_time` and `server_verification` |
| Public PayPal wallet checkout | `one_time` and `capture` |
| Public PlaceToPay bank redirect / DeUna | `one_time` and `server_verification` |
| Public PayPhone wallet | `one_time` and `server_verification` |
| Independently reviewed manual bank method | Existing `one_time`; no invented remote capture/query operation |
| Marketplace flow | All applicable requirements above plus `connected_accounts`, `split_settlement`, `seller_payouts` |
| Datafast's historical `capture` attempt | `one_time` and `server_verification`; preserve the historical operation name because it verifies an automatic debit rather than sending a separate capture |
| PayPal and other true capture attempts | Existing `capture` requirement, plus applicable marketplace requirements |

`loadRuntimeReadyRoutes` is shared by the public capability API, product checkout
method lists and the new hosted-session creation gate. Tightening this boundary
therefore removes an unqualified method from presentation and rejects a new
hosted session requested directly. No client can opt out with an empty required-
capabilities array. The pure `routePayments` engine remains usable for specific
internal operations; it does not itself assert that a complete public checkout
exists. Normalization is idempotent and preserves caller restrictions.

Existing create replay precedes new-session qualification so interrupted clients
can still recover the original operation after capability/account revocation.
This neither contacts a new provider nor authorizes another charge. Datafast and
PayPal create/confirmation attempts remain on the original intent, exact replay
preserves identity and a different capture key cannot create another continuation.

No provider is enabled. No new method, payment link, native flow, financial
transition, external request or migration is added. Payment links and other
unimplemented executors remain behind their existing runtime gates; a generic
provider profile is not proof that a usable customer flow has been implemented.

## Security and rollout

- Prevent presenting or initiating a query-backed payment without verified
  completion capability. Runtime credentials alone are not verification evidence.
- Preserve method-specific verification: another method's capability does not
  qualify this method. Sandbox evidence does not qualify production.
- Retain all marketplace requirements; this change does not authorize custody,
  split settlement or seller payouts without separately qualified provider/legal
  arrangements.
- Do not modify historical attempt names, external references, idempotency keys,
  intents, receipts, ledger entries or held-payment exceptions.
- A newly hidden method is a qualification failure, not evidence of no charge
  on a previous attempt. Keep recovery and reconciliation running under their
  existing authorization and rate limits; never use hiding as permission to retry.
- Application rollback reintroduces the underqualification defect. Disable new
  checkout for affected methods first, retain original-attempt recovery, and keep
  all financial history. No data rollback is needed or recommended.

## Primary sources and confidence

Accessed 2026-09-15; high confidence in the documented technical mechanisms,
not merchant onboarding, enabled accounts or successful sandbox execution:

- [Datafast developer guide](https://developers.datafast.com.ec/index.aspx):
  purchases use `DB` and the return resource is followed by a server-side GET to
  obtain transaction status. Datafast requires its own pre-production integration
  certification. Historical application `capture` naming is our compatibility
  interpretation of the code, not a new provider operation. No published sample
  credential or insecure sample TLS setting was copied into tests or runtime.
- [PayPal Orders v2](https://developer.paypal.com/api/orders/v2) and
  [checkout integration](https://developer.paypal.com/platforms/checkout/standard/integrate):
  create and capture are distinct endpoints; capture completes the payment flow.
  The integration-page evidence supports the operation requirement, not TDF
  marketplace eligibility.
- [PlaceToPay session API](https://docs.placetopay.dev/en/checkout/api/reference/session/):
  authenticated session lookup returns the session and transaction information.
- [PayPhone API Sale](https://docs.payphone.app/api-sale): its integration flow
  includes a GET status lookup after the customer acts in the app. The local
  verification gate is our application policy derived from that required flow.

The generic OPPWA integration-guide URL and an older PlaceToPay query-session URL
could not be opened in this run; they are not claimed as verified sources. The
official pages above were accessible. No pricing/contract/market-matrix row was
silently refreshed. See [integration evidence](../payments/stack-integration-2026-09-15.md).
