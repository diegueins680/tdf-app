# Provider capability and secret-boundary matrix

Last reviewed: 2026-08-14. Values are intentionally omitted. A documented provider feature is not
evidence that the TDF merchant account is enabled for it.

| Rail | Implemented contract | Server verification | Refund | Recurring | Outbound payout | Production state |
|---|---|---|---|---|---|---|
| Datafast | Canonical service checkout/resource creation and server status confirmation | Immutable checkout/resource path, amount, USD currency, environment, merchant entity, internal order and merchant reference | Explicitly disabled; no merchant refund contract verified | Not assumed | Not offered by this checkout integration | Checkout disabled pending certification; webhook/refund capabilities separately off |
| PayPal Checkout | Canonical Orders v2 create/capture, verified webhook inbox, full/partial capture refund and on-demand reconciliation | Remote signature verification over the exact raw event; event replay window/deduplication; order/capture/custom ID, amount, currency, environment and payee merchant; request-id retry protection | Two-person request/approval, immutable allocation, compensating ledger and credit note; sandbox-capable, production-off | Separate Subscriptions capability, not enabled | Separate Payouts product, not enabled | All production flags disabled pending credentialed sandbox evidence and authorization |
| Stripe | Legacy domain paths only | Existing domain-specific webhooks | Domain-specific legacy behavior | Existing legacy course path only | Connect dependency exists for tips | Optional; not an Ecuador-critical dependency and unavailable in shared service checkout |
| Bank/cash/POS | Canonical manual attempt and evidence-review record | Authorized staff approval, never browser selection | Compensating manual record required | No | Admin-reviewed settlement only | Customer selection is implemented for service checkout; approval policy/runtime remains incomplete |
| Cardano donation | Public address display remains separate | No chain confirmation adapter in this branch | Not applicable | No | No | Unverified references must not be shown as received funds |

## Datafast evidence and unknowns

Official Datafast integration documentation separates test and production configuration, requires
the checkout resource to be created server-side, uses USD/`DB` payment parameters, requires a unique
merchant transaction identifier, and provides status-query integration. Production testing and
certification require Datafast coordination:

- <https://developers.datafast.com.ec/index.aspx>
- <https://developers.datafast.com.ec/msdk.aspx>

Still required from Datafast/acquirer: merchant/entity/MID/TID scope, authenticated notification
semantics, refund/void boundary and partial-refund support, installment products, 3-D Secure flow,
tokenization/recurrence, rate limits, reconciliation files, test cards, certification, and an
approved production verification window. None is inferred from the generic docs.

Server-only variable names currently read by the service adapter:
`COMMERCE_CHECKOUT_ENV`, `DATAFAST_ENV`, `DATAFAST_ENTITY_ID`,
`DATAFAST_BEARER_TOKEN`, `DATAFAST_BASE_URL`, and
`DATAFAST_TEST_MODE`. Other repository paths inventory `DATAFAST_MID`, `DATAFAST_TID`,
`DATAFAST_PSERV`, `DATAFAST_USER_DATA2`, and `DATAFAST_VERSIONDF`. Values belong only in the
deployment secret manager.

The current Datafast reference distinguishes checkout creation (`000.200.100`), production payment
success (`000.000.000`), and the documented phase-one/phase-two test approvals (`000.100.110` and
`000.100.112`). The service adapter matches these exact codes to the immutable checkout environment;
it does not treat the broader `000.100.*` range or a pending result as paid.

## PayPal evidence and unknowns

Official PayPal documentation defines Orders v2 server-side create/capture, capture lookup and
refunds, `PayPal-Request-Id` idempotency, and authenticated webhook signature verification/retries:

- <https://developer.paypal.com/api/rest/integration/orders-api/>
- <https://developer.paypal.com/docs/api/payments/v2/>
- <https://developer.paypal.com/api/rest/webhooks/rest/>
- <https://developer.paypal.com/docs/multiparty/issue-refund/>
- <https://developer.paypal.com/api/rest/webhooks/event-names/>

The service adapter reads `COMMERCE_CHECKOUT_ENV`, `PAYPAL_CLIENT_ID`, `PAYPAL_CLIENT_SECRET`,
`PAYPAL_ENV`, the non-secret account binding `PAYPAL_MERCHANT_ID`, `PAYPAL_WEBHOOK_ID`, and
`COMMERCE_EVENT_ENCRYPTION_KEY`. The last two are server-only secret-manager values; the encryption
key is not a provider credential and must be independently rotated. Sandbox and live app identity, webhook registration,
merchant refund permission, supported currencies/markets, and any Subscriptions or Payouts product
must be verified independently.

PayPal explicitly warns that top-level order `COMPLETED` is not sufficient for fulfillment. The
service adapter therefore requires exactly one purchase unit and capture and uses
`purchase_units[].payments.captures[].status = COMPLETED`; a pending or ambiguous nested capture
cannot mark the checkout paid.

The public webhook endpoint validates bounded visible-ASCII headers, allows only PayPal HTTPS
certificate hosts for the immutable environment, rejects events more than four days old or more
than five minutes in the future, and posts the exact raw event to PayPal's verification endpoint.
Only `SUCCESS` evidence is encrypted into the inbox. Event IDs and immutable payload hashes are
deduplicated; processing uses bounded retries and dead-letter review. PayPal's mock webhook
simulator cannot pass remote verification and therefore cannot transition these records.

Refund execution is limited to a succeeded, bound PayPal capture. The immutable request reserves
the remaining captured balance under a checkout lock and snapshots an active bilingual reason from
`commerce_refund_reason_code`; a different authenticated administrator must approve it.
`PayPal-Request-Id` is the internal refund UUID. A refund becomes `succeeded` only
when provider ID, status, exact amount and currency match; then a balanced compensating ledger entry
and one credit note are created. Provider outage leaves a retryable processing record. Production
webhook and refund flags remain false by default.

## Asset and DDEX boundaries

Local staging reads `DDEX_STORAGE_BACKEND=local-private` and an absolute
`DDEX_PRIVATE_STORAGE_ROOT`. Production still needs private object-storage credentials by reference,
server-side encryption, signed-link keys, malware-scanner integration, retention/quarantine/backup
policy, and per-recipient credential references. No provider payload, bearer token, client secret,
protected URL, card data, KYC/tax document, or payout destination may enter logs or documentation.

## Capability approval record

Before turning on a provider/domain flag, record: environment, merchant/legal entity, product name,
documented capability, provider confirmation reference, tested operation, non-sensitive evidence
hash, reviewer, timestamp, limits, rollback/kill switch, reconciliation owner, and expiry/review date.
Capabilities not present in that record remain unavailable.

For the implemented service slice, provider verification mismatch creates an open reconciliation
exception and leaves payment unconfirmed. A verified Datafast status query, PayPal server capture,
or signature-verified PayPal capture event may post one receipt and one balanced ledger transaction.
The refund and webhook runtimes are implemented but remain capability-gated. Local fixtures are not
provider sandbox evidence or production authorization.
