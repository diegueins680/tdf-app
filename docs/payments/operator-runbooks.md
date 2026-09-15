# Payment operator runbooks

These procedures are safe defaults for sandbox and staging. They do not authorize production deployment, provider activation, a real charge, a refund or a payout.

Payment HTTP transport uses a shared no-implicit-retry TLS pool, no redirects, a
15-second total deadline and a one-MiB decompressed response limit. A timeout,
oversized/invalid response or HTTP error does not prove that no charge occurred.
Preserve the original attempt and reconcile; never switch providers based only on
an HTTP 502/503. See [ADR 0117](../adr/0117-shared-bounded-payment-transport.md) and
[transport verification](http-boundary-2026-09-14.md) for scope and rollout limits.

For per-request intent keys, legacy reference preservation, conflicting provider evidence and
the required old/new writer cutover, follow the [2026-09-14 retry safety supplement](retry-reconciliation-safety-2026-09-14.md).
For lost create responses after payment/expiry or account suspension, follow the
[exact-response recovery supplement](terminal-recovery-2026-09-14.md).
For bounded callback retention and compatibility with historical inbox rows, follow the
[notification minimization supplement](notification-minimization-2026-09-14.md) and
[ADR 0116](../adr/0116-minimized-provider-notification-evidence.md). Never export decrypted
callback bodies into support tickets, logs or test evidence.
For PlaceToPay signed identity and the bounded legacy-redelivery compatibility
rule, follow [ADR 0118](../adr/0118-signed-payment-notification-identity.md) and
[identity verification](notification-identity-2026-09-14.md). Never treat a newly
calculated callback ID as proof of a new payment or a verified signature.
For atomic query application and caller-owned transaction/lock requirements,
follow [ADR 0119](../adr/0119-atomic-provider-query-application.md) and
[reconciliation verification](reconciliation-atomicity-2026-09-14.md). The
independent missed-callback worker is not implemented by that change. Do not
manufacture a signed inbox event or infer no charge from a missing callback.

## 1. Configuration and activation

1. Identify the exact legal merchant, provider account, environment, USD settlement account and contracted capabilities.
2. Store provider credentials only in the environment's secret manager. Never put values in `fly.toml`, `.env` examples, issue/PR text, logs, screenshots or the database.
3. Register provider redirect, return and webhook URLs for the same environment. Production URLs must never target a sandbox account and vice versa.
4. Apply the canonical lifecycle migration. Confirm all `commerce_provider_account` rows are disabled.
5. Validate credentials with a read-only/authentication request and record the provider request ID, environment and timestamp. Do not record tokens.
6. Update the provider-account metadata only after contract and credential evidence is reviewed. Production additionally requires the matching `revenue_feature_flag` and a separately authorized change window.
7. Call `/commerce/payment-capabilities` for each intended flow. An empty `routes` array blocks the checkout UI; it is not a reason to bypass the gate.

Required server-only secret names are `DATAFAST_ENTITY_ID`, `DATAFAST_BEARER_TOKEN`, `DATAFAST_BASE_URL`, PayPal client credentials/merchant/webhook identity, `COMMERCE_EVENT_ENCRYPTION_KEY`, `COMMERCE_BANK_TRANSFER_INSTRUCTIONS`, `PLACETOPAY_LOGIN`, `PLACETOPAY_SECRET_KEY`, `PLACETOPAY_RETURN_URL`, `PLACETOPAY_NOTIFICATION_URL`, `PAYPHONE_TOKEN`, `PAYPHONE_STORE_ID`, and `PAYPHONE_RESPONSE_URL`. PlaceToPay also needs at least one exact site-method mapping in `PLACETOPAY_CARD_PAYMENT_METHODS`, `PLACETOPAY_BANK_PAYMENT_METHODS`, or `PLACETOPAY_DEUNA_PAYMENT_METHODS`; these are comma-separated provider IDs, not secrets, but still belong in environment configuration. Presence alone is not validation. A checkout method requires an enabled account plus exact environment-specific method/capability evidence; a documented capability row is never enough. The shared web/mobile recovery UX is implemented, but PlaceToPay and PayPhone remain absent until credentialed sandbox evidence exists and the exact method rows are explicitly activated.

Configure `PLACETOPAY_RETURN_URL` to the environment's HTTPS `/pagos/retorno` route. Test interruption both before and after the create response: the pre-response marker must permit only the exact same provider/method/idempotency key, and the post-response record must restore the exact checkout and attempt without putting the lookup token in the URL. An ambiguous, status-error, processing, or successful attempt must keep Datafast, PayPal, manual bank, and every other hosted rail disabled until the product order reflects the authoritative result. Configure `PAYPHONE_RESPONSE_URL` on the same environment and verify tab/app switching plus durable polling with controlled test users.

## 2. Sandbox qualification

For each provider and method, record commit SHA, provider environment/account alias, time, request/correlation IDs, sanitized response class and database evidence.

Exercise at minimum:

- approved, declined, cancelled, pending and timed-out payment;
- 3DS/OTP or other customer action and app/browser return;
- duplicate create/capture with the same idempotency key;
- interrupted client followed by status restore;
- full and partial refund, and void where supported;
- signed webhook success, invalid signature, duplicate, replay, out-of-order, retry and dead-letter;
- ambiguous transport result followed by authenticated reconciliation—never immediate provider fallback;
- settlement report/transaction match where the sandbox supplies it;
- mandate create/use/cancel/remove for recurring or saved methods;
- connected-seller onboarding, commission, refund/dispute allocation and payout only in a contracted marketplace sandbox.

Screenshots and mocks may support UX review but cannot be recorded as provider success evidence.

## 3. Webhook registration and rotation

1. Use an HTTPS public staging endpoint. Preserve the exact raw request bytes covered by the provider's signature scheme.
2. Store the webhook identity/signing material and inbox encryption key in server-only secrets.
3. Validate algorithm, signature, event ID, timestamp tolerance, environment and merchant before enqueueing.
4. After authenticating the original body, retain only the validated minimal evidence,
   encrypt/hash that projection, and insert through the unique
   provider/environment/merchant/event key. Do not persist arbitrary raw payloads.
5. Acknowledge only according to provider retry semantics. Processing happens from the persistent inbox, not inline assumptions.
6. Bind amount, currency, order and resource before a financial state change.
7. Rotate by accepting old/new secrets only for a short documented overlap. Test both, remove old, and record the rotation audit event.
8. For PayPhone, until a signed scheme is contractually documented, accept notification only as a hint and query the authenticated transaction endpoint before state change. Register `/NotificacionPago` when the provider portal requires the method name documented in PayPhone's current guide; the canonical notification URL is an equivalent alias.
9. For PlaceToPay, verify the documented SHA-256 notification but still query the authenticated session endpoint and bind the stored request ID, reference, amount and currency before state change.
10. PlaceToPay's current session documentation says callbacks are not retried.
    Acknowledge only after durable acceptance and reconcile missed notifications.
    Signed v2 IDs deduplicate new formatting/unsigned-field variants. Exact legacy
    redelivery retains its original row; reformatted pre-upgrade evidence may
    create one additional canonical query trigger. Never delete history to hide it.
    Recurring notifications without `requestId` are not supported by this handler.

## 4. Deployment and rollback

Pre-deploy:

- repository and generated-client checks pass;
- migration applies twice on disposable PostgreSQL and clean rollback succeeds;
- backup/restore and current schema checksum are verified;
- provider accounts and production flags remain disabled;
- no historical provider/order references will be rewritten;
- reconciliation baseline is zero or exceptions are owned;
- legal/accounting/PCI and provider evidence gates are signed off for any proposed activation.

Deploy code and schema separately from activation. Smoke-test health and read-only capabilities. If application behavior fails, roll back the image. If the lifecycle schema is unused, the provided rollback may remove it. Once any lifecycle/financial evidence exists, rollback intentionally refuses; roll forward instead.

The provider-execution rollback additionally refuses when a remote-operation row or an untrusted callback exists. It deletes only exact untouched feature-flag seeds, preserving any operator-modified row. A hosted redirect URL is encrypted and must never be copied into logs or incident tickets.

## 5. Ambiguous transaction incident

If the browser lost the create response, use **Recover original payment** with the original
provider/method/key and checkout lookup token. It can read a contacted operation even if new
payments are disabled. A 404, missing key or expired browser record is not no-charge evidence.
Do not ask the customer to clear storage, alter their PayPhone number, mint a new key or use
another provider. If the attempt ID is known, use the authorized payment-session GET. Otherwise
reconcile through the original order and its stored provider binding; never collect tokens,
secret keys or hosted redirect URLs in support tickets. A merely prepared operation still
requires new-contact gates and must not be manually labeled paid or declined to bypass them.

1. Freeze the checkout and prevent another provider attempt.
2. Record the timeout/transport class without sensitive payloads.
3. Query the original provider using the immutable merchant/order/resource binding and idempotency key.
4. Check the verified event inbox and provider console/settlement report.
5. If confirmed captured, finalize the original attempt exactly once. If confirmed no charge and no delayed event can arrive, record authoritative no-charge evidence before offering fallback.
6. If still unknown, leave the checkout pending and assign a reconciliation exception. Do not fulfill, refund through another provider, or ask the buyer to pay again.

## 6. Refund and void

1. Confirm requester authorization, order ownership/status, refundable policy and captured balance.
2. Select void only for a supported uncaptured authorization or documented same-day provider reversal. Otherwise create a refund against the original capture/provider.
3. Reserve the amount under an idempotency key so concurrent requests cannot exceed the balance.
4. Require an independent approver for staff refunds under the configured policy.
5. Submit with the original provider reference. An ambiguous response remains processing and requires reconciliation.
6. On authoritative success, append state/ledger evidence, allocate refund across tax/commission/seller liability, create the SRI credit-note workflow, and notify the customer.
7. Never change fulfillment/history generically to manufacture a refund.

## 7. Dispute and chargeback

1. Ingest or manually register the provider dispute ID, kind, amount, currency, reason and due date.
2. Link the original capture/order/customer/seller without exposing card or sensitive provider payloads.
3. Freeze related available seller balance up to the provider-defined liability; do not produce a negative payout silently.
4. Assign an owner and evidence deadline. Upload evidence only through the provider's secure channel.
5. Record provider fees, reversals, outcome and ledger entries from authoritative evidence.
6. Notify finance, fulfillment and the seller under the approved policy. A dispute is not automatically a refund.

## 8. Daily reconciliation and settlement

For each provider/environment/currency:

1. Import or query provider transactions and settlement batches with immutable external IDs.
2. Match provider resource → binding → attempt → intent → checkout → domain order.
3. Compare captured/refunded/disputed gross, provider fees, withholding, chargebacks and net settlement in minor units.
4. Confirm settlement allocation sums equal the batch summary and ledger entries balance by currency.
5. Open a deduplicated exception for missing, duplicated, late, amount/currency/merchant mismatched or ambiguous items.
6. Do not mark a batch settled without the bank/provider evidence and `settled_at`.
7. Finance signs the daily gross/fee/tax/refund/net/seller-liability report. Zero-variance or owned-exception evidence is required before provider rollout expands.

## 9. Seller balance and payout

1. Confirm connected-account status, KYC requirements, charges/payout enablement and provider-managed-funds flag.
2. Calculate gross, TDF commission, provider fee, tax/withholding, refunds/disputes and seller net from immutable terms/evidence.
3. Only available, non-reserved balance entries may be allocated.
4. Requester and approver must be different authorized staff. Approval does not mean paid.
5. Submit through the contracted provider with an idempotency key. Bind the provider payout ID once.
6. Mark paid only from signed/provider-query/settlement evidence; reconcile to seller and provider statements.
7. Never transfer pooled seller money from a TDF bank account unless a separately reviewed lawful operating model explicitly authorizes it.

## 10. Incident redaction checklist

Allowed in an incident ticket: internal UUID, correlation ID, provider name/environment, truncated safe external ID, state, amount/currency, timestamps and redacted error class.

Never include: secret/token/signature values, Authorization headers, full provider payloads, PAN, CVV, magnetic-stripe data, bank credentials, unredacted payer/seller personal data, vault tokens, encryption keys or secret digests.
