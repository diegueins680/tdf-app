# Payment operator runbooks

These procedures are safe defaults for sandbox and staging. They do not authorize production deployment, provider activation, a real charge, a refund or a payout.

## 1. Configuration and activation

1. Identify the exact legal merchant, provider account, environment, USD settlement account and contracted capabilities.
2. Store provider credentials only in the environment's secret manager. Never put values in `fly.toml`, `.env` examples, issue/PR text, logs, screenshots or the database.
3. Register provider redirect, return and webhook URLs for the same environment. Production URLs must never target a sandbox account and vice versa.
4. Apply the canonical lifecycle migration. Confirm all `commerce_provider_account` rows are disabled.
5. Validate credentials with a read-only/authentication request and record the provider request ID, environment and timestamp. Do not record tokens.
6. Update the provider-account metadata only after contract and credential evidence is reviewed. Production additionally requires the matching `revenue_feature_flag` and a separately authorized change window.
7. Call `/commerce/payment-capabilities` for each intended flow. An empty `routes` array blocks the checkout UI; it is not a reason to bypass the gate.

Required server-only secret names are `DATAFAST_ENTITY_ID`, `DATAFAST_BEARER_TOKEN`, `DATAFAST_BASE_URL`, PayPal client credentials/merchant/webhook identity, `COMMERCE_EVENT_ENCRYPTION_KEY`, `PLACETOPAY_LOGIN`, `PLACETOPAY_SECRET_KEY`, `PAYPHONE_TOKEN`, and `PAYPHONE_STORE_ID`. Presence alone is not validation. PlaceToPay and PayPhone remain disabled until the adapter executor and credentialed sandbox evidence are complete.

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
4. Encrypt the raw payload, hash it, and insert through the unique provider/environment/merchant/event key.
5. Acknowledge only according to provider retry semantics. Processing happens from the persistent inbox, not inline assumptions.
6. Bind amount, currency, order and resource before a financial state change.
7. Rotate by accepting old/new secrets only for a short documented overlap. Test both, remove old, and record the rotation audit event.
8. For PayPhone, until a signed scheme is contractually documented, accept notification only as a hint and query the authenticated transaction endpoint before state change.
9. For PlaceToPay, verify the documented SHA-256 notification but still query the authenticated session endpoint and bind the stored request ID, reference, amount and currency before state change.

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

## 5. Ambiguous transaction incident

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
