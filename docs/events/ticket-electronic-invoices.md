# SRI electronic invoices for public tickets

Decision of 6 October 2026: TDF must issue the invoice for every ticket sale
before selling, through an authorized provider (Dátil), starting with PATCH
CULTURE vol. 1 (event 141).

## Contract

- A ticket policy opts in with `tax_invoice_required` (immutable once approved).
  While invoicing is not ready for the checkout environment, the storefront is
  closed and checkout creation is refused: TDF does not sell what it cannot
  invoice.
- Ready means: Dátil credentials and issuer identity configured, `DATIL_ENVIRONMENT`
  matching the checkout environment (1 pruebas ↔ sandbox, 2 producción ↔
  production), an enabled row in `commerce_tax_issuer_point` and
  `TAX_INVOICE_WORKER_ENABLED=true`, so that nothing is sold while no worker
  would submit its invoice.
- Buyer identification is captured once at checkout and is immutable.
  Consumidor final is accepted up to USD 50; above that the buyer gives a
  cédula (modulo-10 check), RUC or passport and a name.
- When an invoiced order becomes paid (any rail, including verified bank
  transfer) the database enqueues exactly one invoice with the next sequential
  number of the issuer point. Paying is refused if no issuer point is enabled.
- The worker binds the issue date and a TDF-generated 49-digit access key
  (modulo 11) on first submission and posts IVA 0% lines that must add up to the
  paid total. Dátil signs, sends to the SRI and emails the buyer.
- Outcomes: `authorized` (final), `submitted` (polled), `rejected` (SRI refused),
  `failed` (Dátil refused before creating it; an administrator may resend), and
  `uncertain` (outcome unknown — reconcile in the Dátil dashboard; never resent
  automatically).

## Refunds and credit notes

- Orders paid by bank transfer are refunded through the same canonical ticket
  refund request. Staff return the money by bank transfer, then a staff member
  other than the requester records the transfer reference
  (`POST /social-events/events/{id}/refunds/{refundId}/bank-transfer-complete`).
  That completes the refund, invalidates the selected tickets and releases
  inventory. PayPal orders keep the provider-executed refund.
- Every verified refund of an invoiced order enqueues one SRI credit note
  (own numbering series, bound to the invoice). The worker waits until the
  invoice is authorized, then submits it to Dátil's credit-note endpoint.

## Operator configuration (API host secrets)

`DATIL_API_KEY`, `DATIL_CERTIFICATE_PASSWORD`, `DATIL_ENVIRONMENT`,
`TAX_ISSUER_RUC`, `TAX_ISSUER_LEGAL_NAME`, `TAX_ISSUER_ADDRESS`,
`TAX_ISSUER_ACCOUNTING_REQUIRED`, optional `TAX_ISSUER_TRADE_NAME`,
`TAX_ISSUER_ESTABLISHMENT_ADDRESS`, `TAX_ISSUER_SPECIAL_TAXPAYER`, and
`TAX_INVOICE_WORKER_ENABLED=true`. Then insert the issuer point (establishment,
emission point and next sequential) for the environment and enable it. Use an
emission point not used by any other invoicing tool to avoid number collisions.

## Evidence

- 2026-10-06, local backend with a full migrated schema: guest checkout over USD 50
  without identification refused; identified order, bank transfer selection, evidence,
  staff approval, three issued tickets, one enqueued invoice `001-900-000000001` for
  USD 60, idempotent re-approval, single check-in, reject/resubmit/approve path.
- 2026-10-06, the payload produced by `invoicePayload` was posted to
  `https://link.datil.co/invoices/issue` with an invalid key: Dátil accepted the schema
  and answered `401 INVALID_CREDENTIALS` (validation precedes authentication), so no
  document was created. A real pruebas authorization still requires credentials.

## Limits

Only IVA 0% is supported; other rates are refused rather than guessed.
Taxpayer regime legends (for example RIMPE) are not added automatically. The
credit-note payload was validated against Dátil's schema with an invalid key
(401 after schema validation); a real authorization still requires credentials.
