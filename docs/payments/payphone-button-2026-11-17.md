# PayPhone payment button (2026-11-17)

PayPhone checkout moved from API Sale (charge by phone number, approved in the
PayPhone app) to the hosted payment button, so buyers can pay with a card
without having PayPhone, or with the PayPhone app.

## Flow

1. `POST /commerce/checkouts/{id}/payment-sessions` with provider `payphone`
   and method `card` (`payphone_card`) or `payphone_wallet`. The adapter calls
   `POST /api/button/Prepare`; the binding's provider resource is PayPhone's
   prepared `paymentId`, and the buyer is redirected to PayPhone's hosted card
   page (`/Anonymous/Index`) or app page (`/PayPhone/Index`). No buyer phone
   is collected by TDF.
2. PayPhone returns the buyer to `PAYPHONE_RESPONSE_URL` with `id` and
   `clientTransactionId`. `/pagos/retorno` posts both (untrusted) to
   `POST .../payment-sessions/{attemptId}/payphone-confirmation`.
3. The server requires `clientTransactionId` to be the attempt's own
   reference and calls `POST /api/button/V2/Confirm`. Only a response matching
   transaction, reference, amount and currency is applied through the shared
   reconciliation path (`applyQueryResult`). The call is idempotent; the
   return page repeats it while the result is pending.

## Why only Confirm proves payment

Sandbox evidence (store Tdf Records, app "TDF Pagos Pruebas", modo Prueba):

- Probe 93776650: approved, confirmed → `Approved`; then
  `POST /api/Reverse/Client {clientId}` → `true`, status `Canceled`.
- Probe 93776857: approved, never confirmed → the query reported `statusCode 3
  Approved` with no field distinguishing it from a confirmed payment, then
  `statusCode 2 Canceled` about five minutes later.

So the plain query (`GET /api/Sale/client/{reference}`, which returns a list
for button payments) never reports success for button bindings: `Approved`
stays pending, and `Canceled` is a confirmed no-charge. A charge is recorded
only from a Confirm response. Bindings created by API Sale (numeric resource
IDs) keep their previous query behaviour.

- Live adapter check (2026-11-18, `tdfe2e235851`, card, transaction 93785311):
  the shipped adapter code prepared the payment, its query parsed
  `Pending/Ambiguous` before confirmation, Confirm parsed `Succeeded` for
  100 USD, a replayed Confirm parsed `Succeeded` again, and a Confirm with a
  different reference was rejected.

## Other observations

- PayPhone's hosted page answers "NOT AUTHORIZED" unless the buyer arrives
  from an authorized domain of the app (Referer check). Opening the link from
  another app or a chat fails by design.
- Same-day refunds reuse `POST /api/Reverse/Client`, verified on a button
  payment.

## Before enabling in an environment

- Apply `tdf-hq/sql/2026-11-17_payphone_button_capability_catalog.sql`
  (documentation rows only) and move the `payphone/card` rows to the verified
  state for that environment, as with every other provider capability.
- Register the site's exact origin as an authorized domain in PayPhone
  Developer and set `PAYPHONE_RESPONSE_URL` to that origin's `/pagos/retorno`.
- Follow-up: the unsigned PayPhone notification endpoint still looks bindings
  up by transaction ID, which button bindings do not store. Until it resolves
  by merchant reference and confirms, it is not a recovery path for buyers who
  close the browser before returning; their unconfirmed payments are reversed
  by PayPhone and reconcile as no charge.
- Follow-up: a prepared payment the buyer never pays (or whose hosted session
  expires) does not exist for the query: `GET /api/Sale/client/{ref}` answers
  HTTP 404 (`errorCode 20`, "La transacción no existe"). The shared HTTP layer
  treats every non-2xx as a transport error, so the recovery worker retries
  and, after 24 attempts, dead-letters the job into manual review. No money is
  at risk, but each abandoned checkout becomes a review item. A fix would let
  the PayPhone button query map a 404 to confirmed no-charge only after the
  prepared session can no longer be paid (it also answers 404 while the buyer
  is still on PayPhone's page), which needs a status-aware exception in
  `ProviderAdapter/Http.hs`, deliberately not added here.
