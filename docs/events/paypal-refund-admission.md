# PayPal ticket payment evidence and external refund admission

The official sandbox capture test returned `COMPLETED` but omitted the internal
order binding at the purchase-unit location expected by TDF. A subsequent
authenticated GET of the same PayPal order included `custom_id`, payee merchant,
amount/currency and the capture. Capture now requests a representation and always
reads that original provider order before canonical verification. Readback must
match its original provider order ID; existing internal order, exact amount,
currency and merchant checks remain unchanged. No missing field is defaulted to
success, and retry retains the original capture idempotency key.

This regression was found in the actual isolated TDF API: web created a held USD20
order; the provider captured USD20; TDF returned 502 for missing binding and issued
zero tickets. The captured sandbox funds were submitted for full refund as test
cleanup. Signature-verified capture processing also retried because a zero platform fee
created an invalid zero-valued ledger entry. The fee posting now omits zero values
like the existing tax and organizer postings; ledger constraints remain unchanged.
The actual Haskell posting function was exercised on the isolated checkout under
rollback: three entries, sum zero, zero zero-valued entries, no persisted test
journal. The later official sandbox rerun completed browser purchase and issuance; see the scoped evidence below. Financial refund reconciliation remains incomplete.

## External refund admission fence

A signed `PAYMENT.CAPTURE.REFUNDED` event represents a **refund** resource. Its
`id` is not the original capture ID and its shape need not include capture-only
payee or order fields. Retain the bounded upstream link during inbox evidence
minimization and require the configured PayPal environment, exact provider host,
GET method and an unambiguous capture identifier. No response link is fetched.
Bind that capture to the existing merchant, checkout and payment attempt before
recording a reconciliation exception. Reversals bind their capture resource ID.
Currency and positive amount must fit the bound capture.

For event tickets, recording the external change locks the event and order in
the same order used by scanning. Admission rejects a verified external refund/reversal
exception only when provider, environment, merchant, capture and internal order
all match the purchased checkout. A stale legacy paid/issued row cannot override
this fence. Changing an administrative review to resolved or ignored does not undo the provider refund and cannot restore admission. Unknown and other-merchant events do not establish this binding. Partial-refund ticket allocation remains a separate release requirement.

This is an admission suspension pending reconciliation. It does **not** invent
which ticket a partial external refund represents, restock inventory, mark a
refund completed, issue a credit note or implement the missing PayPal refund
approval path for ticket orders. Those financial transitions remain separate
release requirements; the event must not be activated on this safeguard alone.

Regression coverage: actual PostgreSQL inventory/admission/transfer harness,
including a bound refund and reversal versus another merchant, no admission
audit on denial, the last one/two places under eight buyers, and one redemption
under eight scans. Parser tests cover real refund-shaped resources after inbox
minimization, foreign environment/host and malformed capture URLs. Full provider
purchase/refund is a separate sandbox check, never inferred from these tests.

Sources: [PayPal payment/refund API](https://developer.paypal.com/api/payments/v2)
and [official webhook events](https://developer.paypal.com/api/rest/webhooks/event-names/).

## Official browser purchase and regression evidence — 5 October 2026

The [sanitized run](patch-culture-vol-1/official-sandbox-purchase-2026-10-05.json)
used actual PayPal buyer approval, provider capture and signed callbacks with the
canonical TDF API, all 181 migrations and isolated synthetic event data. On a
390×844 viewport it purchased two USD20 tickets for USD40, issued two distinct
credentials and decoded both rendered QR images back to the issued codes.
Eight concurrent HTTP scans produced one 200 and seven 409 responses, with one
admission audit. Three capture retries and two duplicate signed callbacks retained
two tickets, one balanced capture journal and one confirmation delivery.
The local SMTP receiver accepted one message with both codes and the event link;
external mailbox delivery was not tested.

The first dialog attempt exposed a real portal timing bug: the PayPal SDK was
already loaded but the effect ran before the dialog container existed. A reactive
container reference now triggers rendering when the portal mounts. The regression
fails against the previous ref-only implementation and passes with the correction.
The page reuses the existing public environment reader and avoids exposing raw
payment/fulfillment enum names or implementation instructions to buyers.

The USD40 capture was fully refunded through the official sandbox provider API.
Both capture and refund signatures verified SUCCESS, and the unused ticket was
rejected with 409 after the signed refund callback. This proves the admission
fence, **not** the missing TDF refund approval, accounting or credit-note workflow:
the canonical refund ledger was not posted and the order projection remains paid.
A later defensive check also keeps resolved/ignored review labels from reopening
refunded credentials; its actual PostgreSQL admission suite passes 20 examples.

Additional checks: 263 actual PostgreSQL provider retry/ledger examples, including
four fee/tax combinations; 16 checkout component tests; UI typecheck and targeted
lint. The 15% tax scenario is synthetic and does not approve the event's fiscal
classification. Native device checkout, production rollout and production payments
remain outside this evidence.

## Ticket refund financial components

The canonical refund store now reverses ticket organizer payable, platform fees
and collected tax against the original posted capture, instead of treating the
whole amount as service-storefront revenue. A partial refund allocates cumulative
tax first, then fees from the remaining amount; the organizer receives the
remainder. Integer intermediates avoid overflow, zero entries are omitted, and
full repayment reverses every original component exactly. This is accounting
allocation from the immutable checkout, not a new tax classification.

Original capture components and prior posted refunds must agree with the checkout
snapshot. A mismatch raises inside the caller-owned transaction and rolls back
the intent, refund, receipt and journal together. Four fee/tax combinations are
exercised through real PostgreSQL partial refunds, concurrent completion replays
and a changed-snapshot denial. Arithmetic properties include Int64 boundaries
and exhaustive partitions for totals through twelve minor units.

This financial primitive alone does not enable ticket refunds or live sales.
The authorized request/approval route still needs per-ticket allocation, admission
reservation, provider execution/recovery and order projection integration. The
existing external-refund admission fence remains in force. The existing internal
credit-note record is not proof of an SRI electronic credit-note submission.

Design references: [PayPal refunds](https://developer.paypal.com/api/payments/v2)
and [idempotency](https://developer.paypal.com/api/rest/reference/idempotency/).
Provider outcomes must retain the original execution identity; a timeout does not
authorize a new refund request.

The [subsequent isolated financial check](patch-culture-vol-1/official-sandbox-refund-ledger-2026-10-05.json)
applied authenticated official USD40 refund evidence to the canonical store:
checkout/runtime became refunded, one internal credit note was recorded, the
refund journal balanced to zero and replay made no second mutation. It also
exposed the provider's official `api.sandbox.paypal.com` response links. The
adapter accepts only exact api/api-m references for the bound environment;
outbound queries stay pinned and response links are never followed.
The legacy ticket-order projection and organizer refund endpoint remain outside
this direct-store check. Production sales stay disabled.
