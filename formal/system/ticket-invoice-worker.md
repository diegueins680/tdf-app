# Ticket invoice submission worker — EVT-TICKET-INVOICE-001

Every paid order of an invoiced ticket policy enqueues one tax document
(`commerce_tax_document`). A background worker submits it to the authorized provider
(Dátil), which signs it and sends it to the SRI. The product decision of 2026-10-06
and the domain contract are in
[ticket-electronic-invoices.md](../../docs/events/ticket-electronic-invoices.md); this
page covers only how the worker avoids creating two provider documents for one
invoice and never overwrites an authorization.

## Rules the worker follows

- **Claim.** A worker takes one due `pending` or `submitted` document whose lease is
  absent or expired, with `FOR UPDATE SKIP LOCKED`, and writes a fresh lease token.
- **Mark, then send.** Before a submission request the worker sets `submitted_at` in
  one statement that requires its own lease token and `submitted_at IS NULL`. Only if
  that statement changed the row may the request be sent. This statement is what
  prevents a second submission; the two conditions cover the same hazard.
- **Interrupted submission.** A `pending` document that already has `submitted_at`
  is moved to `uncertain` without a request, so the stall is visible to staff.
- **Result.** Every result is written with `WHERE lease_token = <own token>`; a worker
  whose lease was taken over changes nothing. A refusal before creation (HTTP 4xx
  other than 408/429) becomes `failed`; an unknown outcome becomes `uncertain`; a
  document still being processed becomes `submitted` and is then only polled.
- **Resend.** Only a strict administrator can return a `failed` document to
  `pending`, which also clears `submitted_at`. The number, amount and bound access key
  never change, so a resent document carries the same SRI identity.
- **Final.** A database trigger refuses any change of an `authorized` document's status.

## Executable evidence and scope

`formal/event-operations/InvoiceSubmission.tla` models one document, two workers, up
to four leases and one administrator resend. Workers may stop at any step, leases may
expire at any time, and an unknown outcome may or may not have created the provider
document. No fairness is assumed and no liveness is claimed.

| Requirement property | Formal invariant | Negative control | Runtime check |
| --- | --- | --- | --- |
| The provider never holds two documents for one invoice | AtMostOneProviderDocument | InvoiceSubmissionUnfencedStart, InvoiceSubmissionUnknownAsRefused, InvoiceSubmissionRetryUncertain | One request from four simultaneous workers; no request after an unknown outcome, an interrupted submission or a lease takeover; `uncertain` cannot be resent |
| An authorized document is never reopened or overwritten | AuthorizedIsFinal | InvoiceSubmissionStaleFinish | A worker whose lease was taken over cannot change the document |
| A recorded authorization is the provider's | AuthorizedIsReal | — | Authorization recorded from the submission response and from a later poll |

`InvoiceSubmissionUnfencedStart` removes the whole condition of the mark statement
(lease token and `submitted_at IS NULL`); removing either half alone does not violate
the invariant. Removing the interrupted-submission rule alone is also safe in the
model, because the mark already refuses a second start: that rule exists for
visibility, not safety.

`tdf-hq/test/TDF/InvoiceWorkerSpec.hs` runs the real worker step against the fully
migrated disposable PostgreSQL fixture through `scripts/test-invoice-worker.sh`, with
the provider request replaced by a recording transport. No request leaves the process.
`tdf-hq/test/integration/ticket_tax_invoices.sql` keeps the numbering, immutability
and enqueue-once checks.

## Not covered, and gaps found

- **No in-app recovery for `uncertain` or `rejected`.** Staff reconcile an `uncertain`
  document in the Dátil dashboard, but nothing in TDF can then record the outcome: the
  document stays `uncertain`, and a credit note for that order is refused. A document
  the SRI rejected cannot be corrected and resent either. Adding either path changes
  the declared state machine and needs a product and tax decision.
- **Transitions are enforced by the worker, not the database.** The trigger protects
  identity fields and the finality of `authorized`; it would not stop another writer
  from moving `uncertain` back to `pending`.
- The model is not a verified translation of the Haskell or SQL. It excludes the
  provider's own duplicate handling by access key, credit-note ordering, numbering,
  payload contents, clock skew and more than one document.
- The assumption that an HTTP 4xx (other than 408/429) means nothing was created is
  Dátil's documented behaviour as observed on 2026-10-06 with an invalid key only. A
  real pruebas authorization has not been run; production invoicing is not configured.
