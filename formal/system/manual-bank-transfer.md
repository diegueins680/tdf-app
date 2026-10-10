# Staff-verified ticket bank transfer — EVT-TICKET-MANUAL-001

Product contract: [docs/events/ticket-manual-bank-transfer.md](../../docs/events/ticket-manual-bank-transfer.md)
(owner decision of 2026-10-06 for event 141). This file records the verified
boundary, the bounded model and what remains outside both.

## Property

An order is paid by bank transfer only when a reported transfer is approved by
an authorized, independent reviewer while the effective seat hold is alive. The
approval records one canonical staff-verified payment bound to the exact
checkout amount. A buyer who reported a transfer is never also charged by another
rail, and an expired checkout never reopens.

- Authorized: the event organizer (`social_event.organizer_party_id`) or a strict
  administrator. Other staff, including Reception and module-only grants, are
  denied at the review transaction, not only in the UI.
- Independent: the reviewer is neither the guest submitter party nor a party
  whose primary email matches the buyer email (trimmed, case-folded).
- Effective hold: `GREATEST(hold_expires_at, manual_hold_expires_at)`, bounded by
  the database trigger to `now + hold` and `cutoff + hold`.
- Issuance: the idempotent ticket issuance (`finalizePaidTicketOrder`) commits
  in the same transaction as the approval. If issuance fails, the approval rolls
  back, the evidence stays reviewable and staff get a 409 to retry. A paid
  bank-transfer order without tickets cannot be produced by the review.
- Replay: approving already-approved evidence of a paid order returns success
  and re-runs only the idempotent ticket issuance.
- Second rail: the bank-transfer payment intent stays active after selection,
  including after a rejection, so `createPaymentIntent` refuses any other rail
  for the checkout (PAY-RETRY-001). The buyer can still pay by transfer, or start
  a new order once this hold expires.

## Defect fixed on 2026-10-09

The review locked checkout, attempt and evidence `FOR UPDATE`, decided, then
called Persistent's `transactionSave` before writing. That call commits; it is
not a savepoint. The locks were released before the decision was written, so a
second reviewer of the same order acted on the same pre-image and failed with a
PostgreSQL deadlock (`40P01`) or an evidence-trigger rejection such as
`approved -> under_review`, surfacing as HTTP 500. The evidence state trigger
kept the rows consistent; the defect was the failed staff request and the
unlocked decision window. The review now uses a named savepoint
(`tdf_manual_review`), so the locks cover the whole decision and a concurrent
reviewer waits, then sees the decided state: an identical approval replays,
other actions get the existing 409 messages. The concurrency case below failed
3/3 before the change and passes after it.

## Gap closed on 2026-10-09: issuance after approval

Issuance used to run in a second transaction after the approval committed, as
for provider captures. A failure or restart in between left the order paid,
the evidence `approved`, and no tickets. The staff panel offers review actions
only for `submitted`/`under_review` evidence and does not show fulfillment, so
nothing in the product could recover it. A manual approval has no external
effect to preserve, so issuance now runs inside the approval transaction. This
was established by reading the handler and panel; the old signature cannot
express an injected issuance failure, so there is no failing pre-change run of
the new cases. Provider captures keep two transactions: their money already
moved, and buyers can replay capture/confirmation to finish issuance.

## Executable evidence

`scripts/test-ticket-manual-review.sh` builds an empty disposable
`tdf_ticket_manual_review_test` database from the production schema snapshot plus
the full production migration batch, loads
`tdf-hq/test/integration/ticket_manual_review_fixture.sql`, and runs
`tdf-hq/test/TicketManualReviewMain.hs`. Each order is created through the
canonical checkout runtime and `PaymentRuntime.beginPaymentAttempt`. The harness
then calls the actual `reviewTicketManualPayment` and asserts the authoritative
rows: evidence, checkout, attempt, intent, binding, approval audit and
reconciliation exception. It covers:

- denial of Reception, of an administrator owning the buyer email and of the
  submitter, with every row unchanged;
- organizer approval settling exactly once (paid 2000, attempt succeeded, intent
  captured, one binding, one audit), idempotent replay by another administrator,
  and refusal to reject approved evidence;
- refusal after the effective hold with a recorded reconciliation exception, and
  acceptance after the original hold while the transfer extension is alive;
- rejection, refused approval before a new report, re-report on the same attempt
  and approval through the intent lifecycle;
- concurrent organizer approval, administrator approval and administrator
  rejection of six orders, each ending either settled once or declined;
- an injected issuance failure rolling the whole approval back, a retry
  settling once with issuance run on approval and replay only, and no issuance
  for a rejection;
- refusal of unknown orders and of an order addressed through another event.

The database bounds, expiry and rollback stay covered by
`tdf-hq/test/integration/ticket_manual_bank_transfer.sql`.

## Bounded model

`formal/event-operations/ManualBankTransfer.tla` abstracts one order, a clock of
0..7 ticks, original hold 1, transfer hold 3 and cutoff 2. Actors are the
organizer, an administrator, an administrator owning the buyer email and other
staff; the guest submitter is never staff. Actions are tick, expiry, transfer
selection, evidence report, approval (first or replay), rejection and a card
capture on another rail. Each action is one committed transaction, matching the
`FOR UPDATE` locks of the review and the trigger-side hold guard.

Invariants: `IndependentApproval`, `LiveApproval`, `HoldBound`, `SingleCharge`,
`NoSecondCharge`, `TicketsNeedPayment`, `ExpiryFinal`, `PaidHasTickets`. The
positive configuration finds no violation. Eight controlled mutations must each
fail their named invariant:

| Configuration | Mutation | Expected violation |
|---|---|---|
| `ManualBankTransferSharedReviewer` | buyer-email administrator may review | `IndependentApproval` |
| `ManualBankTransferOutsider` | any staff may review | `IndependentApproval` |
| `ManualBankTransferExpired` | approval ignores the effective hold | `LiveApproval` |
| `ManualBankTransferUnbounded` | extension ignores cutoff + hold | `HoldBound` |
| `ManualBankTransferSecondRail` | card rail allowed after selection | `NoSecondCharge` |
| `ManualBankTransferReplay` | replayed approval records another payment | `SingleCharge` |
| `ManualBankTransferReopen` | rejection reopens an expired checkout | `ExpiryFinal` |
| `ManualBankTransferSplitIssuance` | issuance commits after the approval | `PaidHasTickets` |

No fairness is assumed and no liveness is claimed. The results hold within these
bounds and this abstraction only. The model is not a refinement proof of the SQL
trigger or the Haskell handler.

## Exclusions

- The bank deposit itself is verified by people against the statement; no bank
  API exists, and a transfer that is never reported is invisible to the system.
- Late deposits after expiry are recorded as reconciliation exceptions and
  refunded manually; their resolution is not modeled.
- The issuance itself is passed in by the handler; the harness injects its
  success or failure. `finalizePaidTicketOrder` keeps its own coverage.
- Multi-order capacity, refunds of bank-transfer orders (`completeBankTransferTicketRefund`)
  and electronic invoices (EVT-TICKET-INVOICE-001) are separate requirements.
- HTTP authentication and `requireRefundManagedEvent` admission are exercised by
  their own suites; this harness starts at the review transaction.
