-------------------------- MODULE ManualBankTransfer --------------------------
(* EVT-TICKET-MANUAL-001: one public ticket order whose approved policy opts   *)
(* into staff-verified bank transfer. Bounded design abstraction of           *)
(* docs/events/ticket-manual-bank-transfer.md; not a refinement of the SQL    *)
(* trigger or the Haskell review handler. Each action is one committed        *)
(* transaction (the handler locks checkout, attempt and evidence FOR UPDATE). *)
EXTENDS Naturals
CONSTANTS
  OrigHold,          \* immutable checkout hold_expires_at
  HoldMinutes,       \* policy manual_transfer_hold_minutes (in ticks)
  Cutoff,            \* policy manual_transfer_cutoff_at
  MaxTime,
  \* Controlled mutations; the positive configuration sets all to FALSE.
  SkipIndependence,  \* reviewer may be the submitter or own the buyer email
  OutsiderReview,    \* any staff member may review, not only organizer/admin
  ApproveAfterExpiry,\* approval ignores the effective hold deadline
  UnboundedExtension,\* hold extension ignores cutoff + hold
  IgnoreIntentExclusion, \* a card rail may start after bank transfer selection
  ReplayEffect,      \* re-approval of approved evidence records another payment
  RejectReopens      \* rejection reopens an expired checkout

Max(a, b) == IF a >= b THEN a ELSE b
Min(a, b) == IF a <= b THEN a ELSE b

Staff == {"organizer", "admin", "buyerEmailAdmin", "otherStaff"}
\* Organizer or strict administrator. buyerEmailAdmin is an administrator whose
\* primary email equals the buyer email; the guest submitter is never staff.
Authorized == {"organizer", "admin", "buyerEmailAdmin"}
Independent(a) == a # "guest" /\ a # "buyerEmailAdmin"

VARIABLES
  now, holdEnd, selected, evidence, checkout,
  bankPayments, cardCharged, approver, approvedBefore, issued, everExpired, fundsSent
vars == <<now, holdEnd, selected, evidence, checkout, bankPayments, cardCharged,
          approver, approvedBefore, issued, everExpired, fundsSent>>

Alive == holdEnd > now
Open == checkout \in {"awaiting_payment", "failed"}

Init ==
  /\ now = 0 /\ holdEnd = OrigHold /\ selected = FALSE /\ evidence = "none"
  /\ checkout = "awaiting_payment" /\ bankPayments = 0 /\ cardCharged = FALSE
  /\ approver = "none" /\ approvedBefore = TRUE /\ issued = FALSE
  /\ everExpired = FALSE /\ fundsSent = FALSE

Tick == /\ now < MaxTime /\ now' = now + 1
        /\ UNCHANGED <<holdEnd, selected, evidence, checkout, bankPayments,
                       cardCharged, approver, approvedBefore, issued, everExpired, fundsSent>>

\* event_ticket_checkout_expire_holds: the later of both deadlines.
Expire == /\ Open /\ ~Alive
          /\ checkout' = "expired" /\ everExpired' = TRUE
          /\ UNCHANGED <<now, holdEnd, selected, evidence, bankPayments,
                         cardCharged, approver, approvedBefore, issued, fundsSent>>

\* Trigger bound: grow only, while alive, after selection, <= now + hold and
\* <= cutoff + hold; the first extension must start before the cutoff.
Extend(target) ==
  IF target > holdEnd
     /\ (UnboundedExtension \/ target <= Cutoff + HoldMinutes)
  THEN holdEnd' = target ELSE holdEnd' = holdEnd

SelectTransfer ==
  /\ Alive /\ Open /\ evidence \in {"none", "awaiting_evidence"}
  /\ (now < Cutoff \/ selected) /\ ~cardCharged
  /\ selected' = TRUE
  /\ evidence' = "awaiting_evidence"
  /\ IF ~selected THEN Extend(Min(now + HoldMinutes, Cutoff))
                  ELSE holdEnd' = holdEnd
  /\ UNCHANGED <<now, checkout, bankPayments, cardCharged, approver,
                 approvedBefore, issued, everExpired, fundsSent>>

\* The reported reference is evidence only; it never settles the order.
SubmitEvidence ==
  /\ Alive /\ Open /\ selected
  /\ evidence \in {"awaiting_evidence", "rejected"}
  /\ evidence' = "submitted"
  /\ checkout' = "awaiting_payment"
  /\ fundsSent' = TRUE   \* the buyer reports after transferring
  /\ Extend(IF UnboundedExtension THEN now + HoldMinutes
              ELSE Min(now + HoldMinutes, Cutoff + HoldMinutes))
  /\ UNCHANGED <<now, selected, bankPayments, cardCharged, approver,
                 approvedBefore, issued, everExpired>>

MayReview(a) ==
  /\ (a \in Authorized \/ OutsiderReview)
  /\ (Independent(a) \/ SkipIndependence)

Approve(a) ==
  /\ MayReview(a)
  /\ \/ /\ evidence = "submitted" /\ Open
        /\ (Alive \/ ApproveAfterExpiry)
        /\ evidence' = "approved" /\ checkout' = "paid"
        /\ bankPayments' = bankPayments + 1
        /\ approver' = a /\ approvedBefore' = Alive
        /\ issued' = TRUE
     \* Exact replay of an approval is idempotent issuance only.
     \/ /\ evidence = "approved" /\ checkout = "paid"
        /\ bankPayments' = bankPayments + (IF ReplayEffect THEN 1 ELSE 0)
        /\ UNCHANGED <<evidence, checkout, approver, approvedBefore, issued, fundsSent>>
  /\ UNCHANGED <<now, holdEnd, selected, cardCharged, everExpired, fundsSent>>

Reject(a) ==
  /\ MayReview(a)
  /\ evidence = "submitted" /\ checkout # "paid"
  /\ evidence' = "rejected"
  /\ checkout' = IF checkout = "awaiting_payment" \/ RejectReopens
                 THEN "failed" ELSE checkout
  /\ UNCHANGED <<now, holdEnd, selected, bankPayments, cardCharged,
                 approver, approvedBefore, issued, everExpired, fundsSent>>

\* Another rail needs a fresh payment intent; the bank intent stays active
\* once selected (createPaymentIntent rejects a second active intent).
CardCapture ==
  /\ Alive /\ Open /\ ~cardCharged
  /\ (~selected \/ IgnoreIntentExclusion)
  /\ cardCharged' = TRUE /\ checkout' = "paid" /\ issued' = TRUE
  /\ UNCHANGED <<now, holdEnd, selected, evidence, bankPayments, approver,
                 approvedBefore, everExpired, fundsSent>>

Next ==
  \/ Tick \/ Expire \/ SelectTransfer \/ SubmitEvidence \/ CardCapture
  \/ \E a \in Staff : Approve(a) \/ Reject(a)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ now \in 0..MaxTime
  /\ evidence \in {"none", "awaiting_evidence", "submitted", "rejected", "approved"}
  /\ checkout \in {"awaiting_payment", "failed", "expired", "paid"}

\* Paid by transfer only with approved evidence of an authorized, independent
\* reviewer, approved while the effective hold was alive.
IndependentApproval ==
  bankPayments > 0 => /\ evidence = "approved"
                      /\ approver \in Authorized
                      /\ Independent(approver)
LiveApproval == bankPayments > 0 => approvedBefore
HoldBound == holdEnd <= Max(OrigHold, Cutoff + HoldMinutes)
SingleCharge == bankPayments + (IF cardCharged THEN 1 ELSE 0) <= 1
\* A buyer who was shown the transfer instructions and reported a transfer is
\* never also charged by another rail.
NoSecondCharge == ~(cardCharged /\ fundsSent)
TicketsNeedPayment == issued => checkout = "paid"
ExpiryFinal == everExpired => checkout = "expired"
=============================================================================
