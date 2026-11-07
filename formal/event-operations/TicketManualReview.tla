-------------------------- MODULE TicketManualReview --------------------------
(* Staff review of one bank transfer reference. Each reviewer reads the
   evidence, decides from what was read, and writes. HoldLock says whether the
   read's row lock lasts until the write is committed; TransitionGuard is the
   database's refusal of any evidence change that does not start from the state
   under review; IndependentReviewer is the refusal of the submitter. The
   payment recorder's own idempotency is not modelled. *)
EXTENDS Naturals

CONSTANTS Reviewers, Submitter, HoldLock, TransitionGuard, IndependentReviewer

ASSUME Submitter \in Reviewers

VARIABLES evidence, checkout, payments, approver, lock, pc, intent, seen

vars == <<evidence, checkout, payments, approver, lock, pc, intent, seen>>

CONSTANT Nobody

TypeOK ==
  /\ evidence \in {"submitted", "approved", "rejected"}
  /\ checkout \in {"awaiting", "paid", "failed"}
  /\ payments \in 0..2
  /\ approver \in Reviewers \cup {Nobody}
  /\ lock \in Reviewers \cup {Nobody}
  /\ pc \in [Reviewers -> {"idle", "read", "answered", "refused", "error"}]
  /\ intent \in [Reviewers -> {"approve", "reject"}]
  /\ seen \in [Reviewers -> {"submitted", "approved", "rejected"}]

Init ==
  /\ evidence = "submitted" /\ checkout = "awaiting" /\ payments = 0
  /\ approver = Nobody /\ lock = Nobody
  /\ pc = [r \in Reviewers |-> "idle"]
  /\ intent \in [Reviewers -> {"approve", "reject"}]
  /\ seen = [r \in Reviewers |-> "submitted"]

Read(r) ==
  /\ pc[r] = "idle"
  /\ lock = Nobody
  /\ lock' = IF HoldLock THEN r ELSE Nobody
  /\ seen' = [seen EXCEPT ![r] = evidence]
  /\ pc' = [pc EXCEPT ![r] = "read"]
  /\ UNCHANGED <<evidence, checkout, payments, approver, intent>>

Finish(r, outcome) ==
  /\ pc' = [pc EXCEPT ![r] = outcome]
  /\ lock' = IF lock = r THEN Nobody ELSE lock

Decide(r) ==
  /\ pc[r] = "read"
  /\ UNCHANGED <<intent, seen>>
  /\ IF IndependentReviewer /\ r = Submitter
       THEN Finish(r, "refused") /\ UNCHANGED <<evidence, checkout, payments, approver>>
     ELSE IF seen[r] = "approved" /\ intent[r] = "approve"
       THEN Finish(r, "answered") /\ UNCHANGED <<evidence, checkout, payments, approver>>
     ELSE IF seen[r] = "rejected" /\ intent[r] = "reject"
       THEN Finish(r, "answered") /\ UNCHANGED <<evidence, checkout, payments, approver>>
     ELSE IF seen[r] # "submitted"
       THEN Finish(r, "refused") /\ UNCHANGED <<evidence, checkout, payments, approver>>
     ELSE IF TransitionGuard /\ evidence # "submitted"
       THEN Finish(r, "error") /\ UNCHANGED <<evidence, checkout, payments, approver>>
     ELSE IF intent[r] = "approve"
       THEN /\ Finish(r, "answered")
            /\ evidence' = "approved" /\ checkout' = "paid"
            /\ payments' = IF payments < 2 THEN payments + 1 ELSE payments
            /\ approver' = r
       ELSE /\ Finish(r, "answered")
            /\ evidence' = "rejected" /\ checkout' = "failed"
            /\ UNCHANGED <<payments, approver>>

Next == \E r \in Reviewers : Read(r) \/ Decide(r)

Spec == Init /\ [][Next]_vars

OnePayment == payments <= 1
Consistent == (checkout = "paid") = (evidence = "approved") /\ (payments = 1) = (checkout = "paid")
NoSelfApproval == approver # Submitter
EveryoneAnswered == \A r \in Reviewers : pc[r] # "error"
=============================================================================
