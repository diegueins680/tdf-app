------------------------- MODULE MusicPaymentOutcome -------------------------
EXTENDS Naturals

CONSTANTS CancelOnFailure, GrantClosed, DropException, DuplicateReplay, AllowFailedRetry, DropFailureFence
VARIABLES order, checkout, grant, review, captured, failedUnpaid, admissionStatus, admissionAllowed, failureFence, admissionFence
vars == <<order, checkout, grant, review, captured, failedUnpaid, admissionStatus, admissionAllowed, failureFence, admissionFence>>

Init == /\ \/ /\ order = "awaiting"
                 /\ checkout = "awaiting"
             \/ /\ order = "cancelled"
                 /\ checkout = "failed"
        /\ grant = 0
        /\ review = FALSE
        /\ captured = FALSE
        /\ failedUnpaid = FALSE
        /\ admissionStatus = "none" /\ admissionAllowed = FALSE
        /\ failureFence = (checkout = "failed") /\ admissionFence = FALSE

Fail == /\ checkout = "awaiting"
        /\ checkout' = "failed"
        /\ order' = IF CancelOnFailure THEN "cancelled" ELSE order
        /\ failedUnpaid' = TRUE
        /\ failureFence' = TRUE
        /\ UNCHANGED <<grant, review, captured, admissionStatus, admissionAllowed, admissionFence>>

Close == /\ order = "awaiting"
         /\ checkout \in {"awaiting", "failed"}
         /\ order' = "cancelled"
         /\ checkout' = "cancelled"
         /\ failedUnpaid' = FALSE
         /\ UNCHANGED <<grant, review, captured, admissionStatus, admissionAllowed, failureFence, admissionFence>>

Verify == /\ checkout \in {"awaiting", "failed"}
          /\ checkout' = "paid"
          /\ captured' = TRUE
          /\ failedUnpaid' = FALSE
          /\ order' = IF order = "awaiting" THEN "paid" ELSE order
          /\ grant' = IF order = "awaiting" \/ GrantClosed THEN 1 ELSE 0
          /\ review' = (order = "cancelled" /\ ~DropException)
          /\ UNCHANGED <<admissionStatus, admissionAllowed, failureFence, admissionFence>>

Replay == /\ checkout = "paid"
          /\ grant < 2
          /\ grant' = IF DuplicateReplay /\ grant > 0 THEN grant + 1 ELSE grant
          /\ UNCHANGED <<order, checkout, review, captured, failedUnpaid, admissionStatus, admissionAllowed, failureFence, admissionFence>>

Refund == /\ checkout = "paid" /\ order = "paid"
          /\ checkout' = "refunded" /\ order' = "refunded"
          /\ grant' = 0
          /\ UNCHANGED <<review, captured, failedUnpaid, admissionStatus, admissionAllowed, failureFence, admissionFence>>

Dispute == /\ checkout = "paid" /\ order = "paid"
           /\ checkout' = "disputed" /\ order' = "chargeback"
           /\ grant' = 0
           /\ UNCHANGED <<review, captured, failedUnpaid, admissionStatus, admissionAllowed, failureFence, admissionFence>>

LatePending == /\ checkout = "failed" /\ order = "awaiting"
               /\ checkout' = "awaiting"
               /\ UNCHANGED <<order, grant, review, captured, failedUnpaid,
                               admissionStatus, admissionAllowed, failureFence, admissionFence>>

Admit == /\ admissionStatus' = checkout
         /\ admissionFence' = failureFence
         /\ admissionAllowed' = ((checkout = "awaiting" /\ (~failureFence \/ DropFailureFence))
                                  \/ (AllowFailedRetry /\ checkout = "failed"))
         /\ UNCHANGED <<order, checkout, grant, review, captured, failedUnpaid, failureFence>>

Next == Fail \/ Close \/ Verify \/ Replay \/ Refund \/ Dispute \/ LatePending \/ Admit
Spec == Init /\ [][Next]_vars

TypeOK == /\ order \in {"awaiting", "paid", "cancelled", "refunded", "chargeback"}
          /\ checkout \in {"awaiting", "failed", "paid", "cancelled", "refunded", "disputed"}
          /\ grant \in 0..2
          /\ review \in BOOLEAN /\ captured \in BOOLEAN /\ failedUnpaid \in BOOLEAN
          /\ admissionAllowed \in BOOLEAN /\ failureFence \in BOOLEAN /\ admissionFence \in BOOLEAN
          /\ admissionStatus \in {"none", "awaiting", "failed", "paid", "cancelled", "refunded", "disputed"}
FailureKeepsPayable == failedUnpaid => order = "awaiting"
ActiveOnlyForPaidOrder == grant > 0 => order = "paid"
ClosedMoneyNeedsReview == captured /\ checkout = "paid" /\ order = "cancelled" => review
OneEntitlement == grant <= 1
GrantNeedsEvidence == grant > 0 => captured
NoQuarantinedRetry == admissionFence => ~admissionAllowed
NoFailedRetry == admissionStatus = "failed" => ~admissionAllowed
=============================================================================
