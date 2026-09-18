---- MODULE CheckoutCancellation ----
EXTENDS Naturals, TLC
CONSTANTS GuardReservation, GuardPayment
VARIABLES phase, requestPending, paymentPending, abandoned, lostPayment, reserved
vars == <<phase, requestPending, paymentPending, abandoned, lostPayment, reserved>>
Init == /\ phase = "buyer" /\ requestPending = FALSE /\ paymentPending = FALSE
        /\ abandoned = FALSE /\ lostPayment = FALSE /\ reserved = FALSE
Submit == /\ phase = "buyer" /\ phase' = "readiness"
          /\ UNCHANGED <<requestPending, paymentPending, abandoned, lostPayment, reserved>>
Ready == /\ phase = "readiness" /\ phase' = "reserving" /\ requestPending' = TRUE
         /\ UNCHANGED <<paymentPending, abandoned, lostPayment, reserved>>
Unavailable == /\ phase = "readiness" /\ phase' = "buyer"
               /\ UNCHANGED <<requestPending, paymentPending, abandoned, lostPayment, reserved>>
Cancel == /\ phase \in {"buyer", "readiness", "reserving", "payment", "paying", "confirmed"}
          /\ (~GuardReservation \/ ~requestPending)
          /\ (~GuardPayment \/ ~paymentPending)
          /\ phase' = "closed"
          /\ UNCHANGED <<requestPending, paymentPending, abandoned, lostPayment, reserved>>
Resolve == /\ requestPending
           /\ \E success \in BOOLEAN:
                /\ reserved' = success
                /\ abandoned' = (success /\ phase = "closed")
                /\ phase' = IF phase = "closed" THEN phase ELSE IF success THEN "payment" ELSE "buyer"
           /\ requestPending' = FALSE
           /\ UNCHANGED <<paymentPending, lostPayment>>
Pay == /\ phase = "payment" /\ phase' = "paying" /\ paymentPending' = TRUE
       /\ UNCHANGED <<requestPending, abandoned, lostPayment, reserved>>
Confirm == /\ paymentPending
           /\ \E success \in BOOLEAN:
                /\ lostPayment' = (success /\ phase = "closed")
                /\ phase' = IF phase = "closed" THEN phase ELSE IF success THEN "confirmed" ELSE "payment"
           /\ paymentPending' = FALSE
           /\ UNCHANGED <<requestPending, abandoned, reserved>>
Next == Submit \/ Ready \/ Unavailable \/ Cancel \/ Resolve \/ Pay \/ Confirm
NoAbandonedReservation == ~abandoned
NoLostPayment == ~lostPayment
PendingRetainsDialog == (requestPending => phase = "reserving") /\ (paymentPending => phase = "paying")
ReservationSettles == requestPending ~> ~requestPending
PaymentSettles == paymentPending ~> ~paymentPending
Spec == Init /\ [][Next]_vars /\ WF_vars(Resolve) /\ WF_vars(Confirm)
====
