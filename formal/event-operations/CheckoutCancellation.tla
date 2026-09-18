---- MODULE CheckoutCancellation ----
EXTENDS Naturals, TLC
CONSTANT GuardReservation
VARIABLES phase, requestPending, abandoned, reserved
vars == <<phase, requestPending, abandoned, reserved>>
Init == /\ phase = "buyer" /\ requestPending = FALSE /\ abandoned = FALSE /\ reserved = FALSE
Submit == /\ phase = "buyer" /\ phase' = "readiness"
          /\ UNCHANGED <<requestPending, abandoned, reserved>>
Ready == /\ phase = "readiness" /\ phase' = "reserving" /\ requestPending' = TRUE
         /\ UNCHANGED <<abandoned, reserved>>
Unavailable == /\ phase = "readiness" /\ phase' = "buyer"
               /\ UNCHANGED <<requestPending, abandoned, reserved>>
Cancel == /\ phase \in {"buyer", "readiness", "reserving"}
          /\ (~GuardReservation \/ ~requestPending)
          /\ phase' = "closed"
          /\ UNCHANGED <<requestPending, abandoned, reserved>>
Resolve == /\ requestPending
           /\ \E success \in BOOLEAN:
                /\ reserved' = success
                /\ abandoned' = (success /\ phase = "closed")
                /\ phase' = IF phase = "closed" THEN phase ELSE IF success THEN "payment" ELSE "buyer"
           /\ requestPending' = FALSE
Next == Submit \/ Ready \/ Unavailable \/ Cancel \/ Resolve
NoAbandonedReservation == ~abandoned
PendingRetainsDialog == requestPending => phase = "reserving"
ReservationSettles == requestPending ~> ~requestPending
Spec == Init /\ [][Next]_vars /\ WF_vars(Resolve)
====
