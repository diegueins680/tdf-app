---- MODULE CheckoutReadiness ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANT GateReady, FenceCurrent, SingleFlight
VARIABLES generation, phase, pending, used, requestGeneration, reservations
Requests == {1, 2}
vars == <<generation, phase, pending, used, requestGeneration, reservations>>
Init == /\ generation = 0 /\ phase = "buyer" /\ pending = {} /\ used = {}
        /\ requestGeneration = [r \in Requests |-> 0] /\ reservations = {}
Submit(r) == /\ r \in Requests \ used /\ phase # "closed"
             /\ (~SingleFlight \/ phase = "buyer")
             /\ phase' = "loading" /\ pending' = pending \cup {r}
             /\ used' = used \cup {r}
             /\ requestGeneration' = [requestGeneration EXCEPT ![r] = generation]
             /\ UNCHANGED <<generation, reservations>>
Close == /\ phase # "closed" /\ generation < 2
         /\ generation' = generation + 1 /\ phase' = "closed"
         /\ UNCHANGED <<pending, used, requestGeneration, reservations>>
Open == /\ phase = "closed" /\ phase' = "buyer"
        /\ UNCHANGED <<generation, pending, used, requestGeneration, reservations>>
Resolve(r) == /\ r \in pending
              /\ \E ready \in BOOLEAN:
                   /\ reservations' = IF (~GateReady \/ ready) /\ (~FenceCurrent \/ (requestGeneration[r] = generation /\ phase # "closed"))
                                       THEN reservations \cup {[request |-> r, ready |-> ready, owner |-> requestGeneration[r], current |-> generation, open |-> phase # "closed"]}
                                       ELSE reservations
              /\ pending' = pending \ {r}
              /\ phase' = IF requestGeneration[r] = generation /\ phase # "closed" THEN "buyer" ELSE phase
              /\ UNCHANGED <<generation, used, requestGeneration>>
Next == (\E r \in Requests: Submit(r) \/ Resolve(r)) \/ Close \/ Open
ReadyBeforeReservation == \A x \in reservations: x.ready
CurrentReservation == \A x \in reservations: x.owner = x.current /\ x.open
SingleCurrentFlight == Cardinality({r \in pending: requestGeneration[r] = generation}) <= 1
Settles == \A r \in Requests: (r \in pending) ~> (r \notin pending)
Spec == Init /\ [][Next]_vars /\ (\A r \in Requests: WF_vars(Resolve(r)))
====
