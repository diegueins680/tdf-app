---- MODULE TaskView ----
EXTENDS Naturals, TLC
CONSTANTS CheckContext, ClearOnChange, RequireValid
VARIABLES generation, party, target, requests, finished, visible, validated
vars == <<generation, party, target, requests, finished, visible, validated>>
Context == <<generation, party, target>>
None == "none"
Init == /\ generation = 0 /\ party = "A" /\ target = 1
        /\ requests = [i \in 1..2 |-> None] /\ finished = {}
        /\ visible = None /\ validated = TRUE
Change(p, t) ==
  /\ generation < 2 /\ p \in {"A", "B", None} /\ t \in {1, 2}
  /\ generation' = generation + 1 /\ party' = p /\ target' = t
  /\ visible' = IF ClearOnChange THEN None ELSE visible
  /\ UNCHANGED <<requests, finished, validated>>
Start(i) ==
  /\ party # None /\ requests[i] = None
  /\ requests' = [requests EXCEPT ![i] = Context]
  /\ UNCHANGED <<generation, party, target, finished, visible, validated>>
Finish(i, valid) ==
  /\ requests[i] # None /\ i \notin finished
  /\ finished' = finished \cup {i}
  /\ IF (~CheckContext \/ requests[i] = Context) /\ (~RequireValid \/ valid)
        THEN /\ visible' = requests[i] /\ validated' = valid
        ELSE UNCHANGED <<visible, validated>>
  /\ UNCHANGED <<generation, party, target, requests>>
Next == \/ \E p \in {"A", "B", None}, t \in {1, 2}: Change(p, t)
        \/ \E i \in 1..2: Start(i)
        \/ \E i \in 1..2, valid \in BOOLEAN: Finish(i, valid)
TypeOK == /\ generation \in 0..2 /\ party \in {"A", "B", None}
          /\ target \in {1, 2} /\ finished \subseteq 1..2 /\ validated \in BOOLEAN
CurrentView == visible # None => (visible = Context /\ party # None)
ValidatedView == visible # None => validated
Spec == Init /\ [][Next]_vars
====
