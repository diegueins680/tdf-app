------------------------- MODULE RequestReplay -------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Requests, Recipients, Payloads, UnsafeConflict
Parameters == [recipient: Recipients, payload: Payloads]
VARIABLES admitted, bound, terminal, effects, reply
vars == <<admitted, bound, terminal, effects, reply>>
Init == /\ admitted = {} /\ terminal = {}
        /\ bound \in [Requests -> Parameters]
        /\ effects = [r \in Requests |-> 0]
        /\ reply = [present |-> FALSE, matched |-> FALSE, accepted |-> FALSE]
Submit(r, p) ==
  /\ r \in Requests /\ p \in Parameters
  /\ IF r \notin admitted
        THEN /\ admitted' = admitted \cup {r}
             /\ bound' = [bound EXCEPT ![r] = p]
             /\ reply' = [present |-> TRUE, matched |-> TRUE, accepted |-> TRUE]
        ELSE /\ UNCHANGED <<admitted, bound>>
             /\ reply' = [present |-> TRUE, matched |-> p = bound[r],
                           accepted |-> UnsafeConflict \/ p = bound[r]]
  /\ UNCHANGED <<terminal, effects>>
Finish(r) == /\ r \in admitted \ terminal
             /\ terminal' = terminal \cup {r}
             /\ effects' = [effects EXCEPT ![r] = @ + 1]
             /\ UNCHANGED <<admitted, bound, reply>>
Next == (\E r \in Requests, p \in Parameters: Submit(r,p))
        \/ (\E r \in Requests: Finish(r))
Spec == Init /\ [][Next]_vars /\ \A r \in Requests: WF_vars(Finish(r))
TypeOK == /\ terminal \subseteq admitted /\ admitted \subseteq Requests
          /\ bound \in [Requests -> Parameters] /\ effects \in [Requests -> Nat]
ReplayEquality == reply.present => (reply.accepted = reply.matched)
AtMostOnce == \A r \in Requests: effects[r] <= 1
BindingImmutable == []([\A r \in admitted: bound'[r] = bound[r]]_vars)
Progress == \A r \in Requests: (r \in admitted) ~> (r \in terminal)
=============================================================================
