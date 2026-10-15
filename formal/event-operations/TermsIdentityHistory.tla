---- MODULE TermsIdentityHistory ----
EXTENDS Naturals, FiniteSets
CONSTANT CheckHistoricalIdentity
VARIABLES live, history
vars == <<live, history>>
Scopes == {"course", "event"}
Texts == {"original", "changed"}
Init == /\ live = [s \in Scopes |-> {}]
        /\ history = [s \in Scopes |-> {}]
Insert(s, t) ==
  /\ live[s] \ {t} = {}
  /\ (~CheckHistoricalIdentity \/ history[s] \ {t} = {})
  /\ live' = [live EXCEPT ![s] = @ \cup {t}]
  /\ history' = [history EXCEPT ![s] = @ \cup {t}]
Delete(s) == /\ live[s] # {}
             /\ live' = [live EXCEPT ![s] = {}]
             /\ UNCHANGED history
Next == (\E s \in Scopes, t \in Texts: Insert(s,t)) \/ (\E s \in Scopes: Delete(s))
Spec == Init /\ [][Next]_vars
TypeOK == /\ live \in [Scopes -> SUBSET Texts] /\ history \in [Scopes -> SUBSET Texts]
HistoricalIdentityPreserved == \A s \in Scopes: Cardinality(history[s]) <= 1
====
