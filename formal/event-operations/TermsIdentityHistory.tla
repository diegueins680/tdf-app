---- MODULE TermsIdentityHistory ----
EXTENDS Naturals, FiniteSets
CONSTANTS CheckHistoricalIdentity, ProtectHistory, HistoryMutation
VARIABLES live, history, disclosed
vars == <<live, history, disclosed>>
Scopes == {"course", "event"}
Texts == {"original", "changed"}
Init == /\ live = [s \in Scopes |-> {}]
        /\ history = [s \in Scopes |-> {}]
        /\ disclosed = [s \in Scopes |-> {}]
Insert(s, t) ==
  /\ live[s] \ {t} = {}
  /\ (~CheckHistoricalIdentity \/ history[s] \ {t} = {})
  /\ live' = [live EXCEPT ![s] = @ \cup {t}]
  /\ history' = [history EXCEPT ![s] = @ \cup {t}]
  /\ disclosed' = [disclosed EXCEPT ![s] = @ \cup {t}]
Delete(s) == /\ live[s] # {}
             /\ live' = [live EXCEPT ![s] = {}]
             /\ UNCHANGED <<history, disclosed>>
\* disclosed is independent observation history; erasing the stored archive
\* must not erase the invariant's memory of the text already disclosed.
DeleteHistory(s) == /\ ~ProtectHistory /\ HistoryMutation \in {"all", "delete"}
                    /\ history[s] # {}
                    /\ history' = [history EXCEPT ![s] = {}]
                    /\ UNCHANGED <<live, disclosed>>
UpdateHistory(s, t) == /\ ~ProtectHistory /\ HistoryMutation \in {"all", "update"}
                      /\ history[s] # {} /\ t \notin history[s]
                      /\ history' = [history EXCEPT ![s] = {t}]
                      /\ UNCHANGED <<live, disclosed>>
TruncateHistory == /\ ~ProtectHistory /\ HistoryMutation \in {"all", "truncate"}
                   /\ \E s \in Scopes: history[s] # {}
                   /\ history' = [s \in Scopes |-> {}]
                   /\ UNCHANGED <<live, disclosed>>
Next == (\E s \in Scopes, t \in Texts: Insert(s,t))
        \/ (\E s \in Scopes: Delete(s) \/ DeleteHistory(s))
        \/ (\E s \in Scopes, t \in Texts: UpdateHistory(s,t)) \/ TruncateHistory
Spec == Init /\ [][Next]_vars
TypeOK == /\ live \in [Scopes -> SUBSET Texts] /\ history \in [Scopes -> SUBSET Texts]
          /\ disclosed \in [Scopes -> SUBSET Texts]
HistoricalIdentityPreserved == \A s \in Scopes: Cardinality(disclosed[s]) <= 1
AppendOnlyHistory == history = disclosed
====
