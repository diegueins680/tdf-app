------------------------ MODULE ProviderRollback ------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS Unsafe, PriorSafe, InitialSafe
Machines == {1, 2}
VARIABLES safe, touched, phase, outcome, bound
vars == <<safe, touched, phase, outcome, bound>>

Init == /\ safe = [m \in Machines |-> m \in InitialSafe]
        /\ touched = {}
        /\ phase = "ready"
        /\ outcome = [m \in Machines |-> "pending"]
        /\ bound = FALSE

Migrate == /\ phase = "ready"
           /\ phase' = "deploy"
           /\ UNCHANGED <<safe, touched, outcome, bound>>
Deploy(m) == /\ phase = "deploy" /\ m \notin touched
             /\ safe' = [safe EXCEPT ![m] = TRUE]
             /\ touched' = touched \cup {m}
             /\ phase' = IF touched' = Machines THEN "complete" ELSE "deploy"
             /\ UNCHANGED <<outcome, bound>>
Bind == /\ phase # "ready" /\ ~bound
        /\ \E m \in Machines : safe[m]
        /\ bound' = TRUE
        /\ UNCHANGED <<safe, touched, phase, outcome>>
Fail == /\ phase \in {"deploy", "complete"} /\ touched # {}
        /\ phase' = "rollback"
        /\ UNCHANGED <<safe, touched, outcome, bound>>
Rollback(m) == /\ phase = "rollback" /\ m \in touched
               /\ outcome[m] = "pending"
               /\ IF Unsafe \/ m \in PriorSafe
                     THEN /\ safe' = [safe EXCEPT ![m] = m \in PriorSafe]
                          /\ outcome' = [outcome EXCEPT ![m] = "restored"]
                     ELSE /\ safe' = safe
                          /\ outcome' = [outcome EXCEPT ![m] = "blocked"]
               /\ UNCHANGED <<touched, phase, bound>>
Finish == /\ phase = "rollback"
          /\ \A m \in touched : outcome[m] # "pending"
          /\ phase' = "stopped"
          /\ UNCHANGED <<safe, touched, outcome, bound>>
Next == Migrate \/ Bind \/ Fail \/ Finish
        \/ (\E m \in Machines : Deploy(m) \/ Rollback(m))
Spec == Init /\ [][Next]_vars /\ WF_vars(Finish)
        /\ (\A m \in Machines : WF_vars(Rollback(m)))
NoUnsafeRestoration == \A m \in Machines : outcome[m] = "restored" => safe[m]
ModernNeverDowngrades == \A m \in touched : safe[m]
BindingPreserved == [][bound => bound']_bound
RecoveryDecisionSettles == (phase = "rollback") ~> (phase = "stopped")
=============================================================================
