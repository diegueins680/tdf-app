---- MODULE ExperimentAuthority ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANT Enabled, LockedEligibility, IdempotentExposure, BindAccount
VARIABLES phase, eligible, locks, captured, assigned, exposureCount, validWrites
Accounts == {1, 2}
Requests == {1, 2, 3}
Owner(r) == IF r = 3 THEN 2 ELSE 1
Target(r) == IF BindAccount THEN Owner(r) ELSE 1
vars == <<phase, eligible, locks, captured, assigned, exposureCount, validWrites>>
Init == /\ phase = [r \in Requests |-> "idle"]
        /\ eligible = [a \in Accounts |-> TRUE]
        /\ locks = [a \in Accounts |-> 0]
        /\ captured = [r \in Requests |-> FALSE]
        /\ assigned = {}
        /\ exposureCount = [a \in Accounts |-> 0]
        /\ validWrites = TRUE
Start(r) == /\ phase[r] = "idle"
            /\ phase' = [phase EXCEPT ![r] = "waiting"]
            /\ captured' = [captured EXCEPT ![r] = eligible[Owner(r)]]
            /\ UNCHANGED <<eligible, locks, assigned, exposureCount, validWrites>>
Acquire(r) == /\ phase[r] = "waiting" /\ locks[Target(r)] = 0
              /\ locks' = [locks EXCEPT ![Target(r)] = r]
              /\ phase' = [phase EXCEPT ![r] = "holding"]
              /\ UNCHANGED <<eligible, captured, assigned, exposureCount, validWrites>>
Commit(r) == /\ phase[r] = "holding"
             /\ LET a == Target(r)
                    mayWrite == Enabled /\ (IF LockedEligibility THEN eligible[a] ELSE captured[r])
                 IN /\ assigned' = IF mayWrite THEN assigned \cup {a} ELSE assigned
                    /\ exposureCount' = IF mayWrite /\ (~IdempotentExposure \/ exposureCount[a] = 0)
                                         THEN [exposureCount EXCEPT ![a] = @ + 1] ELSE exposureCount
                    /\ validWrites' = (validWrites /\ (~mayWrite \/ (eligible[a] /\ a = Owner(r))))
             /\ locks' = [locks EXCEPT ![Target(r)] = 0]
             /\ phase' = [phase EXCEPT ![r] = "done"]
             /\ UNCHANGED <<eligible, captured>>
FinishOrExpire(a) == /\ locks[a] = 0 /\ eligible[a]
                     /\ eligible' = [eligible EXCEPT ![a] = FALSE]
                     /\ UNCHANGED <<phase, locks, captured, assigned, exposureCount, validWrites>>
Next == (\E r \in Requests: Start(r) \/ Acquire(r) \/ Commit(r)) \/ (\E a \in Accounts: FinishOrExpire(a))
AccountAndEligibilityAuthority == validWrites
ExposureAtMostOnce == \A a \in Accounts: exposureCount[a] <= 1
PausedDoesNotWrite == ~Enabled => (assigned = {} /\ \A a \in Accounts: exposureCount[a] = 0)
ExposureHasAssignment == \A a \in Accounts: exposureCount[a] > 0 => a \in assigned
RequestsSettle == \A r \in Requests: phase[r] = "waiting" ~> phase[r] = "done"
Spec == Init /\ [][Next]_vars /\ (\A r \in Requests: WF_vars(Acquire(r)) /\ WF_vars(Commit(r)))
====
