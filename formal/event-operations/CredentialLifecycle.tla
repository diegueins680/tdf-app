---- MODULE CredentialLifecycle ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANT Serialize, AtomicIssue, RevokeGoogle
Ops == {"login", "reset1", "reset2"}
VARIABLES active, resetActive, winners, tokens, phase, mutex
vars == <<active, resetActive, winners, tokens, phase, mutex>>
Init == /\ active = TRUE /\ resetActive = TRUE /\ winners = {}
        /\ tokens = {"google"} /\ phase = [o \in Ops |-> "ready"] /\ mutex = "none"
Begin(o) == /\ phase[o] = "ready" /\ active
            /\ (o = "login" \/ resetActive)
            /\ (~Serialize \/ mutex = "none")
            /\ phase' = [phase EXCEPT ![o] = "checked"]
            /\ mutex' = IF Serialize THEN o ELSE mutex
            /\ UNCHANGED <<active, resetActive, winners, tokens>>
\* Intentionally broken mode commits credential effects and releases the lock
\* before session insertion, as the former transactionSave helper did.
EarlyCommit(o) == /\ ~AtomicIssue /\ phase[o] = "checked"
                  /\ phase' = [phase EXCEPT ![o] = "committed-early"] /\ mutex' = "none"
                  /\ resetActive' = IF o = "login" THEN resetActive ELSE FALSE
                  /\ UNCHANGED <<active, winners, tokens>>
Issue(o) == /\ phase[o] = (IF AtomicIssue THEN "checked" ELSE "committed-early")
            /\ phase' = [phase EXCEPT ![o] = "done"] /\ mutex' = "none"
            /\ resetActive' = IF o = "login" THEN resetActive ELSE FALSE
            /\ winners' = IF o = "login" THEN winners ELSE winners \cup {o}
            /\ tokens' = IF o = "login" THEN tokens \cup {o}
                          ELSE {o} \cup (IF RevokeGoogle THEN {} ELSE tokens \cap {"google"})
            /\ UNCHANGED active
Fail(o) == /\ phase[o] = (IF AtomicIssue THEN "checked" ELSE "committed-early")
           /\ phase' = [phase EXCEPT ![o] = "failed"] /\ mutex' = "none"
           /\ UNCHANGED <<active, resetActive, winners, tokens>>
Disable == /\ active /\ mutex = "none" /\ active' = FALSE
           /\ tokens' = IF RevokeGoogle THEN {} ELSE tokens \cap {"google"}
           /\ UNCHANGED <<resetActive, winners, phase, mutex>>
Next == (\E o \in Ops: Begin(o) \/ EarlyCommit(o) \/ Issue(o) \/ Fail(o)) \/ Disable
SingleUseReset == Cardinality(winners) <= 1
NoSessionsAfterDisable == ~active => tokens = {}
AtomicChallengeConsumption == ~resetActive => winners # {}
Spec == Init /\ [][Next]_vars
====
