---- MODULE AccountDeletionIntake ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANT SerializeOwner, NotifyOnlyNew, PrivacyAudience
Attempts == {"first", "retry", "other-session"}
VARIABLES phase, observed, mutex, pending, created, notices, leaked
vars == <<phase, observed, mutex, pending, created, notices, leaked>>
Init == /\ phase = [a \in Attempts |-> "ready"]
        /\ observed = [a \in Attempts |-> FALSE]
        /\ mutex = "none" /\ pending = {} /\ created = {}
        /\ notices = [a \in Attempts |-> 0] /\ leaked = FALSE
Read(a) == /\ phase[a] = "ready" /\ (~SerializeOwner \/ mutex = "none")
           /\ phase' = [phase EXCEPT ![a] = "checked"]
           /\ observed' = [observed EXCEPT ![a] = (pending # {})]
           /\ mutex' = IF SerializeOwner THEN a ELSE mutex
           /\ UNCHANGED <<pending, created, notices, leaked>>
Commit(a) == /\ phase[a] = "checked"
             /\ phase' = [phase EXCEPT ![a] = "done"]
             /\ mutex' = "none"
             /\ pending' = IF observed[a] THEN pending ELSE pending \cup {a}
             /\ created' = IF observed[a] THEN created ELSE created \cup {a}
             /\ notices' = IF ~observed[a] \/ ~NotifyOnlyNew
                            THEN [notices EXCEPT ![a] = @ + 1] ELSE notices
             /\ leaked' = (leaked \/ ((~observed[a] \/ ~NotifyOnlyNew) /\ ~PrivacyAudience))
             /\ UNCHANGED observed
Next == \E a \in Attempts: Read(a) \/ Commit(a)
OnePendingOwnerReceipt == Cardinality(pending) <= 1
OneNoticePerNewReceipt == \A a \in Attempts: notices[a] <= (IF a \in created THEN 1 ELSE 0)
OnlyPrivacyAudience == ~leaked
Spec == Init /\ [][Next]_vars
====
