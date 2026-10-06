---- MODULE AccountDeletionResolution ----
EXTENDS TLC
CONSTANT SerializeResolution
VARIABLES mutex, resolver, intake, pending, observed, orderedAfter, returnedOld
vars == <<mutex, resolver, intake, pending, observed, orderedAfter, returnedOld>>
Init == /\ mutex = "none" /\ resolver = "ready" /\ intake = "ready"
        /\ pending = TRUE /\ observed = FALSE
        /\ orderedAfter = FALSE /\ returnedOld = FALSE
StartResolution == /\ resolver = "ready"
                   /\ (~SerializeResolution \/ mutex = "none")
                   /\ resolver' = "writing"
                   /\ mutex' = (IF SerializeResolution THEN "resolver" ELSE mutex)
                   /\ UNCHANGED <<intake, pending, observed, orderedAfter, returnedOld>>
CommitResolution == /\ resolver = "writing"
                    /\ resolver' = "done" /\ pending' = FALSE
                    /\ mutex' = (IF mutex = "resolver" THEN "none" ELSE mutex)
                    /\ UNCHANGED <<intake, observed, orderedAfter, returnedOld>>
ReadIntake == /\ intake = "ready" /\ mutex = "none"
              /\ intake' = "checked" /\ mutex' = "intake"
              /\ observed' = pending /\ orderedAfter' = (resolver # "ready")
              /\ UNCHANGED <<resolver, pending, returnedOld>>
CommitIntake == /\ intake = "checked" /\ intake' = "done" /\ mutex' = "none"
               /\ returnedOld' = observed
               /\ pending' = (IF observed THEN pending ELSE TRUE)
               /\ UNCHANGED <<resolver, observed, orderedAfter>>
Next == StartResolution \/ CommitResolution \/ ReadIntake \/ CommitIntake
ResolutionFirstGetsFreshReceipt == (intake = "done" /\ orderedAfter) => ~returnedOld
Spec == Init /\ [][Next]_vars
====
