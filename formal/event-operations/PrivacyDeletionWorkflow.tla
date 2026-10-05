--------------------- MODULE PrivacyDeletionWorkflow ---------------------
EXTENDS Naturals, TLC
CONSTANTS AllowEarlyClose, ResetDeadline, AllowStaleWrite
VARIABLES stage, verified, planned, effects, notice, due, version, staleCommitted
vars == <<stage, verified, planned, effects, notice, due, version, staleCommitted>>
Init == /\ stage = "received" /\ verified = FALSE /\ planned = FALSE
        /\ effects = FALSE /\ notice = FALSE /\ due = 2
        /\ version = 0 /\ staleCommitted = FALSE
Identity == /\ stage = "received" /\ stage' = "verified" /\ verified' = TRUE
            /\ UNCHANGED <<planned,effects,notice,due,version,staleCommitted>>
Plan == /\ stage = "verified" /\ stage' = "planned" /\ planned' = TRUE
        /\ UNCHANGED <<verified,effects,notice,due,version,staleCommitted>>
Start == /\ stage = "planned" /\ stage' = "executing"
         /\ due' = IF ResetDeadline THEN 3 ELSE due
         /\ UNCHANGED <<verified,planned,effects,notice,version,staleCommitted>>
Effects == /\ stage = "executing" /\ stage' = "effects_verified" /\ effects' = TRUE
           /\ UNCHANGED <<verified,planned,notice,due,version,staleCommitted>>
Close == /\ (stage = "effects_verified" \/ (AllowEarlyClose /\ stage = "received"))
         /\ stage' = "closed" /\ notice' = TRUE
         /\ UNCHANGED <<verified,planned,effects,due,version,staleCommitted>>
Commit(expected) == /\ version < 2 /\ (expected = version \/ AllowStaleWrite)
                    /\ version' = version + 1
                    /\ staleCommitted' = (staleCommitted \/ expected # version)
                    /\ UNCHANGED <<stage,verified,planned,effects,notice,due>>
Next == Identity \/ Plan \/ Start \/ Effects \/ Close \/ (\E expected \in 0..1 : Commit(expected))
ClosureEvidence == stage = "closed" => (verified /\ planned /\ effects /\ notice)
FixedDeadline == due = 2
NoStaleCommit == ~staleCommitted
Spec == Init /\ [][Next]_vars
=============================================================================
