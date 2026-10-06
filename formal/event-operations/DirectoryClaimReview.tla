---- MODULE DirectoryClaimReview ----
EXTENDS Naturals, TLC
CONSTANT Serialize, RequireAdminRole, SeparateReviewer, ReadOnlyReplay
Actors == {"admin-a", "admin-b", "claimant-admin", "module-only"}
VARIABLES status, manager, reviewer, phase, desired, observed, mutex, grantRevoked
vars == <<status, manager, reviewer, phase, desired, observed, mutex, grantRevoked>>
Init == /\ status = "under_review" /\ manager = FALSE /\ reviewer = "none"
        /\ phase = [a \in Actors |-> "ready"] /\ desired = [a \in Actors |-> "none"]
        /\ observed = [a \in Actors |-> "none"] /\ mutex = "none" /\ grantRevoked = FALSE
Read(a, decision) == /\ phase[a] = "ready" /\ (~Serialize \/ mutex = "none")
                     /\ (~RequireAdminRole \/ a # "module-only")
                     /\ (~SeparateReviewer \/ a # "claimant-admin")
                     /\ (status = "under_review" \/ (status = "approved" /\ decision = "approved"))
                     /\ phase' = [phase EXCEPT ![a] = "checked"]
                     /\ desired' = [desired EXCEPT ![a] = decision]
                     /\ observed' = [observed EXCEPT ![a] = status]
                     /\ mutex' = (IF Serialize THEN a ELSE mutex)
                     /\ UNCHANGED <<status, manager, reviewer, grantRevoked>>
Commit(a) == /\ phase[a] = "checked" /\ phase' = [phase EXCEPT ![a] = "done"]
             /\ mutex' = "none"
             /\ IF observed[a] = "approved" /\ ReadOnlyReplay
                   THEN UNCHANGED <<status, manager, reviewer>>
                   ELSE /\ status' = desired[a] /\ reviewer' = a
                        /\ manager' = (manager \/ desired[a] = "approved")
             /\ UNCHANGED <<desired, observed, grantRevoked>>
RevokeManager == /\ status = "approved" /\ manager /\ ~grantRevoked /\ mutex = "none"
                 /\ manager' = FALSE /\ grantRevoked' = TRUE
                 /\ UNCHANGED <<status, reviewer, phase, desired, observed, mutex>>
Next == (\E a \in Actors, d \in {"approved", "rejected"}: Read(a,d))
        \/ (\E a \in Actors: Commit(a)) \/ RevokeManager
GrantMatchesClaim == manager => status = "approved"
AdminRoleRequired == reviewer # "module-only"
SeparatedReview == reviewer # "claimant-admin"
NoReplayRegrant == grantRevoked => ~manager
Spec == Init /\ [][Next]_vars
====
