---- MODULE DirectoryClaimReview ----
EXTENDS Naturals, TLC
CONSTANT Serialize, RequireAdminRole, SeparateReviewer, ReadOnlyReplay, RevokeWithdrawsGrant
Actors == {"admin-a", "admin-b", "claimant-admin", "module-only"}
VARIABLES status, manager, reviewer, phase, desired, observed, mutex, grantRevoked
vars == <<status, manager, reviewer, phase, desired, observed, mutex, grantRevoked>>
Init == /\ status = "under_review" /\ manager = FALSE /\ reviewer = "none"
        /\ phase = [a \in Actors |-> "ready"] /\ desired = [a \in Actors |-> "none"]
        /\ observed = [a \in Actors |-> "none"] /\ mutex = "none" /\ grantRevoked = FALSE
Read(a, decision) == /\ phase[a] = "ready" /\ (~Serialize \/ mutex = "none")
                     /\ (~RequireAdminRole \/ a # "module-only")
                     /\ (~SeparateReviewer \/ a # "claimant-admin")
                     /\ ((status = "under_review" /\ decision \in {"approved", "rejected"})
                         \/ (status = "approved" /\ decision \in {"approved", "revoked"}))
                     /\ phase' = [phase EXCEPT ![a] = "checked"]
                     /\ desired' = [desired EXCEPT ![a] = decision]
                     /\ observed' = [observed EXCEPT ![a] = status]
                     /\ mutex' = (IF Serialize THEN a ELSE mutex)
                     /\ UNCHANGED <<status, manager, reviewer, grantRevoked>>
Commit(a) == /\ phase[a] = "checked" /\ phase' = [phase EXCEPT ![a] = "done"]
             /\ mutex' = "none"
             \* An identical-state retry is observational; a revocation read from
             \* "approved" is a real transition.
             /\ IF observed[a] = desired[a] /\ ReadOnlyReplay
                   THEN UNCHANGED <<status, manager, reviewer>>
                   ELSE /\ status' = desired[a] /\ reviewer' = a
                        \* AUTHORITY-097: revocation withdraws the grant in the same commit.
                        /\ manager' = IF desired[a] = "revoked"
                                        THEN (IF RevokeWithdrawsGrant THEN FALSE ELSE manager)
                                        ELSE (manager \/ desired[a] = "approved")
             /\ UNCHANGED <<desired, observed, grantRevoked>>
RevokeManager == /\ status = "approved" /\ manager /\ ~grantRevoked /\ mutex = "none"
                 /\ manager' = FALSE /\ grantRevoked' = TRUE
                 /\ UNCHANGED <<status, reviewer, phase, desired, observed, mutex>>
Next == (\E a \in Actors, d \in {"approved", "rejected", "revoked"}: Read(a,d))
        \/ (\E a \in Actors: Commit(a)) \/ RevokeManager
GrantMatchesClaim == manager => status = "approved"
AdminRoleRequired == reviewer # "module-only"
SeparatedReview == reviewer # "claimant-admin"
NoReplayRegrant == grantRevoked => ~manager
RevocationWithdrawsGrant == status = "revoked" => ~manager
Spec == Init /\ [][Next]_vars
====
