---- MODULE StudioBookingScope ----
EXTENDS FiniteSets, TLC
CONSTANTS StaffRoleRequired, StoredAssignment, RevalidateSession
Actors == {"owner", "assigned", "other", "staff"}
Permitted == {"owner", "assigned", "staff"}
VARIABLES sessions, started, readers, writers, revokedCommit
vars == <<sessions, started, readers, writers, revokedCommit>>
Init == /\ sessions = Actors /\ started = {} /\ readers = {} /\ writers = {}
        /\ revokedCommit = FALSE
Authorized(actor) == ~StaffRoleRequired \/ actor \in Permitted
Read(actor) == /\ actor \in sessions /\ Authorized(actor)
               /\ readers' = readers \cup {actor}
               /\ UNCHANGED <<sessions, started, writers, revokedCommit>>
Start(actor) == /\ actor \in sessions /\ started' = started \cup {actor}
                /\ UNCHANGED <<sessions, readers, writers, revokedCommit>>
Commit(actor) == /\ actor \in started
                 /\ (~RevalidateSession \/ actor \in sessions)
                 /\ (~StoredAssignment \/ Authorized(actor))
                 /\ writers' = writers \cup {actor}
                 /\ revokedCommit' = (revokedCommit \/ actor \notin sessions)
                 /\ UNCHANGED <<sessions, started, readers>>
Revoke(actor) == /\ actor \in sessions /\ sessions' = sessions \ {actor}
                 /\ UNCHANGED <<started, readers, writers, revokedCommit>>
Next == \E actor \in Actors: Read(actor) \/ Start(actor) \/ Commit(actor) \/ Revoke(actor)
NoForeignAccess == (readers \cup writers) \subseteq Permitted
NoRevokedCommit == ~revokedCommit
TypeOK == /\ sessions \subseteq Actors /\ started \subseteq Actors
          /\ readers \subseteq Actors /\ writers \subseteq Actors /\ revokedCommit \in BOOLEAN
Spec == Init /\ [][Next]_vars
====
