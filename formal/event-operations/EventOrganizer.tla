---- MODULE EventOrganizer ----
(* EVT-OWNER-CLAIM-001: one event row whose organizer may be missing, and    *)
(* callers that each read the row, decide from what they read, possibly      *)
(* write, and then act as the event's manager or are refused. Each step is   *)
(* one committed statement, so a caller may decide from a row that another   *)
(* caller has changed since.                                                 *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Callers, Admins, NoOne,
          AssignOnlyAllowed, ConditionalWrite, RecheckStored
VARIABLES organizer, initial, first, pc, seen
vars == <<organizer, initial, first, pc, seen>>

Holders == Callers \cup {NoOne}

Init == /\ organizer \in Holders /\ initial = organizer /\ first = organizer
        /\ pc = [c \in Callers |-> "idle"]
        /\ seen = [c \in Callers |-> NoOne]

Read(c) ==
  /\ pc[c] = "idle"
  /\ seen' = [seen EXCEPT ![c] = organizer]
  /\ pc' = [pc EXCEPT ![c] = "read"]
  /\ UNCHANGED <<organizer, initial, first>>

(* The organizer the caller read decides everything except assignment.       *)
Decide(c) ==
  /\ pc[c] = "read"
  /\ \/ /\ seen[c] = c
        /\ pc' = [pc EXCEPT ![c] = "managing"]
     \/ /\ seen[c] \notin {c, NoOne}
        /\ pc' = [pc EXCEPT ![c] = "refused"]
     \/ /\ seen[c] = NoOne
        /\ pc' = [pc EXCEPT ![c] =
             IF AssignOnlyAllowed => c \in Admins THEN "assigning" ELSE "refused"]
  /\ UNCHANGED <<organizer, initial, first, seen>>

(* One UPDATE; conditional on the organizer still being missing.             *)
Assign(c) ==
  /\ pc[c] = "assigning"
  /\ LET wins == ConditionalWrite => organizer = NoOne
     IN /\ organizer' = IF wins THEN c ELSE organizer
        /\ first' = IF first = NoOne /\ wins THEN c ELSE first
  /\ pc' = [pc EXCEPT ![c] = "assigned"]
  /\ UNCHANGED <<initial, seen>>

(* The stored organizer, not the earlier read, decides the outcome.          *)
Conclude(c) ==
  /\ pc[c] = "assigned"
  /\ pc' = [pc EXCEPT ![c] =
       IF RecheckStored => organizer = c THEN "managing" ELSE "refused"]
  /\ UNCHANGED <<organizer, initial, first, seen>>

Finish(c) ==
  /\ pc[c] \in {"managing", "refused"}
  /\ pc' = [pc EXCEPT ![c] = "idle"]
  /\ UNCHANGED <<organizer, initial, first, seen>>

Next == \E c \in Callers : Read(c) \/ Decide(c) \/ Assign(c) \/ Conclude(c) \/ Finish(c)
Spec == Init /\ [][Next]_vars

TypeOK == /\ organizer \in Holders /\ initial \in Holders /\ first \in Holders
          /\ pc \in [Callers -> {"idle", "read", "assigning", "assigned", "managing", "refused"}]
          /\ seen \in [Callers -> Holders]

(* Once an event has an organizer, no request path replaces it.              *)
OrganizerStable == first # NoOne => organizer = first
(* A missing organizer is filled only by a caller allowed to be assigned.    *)
OnlyAllowedAssigned == organizer # initial => organizer \in Admins
(* Nobody acts as manager of an event whose stored organizer is another.     *)
ManagersAreOrganizer == \A c \in Callers : pc[c] = "managing" => organizer = c
====
