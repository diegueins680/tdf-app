---- MODULE MusicPurchaseReplay ----
(* PAY-MUSIC-PURCHASE-REPLAY-001. A request first observes a buyer/key slot,
   then commits. Another request may reserve the slot between those actions.
   PostgreSQL's unique INSERT + subsequent statement is the atomic boundary;
   snapshots and already-created orders remain immutable. No provider calls. *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Actors, Payloads, None, Shared, BindPayload, ScopeBuyer, ReserveAtomically
VARIABLES phases, observed, slots, created, answered
vars == <<phases, observed, slots, created, answered>>
Requests == Actors \X Payloads
Slots == (Actors \cup {Shared})
Slot(r) == IF ScopeBuyer THEN r[1] ELSE Shared

Init == /\ phases = [r \in Requests |-> "new"]
        /\ observed = [r \in Requests |-> None]
        /\ slots = [s \in Slots |-> None]
        /\ created = {}
        /\ answered = [r \in Requests |-> None]

Lookup(r) == /\ phases[r] = "new"
             /\ phases' = [phases EXCEPT ![r] = "looked"]
             /\ observed' = [observed EXCEPT ![r] = slots[Slot(r)]]
             /\ UNCHANGED <<slots, created, answered>>

Commit(r) ==
  /\ phases[r] = "looked"
  /\ LET original == IF ReserveAtomically THEN slots[Slot(r)] ELSE observed[r]
     IN IF original = None
        THEN /\ slots' = [slots EXCEPT ![Slot(r)] = r]
             /\ created' = created \cup {r}
             /\ answered' = [answered EXCEPT ![r] = r]
        ELSE /\ UNCHANGED <<slots, created>>
             /\ answered' = [answered EXCEPT ![r] =
                   IF ~BindPayload \/ original[2] = r[2] THEN original ELSE None]
  /\ phases' = [phases EXCEPT ![r] = "done"]
  /\ UNCHANGED observed

(* A lost response may cause any completed request to run again. *)
Retry(r) == /\ phases[r] = "done"
            /\ phases' = [phases EXCEPT ![r] = "new"]
            /\ UNCHANGED <<observed, slots, created, answered>>
Next == \E r \in Requests : Lookup(r) \/ Commit(r) \/ Retry(r)
Spec == Init /\ [][Next]_vars

TypeOK == /\ phases \in [Requests -> {"new", "looked", "done"}]
          /\ observed \in [Requests -> Requests \cup {None}]
          /\ slots \in [Slots -> Requests \cup {None}]
          /\ created \subseteq Requests
          /\ answered \in [Requests -> Requests \cup {None}]
OneOrderPerBuyerKey == \A a \in Actors : Cardinality({r \in created : r[1] = a}) <= 1
ReplayPayloadMatches == \A r \in Requests : answered[r] # None => answered[r][2] = r[2]
ReplayOwnerMatches == \A r \in Requests : answered[r] # None => answered[r][1] = r[1]
====
