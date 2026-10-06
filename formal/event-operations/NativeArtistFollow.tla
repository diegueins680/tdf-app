---- MODULE NativeArtistFollow ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS CanonicalContract, CheckGeneration
VARIABLES generation, phase, owner, canonical, legacy, completed, applied, contextOK
Slots == 1..2
Party(g) == IF g = 1 THEN 2 ELSE 1
vars == <<generation, phase, owner, canonical, legacy, completed, applied, contextOK>>
Init == /\ generation = 0
        /\ phase = [s \in Slots |-> "idle"]
        /\ owner = [s \in Slots |-> 0]
        /\ canonical = {} /\ legacy = {} /\ completed = {}
        /\ applied = {} /\ contextOK = TRUE
ChangeSession == /\ generation < 2 /\ generation' = generation + 1
                 /\ UNCHANGED <<phase, owner, canonical, legacy, completed, applied, contextOK>>
Dispatch(s) == /\ phase[s] = "idle"
               /\ phase' = [phase EXCEPT ![s] = "pending"]
               /\ owner' = [owner EXCEPT ![s] = generation]
               /\ UNCHANGED <<generation, canonical, legacy, completed, applied, contextOK>>
Persist(s) == /\ phase[s] = "pending"
              /\ phase' = [phase EXCEPT ![s] = "persisted"]
              /\ canonical' = IF CanonicalContract THEN canonical \cup {Party(owner[s])} ELSE canonical
              /\ legacy' = IF CanonicalContract THEN legacy ELSE legacy \cup {Party(owner[s])}
              /\ UNCHANGED <<generation, owner, completed, applied, contextOK>>
Fail(s) == /\ phase[s] = "pending"
           /\ phase' = [phase EXCEPT ![s] = "done"]
           /\ UNCHANGED <<generation, owner, canonical, legacy, completed, applied, contextOK>>
ReceiptAndHandshake(s) ==
  /\ phase[s] = "persisted"
  /\ phase' = [phase EXCEPT ![s] = "done"]
  /\ LET current == IF CheckGeneration THEN owner[s] = generation ELSE Party(owner[s]) = Party(generation)
         actor == Party(owner[s])
     IN /\ applied' = IF current THEN applied \cup {s} ELSE applied
        /\ completed' = IF current /\ actor \in canonical THEN completed \cup {actor} ELSE completed
        /\ contextOK' = (contextOK /\ (~current \/ owner[s] = generation))
  /\ UNCHANGED <<generation, owner, canonical, legacy>>
Next == ChangeSession \/ (\E s \in Slots: Dispatch(s) \/ Persist(s) \/ Fail(s) \/ ReceiptAndHandshake(s))
TypeOK == /\ generation \in 0..2 /\ owner \in [Slots -> 0..2]
          /\ phase \in [Slots -> {"idle", "pending", "persisted", "done"}]
          /\ canonical \subseteq {1,2} /\ legacy \subseteq {1,2}
          /\ completed \subseteq {1,2} /\ applied \subseteq Slots /\ contextOK \in BOOLEAN
CurrentSession == contextOK
PersistedCompletion == completed \subseteq canonical
SuccessfulFollowQualifies == \A s \in applied: Party(owner[s]) \in completed
Return(s) == Persist(s) \/ Fail(s) \/ ReceiptAndHandshake(s)
RequestsSettle == \A s \in Slots: phase[s] = "pending" ~> phase[s] = "done"
Spec == Init /\ [][Next]_vars /\ (\A s \in Slots: WF_vars(Return(s)))
====
