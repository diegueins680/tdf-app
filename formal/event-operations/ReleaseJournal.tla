---- MODULE ReleaseJournal ----
EXTENDS Naturals, FiniteSets
CONSTANTS Runs, Stages, PersistIntent, HonorReservation, BindObservation
VARIABLES owner, reserved, phase, stage, intents, effects, ended, matching, receipts
vars == <<owner, reserved, phase, stage, intents, effects, ended, matching, receipts>>
Init == /\ owner = {} /\ reserved = {} /\ phase = [r \in Runs |-> "idle"]
        /\ stage = [r \in Runs |-> 1] /\ intents = {} /\ effects = {}
        /\ ended = {} /\ matching = {} /\ receipts = {}
Start(r) == /\ owner = {} /\ phase[r] = "idle"
            /\ (~HonorReservation \/ reserved = {})
            /\ owner' = {r} /\ reserved' = reserved \cup {r}
            /\ phase' = [phase EXCEPT ![r] = "ready"]
            /\ UNCHANGED <<stage, intents, effects, ended, matching, receipts>>
Intent(r) == /\ r \in owner /\ phase[r] = "ready"
             /\ phase' = [phase EXCEPT ![r] = "intent"]
             /\ intents' = (IF PersistIntent THEN intents \cup {<<r, stage[r]>>} ELSE intents)
             /\ UNCHANGED <<owner, reserved, stage, effects, ended, matching, receipts>>
Effect(r) == /\ r \in owner /\ phase[r] = "intent"
             /\ effects' = effects \cup {<<r, stage[r]>>}
             /\ phase' = [phase EXCEPT ![r] = "effect"]
             /\ UNCHANGED <<owner, reserved, stage, intents, ended, matching, receipts>>
Observe(r, valid) == /\ phase[r] \in {"effect", "crashed"}
                    /\ <<r, stage[r]>> \in effects \ ended /\ valid \in BOOLEAN
                    /\ ended' = ended \cup {<<r, stage[r]>>}
                    /\ matching' = (IF valid THEN matching \cup {<<r, stage[r]>>} ELSE matching)
                    /\ UNCHANGED <<owner, reserved, phase, stage, intents, effects, receipts>>
Complete(r) == /\ r \in owner /\ phase[r] = "effect"
               /\ <<r, stage[r]>> \in ended
               /\ (~BindObservation \/ <<r, stage[r]>> \in matching)
               /\ receipts' = receipts \cup {<<r, stage[r]>>}
               /\ phase' = [phase EXCEPT ![r] = IF stage[r] = Stages THEN "done" ELSE "ready"]
               /\ owner' = (IF stage[r] = Stages THEN {} ELSE owner)
               /\ stage' = [stage EXCEPT ![r] = IF @ = Stages THEN @ ELSE @ + 1]
               /\ UNCHANGED <<reserved, intents, effects, ended, matching>>
Crash(r) == /\ r \in owner /\ owner' = {}
            /\ phase' = [phase EXCEPT ![r] = "crashed"]
            /\ UNCHANGED <<reserved, stage, intents, effects, ended, matching, receipts>>
Next == \E r \in Runs: Start(r) \/ Intent(r) \/ Effect(r)
        \/ (\E valid \in BOOLEAN: Observe(r, valid)) \/ Complete(r) \/ Crash(r)
TypeOK == /\ owner \subseteq Runs /\ reserved \subseteq Runs
          /\ phase \in [Runs -> {"idle", "ready", "intent", "effect", "crashed", "done"}]
          /\ stage \in [Runs -> 1..Stages]
          /\ intents \subseteq Runs \X (1..Stages) /\ effects \subseteq Runs \X (1..Stages)
          /\ ended \subseteq effects /\ matching \subseteq ended /\ receipts \subseteq effects
DurableIntentBeforeEffect == effects \subseteq intents
OneReservedRelease == Cardinality(reserved) <= 1
BoundCompletion == receipts \subseteq matching
OrderedEffects == \A r \in Runs, s \in 2..Stages: <<r, s>> \in effects => <<r, s-1>> \in receipts
Spec == Init /\ [][Next]_vars
====
