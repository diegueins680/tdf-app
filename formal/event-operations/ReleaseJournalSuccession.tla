------------------------ MODULE ReleaseJournalSuccession ------------------------
EXTENDS Naturals, FiniteSets, Sequences

(***************************************************************************
 Separate bounded target contract for terminal-successor admission. The old
 ReleaseJournal model and its one-lifetime-reservation contract are unchanged.

 A journal tip is represented by its monotonically increasing record index;
 equality abstracts exact comparison of the validated canonical tip hash;
 that hash is not authentication of a writer who can replace the journal.
 Hash collision resistance, filesystem bytes, fsync, inode/path checks, plan
 validation and factual external-effect evidence are NOT established here.
 Acquire/Drop abstract a cooperating process and its permanent global lock.
 Dropping the handle (including process death) never drops a reservation or
 permits another process to publish the lost invocation's observation. An
 already invoked external child may still produce its effect after that drop.

 There are finitely many epochs, nonces and stages. All actions may stutter;
 there is no fairness, liveness, recovery, deployment authorization, Python
 refinement or whole-system correctness claim. Records never disappear in the
 positive contract. Negative constants each remove one named boundary.
***************************************************************************)

CONSTANTS Actors, Nonces, MaxReleases, Stages, WriteStage,
          HonorLock, RequireTerminal, CompareTip, UniqueNonce,
          PersistIntent, BindObservation, PreserveHistory, PreserveWrites

Epochs == 1..MaxReleases
MaxRecords == MaxReleases * (1 + 2 * Stages)
NoActor == "no-actor"

VARIABLES owners, epochs, phase, stage, reserved, invocation,
          intents, effects, observations, matching, history, writesPossible
vars == <<owners, epochs, phase, stage, reserved, invocation,
          intents, effects, observations, matching, history, writesPossible>>

Init == /\ owners = {}
        /\ epochs = <<>>
        /\ phase = [e \in Epochs |-> "unused"]
        /\ stage = [e \in Epochs |-> 1]
        /\ reserved = {}
        /\ invocation = [e \in Epochs |-> NoActor]
        /\ intents = {}
        /\ effects = {}
        /\ observations = {}
        /\ matching = {}
        /\ history = <<>>
        /\ writesPossible = FALSE

Record(kind, epoch, nonce, step, expected, terminal, bound) ==
  [kind |-> kind, epoch |-> epoch, nonce |-> nonce, stage |-> step,
   sequence |-> Len(history) + 1, previousTip |-> Len(history),
   expectedTip |-> expected, terminalPredecessor |-> terminal, bound |-> bound]

Acquire(a) == /\ a \notin owners
              /\ (~HonorLock \/ owners = {})
              /\ owners' = owners \cup {a}
              /\ UNCHANGED <<epochs, phase, stage, reserved, invocation,
                              intents, effects, observations, matching, history, writesPossible>>

Drop(a) == /\ a \in owners
           /\ owners' = owners \ {a}
           /\ invocation' = [e \in Epochs |->
                               IF invocation[e] = a THEN NoActor ELSE invocation[e]]
           /\ UNCHANGED <<epochs, phase, stage, reserved,
                           intents, effects, observations, matching, history, writesPossible>>

Terminal(e) == /\ phase[e] = "done"
               /\ \A s \in 1..Stages: <<e, s>> \in observations

Admit(a, nonce, expected) ==
  LET epoch == Len(epochs) + 1
      terminal == IF Len(epochs) = 0 THEN TRUE ELSE Terminal(Len(epochs))
      record == Record("plan", epoch, nonce, 0, expected, terminal, TRUE)
  IN /\ a \in owners
     /\ Len(epochs) < MaxReleases
     /\ (~RequireTerminal \/ terminal)
     /\ (~CompareTip \/ expected = Len(history))
     /\ (~UniqueNonce \/ nonce \notin {epochs[e] : e \in 1..Len(epochs)})
     /\ epochs' = Append(epochs, nonce)
     /\ phase' = [phase EXCEPT ![epoch] = "ready"]
     /\ stage' = [stage EXCEPT ![epoch] = 1]
     /\ reserved' = reserved \cup {epoch}
     /\ history' = Append(IF PreserveHistory THEN history ELSE <<>>, record)
     /\ writesPossible' = IF PreserveWrites THEN writesPossible ELSE FALSE
     /\ UNCHANGED <<owners, invocation, intents, effects, observations, matching>>

Intent(a, e) ==
  /\ a \in owners
  /\ e = Len(epochs)
  /\ phase[e] = "ready"
  /\ phase' = [phase EXCEPT ![e] = "intent"]
  /\ invocation' = [invocation EXCEPT ![e] = a]
  /\ intents' = IF PersistIntent THEN intents \cup {<<e, stage[e]>>} ELSE intents
  /\ writesPossible' = (writesPossible \/ stage[e] >= WriteStage)
  /\ history' = IF PersistIntent
                 THEN Append(history, Record("intent", e, epochs[e], stage[e],
                                             Len(history), TRUE, TRUE))
                 ELSE history
  /\ UNCHANGED <<owners, epochs, stage, reserved, effects, observations, matching>>

(* No live handle is required: an external child may outlive its caller. *)
Effect(e) == /\ phase[e] = "intent"
             /\ phase' = [phase EXCEPT ![e] = "effect"]
             /\ effects' = effects \cup {<<e, stage[e]>>}
             /\ UNCHANGED <<owners, epochs, stage, reserved, invocation,
                             intents, observations, matching, history, writesPossible>>

Complete(a, e, valid) ==
  /\ a \in owners
  /\ invocation[e] = a
  /\ phase[e] = "effect"
  /\ (~BindObservation \/ valid)
  /\ observations' = observations \cup {<<e, stage[e]>>}
  /\ matching' = IF valid THEN matching \cup {<<e, stage[e]>>} ELSE matching
  /\ phase' = [phase EXCEPT ![e] = IF stage[e] = Stages THEN "done" ELSE "ready"]
  /\ stage' = [stage EXCEPT ![e] = IF @ = Stages THEN @ ELSE @ + 1]
  /\ reserved' = IF stage[e] = Stages THEN reserved \ {e} ELSE reserved
  /\ invocation' = [invocation EXCEPT ![e] = NoActor]
  /\ history' = Append(history, Record("observed", e, epochs[e], stage[e],
                                       Len(history), TRUE, valid))
  /\ UNCHANGED <<owners, epochs, intents, effects, writesPossible>>

Next == (\E a \in Actors: Acquire(a) \/ Drop(a))
        \/ (\E a \in Actors, n \in Nonces, expected \in 0..MaxRecords:
              Admit(a, n, expected))
        \/ (\E a \in Actors, e \in Epochs: Intent(a, e))
        \/ (\E e \in Epochs: Effect(e))
        \/ (\E a \in Actors, e \in Epochs, valid \in BOOLEAN: Complete(a, e, valid))

TypeOK == /\ owners \subseteq Actors
          /\ epochs \in Seq(Nonces)
          /\ Len(epochs) <= MaxReleases
          /\ phase \in [Epochs -> {"unused", "ready", "intent", "effect", "done"}]
          /\ stage \in [Epochs -> 1..Stages]
          /\ reserved \subseteq 1..Len(epochs)
          /\ invocation \in [Epochs -> Actors \cup {NoActor}]
          /\ intents \subseteq Epochs \X (1..Stages)
          /\ effects \subseteq Epochs \X (1..Stages)
          /\ observations \subseteq effects
          /\ matching \subseteq observations
          /\ Len(history) <= MaxRecords
          /\ writesPossible \in BOOLEAN

OneGlobalLock == Cardinality(owners) <= 1
OneUnfinishedRelease == Cardinality(reserved) <= 1
ReservationsRetained == reserved = {e \in 1..Len(epochs) : phase[e] # "done"}
UniqueNonces == Cardinality({epochs[e] : e \in 1..Len(epochs)}) = Len(epochs)
TerminalPredecessor == \A i \in 1..Len(history):
  history[i].kind = "plan" => history[i].terminalPredecessor
ExactTipCompared == \A i \in 1..Len(history):
  history[i].kind = "plan" => history[i].expectedTip = history[i].previousTip
HistoryChain == /\ \A i \in 1..Len(history):
                       /\ history[i].sequence = i
                       /\ history[i].previousTip = i - 1
                       /\ history[i].nonce = epochs[history[i].epoch]
                /\ Cardinality({i \in 1..Len(history) : history[i].kind = "plan"}) = Len(epochs)
DurableIntentBeforeEffect == effects \subseteq intents
BoundCompletion == observations \subseteq matching
OrderedEffects == \A e \in Epochs, s \in 2..Stages:
  <<e, s>> \in effects => <<e, s - 1>> \in observations
(* The may-write flag begins at durable intent, before any effect can occur. *)
WritesNeverForgotten ==
  (\E e \in Epochs, s \in WriteStage..Stages: <<e, s>> \in intents) => writesPossible

(* Deliberately false invariant used only to require a reachable 3-release witness. *)
ThreeCompletedUnreachable == Cardinality({e \in 1..Len(epochs) : Terminal(e)}) < 3

Spec == Init /\ [][Next]_vars
=============================================================================
