---- MODULE ReceiptReplay ----
EXTENDS Naturals, Sequences, TLC
CONSTANTS Reauthorize, FreshClock, Fence
VARIABLES now, grant, phase, snapshotGrant, startedAt, binding, decision
vars == <<now, grant, phase, snapshotGrant, startedAt, binding, decision>>
Grants == {"manage", "read", "none"}
Binding == [actor: BOOLEAN, event: BOOLEAN, hash: BOOLEAN]
Access(g, at) == g # "none" /\ at < 2
Init ==
  /\ now = 0 /\ grant = "manage" /\ phase = "idle"
  /\ snapshotGrant = "none" /\ startedAt = 0
  /\ binding = [actor |-> FALSE, event |-> FALSE, hash |-> FALSE]
  /\ decision = [disclosed |-> FALSE, authorized |-> FALSE, bound |-> FALSE]
Begin(b) ==
  /\ phase = "idle"
  /\ phase' = "waiting" /\ snapshotGrant' = grant /\ startedAt' = now
  /\ binding' = b
  /\ UNCHANGED <<now, grant, decision>>
ChangeGrant(g) ==
  /\ g \in Grants /\ g # grant
  /\ ~Fence \/ phase # "waiting"
  /\ grant' = g
  /\ UNCHANGED <<now, phase, snapshotGrant, startedAt, binding, decision>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<grant, phase, snapshotGrant, startedAt, binding, decision>>
Finish ==
  /\ phase = "waiting"
  /\ LET bound == binding.actor /\ binding.event /\ binding.hash
         observedGrant == IF Fence THEN grant ELSE snapshotGrant
         observedTime == IF FreshClock THEN now ELSE startedAt
         allowed == ~Reauthorize \/ Access(observedGrant, observedTime)
     IN decision' = [disclosed |-> (bound /\ allowed),
                     authorized |-> Access(grant, now), bound |-> bound]
  /\ phase' = "done"
  /\ UNCHANGED <<now, grant, snapshotGrant, startedAt, binding>>
Next == \/ \E b \in Binding: Begin(b)
        \/ \E g \in Grants: ChangeGrant(g)
        \/ Tick \/ Finish
TypeOK == /\ now \in 0..3 /\ startedAt \in 0..3
          /\ grant \in Grants /\ snapshotGrant \in Grants
          /\ phase \in {"idle", "waiting", "done"} /\ binding \in Binding
          /\ decision \in [disclosed: BOOLEAN, authorized: BOOLEAN, bound: BOOLEAN]
NoUnauthorizedDisclosure == decision.disclosed => decision.authorized
ReceiptBindingPreserved == decision.disclosed => decision.bound
Spec == Init /\ [][Next]_vars
====
