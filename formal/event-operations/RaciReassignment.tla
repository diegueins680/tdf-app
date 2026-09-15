---- MODULE RaciReassignment ----
EXTENDS Naturals, TLC
CONSTANTS FreshAuthorization, CheckRevision, Deduplicate, ScopeKeys, AtomicSwap, RecordAudit
Tasks == {"a", "b"}
Commands == {"left", "right"}
Keys == {1, 2}
Slots == Tasks \X Keys
VARIABLES target, key, phase, lock, grant, now, earlyGrant, earlyTime,
          revision, assigned, receipt, effects, audits, badAuth, badReplay, badScope, badVersion
vars == <<target, key, phase, lock, grant, now, earlyGrant, earlyTime,
          revision, assigned, receipt, effects, audits, badAuth, badReplay, badScope, badVersion>>
CanRead(g, t) == g # "none" /\ t < 2
CanWrite(g, t) == g = "manage" /\ t < 2
Slot(c) == <<IF ScopeKeys THEN target[c] ELSE "a", key[c]>>
Init ==
  /\ target \in [Commands -> Tasks] /\ key \in [Commands -> Keys]
  /\ phase = [c \in Commands |-> "idle"] /\ lock = "none"
  /\ grant = "manage" /\ now = 0
  /\ earlyGrant = [c \in Commands |-> "manage"] /\ earlyTime = [c \in Commands |-> 0]
  /\ revision = [t \in Tasks |-> 1] /\ assigned = [t \in Tasks |-> TRUE]
  /\ receipt = [s \in Slots |-> "none"] /\ effects = 0 /\ audits = 0
  /\ badAuth = FALSE /\ badReplay = FALSE /\ badScope = FALSE /\ badVersion = FALSE
Begin(c) ==
  /\ phase[c] = "idle" /\ phase' = [phase EXCEPT ![c] = "waiting"]
  /\ earlyGrant' = [earlyGrant EXCEPT ![c] = grant] /\ earlyTime' = [earlyTime EXCEPT ![c] = now]
  /\ UNCHANGED <<target, key, lock, grant, now, revision, assigned, receipt,
                 effects, audits, badAuth, badReplay, badScope, badVersion>>
Acquire(c) ==
  /\ phase[c] = "waiting" /\ lock = "none"
  /\ lock' = c /\ phase' = [phase EXCEPT ![c] = "locked"]
  /\ UNCHANGED <<target, key, grant, now, earlyGrant, earlyTime, revision, assigned,
                 receipt, effects, audits, badAuth, badReplay, badScope, badVersion>>
Finish(c) ==
  /\ phase[c] = "locked" /\ lock = c
  /\ LET g == IF FreshAuthorization THEN grant ELSE earlyGrant[c]
         t == IF FreshAuthorization THEN now ELSE earlyTime[c]
         readable == CanRead(g,t)
         prior == receipt[Slot(c)] # "none"
         replay == readable /\ prior /\ Deduplicate /\ receipt[Slot(c)] = target[c]
         conflictingKey == readable /\ prior /\ Deduplicate /\ receipt[Slot(c)] # target[c]
         changed == readable /\ ~replay /\ ~conflictingKey /\ CanWrite(g,t)
                    /\ (~CheckRevision \/ revision[target[c]] = 1)
     IN /\ revision' = IF changed THEN [revision EXCEPT ![target[c]] = @ + 1] ELSE revision
        /\ assigned' = IF changed THEN [assigned EXCEPT ![target[c]] = AtomicSwap] ELSE assigned
        /\ receipt' = IF changed THEN [receipt EXCEPT ![Slot(c)] = target[c]] ELSE receipt
        /\ effects' = IF changed THEN effects + 1 ELSE effects
        /\ audits' = IF changed /\ RecordAudit THEN audits + 1 ELSE audits
        /\ badAuth' = (badAuth \/ (changed /\ ~CanWrite(grant,now)) \/ (replay /\ ~CanRead(grant,now)))
        /\ badReplay' = (badReplay \/ (readable /\ prior /\ receipt[Slot(c)] = target[c] /\ ~replay))
        /\ badScope' = (badScope \/ conflictingKey)
        /\ badVersion' = (badVersion \/ (changed /\ revision[target[c]] # 1))
  /\ phase' = [phase EXCEPT ![c] = "done"] /\ lock' = "none"
  /\ UNCHANGED <<target, key, grant, now, earlyGrant, earlyTime>>
ChangeGrant(g) ==
  /\ lock = "none" /\ g \in {"manage", "read", "none"} /\ g # grant /\ grant' = g
  /\ UNCHANGED <<target, key, phase, lock, now, earlyGrant, earlyTime, revision, assigned,
                 receipt, effects, audits, badAuth, badReplay, badScope, badVersion>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<target, key, phase, lock, grant, earlyGrant, earlyTime, revision, assigned,
                 receipt, effects, audits, badAuth, badReplay, badScope, badVersion>>
Next == (\E c \in Commands: Begin(c) \/ Acquire(c) \/ Finish(c))
        \/ (\E g \in {"manage", "read", "none"}: ChangeGrant(g)) \/ Tick
TypeOK ==
  /\ target \in [Commands -> Tasks] /\ key \in [Commands -> Keys]
  /\ phase \in [Commands -> {"idle", "waiting", "locked", "done"}]
  /\ lock \in Commands \cup {"none"} /\ grant \in {"manage", "read", "none"} /\ now \in 0..3
  /\ earlyGrant \in [Commands -> {"manage", "read", "none"}] /\ earlyTime \in [Commands -> 0..3]
  /\ revision \in [Tasks -> 1..3] /\ assigned \in [Tasks -> BOOLEAN]
  /\ receipt \in [Slots -> Tasks \cup {"none"}] /\ effects \in 0..2 /\ audits \in 0..2
  /\ badAuth \in BOOLEAN /\ badReplay \in BOOLEAN /\ badScope \in BOOLEAN /\ badVersion \in BOOLEAN
CurrentAuthority == ~badAuth
ExactRetry == ~badReplay
TaskKeyIsolation == ~badScope
NoStaleReassignment == ~badVersion
NoOrphanResponsibilities == \A t \in Tasks: assigned[t]
AuditCoupled == audits = effects
Spec == Init /\ [][Next]_vars
====
