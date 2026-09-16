---- MODULE TaskCompletion ----
EXTENDS Naturals, TLC
CONSTANTS FreshAuthority, CheckRevision, CheckDependencies, CheckRaci,
          CheckLifecycle, Deduplicate, RecordAudit
Commands == {"left", "right"}
VARIABLES phase, lock, key, grant, now, earlyGrant, earlyTime, revision,
          ready, supported, completed, receipts, effects, audits,
          badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay
vars == <<phase, lock, key, grant, now, earlyGrant, earlyTime, revision,
          ready, supported, completed, receipts, effects, audits,
          badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Read(g,t) == g # "none" /\ t < 2
Write(g,t) == g = "manage" /\ t < 2
Init ==
  /\ phase = [c \in Commands |-> "idle"] /\ lock = "none"
  /\ key \in [Commands -> {1,2}] /\ grant = "manage" /\ now = 0
  /\ earlyGrant = [c \in Commands |-> "manage"] /\ earlyTime = [c \in Commands |-> 0]
  /\ revision = 1 /\ ready \in BOOLEAN /\ supported \in BOOLEAN
  /\ completed = FALSE /\ receipts = {} /\ effects = 0 /\ audits = 0
  /\ badAuth = FALSE /\ badVersion = FALSE /\ badDependencies = FALSE
  /\ badRaci = FALSE /\ badLifecycle = FALSE /\ badReplay = FALSE
Begin(c) ==
  /\ phase[c] = "idle" /\ phase' = [phase EXCEPT ![c] = "waiting"]
  /\ earlyGrant' = [earlyGrant EXCEPT ![c] = grant] /\ earlyTime' = [earlyTime EXCEPT ![c] = now]
  /\ UNCHANGED <<lock, key, grant, now, revision, ready, supported, completed, receipts,
                 effects, audits, badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Acquire(c) ==
  /\ phase[c] = "waiting" /\ lock = "none"
  /\ lock' = c /\ phase' = [phase EXCEPT ![c] = "locked"]
  /\ UNCHANGED <<key, grant, now, earlyGrant, earlyTime, revision, ready, supported, completed,
                 receipts, effects, audits, badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Finish(c) ==
  /\ phase[c] = "locked" /\ lock = c
  /\ LET g == IF FreshAuthority THEN grant ELSE earlyGrant[c]
         t == IF FreshAuthority THEN now ELSE earlyTime[c]
         readable == Read(g,t)
         prior == key[c] \in receipts
         replay == readable /\ prior /\ Deduplicate
         changed == readable /\ ~replay /\ Write(g,t) /\ ~completed
                    /\ (~CheckRevision \/ revision = 1)
                    /\ (~CheckDependencies \/ ready)
                    /\ (~CheckRaci \/ now < 1)
                    /\ (~CheckLifecycle \/ supported)
     IN /\ revision' = IF changed THEN revision + 1 ELSE revision
        /\ completed' = (completed \/ changed)
        /\ receipts' = IF changed THEN receipts \cup {key[c]} ELSE receipts
        /\ effects' = IF changed THEN effects + 1 ELSE effects
        /\ audits' = IF changed /\ RecordAudit THEN audits + 1 ELSE audits
        /\ badAuth' = (badAuth \/ (changed /\ ~Write(grant,now)) \/ (replay /\ ~Read(grant,now)))
        /\ badVersion' = (badVersion \/ (changed /\ revision # 1))
        /\ badDependencies' = (badDependencies \/ (changed /\ ~ready))
        /\ badRaci' = (badRaci \/ (changed /\ now >= 1))
        /\ badLifecycle' = (badLifecycle \/ (changed /\ ~supported))
        /\ badReplay' = (badReplay \/ (readable /\ prior /\ ~replay))
  /\ phase' = [phase EXCEPT ![c] = "done"] /\ lock' = "none"
  /\ UNCHANGED <<key, grant, now, earlyGrant, earlyTime, ready, supported>>
Environment ==
  /\ lock = "none" /\ ~completed
  /\ revision = 1 /\ revision' = 2
  /\ ready' \in BOOLEAN /\ supported' \in BOOLEAN
  /\ UNCHANGED <<phase, lock, key, grant, now, earlyGrant, earlyTime, completed,
                 receipts, effects, audits, badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Revoke(g) ==
  /\ lock = "none" /\ g \in {"manage", "read", "none"} /\ grant' = g
  /\ UNCHANGED <<phase, lock, key, now, earlyGrant, earlyTime, revision, ready, supported, completed,
                 receipts, effects, audits, badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<phase, lock, key, grant, earlyGrant, earlyTime, revision, ready, supported, completed,
                 receipts, effects, audits, badAuth, badVersion, badDependencies, badRaci, badLifecycle, badReplay>>
Next == (\E c \in Commands: Begin(c) \/ Acquire(c) \/ Finish(c))
        \/ Environment \/ (\E g \in {"manage", "read", "none"}: Revoke(g)) \/ Tick
TypeOK ==
  /\ phase \in [Commands -> {"idle","waiting","locked","done"}]
  /\ lock \in Commands \cup {"none"} /\ key \in [Commands -> {1,2}]
  /\ grant \in {"manage","read","none"} /\ now \in 0..3
  /\ earlyGrant \in [Commands -> {"manage","read","none"}] /\ earlyTime \in [Commands -> 0..3]
  /\ revision \in 1..3 /\ ready \in BOOLEAN /\ supported \in BOOLEAN /\ completed \in BOOLEAN
  /\ receipts \subseteq {1,2} /\ effects \in 0..1 /\ audits \in 0..1
  /\ badAuth \in BOOLEAN /\ badVersion \in BOOLEAN /\ badDependencies \in BOOLEAN
  /\ badRaci \in BOOLEAN /\ badLifecycle \in BOOLEAN /\ badReplay \in BOOLEAN
CurrentAuthority == ~badAuth
NoStaleCompletion == ~badVersion
NoBlockedCompletion == ~badDependencies /\ (completed => ready)
CurrentAccountability == ~badRaci
ValidLifecycle == ~badLifecycle
ExactRetry == ~badReplay
AuditCoupled == audits = effects
Spec == Init /\ [][Next]_vars
====
