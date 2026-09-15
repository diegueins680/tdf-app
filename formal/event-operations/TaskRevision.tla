---- MODULE TaskRevision ----
EXTENDS Naturals, TLC
CONSTANTS TrackRaci, CheckInsideFence
Commands == {"left", "right"}
VARIABLES activity, raci, revision, phase, captured, earlyMatch, lock, writerDone, staleCommit
vars == <<activity, raci, revision, phase, captured, earlyMatch, lock, writerDone, staleCommit>>
Snapshot == [activity |-> activity, raci |-> raci, revision |-> revision]
Init ==
  /\ activity = 0 /\ raci = 0 /\ revision = 1
  /\ phase = [c \in Commands |-> "idle"]
  /\ captured = [c \in Commands |-> Snapshot]
  /\ earlyMatch = [c \in Commands |-> FALSE]
  /\ lock = "none" /\ writerDone = FALSE /\ staleCommit = FALSE
Capture(c) ==
  /\ phase[c] = "idle"
  /\ captured' = [captured EXCEPT ![c] = Snapshot]
  /\ earlyMatch' = [earlyMatch EXCEPT ![c] = TRUE]
  /\ phase' = [phase EXCEPT ![c] = "captured"]
  /\ UNCHANGED <<activity, raci, revision, lock, writerDone, staleCommit>>
Acquire(c) ==
  /\ phase[c] = "captured" /\ lock = "none"
  /\ lock' = c /\ phase' = [phase EXCEPT ![c] = "locked"]
  /\ UNCHANGED <<activity, raci, revision, captured, earlyMatch, writerDone, staleCommit>>
Finish(c) ==
  /\ phase[c] = "locked" /\ lock = c
  /\ LET accepted == IF CheckInsideFence THEN captured[c].revision = revision ELSE earlyMatch[c]
     IN /\ activity' = IF accepted THEN activity + 1 ELSE activity
        /\ revision' = IF accepted THEN revision + 1 ELSE revision
        /\ staleCommit' = (staleCommit \/ (accepted /\
             (captured[c].activity # activity \/ captured[c].raci # raci)))
  /\ lock' = "none" /\ phase' = [phase EXCEPT ![c] = "finished"]
  /\ UNCHANGED <<raci, captured, earlyMatch, writerDone>>
RaciWrite ==
  /\ ~writerDone /\ lock = "none"
  /\ raci' = raci + 1
  /\ revision' = IF TrackRaci THEN revision + 1 ELSE revision
  /\ writerDone' = TRUE
  /\ UNCHANGED <<activity, phase, captured, earlyMatch, lock, staleCommit>>
Next == \/ \E c \in Commands: Capture(c) \/ Acquire(c) \/ Finish(c)
        \/ RaciWrite
TypeOK == /\ activity \in 0..2 /\ raci \in 0..1 /\ revision \in 1..4
          /\ phase \in [Commands -> {"idle", "captured", "locked", "finished"}]
          /\ captured \in [Commands -> [activity: 0..2, raci: 0..1, revision: 1..4]]
          /\ earlyMatch \in [Commands -> BOOLEAN]
          /\ lock \in Commands \cup {"none"}
          /\ writerDone \in BOOLEAN /\ staleCommit \in BOOLEAN
NoStaleCommit == ~staleCommit
Spec == Init /\ [][Next]_vars
====
