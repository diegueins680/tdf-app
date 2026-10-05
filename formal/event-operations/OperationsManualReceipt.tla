----------------------- MODULE OperationsManualReceipt -----------------------
EXTENDS Naturals
CONSTANTS ReuseSourceKey, UnboundReplay, PartialCommit
VARIABLES present, retained, effects, touchedSource, disclosed, attempts
vars == <<present,retained,effects,touchedSource,disclosed,attempts>>
Requests == [actor:{1,2}, branch:{1,2}, payload:{1,2}]
Init == /\ present=FALSE /\ retained=[actor |-> 0,branch |-> 0,payload |-> 0]
        /\ effects=0 /\ touchedSource=FALSE /\ disclosed=FALSE /\ attempts=0
Create(r,success) ==
  /\ ~present /\ attempts<2 /\ attempts'=attempts+1
  /\ present'=success
  /\ retained'=IF success THEN r ELSE retained
  /\ effects'=IF success \/ PartialCommit THEN 1 ELSE 0
  /\ touchedSource'=(touchedSource \/ (success /\ ReuseSourceKey))
  /\ UNCHANGED disclosed
Replay(r) ==
  /\ present /\ attempts<2 /\ attempts'=attempts+1
  /\ (r=retained \/ UnboundReplay)
  /\ disclosed'=(disclosed \/ r#retained)
  /\ UNCHANGED <<present,retained,effects,touchedSource>>
Next == (\E r \in Requests, success \in BOOLEAN : Create(r,success))
        \/ (\E r \in Requests : Replay(r))
SourceSeparation == ~touchedSource
BoundReplay == ~disclosed
AtomicCreation == effects=(IF present THEN 1 ELSE 0)
Spec == Init /\ [][Next]_vars
=============================================================================
