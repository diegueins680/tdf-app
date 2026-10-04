---- MODULE TaskRead ----
EXTENDS Naturals, TLC
CONSTANTS ScopeBoundary, BindEvent, FreshAuth, AtomicProjection
VARIABLES permission, sameEvent, grant, now, phase, locked, earlyAllowed,
          authorizedAtDecision, visible, revision, headerRevision, raciRevision
vars == <<permission, sameEvent, grant, now, phase, locked, earlyAllowed,
          authorizedAtDecision, visible, revision, headerRevision, raciRevision>>
Permissions == {"owner", "eventTask", "exactTask", "otherTask", "eventRead",
                "finance", "assigned", "coproducer"}
Allowed == sameEvent /\ grant /\ now < 2
           /\ permission \in {"owner", "eventTask", "exactTask"}
Implemented == (sameEvent \/ ~BindEvent) /\ grant /\ now < 2
               /\ (permission \in {"owner", "eventTask", "exactTask"} \/ ~ScopeBoundary)
Init ==
  /\ permission \in Permissions /\ sameEvent \in BOOLEAN
  /\ grant = TRUE /\ now = 0 /\ phase = "idle" /\ locked = FALSE
  /\ earlyAllowed = FALSE /\ authorizedAtDecision = FALSE /\ visible = FALSE
  /\ revision = 1 /\ headerRevision = 0 /\ raciRevision = 0
Begin ==
  /\ phase = "idle" /\ phase' = "waiting" /\ earlyAllowed' = Implemented
  /\ UNCHANGED <<permission, sameEvent, grant, now, locked, authorizedAtDecision,
                 visible, revision, headerRevision, raciRevision>>
Acquire ==
  /\ phase = "waiting" /\ phase' = "locked" /\ locked' = TRUE
  /\ UNCHANGED <<permission, sameEvent, grant, now, earlyAllowed, authorizedAtDecision,
                 visible, revision, headerRevision, raciRevision>>
Project ==
  /\ phase = "locked" /\ phase' = "projecting"
  /\ authorizedAtDecision' = Allowed
  /\ visible' = IF FreshAuth THEN Implemented ELSE earlyAllowed
  /\ headerRevision' = revision /\ raciRevision' = revision
  /\ UNCHANGED <<permission, sameEvent, grant, now, locked, earlyAllowed, revision>>
Finish ==
  /\ phase = "projecting" /\ phase' = "done" /\ locked' = FALSE
  /\ raciRevision' = IF AtomicProjection THEN raciRevision ELSE revision
  /\ UNCHANGED <<permission, sameEvent, grant, now, earlyAllowed, authorizedAtDecision,
                 visible, revision, headerRevision>>
Revoke ==
  /\ ~locked /\ grant /\ grant' = FALSE
  /\ UNCHANGED <<permission, sameEvent, now, phase, locked, earlyAllowed,
                 authorizedAtDecision, visible, revision, headerRevision, raciRevision>>
WriteTask ==
  /\ revision = 1 /\ revision' = 2
  /\ UNCHANGED <<permission, sameEvent, grant, now, phase, locked, earlyAllowed,
                 authorizedAtDecision, visible, headerRevision, raciRevision>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<permission, sameEvent, grant, phase, locked, earlyAllowed,
                 authorizedAtDecision, visible, revision, headerRevision, raciRevision>>
Next == Begin \/ Acquire \/ Project \/ Finish \/ Revoke \/ WriteTask \/ Tick
NoUnauthorizedTask == visible => authorizedAtDecision
CoherentTaskProjection == (phase = "done" /\ visible) => headerRevision = raciRevision
Spec == Init /\ [][Next]_vars
====
