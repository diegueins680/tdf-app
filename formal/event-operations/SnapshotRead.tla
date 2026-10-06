---- MODULE SnapshotRead ----
EXTENDS Naturals, TLC
CONSTANTS Recheck, SharedClock, RedactLogs
VARIABLES grant, now, phase, locked, earlyAllowed, sampledAt,
          headerAllowed, capabilityAllowed, authorizedAtDecision, log
vars == <<grant, now, phase, locked, earlyAllowed, sampledAt,
          headerAllowed, capabilityAllowed, authorizedAtDecision, log>>
Allowed(at) == grant /\ at < 2
Init ==
  /\ grant = TRUE /\ now = 0 /\ phase = "idle" /\ locked = FALSE
  /\ earlyAllowed = FALSE /\ sampledAt = 0
  /\ headerAllowed = FALSE /\ capabilityAllowed = FALSE
  /\ authorizedAtDecision = FALSE /\ log = [event |-> "database_error"]
Begin ==
  /\ phase = "idle" /\ phase' = "waiting" /\ earlyAllowed' = Allowed(now)
  /\ UNCHANGED <<grant, now, locked, sampledAt, headerAllowed,
                 capabilityAllowed, authorizedAtDecision, log>>
Acquire ==
  /\ phase = "waiting" /\ phase' = "locked" /\ locked' = TRUE
  /\ UNCHANGED <<grant, now, earlyAllowed, sampledAt, headerAllowed,
                 capabilityAllowed, authorizedAtDecision, log>>
Project ==
  /\ phase = "locked" /\ phase' = "projecting" /\ sampledAt' = now
  /\ authorizedAtDecision' = Allowed(now)
  /\ headerAllowed' = IF Recheck THEN Allowed(now) ELSE earlyAllowed
  /\ UNCHANGED <<grant, now, locked, earlyAllowed, capabilityAllowed, log>>
Finish ==
  /\ phase = "projecting" /\ phase' = "done" /\ locked' = FALSE
  /\ capabilityAllowed' = Allowed(IF SharedClock THEN sampledAt ELSE now)
  /\ UNCHANGED <<grant, now, earlyAllowed, sampledAt, headerAllowed,
                 authorizedAtDecision, log>>
ChangeGrant ==
  /\ ~locked /\ grant' = ~grant
  /\ UNCHANGED <<now, phase, locked, earlyAllowed, sampledAt, headerAllowed,
                 capabilityAllowed, authorizedAtDecision, log>>
Tick ==
  /\ now < 3 /\ now' = now + 1
  /\ UNCHANGED <<grant, phase, locked, earlyAllowed, sampledAt, headerAllowed,
                 capabilityAllowed, authorizedAtDecision, log>>
LogFailure(secret) ==
  /\ log' = IF RedactLogs
       THEN [event |-> "database_error", category |-> "unavailable"]
       ELSE [event |-> "database_error", detail |-> secret]
  /\ UNCHANGED <<grant, now, phase, locked, earlyAllowed, sampledAt, headerAllowed,
                 capabilityAllowed, authorizedAtDecision>>
Next == Begin \/ Acquire \/ Project \/ Finish \/ ChangeGrant \/ Tick
        \/ \E secret \in {"credential", "contract"}: LogFailure(secret)
NoUnauthorizedSnapshot == headerAllowed => authorizedAtDecision
CoherentProjection == (phase = "done" /\ headerAllowed) => capabilityAllowed
LogFieldsAllowlisted == DOMAIN log \subseteq {"event", "category"}
Spec == Init /\ [][Next]_vars
====
