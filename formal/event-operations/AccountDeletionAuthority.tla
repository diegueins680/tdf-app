---- MODULE AccountDeletionAuthority ----
EXTENDS Naturals, TLC
CONSTANTS RequireCurrent, HoldSession, HoldGrants
VARIABLES phase, session, operator, moduleGrant, captured, locked, effect
vars == <<phase, session, operator, moduleGrant, captured, locked, effect>>
Init == /\ phase = "new"
        /\ session = TRUE /\ operator = TRUE /\ moduleGrant = TRUE
        /\ captured = FALSE /\ locked = {}
        /\ effect = FALSE
Capture == /\ phase = "new"
           /\ captured' = (session /\ operator /\ moduleGrant)
           /\ phase' = "captured"
           /\ UNCHANGED <<session, operator, moduleGrant, locked, effect>>
Admit == /\ phase = "captured"
         /\ captured
         /\ (~RequireCurrent \/ (session /\ operator /\ moduleGrant))
         /\ locked' = (IF HoldSession THEN {"session"} ELSE {})
                       \cup (IF HoldGrants THEN {"operator", "module"} ELSE {})
         /\ phase' = "admitted"
         /\ UNCHANGED <<session, operator, moduleGrant, captured, effect>>
RevokeSession == /\ phase # "done" /\ session /\ "session" \notin locked
                 /\ session' = FALSE
                 /\ UNCHANGED <<phase, operator, moduleGrant, captured, locked, effect>>
RevokeOperator == /\ phase # "done" /\ operator /\ "operator" \notin locked
                  /\ operator' = FALSE
                  /\ UNCHANGED <<phase, session, moduleGrant, captured, locked, effect>>
RevokeModule == /\ phase # "done" /\ moduleGrant /\ "module" \notin locked
                /\ moduleGrant' = FALSE
                /\ UNCHANGED <<phase, session, operator, captured, locked, effect>>
Commit == /\ phase = "admitted"
          /\ effect' = TRUE /\ phase' = "done"
          /\ UNCHANGED <<session, operator, moduleGrant, captured, locked>>
Next == Capture \/ Admit \/ RevokeSession \/ RevokeOperator \/ RevokeModule \/ Commit
CurrentAuthorityAtEffect == effect => (session /\ operator /\ moduleGrant)
Spec == Init /\ [][Next]_vars
====
