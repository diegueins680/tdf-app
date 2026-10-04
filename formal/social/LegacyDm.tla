---------------------------- MODULE LegacyDm ----------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS UnsafePause, UnsafeConsent
VARIABLES enabled, activated, exists, consent, blocked, closed, phase, observed
vars == <<enabled, activated, exists, consent, blocked, closed, phase, observed>>
Actors == {1,2}
Required == activated \/ exists \/ closed
Permitted == ~closed /\ ~blocked /\ consent = Actors
Init == /\ enabled = FALSE /\ activated = FALSE /\ exists = FALSE
        /\ consent = {} /\ blocked = FALSE /\ closed = FALSE /\ phase = "ready"
        /\ observed = [allowed |-> FALSE, required |-> FALSE, permitted |-> FALSE]
Activate == /\ ~enabled /\ enabled' = TRUE /\ activated' = TRUE
            /\ UNCHANGED <<exists, consent, blocked, closed, phase, observed>>
Pause == /\ enabled /\ enabled' = FALSE
         /\ UNCHANGED <<activated, exists, consent, blocked, closed, phase, observed>>
Consent(a) == /\ ~blocked /\ ~closed /\ a \notin consent
              /\ exists' = TRUE /\ consent' = consent \cup {a}
              /\ UNCHANGED <<enabled, activated, blocked, closed, phase, observed>>
Disconnect(a) == /\ a \in consent /\ consent' = consent \ {a}
                 /\ UNCHANGED <<enabled, activated, exists, blocked, closed, phase, observed>>
Block == /\ ~blocked /\ blocked' = TRUE /\ exists' = TRUE /\ consent' = {}
         /\ UNCHANGED <<enabled, activated, closed, phase, observed>>
Unblock == /\ blocked /\ blocked' = FALSE
           /\ UNCHANGED <<enabled, activated, exists, consent, closed, phase, observed>>
Close == /\ ~closed /\ closed' = TRUE /\ consent' = {}
         /\ UNCHANGED <<enabled, activated, exists, blocked, phase, observed>>
\* Abstract the INSERT authorization boundary under the same pair locks as Block.
\* Legacy permits represent old application checks, not valid new connection consent.
Insert == /\ phase = "ready"
          /\ observed' = [allowed |-> IF UnsafePause /\ ~enabled THEN TRUE
                ELSE IF Required THEN IF UnsafeConsent THEN ~closed /\ ~blocked ELSE Permitted
                ELSE TRUE,
                required |-> Required, permitted |-> Permitted]
          /\ phase' = "done"
          /\ UNCHANGED <<enabled, activated, exists, consent, blocked, closed>>
Next == Activate \/ Pause \/ (\E a \in Actors: Consent(a) \/ Disconnect(a))
        \/ Block \/ Unblock \/ Close \/ Insert
Spec == Init /\ [][Next]_vars /\ WF_vars(Insert)
TypeOK == /\ consent \subseteq Actors /\ phase \in {"ready","done"}
          /\ enabled \in BOOLEAN /\ activated \in BOOLEAN
          /\ exists \in BOOLEAN /\ blocked \in BOOLEAN /\ closed \in BOOLEAN
AuthorityAtInsert == observed.allowed /\ observed.required => observed.permitted
ConsentIntegrity == blocked => consent = {}
ActivationMemory == enabled => activated
Progress == phase = "ready" ~> phase = "done"
=============================================================================
