---------------------------- MODULE DmReads ----------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS Actors, Members, Viewers, UnsafeCache, UnsafeOutsider, UnsafeCursor
VARIABLES enabled, activated, exists, consent, blocked, closed, live,
          viewer, route, cursor, done, observed
vars == <<enabled, activated, exists, consent, blocked, closed, live,
          viewer, route, cursor, done, observed>>
Required == activated \/ exists \/ closed # {}
DomainAllowed == ~blocked /\ closed = {} /\ Members \subseteq live /\ consent = Members
Visible == viewer \in Members /\ (~Required \/ DomainAllowed)
CursorOK == route = "threads" \/ cursor # "foreign"
Init == /\ enabled = FALSE /\ activated = FALSE /\ exists = FALSE
        /\ consent = {} /\ blocked = FALSE /\ closed = {} /\ live = Members
        /\ viewer \in Viewers /\ route \in {"threads","messages"}
        /\ cursor \in {"none","local","foreign"} /\ done = FALSE
        /\ observed = [fields |-> {}, participant |-> FALSE, permitted |-> FALSE,
                       cursorOK |-> FALSE, leakedCursor |-> FALSE]
Activate == /\ ~enabled /\ enabled' = TRUE /\ activated' = TRUE
            /\ UNCHANGED <<exists,consent,blocked,closed,live,viewer,route,cursor,done,observed>>
Pause == /\ enabled /\ enabled' = FALSE
         /\ UNCHANGED <<activated,exists,consent,blocked,closed,live,viewer,route,cursor,done,observed>>
Consent(a) == /\ a \in Members /\ a \notin consent /\ ~blocked /\ closed = {}
              /\ consent' = consent \cup {a} /\ exists' = TRUE
              /\ UNCHANGED <<enabled,activated,blocked,closed,live,viewer,route,cursor,done,observed>>
Disconnect(a) == /\ a \in consent /\ consent' = consent \ {a}
                 /\ UNCHANGED <<enabled,activated,exists,blocked,closed,live,viewer,route,cursor,done,observed>>
Block == /\ ~blocked /\ blocked' = TRUE /\ exists' = TRUE /\ consent' = {}
         /\ UNCHANGED <<enabled,activated,closed,live,viewer,route,cursor,done,observed>>
Unblock == /\ blocked /\ blocked' = FALSE
           /\ UNCHANGED <<enabled,activated,exists,consent,closed,live,viewer,route,cursor,done,observed>>
Close(a) == /\ a \in Members \ closed /\ closed' = closed \cup {a} /\ consent' = {}
            /\ UNCHANGED <<enabled,activated,exists,blocked,live,viewer,route,cursor,done,observed>>
Revoke(a) == /\ a \in live /\ live' = live \ {a}
             /\ UNCHANGED <<enabled,activated,exists,consent,blocked,closed,viewer,route,cursor,done,observed>>
\* One authoritative statement snapshot covers policy, content and cursor checking.
Read == /\ ~done /\ done' = TRUE
        /\ observed' = [fields |-> IF CursorOK /\
              (IF UnsafeOutsider THEN TRUE ELSE IF UnsafeCache THEN viewer \in Members ELSE Visible)
              THEN IF route="threads" THEN {"identity","preview"} ELSE {"author","body"} ELSE {},
            participant |-> viewer \in Members, permitted |-> ~Required \/ DomainAllowed,
            cursorOK |-> CursorOK,
            leakedCursor |-> UnsafeCursor /\ ~Visible /\ cursor="foreign" /\ route="messages"]
        /\ UNCHANGED <<enabled,activated,exists,consent,blocked,closed,live,viewer,route,cursor>>
Next == Activate \/ Pause \/ Block \/ Unblock \/ Read
        \/ (\E a \in Members: Consent(a) \/ Disconnect(a) \/ Close(a) \/ Revoke(a))
Spec == Init /\ [][Next]_vars /\ WF_vars(Read)
FieldsAuthorized == observed.fields # {} =>
  observed.participant /\ observed.permitted /\ observed.cursorOK
NoCursorLeak == ~observed.leakedCursor
Progress == ~done ~> done
=============================================================================
