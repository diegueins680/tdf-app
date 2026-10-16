---- MODULE ChatNotificationAuthorization ----
EXTENDS TLC
CONSTANT RequireFreshAuthorization
VARIABLES phase, cachedThread, displayed, markedRead, badDisplay, badRead
vars == <<phase, cachedThread, displayed, markedRead, badDisplay, badRead>>

\* One notification target with stale cached membership/message data. The model
\* ends at initial authorization; ordinary polling and backend policy are outside
\* this boundary. Rendering and read bookkeeping may run before network completion.
Init == /\ phase = "pending" /\ cachedThread = TRUE
        /\ displayed = FALSE /\ markedRead = FALSE
        /\ badDisplay = FALSE /\ badRead = FALSE
Resolve(allowed) == /\ phase = "pending"
                    /\ phase' = IF allowed THEN "authorized" ELSE "denied"
                    /\ cachedThread' = allowed
                    /\ UNCHANGED <<displayed, markedRead, badDisplay, badRead>>
Fail == /\ phase = "pending" /\ phase' = "failed"
        /\ UNCHANGED <<cachedThread, displayed, markedRead, badDisplay, badRead>>
Retry == /\ phase = "failed" /\ phase' = "pending"
         /\ UNCHANGED <<cachedThread, displayed, markedRead, badDisplay, badRead>>
CanUseThread == cachedThread /\ phase # "failed"
                /\ (~RequireFreshAuthorization \/ phase = "authorized")
Display == /\ CanUseThread /\ displayed' = TRUE
           /\ badDisplay' = (badDisplay \/ phase # "authorized")
           /\ UNCHANGED <<phase, cachedThread, markedRead, badRead>>
MarkRead == /\ CanUseThread /\ markedRead' = TRUE
            /\ badRead' = (badRead \/ phase # "authorized")
            /\ UNCHANGED <<phase, cachedThread, displayed, badDisplay>>
Next == Resolve(TRUE) \/ Resolve(FALSE) \/ Fail \/ Retry \/ Display \/ MarkRead
NoCachedDisplay == ~badDisplay
NoEarlyRead == ~badRead
Spec == Init /\ [][Next]_vars
====
