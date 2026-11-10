---- MODULE ApplicationShutdown ----
EXTENDS Integers
CONSTANTS CloseLateListener, GuardPublication, RequireDrain, RequireStartupJoin
VARIABLES prepared, listener, closed, accepted, startupStopped, active, result, latePublication
vars == <<prepared,listener,closed,accepted,startupStopped,active,result,latePublication>>
Init == /\ prepared = FALSE /\ listener = FALSE /\ closed = FALSE
        /\ accepted = FALSE /\ startupStopped = FALSE /\ active = 0
        /\ result = "running" /\ latePublication = FALSE
Prepare == /\ result = "running" /\ ~prepared /\ ~accepted
           /\ prepared' = TRUE
           /\ UNCHANGED <<listener,closed,accepted,startupStopped,active,result,latePublication>>
Register == /\ result = "running" /\ prepared /\ ~listener
            /\ listener' = TRUE /\ closed' = (accepted /\ CloseLateListener)
            /\ UNCHANGED <<prepared,accepted,startupStopped,active,result,latePublication>>
Enter == /\ result = "running" /\ listener /\ ~closed /\ active < 2
         /\ active' = active+1
         /\ UNCHANGED <<prepared,listener,closed,accepted,startupStopped,result,latePublication>>
Finish == /\ result = "running" /\ active > 0 /\ active' = active-1
          /\ UNCHANGED <<prepared,listener,closed,accepted,startupStopped,result,latePublication>>
Stop == /\ result = "running" /\ ~accepted
        /\ accepted' = TRUE /\ closed' = listener
        /\ UNCHANGED <<prepared,listener,startupStopped,active,result,latePublication>>
JoinStartup == /\ result = "running" /\ accepted /\ ~startupStopped
               /\ startupStopped' = TRUE
               /\ UNCHANGED <<prepared,listener,closed,accepted,active,result,latePublication>>
Publish == /\ result = "running" /\ prepared /\ ~startupStopped
           /\ (~accepted \/ ~GuardPublication)
           /\ latePublication' = (latePublication \/ accepted)
           /\ UNCHANGED <<prepared,listener,closed,accepted,startupStopped,active,result>>
Clean == /\ result = "running" /\ accepted /\ (~prepared \/ (listener /\ closed))
         /\ (active = 0 \/ ~RequireDrain)
         /\ (startupStopped \/ ~RequireStartupJoin)
         /\ result' = "clean"
         /\ UNCHANGED <<prepared,listener,closed,accepted,startupStopped,active,latePublication>>
Fail == /\ result = "running" /\ result' = "failed"
        /\ UNCHANGED <<prepared,listener,closed,accepted,startupStopped,active,latePublication>>
Next == Prepare \/ Register \/ Enter \/ Finish \/ Stop \/ JoinStartup \/ Publish \/ Clean \/ Fail
TypeOK == /\ prepared \in BOOLEAN /\ listener \in BOOLEAN /\ closed \in BOOLEAN
          /\ accepted \in BOOLEAN /\ startupStopped \in BOOLEAN /\ active \in 0..2
          /\ result \in {"running","clean","failed"} /\ latePublication \in BOOLEAN
LateListenerClosed == (accepted /\ listener) => closed
NoLatePublication == ~latePublication
CleanRequiresDrain == result = "clean" => active = 0
CleanRequiresStartupJoin == result = "clean" => startupStopped
Spec == Init /\ [][Next]_vars
====
