---- MODULE CommandBoundary ----
EXTENDS TLC
CONSTANTS ValidateBeforeCommit, BindReceipt
VARIABLES phase, shape, bound, committed, success
vars == <<phase, shape, bound, committed, success>>
Init == /\ phase = "pending" /\ shape \in BOOLEAN /\ bound \in BOOLEAN
        /\ committed = FALSE /\ success = FALSE
DatabaseResult == /\ phase = "pending" /\ phase' = "returned"
                  /\ committed' = ~ValidateBeforeCommit
                  /\ UNCHANGED <<shape, bound, success>>
Validate == /\ phase = "returned"
            /\ phase' = IF shape /\ (bound \/ ~BindReceipt) THEN "validated" ELSE "failed"
            /\ UNCHANGED <<shape, bound, committed, success>>
Commit == /\ phase = "validated" /\ phase' = "done" /\ committed' = TRUE
          /\ success' = TRUE /\ UNCHANGED <<shape, bound>>
Abort == /\ phase = "validated" /\ phase' = "failed"
         /\ UNCHANGED <<shape, bound, committed, success>>
Next == DatabaseResult \/ Validate \/ Commit \/ Abort
TypeOK == /\ phase \in {"pending","returned","validated","done","failed"}
          /\ shape \in BOOLEAN /\ bound \in BOOLEAN /\ committed \in BOOLEAN /\ success \in BOOLEAN
ValidatedCommit == committed => (shape /\ bound)
DurableSuccess == success => (committed /\ shape /\ bound)
Spec == Init /\ [][Next]_vars
====
