---- MODULE TicketCaptureFence ----
EXTENDS Naturals
CONSTANTS UnsafeReadBeforeLock, UnsafeWriterWithoutLock
VARIABLES reader, writer, owner, uncertain, observed, admitted, failedBeforeAdmission
vars == <<reader, writer, owner, uncertain, observed, admitted, failedBeforeAdmission>>
Init == /\ reader = "initial" /\ writer = "initial" /\ owner = "none"
        /\ uncertain = FALSE /\ observed = FALSE
        /\ admitted = FALSE /\ failedBeforeAdmission = FALSE
WriterLock == /\ writer = "initial"
              /\ (owner = "none" \/ UnsafeWriterWithoutLock)
              /\ writer' = "locked"
              /\ owner' = IF UnsafeWriterWithoutLock THEN owner ELSE "writer"
              /\ UNCHANGED <<reader, uncertain, observed, admitted, failedBeforeAdmission>>
WriterCommit == /\ writer = "locked"
                /\ writer' = "done" /\ uncertain' = TRUE
                /\ owner' = IF owner = "writer" THEN "none" ELSE owner
                /\ UNCHANGED <<reader, observed, admitted, failedBeforeAdmission>>
EarlyRead == /\ UnsafeReadBeforeLock /\ reader = "initial"
             /\ reader' = "read" /\ observed' = uncertain
             /\ UNCHANGED <<writer, owner, uncertain, admitted, failedBeforeAdmission>>
ReaderLock == /\ owner = "none"
              /\ (IF UnsafeReadBeforeLock THEN reader = "read" ELSE reader = "initial")
              /\ owner' = "reader" /\ reader' = "locked"
              /\ observed' = IF UnsafeReadBeforeLock THEN observed ELSE uncertain
              /\ UNCHANGED <<writer, uncertain, admitted, failedBeforeAdmission>>
Admit == /\ reader = "locked" /\ owner = "reader"
         /\ admitted' = ~observed /\ failedBeforeAdmission' = uncertain
         /\ reader' = "done" /\ owner' = "none"
         /\ UNCHANGED <<writer, uncertain, observed>>
Next == WriterLock \/ WriterCommit \/ EarlyRead \/ ReaderLock \/ Admit
Spec == Init /\ [][Next]_vars
CommittedUncertaintyFencesAdmission == admitted => ~failedBeforeAdmission
TypeOK == /\ reader \in {"initial", "read", "locked", "done"}
          /\ writer \in {"initial", "locked", "done"}
          /\ owner \in {"none", "writer", "reader"}
          /\ uncertain \in BOOLEAN /\ observed \in BOOLEAN
          /\ admitted \in BOOLEAN /\ failedBeforeAdmission \in BOOLEAN
====
