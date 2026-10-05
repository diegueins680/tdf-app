---- MODULE WorkerCompletionEvidence ----
EXTENDS Naturals, FiniteSets
CONSTANTS Jobs, UnsafeIgnoreFailedItem, UnsafeSkipAcknowledgment
VARIABLES items, phase, outcome, acknowledged, returnedSuccess
vars == <<items, phase, outcome, acknowledged, returnedSuccess>>
Init == /\ items = [j \in Jobs |-> "pending"]
        /\ phase = "working" /\ outcome = "none"
        /\ ~acknowledged /\ ~returnedSuccess
Record(j, result) ==
  /\ phase = "working" /\ items[j] = "pending"
  /\ result \in {"succeeded", "failed"}
  /\ items' = [items EXCEPT ![j] = result]
  /\ UNCHANGED <<phase, outcome, acknowledged, returnedSuccess>>
Finish ==
  /\ phase = "working" /\ \A j \in Jobs : items[j] # "pending"
  /\ phase' = "finalized"
  /\ outcome' = IF UnsafeIgnoreFailedItem \/ (\A j \in Jobs : items[j] = "succeeded")
                 THEN "completed" ELSE "failed"
  /\ UNCHANGED <<items, acknowledged, returnedSuccess>>
Acknowledge ==
  /\ phase = "finalized" /\ ~acknowledged /\ acknowledged' = TRUE
  /\ UNCHANGED <<items, phase, outcome, returnedSuccess>>
ReturnSuccess ==
  /\ phase = "finalized" /\ outcome = "completed" /\ ~returnedSuccess
  /\ acknowledged \/ UnsafeSkipAcknowledgment
  /\ returnedSuccess' = TRUE
  /\ UNCHANGED <<items, phase, outcome, acknowledged>>
Next == (\E j \in Jobs, result \in {"succeeded", "failed"} : Record(j, result))
        \/ Finish \/ Acknowledge \/ ReturnSuccess
TypeOK == /\ items \in [Jobs -> {"pending", "succeeded", "failed"}]
          /\ phase \in {"working", "finalized"}
          /\ outcome \in {"none", "completed", "failed"}
          /\ acknowledged \in BOOLEAN /\ returnedSuccess \in BOOLEAN
CompletedHasNoFailedItems == outcome = "completed" => \A j \in Jobs : items[j] = "succeeded"
SuccessfulReturnHasEvidence == returnedSuccess => acknowledged /\ outcome = "completed"
Spec == Init /\ [][Next]_vars
====
