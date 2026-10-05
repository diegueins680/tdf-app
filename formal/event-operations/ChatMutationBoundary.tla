---- MODULE ChatMutationBoundary ----
EXTENDS Naturals, TLC
CONSTANTS MaxRequests, DecodeBeforeCommit, RollbackRejected
VARIABLES phase, attempt, committed, before, staged, valid
vars == <<phase, attempt, committed, before, staged, valid>>

Init == /\ phase = "idle" /\ attempt = 0 /\ committed = 0
        /\ before = 0 /\ staged = 0 /\ valid \in BOOLEAN
Start == /\ phase = "idle" /\ attempt < MaxRequests
         /\ phase' = "executed" /\ attempt' = attempt + 1
         /\ before' = committed /\ staged' = 1 /\ valid' \in BOOLEAN
         /\ committed' = IF DecodeBeforeCommit THEN committed ELSE committed + 1
Decode == /\ phase = "executed"
          /\ phase' = IF valid THEN "validated" ELSE "rejected"
          /\ staged' = IF valid THEN staged ELSE 0
          /\ committed' = IF ~valid /\ ~RollbackRejected /\ DecodeBeforeCommit
                           THEN committed + staged ELSE committed
          /\ UNCHANGED <<attempt, before, valid>>
Commit == /\ phase = "validated" /\ phase' = "committed"
          /\ committed' = IF DecodeBeforeCommit THEN committed + staged ELSE committed
          /\ staged' = 0 /\ UNCHANGED <<attempt, before, valid>>
Deliver == /\ phase = "committed" /\ phase' \in {"accepted", "response-lost"}
           /\ UNCHANGED <<attempt, committed, before, staged, valid>>
Retry == /\ phase \in {"accepted", "rejected", "response-lost"}
         /\ phase' = "idle" /\ UNCHANGED <<attempt, committed, before, staged, valid>>
Next == Start \/ Decode \/ Commit \/ Deliver \/ Retry

RejectedLeavesNoMutation == phase = "rejected" => committed = before
AcceptedHasOneMutation == phase \in {"accepted", "response-lost"} => committed = before + 1
OnlyValidatedCommit == phase \in {"committed", "accepted", "response-lost"} => valid
BoundedRequests == committed <= attempt /\ attempt <= MaxRequests
Spec == Init /\ [][Next]_vars
====
