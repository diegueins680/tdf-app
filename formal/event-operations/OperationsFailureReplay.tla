---------------------- MODULE OperationsFailureReplay ----------------------
(* One record's projection queue and the failure that reports it. The worker
   runs the head event; the eighth failed attempt dead-letters it, and a
   dead-lettered head blocks the later event of the same record. A manager may
   retry the failure. Requeue says whether the retry puts the head back in the
   queue; Settle whether a projection that runs closes the failure; Merge
   whether a second dead letter returns to the existing failure instead of
   opening another. Attempts before the eighth, backoff, authorization and the
   content of the projection are not modelled. *)
EXTENDS Naturals

CONSTANTS Requeue, Settle, Merge, MaxRetries

VARIABLES head, later, failure, open, cause, retries

vars == <<head, later, failure, open, cause, retries>>

TypeOK ==
  /\ head \in {"pending", "processed", "dead"}
  /\ later \in {"pending", "processed"}
  /\ failure \in {"none", "dead_letter", "retrying", "resolved"}
  /\ open \in 0..(MaxRetries + 1)
  /\ cause \in {"broken", "fixed"}
  /\ retries \in 0..MaxRetries

Init ==
  /\ head = "pending" /\ later = "pending" /\ failure = "none"
  /\ open = 0 /\ cause = "broken" /\ retries = 0

Repair ==
  /\ cause = "broken" /\ cause' = "fixed"
  /\ UNCHANGED <<head, later, failure, open, retries>>

WorkerFails ==
  /\ head = "pending" /\ cause = "broken"
  /\ head' = "dead"
  /\ IF failure \in {"none", "resolved"}
       THEN failure' = "dead_letter" /\ open' = open + 1
     ELSE IF Merge
       THEN failure' = "dead_letter" /\ open' = open
       ELSE failure' = failure /\ open' = open + 1
  /\ UNCHANGED <<later, cause, retries>>

WorkerRuns ==
  /\ head = "pending" /\ cause = "fixed"
  /\ head' = "processed"
  /\ IF Settle /\ failure \in {"dead_letter", "retrying"}
       THEN failure' = "resolved" /\ open' = 0
       ELSE UNCHANGED <<failure, open>>
  /\ UNCHANGED <<later, cause, retries>>

LaterRuns ==
  /\ later = "pending" /\ head = "processed"
  /\ later' = "processed"
  /\ UNCHANGED <<head, failure, open, cause, retries>>

Retry ==
  /\ failure = "dead_letter" /\ retries < MaxRetries
  /\ retries' = retries + 1
  /\ IF ~Requeue
       THEN failure' = "retrying" /\ UNCHANGED <<head, open>>
     ELSE IF head = "processed"
       THEN failure' = "resolved" /\ open' = 0 /\ UNCHANGED head
       ELSE failure' = "retrying" /\ head' = "pending" /\ UNCHANGED open
  /\ UNCHANGED <<later, cause>>

Next == Repair \/ WorkerFails \/ WorkerRuns \/ LaterRuns \/ Retry

Spec == Init /\ [][Next]_vars /\ WF_vars(WorkerFails) /\ WF_vars(WorkerRuns) /\ WF_vars(LaterRuns)

RetryingIsQueued == failure = "retrying" => head = "pending"
OneOpenFailure == open <= 1
RunIsSettled == head = "processed" => failure \in {"none", "resolved"}
LaterWaits == later = "processed" => head = "processed"
RetryEnds == (failure = "retrying") ~> (failure \in {"resolved", "dead_letter"})
=============================================================================
