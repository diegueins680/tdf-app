------------------------- MODULE OperationsWorkerTick -------------------------
(* Operations worker maintenance tick (OPS-WORKER-001):
   operationsMaintenanceTick in tdf-hq/src/TDF/Operations/Worker.hs and
   operations_process_outbox_batch in
   tdf-hq/sql/2026-10-05_operations_scoped_projection.sql.

   Outbox rows are healthy or poisoned (their entity no longer exists, which
   the deferred constraint trigger operations_work_item_entity_reference
   reports). Each tick runs one batch in one transaction: every due row is
   projected or fails alone with a recorded attempt and a backoff; a poisoned
   row is dead-lettered after MaxAttempts. With constraints checked at each
   statement the trigger's error is caught per row; with the deferred default
   it surfaces at commit and the whole tick rolls back, recording nothing.

   Guards, each removed by one negative configuration:
     ImmediateCheck  constraints are checked per statement inside the batch
     RecordAttempt   a failed projection increments the row's attempt
     DeadLetterBound a row is dead-lettered once its attempts reach MaxAttempts *)
EXTENDS Naturals, FiniteSets

CONSTANTS Rows, Poisoned, MaxAttempts, MaxTicks, ImmediateCheck, RecordAttempt, DeadLetterBound

ASSUME Poisoned \subseteq Rows

VARIABLES
  status,   \* [Rows -> {"pending", "processed", "dead_letter"}]
  attempts, \* [Rows -> Nat]
  due,      \* [Rows -> BOOLEAN]: next_attempt_at has passed
  ticks,    \* ticks run so far
  aborted   \* ticks that rolled back entirely

vars == <<status, attempts, due, ticks, aborted>>

TypeOK ==
  /\ status \in [Rows -> {"pending", "processed", "dead_letter"}]
  /\ attempts \in [Rows -> 0..MaxAttempts]
  /\ due \in [Rows -> BOOLEAN] /\ ticks \in 0..MaxTicks /\ aborted \in 0..MaxTicks

Init ==
  /\ status = [r \in Rows |-> "pending"] /\ attempts = [r \in Rows |-> 0]
  /\ due = [r \in Rows |-> TRUE] /\ ticks = 0 /\ aborted = 0

Due == {r \in Rows : status[r] = "pending" /\ due[r]}

(* One batch. Healthy due rows are projected; a poisoned due row fails. *)
Outcome(r) ==
  IF r \notin Due THEN status[r]
  ELSE IF r \notin Poisoned THEN "processed"
  ELSE IF DeadLetterBound /\ attempts[r] + 1 >= MaxAttempts THEN "dead_letter"
  ELSE "pending"

TickCommits ==
  /\ ticks < MaxTicks
  /\ ImmediateCheck \/ Due \cap Poisoned = {}
  /\ status' = [r \in Rows |-> Outcome(r)]
  /\ attempts' = [r \in Rows |-> IF r \in Due \cap Poisoned /\ RecordAttempt
                                  THEN (IF attempts[r] < MaxAttempts THEN attempts[r] + 1 ELSE attempts[r])
                                  ELSE attempts[r]]
  /\ due' = [r \in Rows |-> IF r \in Due \cap Poisoned THEN FALSE ELSE due[r]]
  /\ ticks' = ticks + 1 /\ aborted' = aborted

(* The deferred trigger fires at commit: nothing in the tick survives. *)
TickAborts ==
  /\ ticks < MaxTicks
  /\ ~ImmediateCheck /\ Due \cap Poisoned # {}
  /\ ticks' = ticks + 1 /\ aborted' = aborted + 1
  /\ UNCHANGED <<status, attempts, due>>

(* The backoff of a failed row elapses. *)
Elapse(r) == status[r] = "pending" /\ ~due[r] /\ due' = [due EXCEPT ![r] = TRUE]
  /\ UNCHANGED <<status, attempts, ticks, aborted>>

Done == ticks = MaxTicks /\ UNCHANGED vars

Next == TickCommits \/ TickAborts \/ \E r \in Rows : Elapse(r) \/ Done

Spec == Init /\ [][Next]_vars

(* A healthy due row (another aggregate) is never left pending by a tick that
   ran: a poisoned row cannot hold other aggregates' rows hostage. *)
PoisonIsolated == ticks > 0 => \A r \in Rows \ Poisoned : status[r] = "processed"

(* A tick never rolls back as a whole. *)
NoAbortedTick == aborted = 0

(* Every failed projection is recorded: a poisoned row that was due in a
   committed tick carries at least one attempt afterwards. *)
AttemptsRecorded == \A r \in Poisoned : (ticks > 0 /\ aborted = 0 /\ ~due[r]) => attempts[r] >= 1

(* A poisoned row is dead-lettered exactly when its attempts reach the bound. *)
DeadLetterAtBound == \A r \in Poisoned : status[r] = "dead_letter" <=> attempts[r] = MaxAttempts

=============================================================================
