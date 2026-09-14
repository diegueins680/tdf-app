---- MODULE TaskCommit ----
EXTENDS Naturals, FiniteSets, TLC

CONSTANTS Serialize, FinalValidation
Transactions == {"left", "right"}
Workers == {"worker1", "worker2"}
Operations == {"remove1", "remove2", "complete", "block", "completeThenBlock"}
State == [completed: BOOLEAN, blocked: BOOLEAN, responsible: SUBSET Workers]

VARIABLES committed, phase, proposed, operation, lock
vars == <<committed, phase, proposed, operation, lock>>

Apply(s, op) ==
  CASE op = "remove1" -> [s EXCEPT !.responsible = @ \ {"worker1"}]
    [] op = "remove2" -> [s EXCEPT !.responsible = @ \ {"worker2"}]
    [] op = "complete" -> [s EXCEPT !.completed = TRUE]
    [] op = "block" -> [s EXCEPT !.blocked = TRUE]
    [] op = "completeThenBlock" -> [s EXCEPT !.completed = TRUE, !.blocked = TRUE]

Valid(s) == /\ s.responsible # {}
            /\ (s.completed => ~s.blocked)

Init ==
  /\ committed = [completed |-> FALSE, blocked |-> FALSE, responsible |-> Workers]
  /\ phase = [t \in Transactions |-> "idle"]
  /\ proposed = [t \in Transactions |-> committed]
  /\ operation = [t \in Transactions |-> "complete"]
  /\ lock = "none"

Prepare(t, op) ==
  /\ phase[t] = "idle"
  /\ ~Serialize \/ lock = "none"
  /\ phase' = [phase EXCEPT ![t] = "prepared"]
  /\ proposed' = [proposed EXCEPT ![t] = Apply(committed, op)]
  /\ operation' = [operation EXCEPT ![t] = op]
  /\ lock' = IF Serialize THEN t ELSE lock
  /\ UNCHANGED committed

Finish(t) ==
  /\ phase[t] = "prepared"
  /\ LET checked == IF ~FinalValidation /\ operation[t] = "completeThenBlock"
                    THEN [proposed[t] EXCEPT !.blocked = FALSE]
                    ELSE proposed[t]
     IN committed' = IF Valid(checked)
                     THEN Apply(committed, operation[t]) ELSE committed
  /\ phase' = [phase EXCEPT ![t] = "finished"]
  /\ lock' = IF Serialize THEN "none" ELSE lock
  /\ UNCHANGED <<proposed, operation>>

Next == \/ \E t \in Transactions, op \in Operations: Prepare(t, op)
        \/ \E t \in Transactions: Finish(t)
TypeOK == /\ committed \in State
          /\ phase \in [Transactions -> {"idle", "prepared", "finished"}]
          /\ proposed \in [Transactions -> State]
          /\ operation \in [Transactions -> Operations]
          /\ lock \in Transactions \cup {"none"}
NoBlockedCompletion == committed.completed => ~committed.blocked
NoOrphanResponsibilities == committed.responsible # {}
Spec == Init /\ [][Next]_vars
====
