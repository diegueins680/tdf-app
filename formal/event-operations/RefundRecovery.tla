---- MODULE RefundRecovery ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Refunds, Workers, Captured,
          UnsafeExecutionReplay, UnsafeAccountingReplay, UnsafeAuthority
VARIABLES state, executions, effects, refunded, phase, target, evidence,
          enabled, unauthorizedEffects
vars == <<state, executions, effects, refunded, phase, target, evidence,
          enabled, unauthorizedEffects>>
Init ==
  /\ state = [r \in Refunds |-> "new"]
  /\ executions = [r \in Refunds |-> 0]
  /\ effects = [r \in Refunds |-> 0]
  /\ refunded = 0
  /\ phase = [w \in Workers |-> "idle"]
  /\ target = [w \in Workers |-> "none"]
  /\ evidence = [w \in Workers |-> "held"]
  /\ enabled = TRUE
  /\ unauthorizedEffects = 0
Execute(r) ==
  /\ enabled
  /\ state[r] = "new" \/ (UnsafeExecutionReplay /\ state[r] = "processing")
  /\ executions[r] < 2
  /\ state' = [state EXCEPT ![r] = "processing"]
  /\ executions' = [executions EXCEPT ![r] = @ + 1]
  /\ UNCHANGED <<effects, refunded, phase, target, evidence, enabled, unauthorizedEffects>>
Query(w, r, result) ==
  /\ enabled /\ state[r] = "processing" /\ phase[w] = "idle"
  /\ phase' = [phase EXCEPT ![w] = "ready"]
  /\ target' = [target EXCEPT ![w] = r]
  /\ evidence' = [evidence EXCEPT ![w] = result]
  /\ UNCHANGED <<state, executions, effects, refunded, enabled, unauthorizedEffects>>
Apply(w) ==
  /\ phase[w] = "ready"
  /\ LET r == target[w]
         admitted == (enabled \/ UnsafeAuthority)
           /\ evidence[w] = "verified"
           /\ (state[r] = "processing" \/ UnsafeAccountingReplay)
     IN /\ IF admitted
           THEN /\ state' = [state EXCEPT ![r] = "succeeded"]
                /\ effects' = [effects EXCEPT ![r] = @ + 1]
                /\ refunded' = refunded + 1
                /\ unauthorizedEffects' = unauthorizedEffects + (IF enabled THEN 0 ELSE 1)
           ELSE UNCHANGED <<state, effects, refunded, unauthorizedEffects>>
  /\ phase' = [phase EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<executions, target, evidence, enabled>>
ToggleAuthority ==
  /\ enabled' = ~enabled
  /\ UNCHANGED <<state, executions, effects, refunded, phase, target, evidence, unauthorizedEffects>>
Next ==
  \/ \E r \in Refunds: Execute(r)
  \/ \E w \in Workers, r \in Refunds, result \in {"held", "verified"}: Query(w, r, result)
  \/ \E w \in Workers: Apply(w)
  \/ ToggleAuthority
TypeOK ==
  /\ state \in [Refunds -> {"new", "processing", "succeeded"}]
  /\ executions \in [Refunds -> 0..2]
  /\ effects \in [Refunds -> 0..2]
  /\ refunded \in 0..(2 * Cardinality(Refunds))
  /\ phase \in [Workers -> {"idle", "ready"}]
  /\ target \in [Workers -> Refunds \cup {"none"}]
  /\ evidence \in [Workers -> {"held", "verified"}]
  /\ enabled \in BOOLEAN
NoDuplicateExecution == \A r \in Refunds: executions[r] <= 1
NoDuplicateAccounting == \A r \in Refunds: effects[r] <= 1
ReceiptsMatchAccounting == refunded = Cardinality({r \in Refunds: state[r] = "succeeded"})
Conservation == refunded <= Captured
CurrentAuthorityAtApply == unauthorizedEffects = 0
Spec == Init /\ [][Next]_vars
====
