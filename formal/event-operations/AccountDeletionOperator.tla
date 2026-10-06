---- MODULE AccountDeletionOperator ----
EXTENDS Naturals, TLC
CONSTANTS RequireRole, RequireModule
Actors == {"admin", "manager", "studio", "admin-no-module", "manager-no-module", "studio-no-module", "intern", "unprivileged"}
Operators == {"admin", "manager", "studio", "admin-no-module", "manager-no-module", "studio-no-module"}
ModuleGrants == {"admin", "manager", "studio", "intern"}
Actions == {"read-legacy", "read-queue", "complete", "reject"}
VARIABLE effect
authorized(actor) == actor \in Operators /\ actor \in ModuleGrants
Init == effect = [operator |-> "none", operation |-> "none"]
Invoke(actor, action) ==
  /\ effect.operator = "none"
  /\ (~RequireRole \/ actor \in Operators)
  /\ (~RequireModule \/ actor \in ModuleGrants)
  /\ effect' = [operator |-> actor, operation |-> action]
Next == \E actor \in Actors, action \in Actions: Invoke(actor, action)
OnlyAuthorizedPrivacyEffects == IF effect.operator = "none" THEN TRUE ELSE authorized(effect.operator)
Spec == Init /\ [][Next]_effect
====
