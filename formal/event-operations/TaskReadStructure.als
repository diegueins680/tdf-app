module event_operations/TaskReadStructure

abstract sig Scope {}
one sig TaskRead, TaskManage, EventRead, FinanceRead extends Scope {}
sig Party {}
sig Event {}
sig Task { event: one Event }
sig Grant {
  event: one Event,
  grantee: one Party,
  scope: one Scope,
  task: lone Task
}
// Effective intervals/revocation are abstracted as a set, not assumed for every grant.
one sig Current { effective: set Grant }

pred matches[g: Grant, p: Party, e: Event, t: Task] {
  t.event = e
  g in Current.effective
  g.grantee = p
  g.event = e
  g.scope in TaskRead + TaskManage
  no g.task or g.task = t
}
pred canRead[p: Party, e: Event, t: Task] {
  some g: Grant | matches[g, p, e, t]
}
pred canManage[p: Party, e: Event, t: Task] {
  some g: Grant | matches[g, p, e, t] and g.scope = TaskManage
}
pred editorCandidate[actor, candidate: Party, e: Event, t: Task] {
  canManage[actor, e, t] and canRead[candidate, e, t]
}
assert CandidateRequiresCurrentManager {
  all actor, candidate: Party, e: Event, t: Task | editorCandidate[actor,candidate,e,t]
    implies some g: Current.effective | g.grantee=actor and g.scope=TaskManage and matches[g,actor,e,t]
}
assert CandidateIsTaskScoped {
  all actor, candidate: Party, e: Event, t: Task | editorCandidate[actor,candidate,e,t]
    implies t.event=e and some g: Current.effective | matches[g,candidate,e,t]
}
pred leastPrivilegeScenario {
  some disj p, other: Party, e: Event, disj t, sibling: Task, g: Grant | {
    t.event = e
    sibling.event = e
    g.event = e
    g.grantee = p
    g.scope = TaskRead
    g.task = t
    Current.effective = g
    canRead[p, e, t]
    not canRead[p, e, sibling]
    not canRead[other, e, t]
  }
}
assert TargetIsEventLocal {
  all p: Party, e: Event, t: Task | canRead[p, e, t] implies t.event = e
}
assert ExactGrantDoesNotWiden {
  all g: Grant, p: Party, e: Event, t: Task |
    some g.task and matches[g, p, e, t] implies g.task = t
}
assert NoAmbientAuthority {
  all g: Grant, p: Party, e: Event, t: Task |
    matches[g, p, e, t] implies
      g.grantee = p and g in Current.effective and g.scope in TaskRead + TaskManage
}
run leastPrivilegeScenario for 4 but exactly 2 Party, exactly 2 Event, exactly 2 Task, exactly 1 Grant
check TargetIsEventLocal for 4
check ExactGrantDoesNotWiden for 4
check NoAmbientAuthority for 4
check CandidateRequiresCurrentManager for 4
check CandidateIsTaskScoped for 4
