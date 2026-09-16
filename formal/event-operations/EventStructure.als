module event_operations/EventStructure

open util/integer

abstract sig Flag {}
one sig Yes, No extends Flag {}

abstract sig Scope {}
one sig EventRead, EventManage, TaskRead, TaskManage, FinanceApprove extends Scope {}

abstract sig Visibility {}
one sig Public, Team, RoleOnly, AssignedOnly, Private extends Visibility {}

abstract sig RaciRole {}
one sig Responsible, Accountable, Consulted, Informed extends RaciRole {}

abstract sig ContractState {}
one sig ContractDraft, ContractConfirmed extends ContractState {}

abstract sig BookingState {}
one sig Tentative, Confirmed, Cancelled extends BookingState {}

sig Party {}
sig Currency {}
sig Reason {}
one sig NoReason extends Reason {}

one sig Clock { now: one Int }

sig Event {
  owners: some Party,
  coproducers: set Party
}

sig EventGrant {
  event: one Event,
  grantee: one Party,
  scopes: some Scope,
  validFrom: one Int,
  validUntil: lone Int
}

sig Invitation {
  event: one Event,
  invitee: one Party,
  invitedScopes: some Scope,
  expiresAt: one Int,
  acceptedAt: lone Int,
  convertedGrant: lone EventGrant
}

sig Task {
  event: one Event,
  dependsOn: set Task
}

sig RaciAssignment {
  task: one Task,
  party: one Party,
  role: one RaciRole
}

sig ProtectedObject {
  event: one Event,
  visibility: one Visibility,
  assignedTo: set Party
}

sig Contract {
  event: one Event,
  requiredParties: some Party,
  current: one ContractVersion,
  state: one ContractState
}

sig ContractVersion {
  contract: one Contract,
  ordinal: one Int
}

sig Acceptance {
  party: one Party,
  version: one ContractVersion
}

sig Resource {
  exclusive: one Flag
}

sig TimeWindow {
  startsAt: one Int,
  endsAt: one Int
}

sig BookingOverride {
  approvedBy: one Party,
  reason: one Reason
}

sig Booking {
  event: one Event,
  resource: one Resource,
  window: one TimeWindow,
  state: one BookingState,
  override: lone BookingOverride
}

sig Money {
  minorUnits: one Int,
  currency: one Currency
}

fun eventMembers[e: Event]: set Party {
  e.owners + e.coproducers + {p: Party | some g: EventGrant |
    g.event = e and g.grantee = p and
    lte[g.validFrom, Clock.now] and
    (no g.validUntil or lt[Clock.now, g.validUntil])}
}

pred windowsOverlap[left, right: TimeWindow] {
  lt[left.startsAt, right.endsAt]
  lt[right.startsAt, left.endsAt]
}

pred canRead[p: Party, object: ProtectedObject] {
  object.visibility = Public or
  (object.visibility = Team and p in eventMembers[object.event]) or
  (object.visibility = AssignedOnly and p in object.assignedTo) or
  (object.visibility = Private and p in object.event.owners) or
  (object.visibility = RoleOnly and some g: EventGrant |
    g.event = object.event and g.grantee = p and
    EventRead in g.scopes and lte[g.validFrom, Clock.now] and
    (no g.validUntil or lt[Clock.now, g.validUntil]))
}

fact OwnershipAndCoproduction {
  all e: Event | no e.owners & e.coproducers
}

fact GrantIntervalsAreValid {
  all g: EventGrant |
    no g.validUntil or lt[g.validFrom, g.validUntil]
}

fact InvitationConversionIsAttenuating {
  all i: Invitation |
    some i.convertedGrant implies {
      one i.acceptedAt
      lt[i.acceptedAt, i.expiresAt]
      i.convertedGrant.event = i.event
      i.convertedGrant.grantee = i.invitee
      i.convertedGrant.scopes in i.invitedScopes
    }
}

fact TaskGraphIsEventLocalAndAcyclic {
  all t: Task | t.dependsOn.event in t.event
  no t: Task | t in t.^dependsOn
}

fact RaciIsCompleteAndScoped {
  all t: Task | {
    one {a: RaciAssignment | a.task = t and a.role = Accountable}
    some {a: RaciAssignment | a.task = t and a.role = Responsible}
    {a: RaciAssignment | a.task = t}.party in eventMembers[t.event]
  }
}

fact ProtectedAssignmentsAreScoped {
  all o: ProtectedObject | o.assignedTo in eventMembers[o.event]
}

fact ContractVersionOwnership {
  all c: Contract | c.current.contract = c
  all disj left, right: ContractVersion |
    left.contract = right.contract implies left.ordinal != right.ordinal
}

fact ConfirmedContractsHaveMatchingConsent {
  all c: Contract |
    c.state = ContractConfirmed implies
      all p: c.requiredParties |
        one a: Acceptance | a.party = p and a.version = c.current
}

fact AcceptanceIsUniquePerPartyAndContract {
  all disj left, right: Acceptance |
    left.party = right.party and
    left.version.contract = right.version.contract implies
      left.version = right.version
}

fact TimeWindowsAreNonEmpty {
  all w: TimeWindow | lt[w.startsAt, w.endsAt]
}

fact OverridesAreAuthorizedAndJustified {
  all b: Booking |
    some b.override implies {
      b.override.approvedBy in b.event.owners
      b.override.reason != NoReason
    }
}

fact ExclusiveConfirmedBookingsDoNotSilentlyConflict {
  all disj left, right: Booking |
    left.state = Confirmed and right.state = Confirmed and
    left.resource = right.resource and left.resource.exclusive = Yes and
    windowsOverlap[left.window, right.window] implies
      some left.override + right.override
}

assert ExactlyOneAccountable {
  all t: Task |
    one {a: RaciAssignment | a.task = t and a.role = Accountable}
}

assert RequiredResponsibleIsPresent {
  all t: Task |
    some {a: RaciAssignment | a.task = t and a.role = Responsible}
}

assert DependenciesAreAcyclic {
  no t: Task | t in t.^dependsOn
}

assert ConfirmedContractUsesOneAcceptedVersion {
  all c: Contract |
    c.state = ContractConfirmed implies
      all p: c.requiredParties |
        one a: Acceptance | a.party = p and a.version = c.current
}

assert ExclusiveConflictHasAuthorizedOverride {
  all disj left, right: Booking |
    left.state = Confirmed and right.state = Confirmed and
    left.resource = right.resource and left.resource.exclusive = Yes and
    windowsOverlap[left.window, right.window] implies {
      some left.override + right.override
      (some left.override implies
        left.override.approvedBy in left.event.owners and
        left.override.reason != NoReason)
      (some right.override implies
        right.override.approvedBy in right.event.owners and
        right.override.reason != NoReason)
    }
}

assert InvitationCannotExpandScopes {
  all i: Invitation |
    some i.convertedGrant implies
      i.convertedGrant.scopes in i.invitedScopes
}

assert RestrictedReadRequiresEventRelationship {
  all p: Party, o: ProtectedObject |
    o.visibility != Public and canRead[p, o] implies
      p in eventMembers[o.event]
}

assert RaciAssignmentsAreNotOrphaned {
  all a: RaciAssignment | a.party in eventMembers[a.task.event]
}

pred EventOperationsScenario {
  some Event
  some Task.dependsOn
  some c: Contract | c.state = ContractConfirmed
  some b: Booking | b.state = Confirmed
  some i: Invitation | some i.convertedGrant
  some o: ProtectedObject | o.visibility != Public
}

run EventOperationsScenario for 6 but
  exactly 1 Event, exactly 5 Party, exactly 2 Task,
  exactly 1 Contract, exactly 2 ContractVersion,
  exactly 1 Resource, exactly 2 Booking, 5 Int

check ExactlyOneAccountable for 4 but
  exactly 1 Event, exactly 3 Party, exactly 2 Task,
  exactly 4 RaciAssignment, 4 Int
check RequiredResponsibleIsPresent for 4 but
  exactly 1 Event, exactly 3 Party, exactly 2 Task,
  exactly 4 RaciAssignment, 4 Int
check DependenciesAreAcyclic for 4 but
  exactly 1 Event, exactly 2 Task, 4 Int
check ConfirmedContractUsesOneAcceptedVersion for 4 but
  exactly 1 Event, exactly 3 Party, exactly 1 Contract,
  exactly 2 ContractVersion, exactly 2 Acceptance, 4 Int
check ExclusiveConflictHasAuthorizedOverride for 4 but
  exactly 1 Event, exactly 3 Party, exactly 1 Resource,
  exactly 2 Booking, exactly 2 TimeWindow, 4 Int
check InvitationCannotExpandScopes for 4 but
  exactly 1 Event, exactly 3 Party, exactly 1 Invitation,
  exactly 1 EventGrant, 4 Int
check RestrictedReadRequiresEventRelationship for 4 but
  exactly 1 Event, exactly 3 Party, exactly 1 ProtectedObject, 4 Int
check RaciAssignmentsAreNotOrphaned for 4 but
  exactly 1 Event, exactly 3 Party, exactly 2 Task,
  exactly 4 RaciAssignment, 4 Int
