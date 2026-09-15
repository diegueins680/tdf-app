---- MODULE EventLifecycle ----
EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANTS Owner, Approver, FinanceApprover, Outsider, CommandIds,
          UnsafeFinanceApproval, UnsafeAuditRewrite

States == {
  "draft", "planning", "pending_approval", "approved", "published",
  "staffing", "ready", "in_progress", "completed", "settlement_pending",
  "settled", "archived", "reprogrammed", "cancelled"
}

Actors == {Owner, Approver, FinanceApprover, Outsider}
PublicStates == {"published", "staffing", "ready", "in_progress", "completed"}

Allowed(from, to) ==
  CASE from = "draft"              -> to \in {"planning", "cancelled"}
    [] from = "planning"           -> to \in {"pending_approval", "cancelled"}
    [] from = "pending_approval"   -> to \in {"planning", "approved", "cancelled"}
    [] from = "approved"           -> to \in {"planning", "published", "cancelled"}
    [] from = "published"          -> to \in {"planning", "staffing", "reprogrammed", "cancelled"}
    [] from = "staffing"           -> to \in {"ready", "reprogrammed", "cancelled"}
    [] from = "ready"              -> to \in {"in_progress", "reprogrammed", "cancelled"}
    [] from = "in_progress"        -> to \in {"completed", "cancelled"}
    [] from = "completed"          -> to = "settlement_pending"
    [] from = "settlement_pending" -> to = "settled"
    [] from = "settled"            -> to = "archived"
    [] from = "reprogrammed"       -> to \in {"planning", "cancelled"}
    [] from = "cancelled"          -> to = "archived"
    [] OTHER                        -> FALSE

RequiredAuthority(actor, to) ==
  CASE to = "approved" -> actor = Approver
    [] to = "settled" -> actor = FinanceApprover
    [] OTHER -> actor = Owner

Authorized(actor, from, to) ==
  /\ actor \in Actors
  /\ IF UnsafeFinanceApproval /\ to = "settled"
        THEN actor \in {Approver, FinanceApprover}
        ELSE RequiredAuthority(actor, to)

ExpectedVisibility(state) == state \in PublicStates

VARIABLES state, visible, revision, seenCommands, audit

vars == <<state, visible, revision, seenCommands, audit>>

InitAt(initialState) ==
  /\ state = initialState
  /\ visible = ExpectedVisibility(initialState)
  /\ revision = 0
  /\ seenCommands = {}
  /\ audit = <<>>

Init == InitAt("draft")
\* Exercise authorization from every lifecycle boundary within two commands.
AnyStateInit == \E initialState \in States: InitAt(initialState)

Approve(actor, target, command) ==
  /\ actor \in Actors
  /\ target \in States
  /\ command \in CommandIds \ seenCommands
  /\ Allowed(state, target)
  /\ Authorized(actor, state, target)
  /\ state' = target
  /\ visible' = ExpectedVisibility(target)
  /\ revision' = revision + 1
  /\ seenCommands' = seenCommands \cup {command}
  /\ audit' = Append(audit,
       [command |-> command, actor |-> actor, from |-> state, to |-> target,
        accepted |-> TRUE])

Deny(actor, target, command) ==
  /\ actor \in Actors
  /\ target \in States
  /\ command \in CommandIds \ seenCommands
  /\ ~(Allowed(state, target) /\ Authorized(actor, state, target))
  /\ UNCHANGED <<state, visible, revision>>
  /\ seenCommands' = seenCommands \cup {command}
  /\ audit' = Append(audit,
       [command |-> command, actor |-> actor, from |-> state, to |-> target,
        accepted |-> FALSE])

RewriteAudit ==
  /\ UnsafeAuditRewrite
  /\ Len(audit) > 0
  /\ audit[1].actor # Outsider
  /\ audit' = [audit EXCEPT ![1].actor = Outsider]
  /\ UNCHANGED <<state, visible, revision, seenCommands>>

Next ==
  \/ RewriteAudit
  \/ \E actor \in Actors, target \in States, command \in CommandIds:
       Approve(actor, target, command)
  \/ \E actor \in Actors, target \in States, command \in CommandIds:
       Deny(actor, target, command)

TypeOK ==
  /\ state \in States
  /\ visible \in BOOLEAN
  /\ revision \in Nat
  /\ seenCommands \subseteq CommandIds
  /\ audit \in Seq([
       command: CommandIds, actor: Actors, from: States, to: States,
       accepted: BOOLEAN])

VisibilityMatchesLifecycle == visible = ExpectedVisibility(state)

AcceptedAuditIsAuthorized ==
  \A i \in 1..Len(audit):
    audit[i].accepted =>
      Allowed(audit[i].from, audit[i].to)
      /\ RequiredAuthority(audit[i].actor, audit[i].to)

RejectedAuditDidNotAdvance ==
  revision = Cardinality({i \in 1..Len(audit): audit[i].accepted})

OneAuditEntryPerCommand ==
  Cardinality({audit[i].command: i \in 1..Len(audit)}) = Len(audit)

AuditAppendOnly == []([
  /\ Len(audit') >= Len(audit)
  /\ \A i \in 1..Len(audit): audit'[i] = audit[i]
]_vars)

Spec == Init /\ [][Next]_vars
AllStatesSpec == AnyStateInit /\ [][Next]_vars

====
