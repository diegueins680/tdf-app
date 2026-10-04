---- MODULE InvitationSafety ----
EXTENDS FiniteSets, Naturals, Sequences, TLC

CONSTANTS Owner, Guest, Outsider, ReadScope, MutateScope, CommandIds,
          Expiry, Horizon

Actors == {Owner, Guest, Outsider}
InvitationScopes == {ReadScope}

VARIABLES now, status, acceptedBy, guestGrant, seenCommands, audit

vars == <<now, status, acceptedBy, guestGrant, seenCommands, audit>>

Init ==
  /\ now = 0
  /\ status = "issued"
  /\ acceptedBy = "none"
  /\ guestGrant = {}
  /\ seenCommands = {}
  /\ audit = <<>>

Tick ==
  /\ now < Horizon
  /\ now' = now + 1
  /\ UNCHANGED <<status, acceptedBy, guestGrant, seenCommands, audit>>

Accept(actor, command) ==
  /\ actor \in Actors
  /\ command \in CommandIds \ seenCommands
  /\ status = "issued"
  /\ now < Expiry
  /\ actor = Guest
  /\ status' = "accepted"
  /\ acceptedBy' = actor
  /\ guestGrant' = InvitationScopes
  /\ seenCommands' = seenCommands \cup {command}
  /\ audit' = Append(audit,
       [command |-> command, actor |-> actor, at |-> now,
        result |-> "accepted"])
  /\ UNCHANGED now

Revoke(command) ==
  /\ command \in CommandIds \ seenCommands
  /\ status \in {"issued", "accepted"}
  /\ status' = "revoked"
  /\ acceptedBy' = "none"
  /\ guestGrant' = {}
  /\ seenCommands' = seenCommands \cup {command}
  /\ audit' = Append(audit,
       [command |-> command, actor |-> Owner, at |-> now,
        result |-> "revoked"])
  /\ UNCHANGED now

Expire ==
  /\ status = "issued"
  /\ now >= Expiry
  /\ status' = "expired"
  /\ UNCHANGED <<now, acceptedBy, guestGrant, seenCommands, audit>>

Deny(actor, command) ==
  /\ actor \in Actors
  /\ command \in CommandIds \ seenCommands
  /\ ~(status = "issued" /\ now < Expiry /\ actor = Guest)
  /\ seenCommands' = seenCommands \cup {command}
  /\ audit' = Append(audit,
       [command |-> command, actor |-> actor, at |-> now,
        result |-> "denied"])
  /\ UNCHANGED <<now, status, acceptedBy, guestGrant>>

Next ==
  \/ Tick
  \/ Expire
  \/ \E command \in CommandIds: Revoke(command)
  \/ \E actor \in Actors, command \in CommandIds: Accept(actor, command)
  \/ \E actor \in Actors, command \in CommandIds: Deny(actor, command)

TypeOK ==
  /\ now \in 0..Horizon
  /\ status \in {"issued", "accepted", "revoked", "expired"}
  /\ acceptedBy \in Actors \cup {"none"}
  /\ guestGrant \subseteq {ReadScope, MutateScope}
  /\ seenCommands \subseteq CommandIds
  /\ audit \in Seq([
       command: CommandIds, actor: Actors, at: 0..Horizon,
       result: {"accepted", "revoked", "denied"}])

AcceptedOnlyByIntendedGuest ==
  status = "accepted" => acceptedBy = Guest

AcceptanceWasFresh ==
  \A i \in 1..Len(audit):
    audit[i].result = "accepted" =>
      audit[i].actor = Guest /\ audit[i].at < Expiry

NoInvitationPrivilegeEscalation == guestGrant \subseteq InvitationScopes

NoGrantAfterRevocationOrExpiry ==
  status \in {"revoked", "expired"} => guestGrant = {}

OneDecisionPerCommand ==
  Cardinality({audit[i].command: i \in 1..Len(audit)}) = Len(audit)

Spec == Init /\ [][Next]_vars

====
