---- MODULE ContractPayment ----
EXTENDS FiniteSets, Naturals, TLC

CONSTANTS Owner, Worker, FinanceApprover, MaxVersion, PayoutCommands

RequiredParties == {Owner, Worker}

VARIABLES version, acceptedVersion, contractState, milestoneState,
          payoutState, seenPayoutCommands, payoutEffects

vars == <<version, acceptedVersion, contractState, milestoneState,
          payoutState, seenPayoutCommands, payoutEffects>>

Init ==
  /\ version = 1
  /\ acceptedVersion = [party \in RequiredParties |-> 0]
  /\ contractState = "draft"
  /\ milestoneState = "pending"
  /\ payoutState = "none"
  /\ seenPayoutCommands = {}
  /\ payoutEffects = {}

Amend ==
  /\ version < MaxVersion
  /\ payoutState = "none"
  /\ version' = version + 1
  /\ acceptedVersion' = [party \in RequiredParties |-> 0]
  /\ contractState' = "draft"
  /\ milestoneState' = "pending"
  /\ UNCHANGED <<payoutState, seenPayoutCommands, payoutEffects>>

Accept(party) ==
  /\ party \in RequiredParties
  /\ contractState = "draft"
  /\ acceptedVersion' = [acceptedVersion EXCEPT ![party] = version]
  /\ UNCHANGED <<version, contractState, milestoneState, payoutState,
                  seenPayoutCommands, payoutEffects>>

Confirm ==
  /\ contractState = "draft"
  /\ \A party \in RequiredParties: acceptedVersion[party] = version
  /\ contractState' = "confirmed"
  /\ UNCHANGED <<version, acceptedVersion, milestoneState, payoutState,
                  seenPayoutCommands, payoutEffects>>

ApproveMilestone ==
  /\ contractState = "confirmed"
  /\ milestoneState = "pending"
  /\ milestoneState' = "approved"
  /\ UNCHANGED <<version, acceptedVersion, contractState, payoutState,
                  seenPayoutCommands, payoutEffects>>

RequestPayout(command) ==
  /\ command \in PayoutCommands \ seenPayoutCommands
  /\ payoutState = "none"
  /\ contractState = "confirmed"
  /\ milestoneState = "approved"
  /\ payoutState' = "pending"
  /\ seenPayoutCommands' = seenPayoutCommands \cup {command}
  /\ UNCHANGED <<version, acceptedVersion, contractState, milestoneState,
                  payoutEffects>>

ReleasePayout(command, actor) ==
  /\ command \in seenPayoutCommands
  /\ command \notin payoutEffects
  /\ actor = FinanceApprover
  /\ payoutState = "pending"
  /\ contractState = "confirmed"
  /\ milestoneState = "approved"
  /\ payoutState' = "released"
  /\ payoutEffects' = payoutEffects \cup {command}
  /\ UNCHANGED <<version, acceptedVersion, contractState, milestoneState,
                  seenPayoutCommands>>

Next ==
  \/ Amend
  \/ \E party \in RequiredParties: Accept(party)
  \/ Confirm
  \/ ApproveMilestone
  \/ \E command \in PayoutCommands: RequestPayout(command)
  \/ \E command \in PayoutCommands,
       actor \in RequiredParties \cup {FinanceApprover}:
       ReleasePayout(command, actor)

TypeOK ==
  /\ version \in 1..MaxVersion
  /\ acceptedVersion \in [RequiredParties -> 0..MaxVersion]
  /\ contractState \in {"draft", "confirmed"}
  /\ milestoneState \in {"pending", "approved"}
  /\ payoutState \in {"none", "pending", "released"}
  /\ seenPayoutCommands \subseteq PayoutCommands
  /\ payoutEffects \subseteq PayoutCommands

ConfirmedVersionAcceptedByAll ==
  contractState = "confirmed" =>
    \A party \in RequiredParties: acceptedVersion[party] = version

PayoutRequiresConfirmedApprovedWork ==
  payoutState \in {"pending", "released"} =>
    contractState = "confirmed" /\ milestoneState = "approved"

NoDuplicatePayoutEffect == Cardinality(payoutEffects) <= 1

EffectHasIdempotencyRecord == payoutEffects \subseteq seenPayoutCommands

Spec == Init /\ [][Next]_vars

====
