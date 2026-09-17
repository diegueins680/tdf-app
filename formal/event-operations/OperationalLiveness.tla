---- MODULE OperationalLiveness ----
EXTENDS Naturals, TLC

CONSTANTS Horizon, HoldExpiry, MaxNotificationAttempts

VARIABLES now, hold, notification, attempts, offline, work, finance

vars == <<now, hold, notification, attempts, offline, work, finance>>

Init ==
  /\ now = 0
  /\ hold = "held"
  /\ notification = "pending"
  /\ attempts = 0
  /\ offline = "queued"
  /\ work = "confirmed"
  /\ finance = "pending"

Tick ==
  /\ now < Horizon
  /\ now' = now + 1
  /\ UNCHANGED <<hold, notification, attempts, offline, work, finance>>

ExpireHold ==
  /\ hold = "held"
  /\ now >= HoldExpiry
  /\ hold' = "expired"
  /\ UNCHANGED <<now, notification, attempts, offline, work, finance>>

TryNotification ==
  /\ notification = "pending"
  /\ attempts < MaxNotificationAttempts
  /\ attempts' = attempts + 1
  /\ UNCHANGED <<now, hold, notification, offline, work, finance>>

DeadLetterNotification ==
  /\ notification = "pending"
  /\ attempts = MaxNotificationAttempts
  /\ notification' = "dead_letter"
  /\ UNCHANGED <<now, hold, attempts, offline, work, finance>>

SynchronizeOffline ==
  /\ offline = "queued"
  /\ offline' \in {"synchronized", "conflict"}
  /\ UNCHANGED <<now, hold, notification, attempts, work, finance>>

ResolveWork ==
  /\ work = "confirmed"
  /\ work' \in {"completed", "cancelled", "disputed", "attention"}
  /\ UNCHANGED <<now, hold, notification, attempts, offline, finance>>

ResolveFinance ==
  /\ finance = "pending"
  /\ finance' \in {"reconciled", "failed", "alerted"}
  /\ UNCHANGED <<now, hold, notification, attempts, offline, work>>

Next ==
  \/ Tick
  \/ ExpireHold
  \/ TryNotification
  \/ DeadLetterNotification
  \/ SynchronizeOffline
  \/ ResolveWork
  \/ ResolveFinance

TypeOK ==
  /\ now \in 0..Horizon
  /\ hold \in {"held", "expired"}
  /\ notification \in {"pending", "dead_letter"}
  /\ attempts \in 0..MaxNotificationAttempts
  /\ offline \in {"queued", "synchronized", "conflict"}
  /\ work \in {"confirmed", "completed", "cancelled", "disputed", "attention"}
  /\ finance \in {"pending", "reconciled", "failed", "alerted"}

HoldTerminates == <> (hold = "expired")
NotificationTerminates == <> (notification = "dead_letter")
OfflineTerminates == <> (offline \in {"synchronized", "conflict"})
WorkTerminates == <> (work \in {"completed", "cancelled", "disputed", "attention"})
FinanceTerminates == <> (finance \in {"reconciled", "failed", "alerted"})

Spec ==
  /\ Init
  /\ [][Next]_vars
  /\ WF_vars(Tick)
  /\ WF_vars(ExpireHold)
  /\ WF_vars(TryNotification)
  /\ WF_vars(DeadLetterNotification)
  /\ WF_vars(SynchronizeOffline)
  /\ WF_vars(ResolveWork)
  /\ WF_vars(ResolveFinance)

====
