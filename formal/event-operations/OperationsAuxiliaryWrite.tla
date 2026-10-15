-------------------- MODULE OperationsAuxiliaryWrite --------------------
(* Auxiliary Operations mutations (OPS-AUX-WRITE-001): notes, failure retry,
   saved views and push registration in tdf-hq/src/TDF/Operations/Server.hs
   (runOperationsWrite, lockOperationsScope, withLockedWorkItem,
   createNoteHandler, replayFailureHandler, createSavedViewHandler,
   createPushSubscriptionHandler).

   Two commands of one actor, each a note, a retry, a view or a push. A
   command takes its locks (session, grant, scope membership, and the work
   item or failure row), reads authority under them, then commits its result
   together with its audit row or is rejected without effect. For a note,
   "assigned" stands for the actor's assignment-dependent visibility of the
   work item (Teacher and Engineer see only their assignments; managers see
   every item), read under the item's lock. The environment may revoke the
   session, deactivate the grant, remove the membership, take that
   visibility away or resolve the failure; while a command holds its locks
   those changes wait.

   Guards, each removed by one negative configuration:
     LocksHeld           environment changes wait for a locked command
     AtomicAudit         a result commits only together with its audit
     SingleRetry         a failure is admitted to retrying once
     EncryptionRequired  a push token is stored only with a configured key *)
EXTENDS Naturals, FiniteSets

CONSTANTS LocksHeld, AtomicAudit, SingleRetry, EncryptionRequired

Commands == {1, 2}
Kinds == {"note", "retry", "view", "push"}

VARIABLES
  session,    \* TRUE while the actor's session is valid
  grant,      \* TRUE while the actor's role grant is active
  member,     \* TRUE while the actor's scope membership is active
  assigned,   \* TRUE while the actor's assignment-dependent visibility of the item holds
  failure,    \* open | retrying | resolved
  encryption, \* TRUE while the push key is configured
  cmd,        \* [Commands -> [kind, phase, auth, result, audit]]; auth is the authority read under the locks
  badCommit,  \* a command committed without current authority
  plainPush   \* a push token was stored without encryption

vars == <<session, grant, member, assigned, failure, encryption, cmd, badCommit, plainPush>>

TypeOK ==
  /\ session \in BOOLEAN /\ grant \in BOOLEAN /\ member \in BOOLEAN
  /\ assigned \in BOOLEAN /\ encryption \in BOOLEAN
  /\ failure \in {"open", "retrying", "resolved"}
  /\ \A c \in Commands :
       /\ cmd[c].kind \in Kinds
       /\ cmd[c].phase \in {"idle", "locked", "done"}
       /\ cmd[c].auth \in BOOLEAN
       /\ cmd[c].result \in BOOLEAN /\ cmd[c].audit \in BOOLEAN
  /\ badCommit \in BOOLEAN /\ plainPush \in BOOLEAN

Init ==
  /\ session = TRUE /\ grant = TRUE /\ member = TRUE /\ assigned = TRUE
  /\ failure = "open"
  /\ encryption \in BOOLEAN
  /\ cmd \in [Commands -> [kind : Kinds, phase : {"idle"}, auth : {FALSE}, result : {FALSE}, audit : {FALSE}]]
  /\ badCommit = FALSE /\ plainPush = FALSE

Locked == \E c \in Commands : cmd[c].phase = "locked"

(* Environment changes wait while a command holds its locks. *)
EnvFree == ~LocksHeld \/ ~Locked

RevokeSession == EnvFree /\ session /\ session' = FALSE
  /\ UNCHANGED <<grant, member, assigned, failure, encryption, cmd, badCommit, plainPush>>
DeactivateGrant == EnvFree /\ grant /\ grant' = FALSE
  /\ UNCHANGED <<session, member, assigned, failure, encryption, cmd, badCommit, plainPush>>
RemoveMember == EnvFree /\ member /\ member' = FALSE
  /\ UNCHANGED <<session, grant, assigned, failure, encryption, cmd, badCommit, plainPush>>
Unassign == EnvFree /\ assigned /\ assigned' = FALSE
  /\ UNCHANGED <<session, grant, member, failure, encryption, cmd, badCommit, plainPush>>
Resolve == EnvFree /\ failure # "resolved" /\ failure' = "resolved"
  /\ UNCHANGED <<session, grant, member, assigned, encryption, cmd, badCommit, plainPush>>

(* Authority of command c in the current state. *)
Authorized(c) ==
  /\ session /\ grant /\ member
  /\ cmd[c].kind = "note" => assigned
  /\ cmd[c].kind = "retry" => (failure = "open" \/ (~SingleRetry /\ failure = "retrying"))
  /\ cmd[c].kind = "push" => (encryption \/ ~EncryptionRequired)

(* The locks are taken and authority is read once, under them. *)
Lock(c) ==
  /\ cmd[c].phase = "idle" /\ ~Locked
  /\ cmd' = [cmd EXCEPT ![c].phase = "locked", ![c].auth = Authorized(c)]
  /\ UNCHANGED <<session, grant, member, assigned, failure, encryption, badCommit, plainPush>>

(* The decision uses the authority read under the locks; with LocksHeld that
   is still the current state, without it the environment may have moved. *)
Commit(c) ==
  /\ cmd[c].phase = "locked" /\ cmd[c].auth
  /\ \E audited \in {TRUE, FALSE} :
       /\ AtomicAudit => audited
       /\ cmd' = [cmd EXCEPT ![c].phase = "done", ![c].result = TRUE, ![c].audit = audited]
  /\ failure' = IF cmd[c].kind = "retry" THEN "retrying" ELSE failure
  /\ badCommit' = (badCommit \/ ~(session /\ grant /\ member
       /\ (cmd[c].kind = "note" => assigned)
       /\ (cmd[c].kind = "retry" => failure = "open")))
  /\ plainPush' = (plainPush \/ (cmd[c].kind = "push" /\ ~encryption))
  /\ UNCHANGED <<session, grant, member, assigned, encryption>>

Reject(c) ==
  /\ cmd[c].phase = "locked" /\ ~cmd[c].auth
  /\ cmd' = [cmd EXCEPT ![c].phase = "done"]
  /\ UNCHANGED <<session, grant, member, assigned, failure, encryption, badCommit, plainPush>>

Finished == (\A c \in Commands : cmd[c].phase = "done") /\ UNCHANGED vars

Next ==
  \/ RevokeSession \/ DeactivateGrant \/ RemoveMember \/ Unassign \/ Resolve
  \/ \E c \in Commands : Lock(c) \/ Commit(c) \/ Reject(c)
  \/ Finished

Spec == Init /\ [][Next]_vars

(* Isolate failure resolution from session/grant changes in its negative control. *)
ResolutionNext == Resolve \/ (\E c \in Commands : Lock(c) \/ Commit(c) \/ Reject(c))
                  \/ Finished
ResolutionSpec == Init /\ [][ResolutionNext]_vars

(* A command commits only with a valid session, an active grant, an active
   membership and, for a note, the current assignment-dependent visibility;
   a retry also requires a currently eligible failure, not a stale snapshot. *)
CurrentAuthority == ~badCommit

(* A result exists exactly when its audit row does. *)
AtomicEvidence == \A c \in Commands : cmd[c].result = cmd[c].audit

(* At most one retry is admitted for the failure. *)
OneRetry == Cardinality({c \in Commands : cmd[c].kind = "retry" /\ cmd[c].result}) <= 1

(* No push token is stored without the encryption key. *)
EncryptedPush == ~plainPush

=============================================================================
