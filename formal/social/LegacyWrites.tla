------------------------- MODULE LegacyWrites -------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS UnsafeLocks, UnsafePause, UnsafePair
VARIABLES enabled, activated, pair, closed, token, phase, attempt, operation,
          history, decision, observed
vars == <<enabled,activated,pair,closed,token,phase,attempt,operation,
          history,decision,observed>>
RuntimeHeld == phase \in {"runtime","accounts","checked"}
AccountsHeld == phase \in {"accounts","checked"}
Required == activated \/ pair \/ closed
Gate == ~(IF UnsafePause THEN enabled ELSE activated) /\ (~pair \/ UnsafePair) /\ ~closed
Init == /\ enabled=FALSE /\ activated=FALSE /\ pair=FALSE /\ closed=FALSE /\ token=TRUE
 /\ phase="start" /\ attempt=1 /\ operation \in {"add","vcard","remove"}
 /\ history \in SUBSET {1,2} /\ decision=FALSE
 /\ observed=[success |-> FALSE, permitted |-> TRUE, before |-> {}, after |-> {}, op |-> "add"]
Activate == /\ ~enabled /\ (~RuntimeHeld \/ UnsafeLocks)
 /\ enabled'=TRUE /\ activated'=TRUE
 /\ UNCHANGED <<pair,closed,token,phase,attempt,operation,history,decision,observed>>
Pause == /\ enabled /\ (~RuntimeHeld \/ UnsafeLocks) /\ enabled'=FALSE
 /\ UNCHANGED <<activated,pair,closed,token,phase,attempt,operation,history,decision,observed>>
Govern == /\ ~pair /\ (~AccountsHeld \/ UnsafeLocks) /\ pair'=TRUE
 /\ UNCHANGED <<enabled,activated,closed,token,phase,attempt,operation,history,decision,observed>>
Close == /\ ~closed /\ (~AccountsHeld \/ UnsafeLocks) /\ closed'=TRUE
 /\ UNCHANGED <<enabled,activated,pair,token,phase,attempt,operation,history,decision,observed>>
Revoke == /\ token /\ (~AccountsHeld \/ UnsafeLocks) /\ token'=FALSE
 /\ UNCHANGED <<enabled,activated,pair,closed,phase,attempt,operation,history,decision,observed>>
Runtime == /\ phase="start" /\ phase'="runtime"
 /\ UNCHANGED <<enabled,activated,pair,closed,token,attempt,operation,history,decision,observed>>
Accounts == /\ phase="runtime" /\ phase'="accounts"
 /\ UNCHANGED <<enabled,activated,pair,closed,token,attempt,operation,history,decision,observed>>
Check == /\ phase="accounts" /\ phase'="checked" /\ decision'=(Gate /\ token)
 /\ UNCHANGED <<enabled,activated,pair,closed,token,attempt,operation,history,observed>>
Commit == /\ phase="checked" /\ phase'="done"
 /\ history'=(IF decision THEN (IF operation="remove" THEN {} ELSE {1,2}) ELSE history)
 /\ observed'=[success |-> decision, permitted |-> (~Required /\ token),
                before |-> history, after |-> history', op |-> operation]
 /\ UNCHANGED <<enabled,activated,pair,closed,token,attempt,operation,decision>>
Retry == /\ phase="done" /\ attempt=1 /\ attempt'=2 /\ phase'="start"
 /\ UNCHANGED <<enabled,activated,pair,closed,token,operation,history,decision,observed>>
Next == Activate \/ Pause \/ Govern \/ Close \/ Revoke \/ Runtime \/ Accounts \/ Check \/ Commit \/ Retry
Spec == Init /\ [][Next]_vars /\ WF_vars(Runtime) /\ WF_vars(Accounts)
 /\ WF_vars(Check) /\ WF_vars(Commit) /\ WF_vars(Retry)
AuthorizedEffect == observed.success => observed.permitted
DeniedPreservesHistory == ~observed.success => observed.after=observed.before
LegacyEffect == observed.success => observed.after=(IF observed.op="remove" THEN {} ELSE {1,2})
Progress == <>(attempt=2 /\ phase="done")
=============================================================================
