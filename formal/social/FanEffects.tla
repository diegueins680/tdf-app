-------------------------- MODULE FanEffects --------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS UnsafeLocks, UnsafePause, UnsafeGoverned, UnsafeRetry
VARIABLES enabled, activated, governed, eligible, token, phase, attempt, subscription,
          club, profile, edges, alerts, mode, accepted, observed
vars == <<enabled,activated,governed,eligible,token,phase,attempt,subscription,
          club,profile,edges,alerts,mode,accepted,observed>>
RuntimeHeld == phase \in {"locked","checked"}
TokenHeld == phase="checked"
Legacy == ~activated /\ ~governed
Gate == ~(IF UnsafePause THEN enabled ELSE activated) /\ (~governed \/ UnsafeGoverned)
AllEdges == {1,2,3,4}
Init == /\ enabled=FALSE /\ activated=FALSE /\ governed \in BOOLEAN /\ eligible=TRUE /\ token=TRUE
 /\ phase="start" /\ attempt=1 /\ subscription \in BOOLEAN /\ club \in BOOLEAN
 /\ profile \in BOOLEAN /\ (profile => club)
 /\ edges \in {{},{1,2},{3,4},AllEdges} /\ alerts=0 /\ mode=FALSE /\ accepted=FALSE
 /\ observed=[changed |-> FALSE, legacy |-> TRUE, success |-> FALSE, permitted |-> TRUE,
      subscribed |-> FALSE, afterSubscription |-> FALSE, beforeAlerts |-> 0, afterAlerts |-> 0]
Activate == /\ ~enabled /\ (~RuntimeHeld \/ UnsafeLocks) /\ enabled'=TRUE /\ activated'=TRUE
 /\ UNCHANGED <<governed,eligible,token,phase,attempt,subscription,club,profile,edges,alerts,mode,accepted,observed>>
Pause == /\ enabled /\ (~RuntimeHeld \/ UnsafeLocks) /\ enabled'=FALSE
 /\ UNCHANGED <<activated,governed,eligible,token,phase,attempt,subscription,club,profile,edges,alerts,mode,accepted,observed>>
Govern == /\ enabled /\ ~governed /\ governed'=TRUE
 /\ UNCHANGED <<enabled,activated,eligible,token,phase,attempt,subscription,club,profile,edges,alerts,mode,accepted,observed>>
DenyAccess == /\ ~Legacy /\ eligible /\ (~TokenHeld \/ UnsafeLocks) /\ eligible'=FALSE
 /\ UNCHANGED <<enabled,activated,governed,token,phase,attempt,subscription,club,profile,edges,alerts,mode,accepted,observed>>
Revoke == /\ token /\ (~TokenHeld \/ UnsafeLocks) /\ token'=FALSE
 /\ UNCHANGED <<enabled,activated,governed,eligible,phase,attempt,subscription,club,profile,edges,alerts,mode,accepted,observed>>
Lock == /\ phase="start" /\ phase'="locked" /\ mode'=Gate
 /\ UNCHANGED <<enabled,activated,governed,eligible,token,attempt,subscription,club,profile,edges,alerts,accepted,observed>>
Check == /\ phase="locked" /\ phase'="checked" /\ accepted'=(token /\ eligible)
 /\ UNCHANGED <<enabled,activated,governed,eligible,token,attempt,subscription,club,profile,edges,alerts,mode,observed>>
Commit == /\ phase="checked" /\ phase'="done"
 /\ subscription'=(subscription \/ accepted)
 /\ profile'=(profile \/ (accepted /\ mode /\ club))
 /\ edges'=(IF accepted /\ mode /\ club THEN AllEdges ELSE edges)
 /\ alerts'=(IF accepted /\ mode /\ (~subscription \/ UnsafeRetry) THEN alerts+1 ELSE alerts)
 /\ observed'=[changed |-> (profile' # profile \/ edges' # edges \/ alerts' # alerts),
      legacy |-> Legacy, success |-> accepted, permitted |-> (token /\ eligible), subscribed |-> subscription,
      afterSubscription |-> subscription', beforeAlerts |-> alerts, afterAlerts |-> alerts']
 /\ UNCHANGED <<enabled,activated,governed,eligible,token,attempt,club,mode,accepted>>
Retry == /\ phase="done" /\ attempt=1 /\ phase'="start" /\ attempt'=2
 /\ UNCHANGED <<enabled,activated,governed,eligible,token,subscription,club,profile,edges,alerts,mode,accepted,observed>>
Unfollow == /\ token /\ ~TokenHeld /\ subscription /\ subscription'=FALSE
 /\ UNCHANGED <<enabled,activated,governed,eligible,token,phase,attempt,club,profile,edges,alerts,mode,accepted,observed>>
Next == Unfollow \/ Activate \/ Pause \/ Govern \/ DenyAccess \/ Revoke \/ Lock \/ Check \/ Commit \/ Retry
Spec == Init /\ [][Next]_vars /\ WF_vars(Lock) /\ WF_vars(Check) /\ WF_vars(Commit) /\ WF_vars(Retry)
NoImplicitEffects == observed.changed => (observed.legacy /\ observed.permitted)
CurrentSession == observed.success => observed.permitted
SubscriptionPreserved == observed.success => observed.afterSubscription
NoDuplicateAlerts == observed.subscribed => observed.beforeAlerts=observed.afterAlerts
Progress == <>(attempt=2 /\ phase="done")
=============================================================================
