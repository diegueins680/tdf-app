-------------------------- MODULE OperationsApproval --------------------------
EXTENDS Naturals
CONSTANTS UnboundReplay, DuplicateAudit, Reopen, IgnoreExpiry, SelfApprove
VARIABLES retained, decision, audit, now, badBinding, badDecision, attempts
vars == <<retained,decision,audit,now,badBinding,badDecision,attempts>>
Requests == [actor:{1,2}, branch:{1,2}, payload:{1,2}]
Init == /\ retained=[actor |-> 0,branch |-> 0,payload |-> 0] /\ decision="absent" /\ audit=0 /\ now=0
        /\ badBinding=FALSE /\ badDecision=FALSE /\ attempts=0
Create(r) == /\ decision="absent" /\ retained'=r /\ decision'="pending"
             /\ audit'=1 /\ UNCHANGED <<now,badBinding,badDecision,attempts>>
Replay(r) == /\ decision#"absent" /\ attempts<2
             /\ (r=retained \/ UnboundReplay)
             /\ attempts'=attempts+1
             /\ badBinding'=(badBinding \/ r#retained)
             /\ audit'=audit + (IF DuplicateAudit THEN 1 ELSE 0)
             /\ UNCHANGED <<retained,decision,now,badDecision>>
Decide(a,d) == /\ decision#"absent" /\ attempts<2
               /\ (decision="pending" \/ Reopen)
               /\ (now<1 \/ IgnoreExpiry)
               /\ (a#retained.actor \/ SelfApprove)
               /\ badDecision'=(badDecision \/ decision#"pending" \/ now>=1 \/ a=retained.actor)
               /\ decision'=d /\ audit'=audit+1 /\ attempts'=attempts+1
               /\ UNCHANGED <<retained,now,badBinding>>
Tick == /\ now=0 /\ now'=1
        /\ UNCHANGED <<retained,decision,audit,badBinding,badDecision,attempts>>
Next == (\E r \in Requests : Create(r) \/ Replay(r))
        \/ (\E a \in {1,2},d \in {"approved","rejected"} : Decide(a,d)) \/ Tick
BoundReplay == ~badBinding
DecisionGuard == ~badDecision
SingleEvidence == audit=(IF decision="absent" THEN 0 ELSE IF decision="pending" THEN 1 ELSE 2)
Spec == Init /\ [][Next]_vars
=============================================================================
