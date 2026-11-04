---- MODULE EventAgreement ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS BreakRecipient, BreakVersion, BreakFrozen, BreakAnnex, BreakDuplicate
VARIABLES version, planVersion, contentPlan, ready, approved, sent, state,
          acceptedVersion, acceptedPlan, acceptedActor, acceptanceCount
vars == <<version, planVersion, contentPlan, ready, approved, sent, state,
          acceptedVersion, acceptedPlan, acceptedActor, acceptanceCount>>
Init == /\ version = 1 /\ planVersion = 1 /\ contentPlan = 1
        /\ ready = FALSE /\ approved = 0 /\ sent = {} /\ state = "draft"
        /\ acceptedVersion = 0 /\ acceptedPlan = 0
        /\ acceptedActor = "none" /\ acceptanceCount = 0
Complete == /\ ~ready /\ state = "draft" /\ ready' = TRUE
            /\ UNCHANGED <<version,planVersion,contentPlan,approved,sent,state,
                           acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
Approve == /\ ready /\ state = "draft" /\ approved' = version
           /\ UNCHANGED <<version,planVersion,contentPlan,ready,sent,state,
                          acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
Send == /\ ready /\ state = "draft" /\ approved = version
        /\ sent' = sent \cup {version} /\ state' = "sent"
        /\ UNCHANGED <<version,planVersion,contentPlan,ready,approved,
                       acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
RequestChanges == /\ state = "sent" /\ state' = "changes_requested"
                  /\ UNCHANGED <<version,planVersion,contentPlan,ready,approved,sent,
                                 acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
Revise == /\ version < 2
          /\ (state \in {"draft","changes_requested","withdrawn"} \/ (BreakFrozen /\ state="accepted"))
          /\ version' = version+1 /\ contentPlan' = planVersion
          /\ approved' = 0 /\ ready' = FALSE /\ state' = "draft"
          /\ UNCHANGED <<planVersion,sent,acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
EditPlan == /\ planVersion < 2 /\ planVersion' = planVersion+1
            /\ acceptedPlan' = IF BreakAnnex /\ acceptedVersion>0 THEN planVersion+1 ELSE acceptedPlan
            /\ UNCHANGED <<version,contentPlan,ready,approved,sent,state,acceptedVersion,acceptedActor,acceptanceCount>>
Accept(actor,v) ==
 /\ actor \in {"owner","recipient","stranger"}
 /\ (actor="recipient" \/ BreakRecipient)
 /\ v \in sent /\ (v=version \/ BreakVersion)
 /\ (state="sent" \/ (BreakDuplicate /\ state="accepted" /\ acceptanceCount<2))
 /\ approved=version /\ ready
 /\ acceptedVersion'=v /\ acceptedPlan'=contentPlan
 /\ acceptedActor'=actor /\ acceptanceCount'=acceptanceCount+1 /\ state'="accepted"
 /\ UNCHANGED <<version,planVersion,contentPlan,ready,approved,sent>>
Withdraw == /\ state \in {"draft","sent","changes_requested"} /\ state'="withdrawn"
            /\ UNCHANGED <<version,planVersion,contentPlan,ready,approved,sent,
                           acceptedVersion,acceptedPlan,acceptedActor,acceptanceCount>>
Next == Complete \/ Approve \/ Send \/ RequestChanges \/ Revise \/ EditPlan \/ Withdraw
        \/ (\E actor \in {"owner","recipient","stranger"}, v \in 1..2: Accept(actor,v))
Spec == Init /\ [][Next]_vars
FrozenEvidence == acceptedVersion>0 =>
 /\ acceptedVersion=version /\ acceptedPlan=contentPlan
 /\ acceptedActor="recipient" /\ acceptanceCount=1 /\ state="accepted"
ApprovalBeforeAcceptance == acceptedVersion>0 => approved=acceptedVersion /\ ready /\ acceptedVersion \in sent
====
