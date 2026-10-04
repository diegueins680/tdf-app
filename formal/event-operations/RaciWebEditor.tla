---- MODULE RaciWebEditor ----
EXTENDS Naturals, TLC
CONSTANTS RequireConfirm, CheckContext, SingleFlight, ExactRetry, ValidateReceipt
VARIABLES generation, phase, eligible, revision, frozen, attempts, pending,
          confirmed, visible, badConsent, badContext, badRetry, badReceipt
None == "none"
vars == <<generation,phase,eligible,revision,frozen,attempts,pending,confirmed,
          visible,badConsent,badContext,badRetry,badReceipt>>
Init == /\ generation=0 /\ phase="idle" /\ eligible \in BOOLEAN /\ revision \in 1..2
        /\ frozen=None /\ attempts=0 /\ pending=0 /\ confirmed=FALSE /\ visible=None
        /\ badConsent=FALSE /\ badContext=FALSE /\ badRetry=FALSE /\ badReceipt=FALSE
Review == /\ phase="idle" /\ eligible /\ phase'="review"
          /\ frozen' = <<generation,revision>> /\ confirmed'=FALSE
          /\ UNCHANGED <<generation,eligible,revision,attempts,pending,visible,
                         badConsent,badContext,badRetry,badReceipt>>
Confirm == /\ phase="review" /\ confirmed'=TRUE
           /\ UNCHANGED <<generation,phase,eligible,revision,frozen,attempts,pending,
                          visible,badConsent,badContext,badRetry,badReceipt>>
Send == /\ frozen # None /\ attempts < 2
        /\ (phase \in {"review","uncertain"} \/ (~SingleFlight /\ phase="sending"))
        /\ (~RequireConfirm \/ confirmed) /\ (~CheckContext \/ frozen[1]=generation)
        /\ (~SingleFlight \/ pending=0)
        /\ phase'="sending" /\ pending'=pending+1 /\ attempts'=attempts+1
        /\ badConsent'=(badConsent \/ ~confirmed)
        /\ badContext'=(badContext \/ frozen[1] # generation)
        /\ LET outgoing == IF ExactRetry THEN frozen ELSE <<generation,3-revision>>
           IN badRetry'=(badRetry \/ (attempts>0 /\ outgoing # frozen))
        /\ UNCHANGED <<generation,eligible,revision,frozen,confirmed,visible,badReceipt>>
Finish(valid) ==
  /\ pending>0 /\ pending'=pending-1
  /\ IF ~CheckContext \/ frozen[1]=generation THEN
       /\ phase'=IF valid \/ ~ValidateReceipt THEN "success" ELSE "uncertain"
       /\ visible'=IF valid \/ ~ValidateReceipt THEN <<frozen[1]>> ELSE None
       /\ badReceipt'=(badReceipt \/ (~valid /\ ~ValidateReceipt))
     ELSE UNCHANGED <<phase,visible,badReceipt>>
  /\ UNCHANGED <<generation,eligible,revision,frozen,attempts,confirmed,badConsent,badContext,badRetry>>
Change == /\ generation<2 /\ generation'=generation+1 /\ visible'=None
          /\ UNCHANGED <<phase,eligible,revision,frozen,attempts,pending,confirmed,
                         badConsent,badContext,badRetry,badReceipt>>
Next == Review \/ Confirm \/ Send \/ Change \/ (\E valid \in BOOLEAN: Finish(valid))
TypeOK == /\ generation \in 0..2 /\ phase \in {"idle","review","sending","success","uncertain"}
          /\ attempts \in 0..2 /\ pending \in 0..2 /\ eligible \in BOOLEAN /\ revision \in 1..2
ExplicitConfirmation == ~badConsent
CurrentEditor == ~badContext /\ (visible # None => visible = <<generation>>)
OneFlight == pending<=1
SameRetry == ~badRetry
ValidatedSuccess == ~badReceipt
Spec == Init /\ [][Next]_vars
====
