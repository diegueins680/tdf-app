---- MODULE TaskCompletionClient ----
EXTENDS Naturals, TLC
CONSTANTS CaptureRequest, CheckShape, BindReceipt, NoRetry
VARIABLES phase, original, caller, captured, sent, attempts, receipt, shape, published
vars == <<phase, original, caller, captured, sent, attempts, receipt, shape, published>>
Init == /\ phase = "captured" /\ original = 1 /\ caller = 1 /\ captured = 1
        /\ sent = 0 /\ attempts = 0 /\ receipt \in {1,2} /\ shape \in BOOLEAN
        /\ published = FALSE
Edit == /\ phase \in {"captured","waiting"} /\ caller' = 3-caller
        /\ UNCHANGED <<phase,original,captured,sent,attempts,receipt,shape,published>>
Dispatch == /\ phase = "captured" /\ phase' = "waiting"
            /\ sent' = IF CaptureRequest THEN captured ELSE caller
            /\ attempts' = attempts+1
            /\ UNCHANGED <<original,caller,captured,receipt,shape,published>>
Receive == /\ phase = "waiting" /\ phase' = "terminal"
           /\ published' = ((shape \/ ~CheckShape) /\ (receipt=sent \/ ~BindReceipt))
           /\ UNCHANGED <<original,caller,captured,sent,attempts,receipt,shape>>
Fail == /\ phase = "waiting" /\ phase' = "failed"
        /\ UNCHANGED <<original,caller,captured,sent,attempts,receipt,shape,published>>
Retry == /\ ~NoRetry /\ phase = "failed" /\ attempts < 2 /\ phase' = "captured"
         /\ UNCHANGED <<original,caller,captured,sent,attempts,receipt,shape,published>>
Next == Edit \/ Dispatch \/ Receive \/ Fail \/ Retry
TypeOK == /\ phase \in {"captured","waiting","failed","terminal"}
          /\ original=1 /\ captured=1 /\ caller \in {1,2} /\ sent \in {0,1,2}
          /\ attempts \in 0..2 /\ receipt \in {1,2} /\ shape \in BOOLEAN /\ published \in BOOLEAN
OriginalRequestSent == attempts>0 => sent=original
ValidatedReceipt == published => (shape /\ receipt=original)
SingleDispatch == attempts<=1
Spec == Init /\ [][Next]_vars
====
