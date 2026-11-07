---- MODULE PaymentIntentExpiry ----
EXTENDS Naturals
CONSTANTS Expiry, MaxTime, ClockMode
VARIABLES now, phase, requestTime, transactionTime, beforeLockTime, sample, outcome
vars == <<now, phase, requestTime, transactionTime, beforeLockTime, sample, outcome>>
Times == 0..MaxTime

Init == /\ now = 0 /\ phase = "idle"
        /\ requestTime = 0 /\ transactionTime = 0
        /\ beforeLockTime = 0 /\ sample = 0 /\ outcome = "none"
Tick == /\ now < MaxTime /\ now' = now + 1
        /\ UNCHANGED <<phase, requestTime, transactionTime, beforeLockTime, sample, outcome>>
Request(t) == /\ phase = "idle" /\ t \in Times
              /\ requestTime' = t /\ transactionTime' = now /\ phase' = "waiting"
              /\ beforeLockTime' = now
              /\ UNCHANGED <<now, sample, outcome>>
Acquire == /\ phase = "waiting" /\ phase' = "locked"
           /\ UNCHANGED <<now, requestTime, transactionTime, beforeLockTime, sample, outcome>>
Check == /\ phase = "locked" /\ phase' = "done"
         /\ sample' = now
         /\ LET compared == CASE ClockMode = "request" -> requestTime
                              [] ClockMode = "transaction" -> transactionTime
                              [] ClockMode = "before_lock" -> beforeLockTime
                              [] OTHER -> now
                allowed == IF ClockMode = "inclusive" THEN compared <= Expiry
                           ELSE compared < Expiry
            IN outcome' = IF allowed THEN "admitted" ELSE "refused"
         /\ UNCHANGED <<now, requestTime, transactionTime, beforeLockTime>>
Next == Tick \/ (\E t \in Times : Request(t)) \/ Acquire \/ Check
TypeOK == /\ now \in Times /\ phase \in {"idle", "waiting", "locked", "done"}
          /\ requestTime \in Times /\ transactionTime \in Times
          /\ beforeLockTime \in Times /\ sample \in Times
          /\ outcome \in {"none", "admitted", "refused"}
ExpiredAdmissionDenied == outcome = "admitted" => sample < Expiry
LiveAdmissionAllowed == outcome = "refused" => sample >= Expiry
Spec == Init /\ [][Next]_vars
====
