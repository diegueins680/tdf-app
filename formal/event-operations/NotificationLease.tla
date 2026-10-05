-------------------------- MODULE NotificationLease --------------------------
EXTENDS Naturals
CONSTANTS StaleClock, IgnoreLease, StaleClaimClock
VARIABLES state, clock, lease, attempts, requestLease, requestClock, waiting,
          acceptedAt, acceptedLease, claimAt, expires
vars == <<state,clock,lease,attempts,requestLease,requestClock,waiting,
          acceptedAt,acceptedLease,claimAt,expires>>
Init == /\ state="pending" /\ clock=0 /\ lease=0 /\ attempts=0
        /\ requestLease=0 /\ requestClock=0 /\ waiting=FALSE
        /\ acceptedAt=0 /\ acceptedLease=0 /\ claimAt=0 /\ expires=0
Claim == /\ state="pending" /\ attempts<2
         /\ lease'=attempts+1 /\ attempts'=attempts+1 /\ state'="processing"
         /\ claimAt'=clock /\ expires'=IF StaleClaimClock THEN 2 ELSE clock+2
         /\ UNCHANGED <<clock,requestLease,requestClock,waiting,acceptedAt,acceptedLease>>
BeginFinish(id) ==
  /\ state="processing" /\ ~waiting
  /\ requestLease'=id /\ requestClock'=clock /\ waiting'=TRUE
  /\ UNCHANGED <<state,clock,lease,attempts,acceptedAt,acceptedLease,claimAt,expires>>
Tick == /\ clock<3 /\ clock'=clock+1
        /\ UNCHANGED <<state,lease,attempts,requestLease,requestClock,waiting,acceptedAt,acceptedLease,claimAt,expires>>
Finish == /\ state="processing" /\ waiting
          /\ (IgnoreLease \/ requestLease=lease)
          /\ (IF StaleClock THEN requestClock ELSE clock)<expires
          /\ state'="accepted" /\ acceptedAt'=clock /\ acceptedLease'=requestLease
          /\ waiting'=FALSE
          /\ UNCHANGED <<clock,lease,attempts,requestLease,requestClock,claimAt,expires>>
Next == Claim \/ (\E id \in {1,2}: BeginFinish(id)) \/ Tick \/ Finish
FreshClaim == attempts>0 => expires>=claimAt+2
CurrentLease == state="accepted" => acceptedLease=lease
UnexpiredCompletion == state="accepted" => acceptedAt<expires
Spec == Init /\ [][Next]_vars
=============================================================================
