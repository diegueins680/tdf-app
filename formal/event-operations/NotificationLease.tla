-------------------------- MODULE NotificationLease --------------------------
EXTENDS Naturals
CONSTANTS StaleClock, IgnoreLease
VARIABLES state, clock, lease, attempts, requestLease, requestClock, waiting,
          acceptedAt, acceptedLease
vars == <<state,clock,lease,attempts,requestLease,requestClock,waiting,
          acceptedAt,acceptedLease>>
Expiry == 2
Init == /\ state="pending" /\ clock=0 /\ lease=0 /\ attempts=0
        /\ requestLease=0 /\ requestClock=0 /\ waiting=FALSE
        /\ acceptedAt=0 /\ acceptedLease=0
Claim == /\ state="pending" /\ attempts<2
         /\ lease'=attempts+1 /\ attempts'=attempts+1 /\ state'="processing"
         /\ UNCHANGED <<clock,requestLease,requestClock,waiting,acceptedAt,acceptedLease>>
BeginFinish(id) ==
  /\ state="processing" /\ ~waiting
  /\ requestLease'=id /\ requestClock'=clock /\ waiting'=TRUE
  /\ UNCHANGED <<state,clock,lease,attempts,acceptedAt,acceptedLease>>
Tick == /\ clock<3 /\ clock'=clock+1
        /\ UNCHANGED <<state,lease,attempts,requestLease,requestClock,waiting,acceptedAt,acceptedLease>>
Finish == /\ state="processing" /\ waiting
          /\ (IgnoreLease \/ requestLease=lease)
          /\ (IF StaleClock THEN requestClock ELSE clock)<Expiry
          /\ state'="accepted" /\ acceptedAt'=clock /\ acceptedLease'=requestLease
          /\ waiting'=FALSE
          /\ UNCHANGED <<clock,lease,attempts,requestLease,requestClock>>
Next == Claim \/ (\E id \in {1,2}: BeginFinish(id)) \/ Tick \/ Finish
CurrentLease == state="accepted" => acceptedLease=lease
UnexpiredCompletion == state="accepted" => acceptedAt<Expiry
Spec == Init /\ [][Next]_vars
=============================================================================
