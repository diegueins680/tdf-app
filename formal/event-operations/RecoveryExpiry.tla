---- MODULE RecoveryExpiry ----
EXTENDS Naturals, Integers, TLC
CONSTANT CheckExpiry, FreshClock, RequireMetadata, CheckBinding
VARIABLES clock, sampled, metadata, binding, phase, accepted, acceptedAt, consumed
vars == <<clock,sampled,metadata,binding,phase,accepted,acceptedAt,consumed>>
Init == /\ clock = 0 /\ sampled = 0 /\ metadata \in BOOLEAN
        /\ binding = "original" /\ phase = "ready" /\ accepted = FALSE
        /\ acceptedAt = 0 /\ consumed = FALSE
Start == /\ phase = "ready" /\ phase' = "waiting" /\ sampled' = clock
         /\ UNCHANGED <<clock,metadata,binding,accepted,acceptedAt,consumed>>
Tick == /\ clock < 3 /\ clock' = clock + 1
        /\ UNCHANGED <<sampled,metadata,binding,phase,accepted,acceptedAt,consumed>>
ChangeBinding == /\ phase = "waiting" /\ binding = "original"
                 /\ binding' = "other"
                 /\ UNCHANGED <<clock,sampled,metadata,phase,accepted,acceptedAt,consumed>>
Consume == /\ phase = "waiting" /\ phase' = "done"
           /\ LET time == IF FreshClock THEN clock ELSE sampled
                  valid == (~RequireMetadata \/ metadata)
                        /\ (~CheckBinding \/ binding = "original")
                        /\ (~CheckExpiry \/ time < 2)
              IN /\ accepted' = valid /\ consumed' = valid
                 /\ acceptedAt' = clock
           /\ UNCHANGED <<clock,sampled,metadata,binding>>
Next == Start \/ Tick \/ ChangeBinding \/ Consume
UnexpiredAtConsumption == accepted => acceptedAt < 2
LegacyFailsClosed == accepted => metadata
BoundCredential == accepted => binding = "original"
Spec == Init /\ [][Next]_vars
====
