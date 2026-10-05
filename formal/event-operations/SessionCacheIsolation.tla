---- MODULE SessionCacheIsolation ----
EXTENDS Naturals, TLC
CONSTANTS SeparateCache, ReuseActorClients, BindExpiryEpoch
Epochs == 1..3
Actor(e) == IF e = 2 THEN "B" ELSE "A"
Slot(e) == IF ~SeparateCache THEN 0 ELSE IF ReuseActorClients /\ e = 3 THEN 1 ELSE e
VARIABLES epoch, active, phase, cache, staleExpiry
vars == <<epoch, active, phase, cache, staleExpiry>>
Init == /\ epoch = 0 /\ active = FALSE
        /\ phase = [e \in Epochs |-> "idle"]
        /\ cache = [s \in 0..3 |-> 0]
        /\ staleExpiry = FALSE
\* Three bounded session occurrences A -> B -> A. Each has one asynchronous
\* operation; its completion also abstracts an old mutation's cache callback.
Switch == /\ epoch < 3 /\ epoch' = epoch + 1 /\ active' = TRUE
          /\ UNCHANGED <<phase, cache, staleExpiry>>
Start == /\ active /\ epoch \in Epochs /\ phase[epoch] = "idle"
         /\ phase' = [phase EXCEPT ![epoch] = "pending"]
         /\ UNCHANGED <<epoch, active, cache, staleExpiry>>
Complete(e) == /\ phase[e] = "pending"
               /\ phase' = [phase EXCEPT ![e] = "done"]
               /\ cache' = [cache EXCEPT ![Slot(e)] = e]
               /\ UNCHANGED <<epoch, active, staleExpiry>>
AuthFailure(e) == /\ phase[e] = "pending"
                  /\ phase' = [phase EXCEPT ![e] = "done"]
                  /\ LET admitted == active /\
                            (IF BindExpiryEpoch THEN e = epoch ELSE Actor(e) = Actor(epoch))
                     IN /\ active' = IF admitted THEN FALSE ELSE active
                        /\ staleExpiry' = (staleExpiry \/ (admitted /\ e # epoch))
                  /\ UNCHANGED <<epoch, cache>>
Next == Switch \/ Start \/ (\E e \in Epochs: Complete(e) \/ AuthFailure(e))
TypeOK == /\ epoch \in 0..3 /\ active \in BOOLEAN
          /\ phase \in [Epochs -> {"idle", "pending", "done"}]
          /\ cache \in [0..3 -> 0..3] /\ staleExpiry \in BOOLEAN
PrivateProjection == active => cache[Slot(epoch)] \in {0, epoch}
CurrentExpiry == ~staleExpiry
Spec == Init /\ [][Next]_vars
====
