---- MODULE ArtistClaimTarget ----
EXTENDS Naturals, TLC
CONSTANT UseLock
VARIABLES twins, owner, phase, observed, allowed, returned, grants, published
vars == <<twins, owner, phase, observed, allowed, returned, grants, published>>
Clients == 1..2
Init == /\ twins \in 0..1 /\ owner = 0
        /\ phase = [c \in Clients |-> "start"]
        /\ observed = [c \in Clients |-> 0]
        /\ allowed \in BOOLEAN /\ returned = {}
        /\ grants = {} /\ published = FALSE
Acquire(c) == /\ phase[c] = "start" /\ (~UseLock \/ owner = 0)
              /\ owner' = IF UseLock THEN c ELSE owner
              /\ phase' = [phase EXCEPT ![c] = "read"]
              /\ UNCHANGED <<twins, observed, allowed, returned, grants, published>>
Read(c) == /\ phase[c] = "read"
           /\ observed' = [observed EXCEPT ![c] = twins]
           /\ phase' = [phase EXCEPT ![c] = "write"]
           /\ UNCHANGED <<twins, owner, allowed, returned, grants, published>>
Commit(c) == /\ phase[c] = "write"
             /\ twins' = IF allowed /\ observed[c] = 0 THEN twins + 1 ELSE twins
             /\ returned' = IF allowed THEN returned \cup {c} ELSE returned
             /\ phase' = [phase EXCEPT ![c] = "done"]
             /\ owner' = IF UseLock THEN 0 ELSE owner
             /\ UNCHANGED <<observed, allowed, grants, published>>
Step(c) == Acquire(c) \/ Read(c) \/ Commit(c)
Next == \E c \in Clients: Step(c)
UniqueTarget == twins <= 1
NoAuthorityFromPreparation == grants = {} /\ ~published
BlockedNotReturned == ~allowed => returned = {}
PersistedTarget == returned # {} => twins > 0
RequestsTerminate == <> (\A c \in Clients: phase[c] = "done")
Spec == Init /\ [][Next]_vars /\ (\A c \in Clients: WF_vars(Step(c)))
====
