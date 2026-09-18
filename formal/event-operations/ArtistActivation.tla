---- MODULE ArtistActivation ----
EXTENDS Naturals, TLC
CONSTANT FenceGeneration
VARIABLES generation, requestGeneration, phase, persisted, authoritative, applied, acceptedGeneration
vars == <<generation, requestGeneration, phase, persisted, authoritative, applied, acceptedGeneration>>
Init == /\ generation = 0 /\ requestGeneration = 0 /\ phase = "activate"
        /\ persisted = FALSE /\ authoritative = FALSE /\ applied = FALSE /\ acceptedGeneration = 0
ChangeContext == /\ generation < 2 /\ generation' = generation + 1
                 /\ applied' = FALSE
                 /\ UNCHANGED <<requestGeneration, phase, persisted, authoritative, acceptedGeneration>>
ResolveActivation == /\ phase = "activate"
                     /\ persisted' \in BOOLEAN
                     /\ phase' = IF persisted' /\ (~FenceGeneration \/ generation = requestGeneration) THEN "refresh" ELSE "done"
                     /\ UNCHANGED <<generation, requestGeneration, authoritative, applied, acceptedGeneration>>
ResolveSession == /\ phase = "refresh"
                  /\ authoritative' \in BOOLEAN
                  /\ phase' = "done"
                  /\ applied' = (authoritative' /\ (~FenceGeneration \/ generation = requestGeneration))
                  /\ acceptedGeneration' = requestGeneration
                  /\ UNCHANGED <<generation, requestGeneration, persisted>>
Next == ChangeContext \/ ResolveActivation \/ ResolveSession
CurrentContext == applied => acceptedGeneration = generation
PersistedAuthority == applied => persisted /\ authoritative
Terminates == <>(phase = "done")
Spec == Init /\ [][Next]_vars /\ WF_vars(ResolveActivation) /\ WF_vars(ResolveSession)
====
