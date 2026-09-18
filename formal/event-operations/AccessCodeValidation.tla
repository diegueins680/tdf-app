---- MODULE AccessCodeValidation ----
EXTENDS Naturals, TLC
CONSTANT FenceGeneration, CheckAccount
VARIABLES generation, phase, validAccount, enabled, acceptedGeneration
vars == <<generation, phase, validAccount, enabled, acceptedGeneration>>
Init == /\ generation = 0 /\ phase = "pending" /\ validAccount = FALSE
        /\ enabled = FALSE /\ acceptedGeneration = 0
EditCode == /\ generation < 2 /\ generation' = generation + 1 /\ enabled' = FALSE
            /\ UNCHANGED <<phase, validAccount, acceptedGeneration>>
Resolve == /\ phase = "pending" /\ validAccount' \in BOOLEAN /\ phase' = "done"
           /\ enabled' = ((~CheckAccount \/ validAccount') /\ (~FenceGeneration \/ generation = 0))
           /\ acceptedGeneration' = 0 /\ UNCHANGED generation
Timeout == /\ phase = "pending" /\ phase' = "done" /\ enabled' = FALSE
           /\ UNCHANGED <<generation, validAccount, acceptedGeneration>>
Next == EditCode \/ Resolve \/ Timeout
CurrentCredential == enabled => acceptedGeneration = generation
VerifiedAccount == enabled => validAccount
Terminates == <>(phase = "done")
Spec == Init /\ [][Next]_vars /\ WF_vars(Resolve \/ Timeout)
====
