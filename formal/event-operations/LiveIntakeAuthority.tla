---- MODULE LiveIntakeAuthority ----
EXTENDS Naturals, TLC
CONSTANT ExplicitCredential, FenceReceipt, RequirePersistence
VARIABLES generation, phase, requestGeneration, ambient, writer, persisted, receipt, receiptGeneration
vars == <<generation, phase, requestGeneration, ambient, writer, persisted, receipt, receiptGeneration>>
Init == /\ generation = 0 /\ phase = "ready" /\ requestGeneration = 0
        /\ ambient \in {1, 2} /\ writer = 0 /\ persisted = FALSE
        /\ receipt = FALSE /\ receiptGeneration = 0
\* Account 1 owns the explicitly validated code; account 2 is an unrelated cookie.
Submit == /\ phase = "ready" /\ phase' = "pending"
          /\ requestGeneration' = generation
          /\ writer' = IF ExplicitCredential THEN 1 ELSE ambient
          /\ UNCHANGED <<generation, ambient, persisted, receipt, receiptGeneration>>
EditCode == /\ generation < 2 /\ generation' = generation + 1
            /\ receipt' = FALSE
            /\ UNCHANGED <<phase, requestGeneration, ambient, writer, persisted, receiptGeneration>>
SwitchCookie == /\ ambient' = 3 - ambient
                /\ UNCHANGED <<generation, phase, requestGeneration, writer, persisted, receipt, receiptGeneration>>
Resolve == /\ phase = "pending" /\ phase' = "done"
           /\ persisted' \in BOOLEAN
           /\ receipt' = ((~RequirePersistence \/ persisted') /\ (~FenceReceipt \/ generation = requestGeneration))
           /\ receiptGeneration' = requestGeneration
           /\ UNCHANGED <<generation, requestGeneration, ambient, writer>>
Next == Submit \/ EditCode \/ SwitchCookie \/ Resolve
ExplicitAuthority == writer \in {0, 1}
CurrentReceipt == receipt => receiptGeneration = generation
PersistedReceipt == receipt => persisted
Settles == (phase = "pending") ~> (phase = "done")
Spec == Init /\ [][Next]_vars /\ WF_vars(Resolve)
====
