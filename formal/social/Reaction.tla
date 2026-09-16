---------------------------- MODULE Reaction ----------------------------
EXTENDS Naturals
VARIABLES active, additions, writes, lostEvidence, duplicateEvidence
vars == <<active, additions, writes, lostEvidence, duplicateEvidence>>
Init == /\ active = FALSE /\ additions = 0 /\ writes = 0
        /\ lostEvidence = FALSE /\ duplicateEvidence = FALSE
SetActive(want) == /\ writes < 4
  /\ active' = want /\ writes' = writes + 1
  /\ additions' = (IF want /\ ~active THEN additions + 1 ELSE additions)
  /\ lostEvidence' = (lostEvidence \/ additions' < additions)
  /\ duplicateEvidence' = (duplicateEvidence \/ (want /\ active /\ additions' # additions))
Next == (\E want \in BOOLEAN: SetActive(want)) \/ (writes = 4 /\ UNCHANGED vars)
Spec == Init /\ [][Next]_vars
RetrySafe == ~lostEvidence /\ ~duplicateEvidence /\ additions <= writes
=============================================================================
