---- MODULE SessionFence ----
EXTENDS Naturals, TLC
CONSTANTS Recheck, Fence, BindParty, BindCredential, CheckPurpose, RequireWitness
VARIABLES phase, token, actor, witness, operation, checked, accepted, validAtCommit
vars == <<phase, token, actor, witness, operation, checked, accepted, validAtCommit>>
Tokens == [present: BOOLEAN, active: BOOLEAN, normal: BOOLEAN,
           party: {"owner", "other"}, credential: {"original", "rotated"}]
Original == [present |-> TRUE, active |-> TRUE, normal |-> TRUE,
             party |-> "owner", credential |-> "original"]
Valid(t) == /\ witness /\ t.present /\ t.active /\ t.normal
            /\ actor = "owner" /\ t.party = "owner" /\ t.credential = "original"
Guard(t) == /\ (~RequireWitness \/ witness) /\ t.present /\ t.active
            /\ (~CheckPurpose \/ t.normal)
            /\ (~BindParty \/ (actor = "owner" /\ t.party = "owner"))
            /\ (~BindCredential \/ t.credential = "original")
Init == /\ phase = "new" /\ token = Original /\ actor = "owner" /\ witness = TRUE
        /\ operation = "read" /\ checked = FALSE /\ accepted = FALSE /\ validAtCommit = FALSE
Authenticate(w, a, op) ==
  /\ phase = "new" /\ phase' = "authenticated"
  /\ witness' = w /\ actor' = a /\ operation' = op
  /\ UNCHANGED <<token, checked, accepted, validAtCommit>>
MutateToken(t) ==
  /\ phase \in {"authenticated", "held"} /\ t \in Tokens /\ t # token
  /\ (~Fence \/ phase # "held")
  /\ token' = t
  /\ UNCHANGED <<phase, actor, witness, operation, checked, accepted, validAtCommit>>
Acquire ==
  /\ phase = "authenticated" /\ phase' = "held"
  /\ checked' = Guard(IF Recheck THEN token ELSE Original)
  /\ UNCHANGED <<token, actor, witness, operation, accepted, validAtCommit>>
Commit ==
  /\ phase = "held" /\ phase' = "done"
  /\ accepted' = checked /\ validAtCommit' = Valid(token)
  /\ UNCHANGED <<token, actor, witness, operation, checked>>
Next == \/ \E w \in BOOLEAN, a \in {"owner", "other"}, op \in {"read", "new", "replay"}:
             Authenticate(w, a, op)
        \/ \E t \in Tokens: MutateToken(t)
        \/ Acquire \/ Commit
TypeOK == /\ phase \in {"new", "authenticated", "held", "done"} /\ token \in Tokens
          /\ actor \in {"owner", "other"} /\ operation \in {"read", "new", "replay"}
          /\ witness \in BOOLEAN /\ checked \in BOOLEAN
          /\ accepted \in BOOLEAN /\ validAtCommit \in BOOLEAN
CurrentBoundSession == accepted => validAtCommit
Spec == Init /\ [][Next]_vars
====
