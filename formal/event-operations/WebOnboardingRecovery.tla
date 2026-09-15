---- MODULE WebOnboardingRecovery ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS CheckGeneration, CheckReceipt, Coalesce
VARIABLES generation, party, pending, used, consumed
vars == <<generation, party, pending, used, consumed>>
Parties == {"A", "B", "none"}
Requests == [slot: 1..2, generation: 0..2, party: {"A", "B"}]
Init == /\ generation = 0 /\ party = "A"
        /\ pending = {} /\ used = {} /\ consumed = {}
Invalidate(p) ==
  /\ generation < 2 /\ p \in Parties
  /\ generation' = generation + 1 /\ party' = p
  /\ UNCHANGED <<pending, used, consumed>>
Start(s) ==
  /\ party # "none" /\ s \in (1..2) \ used
  /\ (~Coalesce \/ ~\E r \in pending: r.generation = generation)
  /\ pending' = pending \cup {[slot |-> s, generation |-> generation, party |-> party]}
  /\ used' = used \cup {s}
  /\ UNCHANGED <<generation, party, consumed>>
Respond(r, won, canonical) ==
  /\ r \in pending /\ won \in BOOLEAN /\ canonical \in BOOLEAN
  /\ pending' = pending \ {r}
  /\ consumed' = IF r.party = party
                    /\ (~CheckGeneration \/ r.generation = generation)
                    /\ (~CheckReceipt \/ (won /\ canonical))
                  THEN consumed \cup {[current |-> r.generation = generation,
                                        authoritative |-> won /\ canonical]}
                  ELSE consumed
  /\ UNCHANGED <<generation, party, used>>
Next == \/ \E p \in Parties: Invalidate(p)
        \/ \E s \in 1..2: Start(s)
        \/ \E r \in pending, won \in BOOLEAN, canonical \in BOOLEAN:
             Respond(r, won, canonical)
TypeOK == /\ generation \in 0..2 /\ party \in Parties
          /\ pending \subseteq Requests /\ used \subseteq 1..2
          /\ consumed \subseteq [current: BOOLEAN, authoritative: BOOLEAN]
SingleFlight == \A g \in 0..2: Cardinality({r \in pending: r.generation = g}) <= 1
CurrentSessionOnly == \A receipt \in consumed: receipt.current
AuthoritativeOnly == \A receipt \in consumed: receipt.authoritative
Spec == Init /\ [][Next]_vars
====
