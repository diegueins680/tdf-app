---------------------------- MODULE Feed ----------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS MaxPost, UnsafeOrder
VARIABLES published, eligible, cursor, seen, duplicate, started, highWater
vars == <<published, eligible, cursor, seen, duplicate, started, highWater>>
Init == /\ published = {} /\ eligible = {} /\ cursor = MaxPost + 1
        /\ seen = {} /\ duplicate = FALSE /\ started = FALSE /\ highWater = 0
Publish == /\ published # 1..MaxPost
           /\ LET n == Cardinality(published) + 1 IN
                /\ published' = published \cup {n} /\ eligible' = eligible \cup {n}
           /\ UNCHANGED <<cursor, seen, duplicate, started, highWater>>
Hide(p) == /\ p \in eligible /\ eligible' = eligible \ {p}
           /\ UNCHANGED <<published, cursor, seen, duplicate, started, highWater>>
Start == /\ ~started /\ started' = TRUE
         /\ highWater' = Cardinality(published)
         /\ UNCHANGED <<published, eligible, cursor, seen, duplicate>>
Candidates == {p \in eligible: p <= highWater /\ p < cursor}
Page == /\ started /\ Candidates # {}
        /\ LET p == IF UnsafeOrder THEN CHOOSE n \in Candidates: TRUE
                    ELSE CHOOSE n \in Candidates: \A m \in Candidates: m <= n IN
             /\ cursor' = p /\ seen' = seen \cup {p}
             /\ duplicate' = (duplicate \/ p \in seen)
        /\ UNCHANGED <<published, eligible, started, highWater>>
Next == Publish \/ Start \/ Page \/ (\E p \in 1..MaxPost: Hide(p))
        \/ (published = 1..MaxPost /\ eligible = {} /\ UNCHANGED vars)
Spec == Init /\ [][Next]_vars
TypeOK == /\ eligible \subseteq published /\ published \subseteq 1..MaxPost
          /\ highWater \in 0..MaxPost /\ cursor \in 1..(MaxPost + 1)
          /\ seen \subseteq 1..highWater /\ started \in BOOLEAN
StablePagination == /\ ~duplicate
                    /\ started => \A p \in eligible:
                         (p <= highWater /\ p >= cursor) => p \in seen
CompleteAtEnd == (started /\ Candidates = {}) =>
                  {p \in eligible: p <= highWater} \subseteq seen
=============================================================================
