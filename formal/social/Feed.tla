---------------------------- MODULE Feed ----------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANT MaxPost
VARIABLES published, eligible, cursor, seen, duplicate, gap
vars == <<published, eligible, cursor, seen, duplicate, gap>>
Init == /\ published = {} /\ eligible = {} /\ cursor = MaxPost + 1
        /\ seen = {} /\ duplicate = FALSE /\ gap = FALSE
Publish == /\ published # 1..MaxPost
           /\ LET n == Cardinality(published) + 1 IN
                /\ published' = published \cup {n} /\ eligible' = eligible \cup {n}
           /\ UNCHANGED <<cursor, seen, duplicate, gap>>
Hide(p) == /\ p \in eligible /\ eligible' = eligible \ {p}
           /\ UNCHANGED <<published, cursor, seen, duplicate, gap>>
Candidates == {p \in eligible: p < cursor}
Page == /\ Candidates # {}
        /\ LET p == CHOOSE n \in Candidates: \A m \in Candidates: m <= n IN
             /\ cursor' = p /\ seen' = seen \cup {p}
             /\ duplicate' = (duplicate \/ p \in seen)
             /\ gap' = (gap \/ (\E n \in Candidates: n > p))
        /\ UNCHANGED <<published, eligible>>
Next == Publish \/ Page \/ (\E p \in 1..MaxPost: Hide(p))
        \/ (published = 1..MaxPost /\ eligible = {} /\ UNCHANGED vars)
Spec == Init /\ [][Next]_vars
TypeOK == eligible \subseteq published /\ published \subseteq 1..MaxPost
StablePagination == ~duplicate /\ ~gap
=============================================================================
