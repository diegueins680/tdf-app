---- MODULE VenueSearch ----
\* EVT-VENUE-SEARCH-001 (formal/system/venue-search.md). A bounded model of the
\* three ways the venue list can be computed, over a small discrete globe:
\*
\*   Specification  filter every venue, in (name, id) order, then take the page
\*   Sql            the same with a bounding box applied before the exact test
\*   Legacy         the exact test over only the first Cap venues
\*
\* Latitude is a line 0..MaxLat whose ends are poles; longitude is a ring of
\* Ring positions. Distance is the shorter of the direct way (latitude steps
\* plus ring steps) and the way over either pole, where all longitudes meet.
\* That keeps the three features the real box must respect: a latitude bound,
\* a longitude bound that wraps, and a pole where the longitude bound is void.
\*
\* The constants switch off, one at a time, the wrap, the pole guard and the
\* sufficiency of the cap; each is a negative control. The model says nothing
\* about floating point, SQL text or the real haversine formula.
EXTENDS Naturals, Sequences
CONSTANTS Ring, MaxLat, Cap, WrapBox, PoleGuard
Venues == {1, 2}
VARIABLES name, lat, lng, hasText, qLat, qLng, radius, useText, offset
vars == <<name, lat, lng, hasText, qLat, qLng, radius, useText, offset>>

Init == /\ name \in [Venues -> {1, 2}]
        /\ lat \in [Venues -> 0..MaxLat]
        /\ lng \in [Venues -> 0..(Ring - 1)]
        /\ hasText \in [Venues -> BOOLEAN]
        \* The ring is symmetric under rotation, so the query longitude is fixed.
        /\ qLat \in 0..MaxLat /\ qLng = 0
        /\ radius \in {1, 2} /\ useText \in BOOLEAN /\ offset \in {0, 1}
Next == UNCHANGED vars
Spec == Init /\ [][Next]_vars

Abs(a, b) == IF a >= b THEN a - b ELSE b - a
Min(a, b) == IF a <= b THEN a ELSE b
RingSteps(a, b) == Min(Abs(a, b), Ring - Abs(a, b))
Distance(v) == Min(Abs(lat[v], qLat) + RingSteps(lng[v], qLng),
                   Min(lat[v] + qLat, (MaxLat - lat[v]) + (MaxLat - qLat)))

Within(v) == Distance(v) <= radius
ReachesPole == qLat <= radius \/ MaxLat - qLat <= radius
InBox(v) == /\ Abs(lat[v], qLat) <= radius
            /\ \/ PoleGuard /\ ReachesPole
               \/ IF WrapBox THEN RingSteps(lng[v], qLng) <= radius
                             ELSE Abs(lng[v], qLng) <= radius

Exact(v) == (~useText \/ hasText[v]) /\ Within(v)
\* Ordered by name, then id.
Ordered == IF name[1] <= name[2] THEN <<1, 2>> ELSE <<2, 1>>
Page(s) == SubSeq(s, offset + 1, Min(Len(s), offset + 1))

Specification == SelectSeq(Ordered, Exact)
Sql == SelectSeq(Ordered, LAMBDA v: InBox(v) /\ Exact(v))
Legacy == SelectSeq(SubSeq(Ordered, 1, Min(Cap, Len(Ordered))), Exact)

BoxSound == \A v \in Venues: Within(v) => InBox(v)
SqlMatchesSpecification == Page(Sql) = Page(Specification)
LegacyMatchesSpecification == Page(Legacy) = Page(Specification)
====
