---- MODULE StudioBookingProjection ----
EXTENDS Naturals, TLC
CONSTANTS UpdateProjection, ReactivateProjection, EnforceExclusion
VARIABLES location, active, calendarLocation, calendarActive
vars == <<location, active, calendarLocation, calendarActive>>
Bookings == {"first", "second"}
Slots == {1, 2, 3}
Exclusive(loc, enabled) == \A a,b \in Bookings: a # b /\ enabled[a] /\ enabled[b] => loc[a] # loc[b]
Init == /\ location = [b \in Bookings |-> IF b = "first" THEN 1 ELSE 3]
        /\ active = [b \in Bookings |-> TRUE]
        /\ calendarLocation = location /\ calendarActive = active
Edit(b, slot, enabled) ==
  LET nextLocation == IF UpdateProjection THEN [calendarLocation EXCEPT ![b] = slot] ELSE calendarLocation
      nextActive == IF UpdateProjection /\ (ReactivateProjection \/ ~enabled \/ active[b])
                    THEN [calendarActive EXCEPT ![b] = enabled] ELSE calendarActive
  IN /\ (~EnforceExclusion \/ Exclusive(nextLocation, nextActive))
     /\ location' = [location EXCEPT ![b] = slot]
     /\ active' = [active EXCEPT ![b] = enabled]
     /\ calendarLocation' = nextLocation /\ calendarActive' = nextActive
Next == \E b \in Bookings, slot \in Slots, enabled \in BOOLEAN: Edit(b, slot, enabled)
ProjectionCurrent == location = calendarLocation /\ active = calendarActive
NoActiveOverlap == Exclusive(location, active)
TypeOK == /\ location \in [Bookings -> Slots] /\ calendarLocation \in [Bookings -> Slots]
          /\ active \in [Bookings -> BOOLEAN] /\ calendarActive \in [Bookings -> BOOLEAN]
Spec == Init /\ [][Next]_vars
====
