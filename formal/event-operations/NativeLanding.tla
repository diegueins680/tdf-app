---- MODULE NativeLanding ----
EXTENDS TLC
CONSTANT CurrentSessionFirst
VARIABLES auth, marker
vars == <<auth, marker>>
AuthStates == {"hydrating", "guest", "member"}
Markers == {"pending", "seen", "unseen"}
Init == /\ auth = "hydrating" /\ marker = "pending"
ResolveAuth == /\ auth = "hydrating" /\ auth' \in {"guest", "member"}
               /\ UNCHANGED marker
ChangeSession == /\ auth # "hydrating" /\ auth' \in {"guest", "member"}
                 /\ UNCHANGED marker
ResolveMarker == /\ marker = "pending" /\ marker' \in {"seen", "unseen"}
                 /\ UNCHANGED auth
Next == ResolveAuth \/ ChangeSession \/ ResolveMarker
Destination == IF auth = "hydrating" THEN "waiting"
               ELSE IF CurrentSessionFirst /\ auth = "member" THEN "landing"
               ELSE IF marker = "pending" THEN "waiting"
               ELSE IF marker = "seen" THEN "landing" ELSE "welcome"
TypeOK == /\ auth \in AuthStates /\ marker \in Markers
HydrationBeforeNavigation == auth = "hydrating" => Destination = "waiting"
CurrentSessionSkipsMarker == auth = "member" => Destination = "landing"
GuestChoicePreserved == auth = "guest" /\ marker # "pending" =>
                       Destination = (IF marker = "seen" THEN "landing" ELSE "welcome")
HydrationSettles == auth = "hydrating" ~> auth # "hydrating"
\* Marker completion is deliberately NOT fair: optional storage may never reply.
Spec == Init /\ [][Next]_vars /\ WF_vars(ResolveAuth)
====
