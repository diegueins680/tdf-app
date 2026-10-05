---- MODULE SignupIdentity ----
EXTENDS Naturals, TLC
CONSTANT RejectPublicClaim, ReviewBeforeGrant, PreservePrincipal
VARIABLES registered, principal, claim, manager
vars == <<registered, principal, claim, manager>>
Init == /\ registered = FALSE /\ principal = "none"
        /\ claim = "none" /\ manager = FALSE
Signup == /\ ~registered /\ registered' = TRUE /\ principal' = "new-account"
          /\ UNCHANGED <<claim, manager>>
PublicClaim == /\ ~registered /\ ~RejectPublicClaim
               /\ registered' = TRUE /\ principal' = "existing-artist"
               /\ manager' = TRUE /\ UNCHANGED claim
Submit == /\ registered /\ claim = "none" /\ claim' = "submitted"
          /\ manager' = ~ReviewBeforeGrant /\ UNCHANGED <<registered, principal>>
Approve == /\ claim = "submitted" /\ claim' = "approved" /\ manager' = TRUE
           /\ principal' = IF PreservePrincipal THEN principal ELSE "existing-artist"
           /\ UNCHANGED registered
Reject == /\ claim = "submitted" /\ claim' = "rejected"
          /\ UNCHANGED <<registered, principal, manager>>
Next == Signup \/ PublicClaim \/ Submit \/ Approve \/ Reject
IndependentPrincipal == registered => principal = "new-account"
ReviewedManagement == manager => claim = "approved"
Spec == Init /\ [][Next]_vars
====
