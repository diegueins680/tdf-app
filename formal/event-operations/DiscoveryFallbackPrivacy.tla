---- MODULE DiscoveryFallbackPrivacy ----
EXTENDS TLC
CONSTANTS UnsafePrivateSubject, UnsafeLegacyLink
VARIABLES subject, linked
vars == <<subject, linked>>
SubjectStates == {"none", "private", "public"}
Init == /\ subject = "none" /\ linked = FALSE
Next == \/ \E value \in SubjectStates: /\ subject' = value /\ UNCHANGED linked
        \/ /\ linked' = ~linked /\ UNCHANGED subject
Spec == Init /\ [][Next]_vars
\* One SQL observation of an existing legacy artist. Directory publication
\* and the two identity-binding paths are distinct from client cache lifetime.
CanonicalVisible == subject = "public"
LegacyVisible == /\ (subject = "none" \/ (UnsafePrivateSubject /\ subject = "private"))
                 /\ (~linked \/ UnsafeLegacyLink)
FallbackRequiresNoAuthority == LegacyVisible => (subject = "none" /\ ~linked)
CanonicalAndLegacyDoNotDuplicate == ~(CanonicalVisible /\ LegacyVisible)
TypeOK == /\ subject \in SubjectStates /\ linked \in BOOLEAN
====
