---- MODULE ArtistClaimKind ----
EXTENDS TLC
CONSTANT FilterSource, GuardCanonical
VARIABLES sourceKind, canonicalKind, prepared, done
vars == <<sourceKind, canonicalKind, prepared, done>>
Init == /\ sourceKind \in {"artist", "band"}
        /\ canonicalKind \in {"artist", "band"}
        /\ prepared = "none" /\ done = FALSE
Prepare == /\ ~done
           /\ prepared' = IF FilterSource /\ sourceKind # "artist"
                           THEN "artist"
                           ELSE IF GuardCanonical /\ canonicalKind # "artist"
                                THEN "none" ELSE canonicalKind
           /\ done' = TRUE
           /\ UNCHANGED <<sourceKind, canonicalKind>>
OnlyArtist == prepared \in {"none", "artist"}
NoWrongSourceReuse == (FilterSource /\ sourceKind # "artist" /\ done) => prepared = "artist"
Spec == Init /\ [][Prepare]_vars /\ WF_vars(Prepare)
Terminates == <>done
====
