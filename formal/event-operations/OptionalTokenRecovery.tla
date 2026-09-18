-------------------- MODULE OptionalTokenRecovery --------------------
EXTENDS Naturals, TLC
CONSTANTS CatchStorageDenial, PreferFragment
VARIABLES mode, fragment, cached, storageThrows, hasReference, phase, lookup, requests
vars == <<mode, fragment, cached, storageThrows, hasReference, phase, lookup, requests>>
Init == /\ mode \in {"tracking", "return"}
        /\ fragment \in {"", "fragment-key"} /\ cached \in {"", "cache-key"}
        /\ storageThrows \in BOOLEAN /\ hasReference \in BOOLEAN
        /\ phase = "resolve" /\ lookup = "" /\ requests = 0
HasFragment == mode = "tracking" /\ fragment # ""
Resolve == /\ phase = "resolve"
           /\ IF HasFragment /\ PreferFragment
                 THEN /\ lookup' = fragment /\ phase' = "ready"
                 ELSE IF storageThrows /\ ~CatchStorageDenial
                   THEN /\ phase' = "crashed" /\ UNCHANGED lookup
                   ELSE /\ lookup' = IF storageThrows THEN "" ELSE cached
                        /\ phase' = "ready"
           /\ UNCHANGED <<mode, fragment, cached, storageThrows, hasReference, requests>>
Dispatch == /\ phase = "ready" /\ phase' = "done"
            /\ requests' = IF lookup # "" /\ hasReference THEN 1 ELSE 0
            /\ UNCHANGED <<mode, fragment, cached, storageThrows, hasReference, lookup>>
Next == Resolve \/ Dispatch
Spec == Init /\ [][Next]_vars /\ WF_vars(Resolve) /\ WF_vars(Dispatch)
NoRequestWithoutToken == requests > 0 => lookup # "" /\ hasReference
NoInventedToken == lookup = "" \/ lookup = fragment \/ (~storageThrows /\ lookup = cached)
FragmentPrecedence == HasFragment /\ phase \in {"ready", "done"} => lookup = fragment
RecoverySettles == <> (phase = "done")
=============================================================================
