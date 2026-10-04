-------------------- MODULE MarketplaceStorage --------------------
EXTENDS Naturals, TLC
CONSTANTS CatchOptionalStorage, RequireDurableKey
VARIABLES storageThrows, phase, attempts, persistedKey, requests
vars == <<storageThrows, phase, attempts, persistedKey, requests>>
Init == /\ storageThrows \in BOOLEAN /\ phase = "boot"
        /\ attempts = 0 /\ persistedKey = FALSE /\ requests = 0
Open == /\ phase = "boot"
        /\ phase' = IF storageThrows /\ ~CatchOptionalStorage THEN "crashed" ELSE "browse"
        /\ UNCHANGED <<storageThrows, attempts, persistedKey, requests>>
Checkout == /\ phase = "browse" /\ attempts < 2
            /\ attempts' = attempts + 1
            /\ persistedKey' = (persistedKey \/ ~storageThrows)
            /\ requests' = IF storageThrows /\ RequireDurableKey THEN requests ELSE requests + 1
            /\ UNCHANGED <<storageThrows, phase>>
Next == Open \/ Checkout
Spec == Init /\ [][Next]_vars /\ WF_vars(Open)
BrowsingAvailable == <> (phase = "browse")
NoDispatchWithoutDurableKey == requests > 0 => persistedKey
NoStorageExceptionEscapes == phase # "crashed"
=============================================================================
