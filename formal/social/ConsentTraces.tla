------------------------- MODULE ConsentTraces -------------------------
EXTENDS Relationships
TraceNext == (\E a \in Actors: Request(a, version) \/ Withdraw(a, version) \/ Block(a, version) \/ Unblock(a, version))
             \/ (version = MaxVersion /\ UNCHANGED vars)
TraceSpec == Init /\ [][TraceNext]_vars
=============================================================================
