------------------------- MODULE ConsentTraces -------------------------
EXTENDS Relationships
TraceNext == (\E a \in Actors: Request(a) \/ Block(a) \/ Unblock(a)) \/ Withdraw
             \/ (version = MaxVersion /\ UNCHANGED vars)
TraceSpec == Init /\ [][TraceNext]_vars
=============================================================================
