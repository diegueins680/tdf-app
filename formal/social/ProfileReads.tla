-------------------------- MODULE ProfileReads --------------------------
EXTENDS Naturals, Sequences, FiniteSets
CONSTANTS a, b, z, UnsafeCache, UnsafePause, UnsafePreferences
VARIABLES enabled, activated, pairExists, blocked, closed, live,
          discoverable, muted, requested, done, observed
vars == <<enabled, activated, pairExists, blocked, closed, live,
          discoverable, muted, requested, done, observed>>
People == {a,b}
Required(t) == activated \/ (t=b /\ pairExists) \/ a \in closed \/ t \in closed
DomainAllowed(t) == a \in live /\ t \in live /\ a \notin closed /\ t \notin closed
                    /\ (t # b \/ ~blocked)
Allowed(t) == t \in People /\ (~Required(t) \/ DomainAllowed(t))
Returned(t) == t \in People /\
  (IF UnsafeCache THEN TRUE
   ELSE IF UnsafePause THEN
     (~(enabled \/ (t=b /\ pairExists) \/ a \in closed \/ t \in closed) \/ DomainAllowed(t))
   ELSE Allowed(t)) /\
  (~UnsafePreferences \/ t # b \/ (discoverable /\ ~muted))
Init == /\ enabled=FALSE /\ activated=FALSE /\ pairExists=FALSE
        /\ blocked=FALSE /\ closed={} /\ live=People
        /\ discoverable=TRUE /\ muted=FALSE /\ done=FALSE
        /\ requested \in {<<>>,<<a>>,<<b>>,<<z>>,<<b,a,z>>,<<z,a,b>>}
        /\ observed=[returned |-> <<>>, permitted |-> {}, expected |-> <<>>]
Activate == /\ ~enabled /\ enabled'=TRUE /\ activated'=TRUE
 /\ UNCHANGED <<pairExists,blocked,closed,live,discoverable,muted,requested,done,observed>>
Pause == /\ enabled /\ enabled'=FALSE
 /\ UNCHANGED <<activated,pairExists,blocked,closed,live,discoverable,muted,requested,done,observed>>
Block == /\ ~blocked /\ blocked'=TRUE /\ pairExists'=TRUE
 /\ UNCHANGED <<enabled,activated,closed,live,discoverable,muted,requested,done,observed>>
Unblock == /\ blocked /\ blocked'=FALSE
 /\ UNCHANGED <<enabled,activated,pairExists,closed,live,discoverable,muted,requested,done,observed>>
Mute == /\ muted'=~muted /\ pairExists'=TRUE
 /\ UNCHANGED <<enabled,activated,blocked,closed,live,discoverable,requested,done,observed>>
Discover == /\ discoverable'=~discoverable
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,live,muted,requested,done,observed>>
Close(t) == /\ t \in People \ closed /\ closed'=closed \cup {t}
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,live,discoverable,muted,requested,done,observed>>
Revoke(t) == /\ t \in live /\ live'=live \ {t}
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,discoverable,muted,requested,done,observed>>
\* The whole payload uses one authoritative statement snapshot. Later mutations
\* cannot retract bytes already emitted; stored observations describe that boundary.
Read == /\ ~done /\ done'=TRUE
 /\ observed'=[returned |-> SelectSeq(requested,Returned),
               permitted |-> {t \in People: Allowed(t)},
               expected |-> SelectSeq(requested,Allowed)]
 /\ UNCHANGED <<enabled,activated,pairExists,blocked,closed,live,discoverable,muted,requested>>
Next == ~done /\ (Activate \/ Pause \/ Block \/ Unblock \/ Mute \/ Discover \/ Read
                  \/ (\E t \in People: Close(t) \/ Revoke(t)))
Spec == Init /\ [][Next]_vars /\ WF_vars(Read)
FieldsAuthorized == \A i \in 1..Len(observed.returned): observed.returned[i] \in observed.permitted
DirectReadContract == observed.returned=observed.expected
Progress == ~done ~> done
=============================================================================
