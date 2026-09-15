------------------------- MODULE Relationships -------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Actors, MaxVersion, UnsafeCache
VARIABLES consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, leaked
vars == <<consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, leaked>>
Available == alive = Actors /\ blocked = {}
Connected == Available /\ consent = Actors
Permitted == Available /\ (~private \/ members = Actors)
Init == /\ consent = {} /\ blocked = {} /\ alive = Actors
        /\ members = Actors /\ private \in BOOLEAN /\ version = 0
        /\ cacheConsent = {} /\ cacheVersion = 0 /\ pending = {}
        /\ terminal = {} /\ delivered = {} /\ leaked = FALSE
Request(a) == /\ Available /\ a \notin consent /\ version < MaxVersion
              /\ consent' = consent \cup {a} /\ version' = version + 1
              /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
                   cacheVersion, pending, terminal, delivered, leaked>>
Withdraw == /\ consent # {} /\ version < MaxVersion
            /\ consent' = {} /\ version' = version + 1
            /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
                 cacheVersion, pending, terminal, delivered, leaked>>
Block(a) == /\ a \notin blocked /\ version < MaxVersion
            /\ blocked' = blocked \cup {a} /\ consent' = {}
            /\ version' = version + 1
            /\ UNCHANGED <<alive, members, private, cacheConsent, cacheVersion,
                 pending, terminal, delivered, leaked>>
Unblock(a) == /\ a \in blocked /\ version < MaxVersion
              /\ blocked' = blocked \ {a} /\ version' = version + 1
              /\ UNCHANGED <<consent, alive, members, private, cacheConsent,
                   cacheVersion, pending, terminal, delivered, leaked>>
Revoke(a) == /\ a \in members /\ version < MaxVersion
             /\ members' = members \ {a} /\ version' = version + 1
             /\ UNCHANGED <<consent, blocked, alive, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, leaked>>
Delete(a) == /\ a \in alive /\ version < MaxVersion
             /\ alive' = alive \ {a} /\ consent' = {}
             /\ version' = version + 1
             /\ UNCHANGED <<blocked, members, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, leaked>>
Privacy == /\ ~private /\ version < MaxVersion
           /\ private' = TRUE /\ version' = version + 1
           /\ UNCHANGED <<consent, blocked, alive, members, cacheConsent,
                cacheVersion, pending, terminal, delivered, leaked>>
Refresh == /\ cacheConsent' = consent /\ cacheVersion' = version
           /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                pending, terminal, delivered, leaked>>
Read == /\ leaked' = (leaked \/
              (IF UnsafeCache THEN cacheConsent = Actors /\ ~Permitted ELSE FALSE))
        /\ UNCHANGED <<consent, blocked, alive, members, private, version,
             cacheConsent, cacheVersion, pending, terminal, delivered>>
Queue == /\ Connected /\ Permitted /\ version \notin terminal
         /\ pending' = pending \cup {version}
         /\ UNCHANGED <<consent, blocked, alive, members, private, version,
              cacheConsent, cacheVersion, terminal, delivered, leaked>>
Finish(v) == /\ v \in pending /\ pending' = pending \ {v}
             /\ terminal' = terminal \cup {v}
             /\ delivered' = IF v = version /\ Connected /\ Permitted
                              THEN delivered \cup {v} ELSE delivered
             /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                  cacheConsent, cacheVersion, leaked>>
Next == (\E a \in Actors: Request(a) \/ Block(a) \/ Unblock(a) \/ Revoke(a) \/ Delete(a))
        \/ Withdraw \/ Privacy \/ Refresh \/ Read \/ Queue
        \/ (\E v \in 0..MaxVersion: Finish(v))
Spec == Init /\ [][Next]_vars /\ \A v \in 0..MaxVersion: WF_vars(Finish(v))
TypeOK == /\ consent \subseteq Actors /\ blocked \subseteq Actors
          /\ alive \subseteq Actors /\ members \subseteq Actors
          /\ version \in 0..MaxVersion /\ cacheVersion <= version
          /\ pending \subseteq 0..MaxVersion /\ terminal \subseteq 0..MaxVersion
          /\ delivered \subseteq terminal /\ pending \cap terminal = {}
ConsentIntegrity == (blocked # {} \/ alive # Actors) => consent = {}
AuthoritativeDenial == ~leaked
Progress == \A v \in 0..MaxVersion: (v \in pending) ~> (v \in terminal)
=============================================================================
