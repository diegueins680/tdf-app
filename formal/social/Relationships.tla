------------------------- MODULE Relationships -------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Actors, Requests, MaxVersion, UnsafeCache, UnsafeWithdrawal, UnsafeRevisionIdentity
VARIABLES consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe
vars == <<consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Available == alive = Actors /\ blocked = {}
Connected == Available /\ consent = Actors
Permitted == Available /\ (~private \/ members = Actors)
Init == /\ consent = {} /\ blocked = {} /\ alive = Actors
        /\ members = Actors /\ private \in BOOLEAN /\ version = 0
        /\ cacheConsent = {} /\ cacheVersion = 0 /\ pending = {}
        /\ terminal = {} /\ delivered = {} /\ leaked = FALSE
        /\ queuedVersion = [r \in Requests |-> 0]
        /\ deliveryCount = [r \in Requests |-> 0]
        /\ consentOwnerSafe = TRUE
Request(a) == /\ Available /\ a \notin consent /\ version < MaxVersion
              /\ consent' = consent \cup {a} /\ version' = version + 1
              /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
                   cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Withdraw(a) == /\ a \in consent /\ version < MaxVersion
               /\ consent' = IF UnsafeWithdrawal THEN {} ELSE consent \ {a}
               /\ version' = version + 1
               /\ consentOwnerSafe' =
                    (consentOwnerSafe /\ consent' \ {a} = consent \ {a})
               /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
                    cacheVersion, pending, terminal, delivered, leaked,
                    queuedVersion, deliveryCount>>
Block(a) == /\ a \notin blocked /\ version < MaxVersion
            /\ blocked' = blocked \cup {a} /\ consent' = {}
            /\ version' = version + 1
            /\ UNCHANGED <<alive, members, private, cacheConsent, cacheVersion,
                 pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Unblock(a) == /\ a \in blocked /\ version < MaxVersion
              /\ blocked' = blocked \ {a} /\ version' = version + 1
              /\ UNCHANGED <<consent, alive, members, private, cacheConsent,
                   cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Revoke(a) == /\ a \in members /\ version < MaxVersion
             /\ members' = members \ {a} /\ version' = version + 1
             /\ UNCHANGED <<consent, blocked, alive, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Delete(a) == /\ a \in alive /\ version < MaxVersion
             /\ alive' = alive \ {a} /\ consent' = {}
             /\ version' = version + 1
             /\ UNCHANGED <<blocked, members, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Privacy == /\ ~private /\ version < MaxVersion
           /\ private' = TRUE /\ version' = version + 1
           /\ UNCHANGED <<consent, blocked, alive, members, cacheConsent,
                cacheVersion, pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Refresh == /\ cacheConsent' = consent /\ cacheVersion' = version
           /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                pending, terminal, delivered, leaked, queuedVersion, deliveryCount, consentOwnerSafe>>
Read == /\ leaked' = (leaked \/
              (IF UnsafeCache THEN cacheConsent = Actors /\ ~Permitted ELSE FALSE))
        /\ UNCHANGED <<consent, blocked, alive, members, private, version,
             cacheConsent, cacheVersion, pending, terminal, delivered,
             queuedVersion, deliveryCount, consentOwnerSafe>>
Queue(r) == /\ Connected /\ Permitted /\ r \notin pending \cup terminal
            /\ IF UnsafeRevisionIdentity
                  THEN version \notin {queuedVersion[t] : t \in terminal}
                  ELSE TRUE
            /\ pending' = pending \cup {r}
            /\ queuedVersion' = [queuedVersion EXCEPT ![r] = version]
            /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                 cacheConsent, cacheVersion, terminal, delivered, leaked,
                 deliveryCount, consentOwnerSafe>>
Finish(r) == /\ r \in pending /\ pending' = pending \ {r}
             /\ terminal' = terminal \cup {r}
             /\ LET authorized == queuedVersion[r] = version /\ Connected /\ Permitted
                IN /\ delivered' = IF authorized THEN delivered \cup {r} ELSE delivered
                   /\ deliveryCount' = IF authorized
                         THEN [deliveryCount EXCEPT ![r] = @ + 1]
                         ELSE deliveryCount
             /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                  cacheConsent, cacheVersion, leaked, queuedVersion, consentOwnerSafe>>
Next == (\E a \in Actors:
          Request(a) \/ Withdraw(a) \/ Block(a) \/ Unblock(a) \/ Revoke(a) \/ Delete(a))
        \/ Privacy \/ Refresh \/ Read
        \/ (\E r \in Requests: Queue(r) \/ Finish(r))
Spec == Init /\ [][Next]_vars /\ \A r \in Requests: WF_vars(Finish(r))
TypeOK == /\ consent \subseteq Actors /\ blocked \subseteq Actors
          /\ alive \subseteq Actors /\ members \subseteq Actors
          /\ version \in 0..MaxVersion /\ cacheVersion <= version
          /\ pending \subseteq Requests /\ terminal \subseteq Requests
          /\ delivered \subseteq terminal /\ pending \cap terminal = {}
          /\ queuedVersion \in [Requests -> 0..MaxVersion]
          /\ deliveryCount \in [Requests -> Nat]
          /\ consentOwnerSafe \in BOOLEAN
ConsentIntegrity == (blocked # {} \/ alive # Actors) => consent = {}
OwnConsentOnly == consentOwnerSafe
AuthoritativeDenial == ~leaked
AtMostOnce == \A r \in Requests: deliveryCount[r] <= 1
RequestAdmission == (Connected /\ Permitted) =>
                      \A r \in Requests:
                        (r \notin pending \cup terminal) => ENABLED Queue(r)
Progress == \A r \in Requests: (r \in pending) ~> (r \in terminal)
=============================================================================
