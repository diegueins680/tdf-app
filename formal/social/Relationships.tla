------------------------- MODULE Relationships -------------------------
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Actors, Requests, MaxVersion, UnsafeCache, UnsafeWithdrawal, UnsafeRevisionIdentity, UnsafeInactiveActor,
          UnsafeDeletedTarget, UnsafeStaleVersion, UnsafeDelivery,
          UnsafeRequestConsent, UnsafeRestoreConsent, UnsafeUnblockAll
VARIABLES consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence
vars == <<consent, blocked, alive, members, private, version,
          cacheConsent, cacheVersion, pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
ValidCommandActor(a) == UnsafeInactiveActor \/
                         (IF UnsafeDeletedTarget THEN a \in alive ELSE alive = Actors)
CurrentCommand(expectedVersion) == UnsafeStaleVersion \/ expectedVersion = version
Available == alive = Actors /\ blocked = {}
Connected == Available /\ consent = Actors
Permitted == Available /\ (~private \/ members = Actors)
Init == /\ consent = {} /\ blocked = {} /\ alive = Actors
        /\ members = Actors /\ private \in BOOLEAN /\ version = 0
        /\ cacheConsent = {} /\ cacheVersion = 0 /\ pending = {}
        /\ terminal = {} /\ delivered = {} /\ lastRead = [granted |-> FALSE, consent |-> {}, blocked |-> {}, alive |-> Actors, members |-> Actors, private |-> FALSE]
        /\ queuedVersion = [r \in Requests |-> 0]
        /\ deliveryCount = [r \in Requests |-> 0]
        /\ consentOwnerSafe = TRUE
        /\ deliveryEvidence = [r \in Requests |->
              [version |-> 0, queued |-> 0, consent |-> {}, blocked |-> {},
               alive |-> Actors, members |-> Actors, private |-> FALSE]]
Request(a, expectedVersion) ==
  /\ CurrentCommand(expectedVersion) /\ Available
  /\ a \notin consent /\ version < MaxVersion
  /\ consent' = (IF UnsafeRequestConsent THEN Actors ELSE consent \cup {a})
  /\ version' = version + 1
  /\ consentOwnerSafe' = (consentOwnerSafe /\ consent' \ {a} = consent \ {a})
  /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
       cacheVersion, pending, terminal, delivered, lastRead, queuedVersion,
       deliveryCount, deliveryEvidence>>
Withdraw(a, expectedVersion) ==
  /\ CurrentCommand(expectedVersion) /\ alive = Actors
  /\ a \in consent /\ version < MaxVersion
  /\ consent' = (IF UnsafeWithdrawal THEN {} ELSE consent \ {a})
  /\ version' = version + 1
  /\ consentOwnerSafe' = (consentOwnerSafe /\ consent' \ {a} = consent \ {a})
  /\ UNCHANGED <<blocked, alive, members, private, cacheConsent,
       cacheVersion, pending, terminal, delivered, lastRead,
       queuedVersion, deliveryCount, deliveryEvidence>>
Block(a, expectedVersion) ==
  /\ CurrentCommand(expectedVersion) /\ ValidCommandActor(a)
  /\ a \notin blocked /\ version < MaxVersion
  /\ blocked' = blocked \cup {a} /\ consent' = {}
  /\ version' = version + 1
  /\ UNCHANGED <<alive, members, private, cacheConsent, cacheVersion,
       pending, terminal, delivered, lastRead, queuedVersion,
       deliveryCount, consentOwnerSafe, deliveryEvidence>>
Unblock(a, expectedVersion) ==
  /\ CurrentCommand(expectedVersion) /\ ValidCommandActor(a)
  /\ a \in blocked /\ version < MaxVersion
  /\ blocked' = (IF UnsafeUnblockAll THEN {} ELSE blocked \ {a})
  /\ consent' = (IF UnsafeRestoreConsent THEN Actors ELSE consent)
  /\ version' = version + 1
  /\ UNCHANGED <<alive, members, private, cacheConsent,
       cacheVersion, pending, terminal, delivered, lastRead, queuedVersion,
       deliveryCount, consentOwnerSafe, deliveryEvidence>>
Revoke(a) == /\ a \in members /\ version < MaxVersion
             /\ members' = members \ {a} /\ version' = version + 1
             /\ UNCHANGED <<consent, blocked, alive, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
Delete(a) == /\ a \in alive /\ version < MaxVersion
             /\ alive' = alive \ {a} /\ consent' = {}
             /\ version' = version + 1
             /\ UNCHANGED <<blocked, members, private, cacheConsent,
                  cacheVersion, pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
Privacy == /\ ~private /\ version < MaxVersion
           /\ private' = TRUE /\ version' = version + 1
           /\ UNCHANGED <<consent, blocked, alive, members, cacheConsent,
                cacheVersion, pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
Refresh == /\ cacheConsent' = consent /\ cacheVersion' = version
           /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                pending, terminal, delivered, lastRead, queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
Read == /\ lastRead' =
              [granted |-> IF UnsafeCache THEN cacheConsent = Actors ELSE Connected /\ Permitted,
               consent |-> consent, blocked |-> blocked, alive |-> alive,
               members |-> members, private |-> private]
        /\ UNCHANGED <<consent, blocked, alive, members, private, version,
             cacheConsent, cacheVersion, pending, terminal, delivered,
             queuedVersion, deliveryCount, consentOwnerSafe, deliveryEvidence>>
Queue(r) == /\ Connected /\ Permitted /\ r \notin pending \cup terminal
            /\ IF UnsafeRevisionIdentity
                  THEN version \notin {queuedVersion[t] : t \in terminal}
                  ELSE TRUE
            /\ pending' = pending \cup {r}
            /\ queuedVersion' = [queuedVersion EXCEPT ![r] = version]
            /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                 cacheConsent, cacheVersion, terminal, delivered, lastRead,
                 deliveryCount, consentOwnerSafe, deliveryEvidence>>
Finish(r) == /\ r \in pending /\ pending' = pending \ {r}
             /\ terminal' = terminal \cup {r}
             /\ LET authorized == UnsafeDelivery \/ (queuedVersion[r] = version /\ Connected /\ Permitted)
                IN /\ deliveryEvidence' = IF authorized THEN
                         [deliveryEvidence EXCEPT ![r] =
                           [version |-> version, queued |-> queuedVersion[r],
                            consent |-> consent, blocked |-> blocked, alive |-> alive,
                            members |-> members, private |-> private]]
                         ELSE deliveryEvidence
                   /\ delivered' = IF authorized THEN delivered \cup {r} ELSE delivered
                   /\ deliveryCount' = IF authorized
                         THEN [deliveryCount EXCEPT ![r] = @ + 1]
                         ELSE deliveryCount
             /\ UNCHANGED <<consent, blocked, alive, members, private, version,
                  cacheConsent, cacheVersion, lastRead, queuedVersion, consentOwnerSafe>>
Next == (\E a \in Actors, expectedVersion \in 0..MaxVersion:
          Request(a, expectedVersion) \/ Withdraw(a, expectedVersion)
          \/ Block(a, expectedVersion) \/ Unblock(a, expectedVersion))
        \/ (\E a \in Actors: Revoke(a) \/ Delete(a))
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
InactiveActorCannotMutate == \A a \in Actors \ alive:
                              ~ENABLED Block(a, version) /\ ~ENABLED Unblock(a, version)
DeletedTargetCannotMutate == alive # Actors =>
  \A a \in Actors:
    /\ ~ENABLED Request(a, version) /\ ~ENABLED Withdraw(a, version)
    /\ ~ENABLED Block(a, version) /\ ~ENABLED Unblock(a, version)
StaleRelationshipCommandsDenied ==
  \A expectedVersion \in {v \in 0..MaxVersion: v < version}, a \in Actors:
    /\ ~ENABLED Request(a, expectedVersion) /\ ~ENABLED Withdraw(a, expectedVersion)
    /\ ~ENABLED Block(a, expectedVersion) /\ ~ENABLED Unblock(a, expectedVersion)
ReadWasAuthorized == /\ lastRead.consent = Actors
                     /\ lastRead.blocked = {} /\ lastRead.alive = Actors
                     /\ (~lastRead.private \/ lastRead.members = Actors)
AuthoritativeDenial == lastRead.granted => ReadWasAuthorized
AuthorizedReadAvailable == ReadWasAuthorized => lastRead.granted
DeliveryAuthority == \A r \in delivered:
  /\ deliveryEvidence[r].version = deliveryEvidence[r].queued
  /\ deliveryEvidence[r].consent = Actors /\ deliveryEvidence[r].blocked = {}
  /\ deliveryEvidence[r].alive = Actors
  /\ (~deliveryEvidence[r].private \/ deliveryEvidence[r].members = Actors)
NoConsentResurrection == []([
  (\E a \in Actors, v \in 0..MaxVersion: Unblock(a,v)) => consent' = consent
]_vars)
BlockOwnership == []([
  \A a \in Actors, v \in 0..MaxVersion:
    Unblock(a,v) => blocked' \ {a} = blocked \ {a}
]_vars)
AtMostOnce == \A r \in Requests: deliveryCount[r] <= 1
RequestAdmission == (Connected /\ Permitted) =>
                      \A r \in Requests:
                        (r \notin pending \cup terminal) => ENABLED Queue(r)
Progress == \A r \in Requests: (r \in pending) ~> (r \in terminal)
=============================================================================
