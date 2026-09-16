------------------------- MODULE SessionBoundary -------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS Actors, Tokens, Requests, Owner, NoRequest, PrimaryActor,
          UnsafeCachedRead, UnsafeUnlockedWrite, UnsafeIdentity
VARIABLES active, alive, phase, token, principal, acting, observed, held
vars == <<active, alive, phase, token, principal, acting, observed, held>>
TokenOwners == [t \in Tokens |-> PrimaryActor]
EmptyObservation == [allowed |-> FALSE, active |-> FALSE, live |-> FALSE, bound |-> FALSE]
Init == /\ active = Tokens /\ alive = Actors /\ held = NoRequest
        /\ phase = [r \in Requests |-> "new"]
        /\ token \in [Requests -> Tokens]
        /\ principal \in [Requests -> Actors]
        /\ acting \in [Requests -> Actors]
        /\ observed = [r \in Requests |-> EmptyObservation]
Bound(r) == principal[r] = Owner[token[r]] /\ acting[r] = principal[r]
Current(r) == token[r] \in active /\ acting[r] \in alive
              /\ (UnsafeIdentity \/ Bound(r))
Evidence(r, allowed) == [allowed |-> allowed, active |-> token[r] \in active,
                        live |-> acting[r] \in alive, bound |-> Bound(r)]
\* Authentication happened before application dispatch. This does not authorize
\* a later query. The acting entity is separate and has no implicit delegation.
Authenticate(r) == /\ phase[r] = "new" /\ token[r] \in active
                   /\ principal[r] = Owner[token[r]]
                   /\ phase' = [phase EXCEPT ![r] = "ready"]
                   /\ UNCHANGED <<active, alive, token, principal, acting, observed, held>>
Read(r) == /\ phase[r] = "ready"
           /\ observed' = [observed EXCEPT ![r] = Evidence(r,
                IF UnsafeCachedRead THEN Bound(r) ELSE Current(r))]
           /\ phase' = [phase EXCEPT ![r] = "done"]
           /\ UNCHANGED <<active, alive, token, principal, acting, held>>
\* Aggregate one pair's ordered account/credential/token locks. SQL/HTTP tests
\* refine their concrete order; this model does not prove multi-pair deadlock freedom.
Acquire(r) == /\ phase[r] = "ready" /\ held = NoRequest /\ Current(r)
              /\ held' = r /\ phase' = [phase EXCEPT ![r] = "held"]
              /\ UNCHANGED <<active, alive, token, principal, acting, observed>>
Finish(r) == /\ phase[r] = "held" /\ held = r
             /\ observed' = [observed EXCEPT ![r] = Evidence(r, TRUE)]
             /\ phase' = [phase EXCEPT ![r] = "done"] /\ held' = NoRequest
             /\ UNCHANGED <<active, alive, token, principal, acting>>
Revoke(t) == /\ t \in active
             /\ (IF held = NoRequest THEN TRUE ELSE UnsafeUnlockedWrite \/ token[held] # t)
             /\ active' = active \ {t}
             /\ UNCHANGED <<alive, phase, token, principal, acting, observed, held>>
Close(a) == /\ a \in alive
            /\ (IF held = NoRequest THEN TRUE ELSE acting[held] # a)
            /\ alive' = alive \ {a}
            /\ UNCHANGED <<active, phase, token, principal, acting, observed, held>>
Advance(r) == Read(r) \/ Acquire(r) \/ Finish(r)
Next == (\E r \in Requests: Authenticate(r) \/ Advance(r))
        \/ (\E t \in Tokens: Revoke(t)) \/ (\E a \in Actors: Close(a))
Spec == Init /\ [][Next]_vars /\ (\A r \in Requests: WF_vars(Advance(r)))
TypeOK == /\ active \subseteq Tokens /\ alive \subseteq Actors
          /\ held \in Requests \cup {NoRequest}
          /\ phase \in [Requests -> {"new", "ready", "held", "done"}]
AuthorityAtResult == \A r \in Requests:
  observed[r].allowed => (observed[r].active /\ observed[r].live /\ observed[r].bound)
LockIntegrity == IF held = NoRequest THEN TRUE ELSE phase[held] = "held"
Progress == \A r \in Requests: phase[r] \in {"ready", "held"} ~> phase[r] = "done"
=============================================================================
