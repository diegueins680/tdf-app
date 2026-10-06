---- MODULE ApplicationCanary ----
EXTENDS Naturals, FiniteSets
CONSTANTS Runs, RequireIsolation, RequireRemoved, RequireVerified
VARIABLES phase, owner, pending, databases, requested, applications, isolated, verified, receipts
vars == <<phase, owner, pending, databases, requested, applications, isolated, verified, receipts>>
Init == /\ phase = [r \in Runs |-> "idle"] /\ owner = {} /\ pending = {}
        /\ databases = {} /\ requested = {} /\ applications = {} /\ isolated = {}
        /\ verified = {} /\ receipts = {}
Start(r) == /\ phase[r] = "idle" /\ owner = {} /\ pending = {}
            /\ phase' = [phase EXCEPT ![r] = "restored"]
            /\ owner' = {r} /\ pending' = {r} /\ databases' = databases \cup {r}
            /\ UNCHANGED <<requested, applications, isolated, verified, receipts>>
Request(r, safe) == /\ phase[r] = "restored" /\ r \in owner /\ safe \in BOOLEAN
                   /\ (~RequireIsolation \/ safe)
                   /\ phase' = [phase EXCEPT ![r] = "requested"] /\ requested' = requested \cup {r}
                   /\ isolated' = (IF safe THEN isolated \cup {r} ELSE isolated)
                   /\ UNCHANGED <<owner, pending, databases, applications, verified, receipts>>
DockerCreates(r) == /\ r \in requested
                    /\ requested' = requested \ {r} /\ applications' = applications \cup {r}
                    /\ UNCHANGED <<phase, owner, pending, databases, isolated, verified, receipts>>
Observe(r, ok) == /\ phase[r] = "requested" /\ r \in owner /\ r \in applications /\ ok \in BOOLEAN
                  /\ phase' = [phase EXCEPT ![r] = "observed"]
                  /\ verified' = (IF ok THEN verified \cup {r} ELSE verified)
                  /\ UNCHANGED <<owner, pending, databases, requested, applications, isolated, receipts>>
RemoveApp(r) == /\ phase[r] \in {"observed", "failed"} /\ r \notin requested
                /\ applications' = applications \ {r}
                /\ UNCHANGED <<phase, owner, pending, databases, requested, isolated, verified, receipts>>
RemoveDatabase(r) == /\ phase[r] \in {"observed", "failed"} /\ r \in databases
                     /\ (~RequireRemoved \/ (r \notin applications /\ r \notin requested))
                     /\ databases' = databases \ {r}
                     /\ UNCHANGED <<phase, owner, pending, requested, applications, isolated, verified, receipts>>
Complete(r) == /\ phase[r] = "observed" /\ r \in owner /\ r \notin databases
               /\ (~RequireVerified \/ r \in verified)
               /\ phase' = [phase EXCEPT ![r] = "passed"] /\ receipts' = receipts \cup {r}
               /\ owner' = {} /\ pending' = pending \ {r}
               /\ UNCHANGED <<databases, requested, applications, isolated, verified>>
Crash(r) == /\ r \in owner /\ phase[r] \in {"restored", "requested", "observed"}
            /\ phase' = [phase EXCEPT ![r] = "failed"] /\ owner' = {}
            /\ UNCHANGED <<pending, databases, requested, applications, isolated, verified, receipts>>
Resolve(r) == /\ phase[r] = "failed" /\ r \in pending /\ r \notin databases
              /\ r \notin requested /\ r \notin applications /\ owner = {}
              /\ pending' = pending \ {r}
              /\ UNCHANGED <<phase, owner, databases, requested, applications, isolated, verified, receipts>>
Next == \E r \in Runs: Start(r) \/ (\E safe \in BOOLEAN: Request(r, safe)) \/ DockerCreates(r)
        \/ (\E ok \in BOOLEAN: Observe(r, ok)) \/ RemoveApp(r) \/ RemoveDatabase(r)
        \/ Complete(r) \/ Crash(r) \/ Resolve(r)
TypeOK == /\ phase \in [Runs -> {"idle", "restored", "requested", "observed", "passed", "failed"}]
          /\ owner \subseteq Runs /\ pending \subseteq Runs /\ databases \subseteq Runs
          /\ requested \subseteq Runs /\ applications \subseteq Runs /\ isolated \subseteq Runs
          /\ verified \subseteq Runs /\ receipts \subseteq Runs
NoProductionConnectivity == applications \subseteq isolated
ReservationCoversApp == (requested \cup applications) \subseteq pending
DatabaseOutlivesApp == (requested \cup applications) \subseteq databases
VerifiedReceipt == receipts \subseteq verified
OneCanary == Cardinality(requested \cup applications) <= 1
Spec == Init /\ [][Next]_vars
====
