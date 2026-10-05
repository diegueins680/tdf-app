---- MODULE CatalogReorder ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS AtomicRollback, FreshRevision, RequireAuthorization, RequireAudit
Requests == {"first", "second"}
Kinds == {"valid", "foreign-member", "audit-failure"}
VARIABLES kind, authorized, phase, revision, itemWrites, audits, committed,
          rejectedEffect, staleCommit, unauthorizedCommit
vars == <<kind, authorized, phase, revision, itemWrites, audits, committed,
          rejectedEffect, staleCommit, unauthorizedCommit>>
Init == /\ kind \in [Requests -> Kinds]
        /\ authorized \in SUBSET Requests
        /\ phase = [r \in Requests |-> "ready"]
        /\ revision = 0 /\ itemWrites = 0 /\ audits = 0 /\ committed = {}
        /\ rejectedEffect = FALSE /\ staleCommit = FALSE /\ unauthorizedCommit = FALSE
\* Both requests carry expectedCatalogRevision=0. Staging represents an open
\* transaction; its row changes are not yet externally committed.
Stage(r) == /\ phase[r] = "ready"
            /\ phase' = [phase EXCEPT ![r] = "staged"]
            /\ UNCHANGED <<kind, authorized, revision, itemWrites, audits, committed,
                           rejectedEffect, staleCommit, unauthorizedCommit>>
Admitted(r) == (~RequireAuthorization \/ r \in authorized)
              /\ (~FreshRevision \/ revision = 0)
Reject(r) == /\ phase[r] = "staged"
             /\ (~Admitted(r) \/ kind[r] = "foreign-member"
                 \/ (RequireAudit /\ kind[r] = "audit-failure"))
             /\ LET leaked == ~AtomicRollback /\ Admitted(r)
                IN /\ itemWrites' = itemWrites + IF leaked THEN 1 ELSE 0
                   /\ rejectedEffect' = (rejectedEffect \/ leaked)
             /\ phase' = [phase EXCEPT ![r] = "done"]
             /\ UNCHANGED <<kind, authorized, revision, audits, committed,
                            staleCommit, unauthorizedCommit>>
Commit(r) == /\ phase[r] = "staged" /\ Admitted(r)
             /\ kind[r] # "foreign-member"
             /\ (~RequireAudit \/ kind[r] # "audit-failure")
             /\ revision' = revision + 1 /\ itemWrites' = itemWrites + 1
             /\ audits' = audits + IF kind[r] = "audit-failure" THEN 0 ELSE 1
             /\ committed' = committed \cup {r}
             /\ staleCommit' = (staleCommit \/ revision # 0)
             /\ unauthorizedCommit' = (unauthorizedCommit \/ r \notin authorized)
             /\ phase' = [phase EXCEPT ![r] = "done"]
             /\ UNCHANGED <<kind, authorized, rejectedEffect>>
Next == \E r \in Requests: Stage(r) \/ Reject(r) \/ Commit(r)
TypeOK == /\ kind \in [Requests -> Kinds] /\ authorized \subseteq Requests
          /\ phase \in [Requests -> {"ready", "staged", "done"}]
          /\ revision \in 0..2 /\ itemWrites \in 0..2 /\ audits \in 0..2
          /\ committed \subseteq Requests
          /\ rejectedEffect \in BOOLEAN /\ staleCommit \in BOOLEAN
          /\ unauthorizedCommit \in BOOLEAN
AtomicEvidence == revision = Cardinality(committed) /\ itemWrites = revision /\ audits = revision
NoRejectedEffect == ~rejectedEffect
NoStaleCommit == ~staleCommit
NoUnauthorizedCommit == ~unauthorizedCommit
Spec == Init /\ [][Next]_vars
====
