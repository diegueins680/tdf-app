---- MODULE LogicalBackup ----
EXTENDS Naturals, FiniteSets
CONSTANTS Runs, HonorPending, RequireCompletedWork, BindSource
VARIABLES phase, owners, pending, inflight, completed, sourceValid, receipts
vars == <<phase, owners, pending, inflight, completed, sourceValid, receipts>>
Init == /\ phase = [r \in Runs |-> "idle"] /\ owners = {} /\ pending = {}
        /\ inflight = {} /\ completed = {} /\ sourceValid = Runs /\ receipts = {}
Start(r) == /\ phase[r] = "idle" /\ owners = {}
            /\ (~HonorPending \/ pending = {})
            /\ phase' = [phase EXCEPT ![r] = "running"]
            /\ owners' = {r} /\ pending' = pending \cup {r} /\ inflight' = inflight \cup {r}
            /\ UNCHANGED <<completed, sourceValid, receipts>>
ChildEnds(r, ok) == /\ r \in inflight /\ ok \in BOOLEAN
                    /\ inflight' = inflight \ {r}
                    /\ completed' = IF ok THEN completed \cup {r} ELSE completed
                    /\ phase' = IF phase[r] = "failed" THEN phase ELSE [phase EXCEPT ![r] = "archived"]
                    /\ UNCHANGED <<owners, pending, sourceValid, receipts>>
ChangeSource(r) == /\ phase[r] \in {"running", "archived"} /\ r \in sourceValid
                   /\ sourceValid' = sourceValid \ {r}
                   /\ UNCHANGED <<phase, owners, pending, inflight, completed, receipts>>
Publish(r) == /\ phase[r] = "archived" /\ r \in owners
              /\ (~RequireCompletedWork \/ r \in completed)
              /\ (~BindSource \/ r \in sourceValid)
              /\ phase' = [phase EXCEPT ![r] = "passed"]
              /\ receipts' = receipts \cup {r} /\ owners' = {} /\ pending' = pending \ {r}
              /\ UNCHANGED <<inflight, completed, sourceValid>>
Crash(r) == /\ r \in owners /\ phase[r] \in {"running", "archived"}
            /\ phase' = [phase EXCEPT ![r] = "failed"] /\ owners' = {}
            /\ UNCHANGED <<pending, inflight, completed, sourceValid, receipts>>
Resolve(r) == /\ phase[r] = "failed" /\ r \in pending /\ r \notin inflight /\ owners = {}
              /\ pending' = pending \ {r}
              /\ UNCHANGED <<phase, owners, inflight, completed, sourceValid, receipts>>
Next == \E r \in Runs: Start(r) \/ (\E ok \in BOOLEAN: ChildEnds(r, ok)) \/ ChangeSource(r) \/ Publish(r) \/ Crash(r) \/ Resolve(r)
TypeOK == /\ phase \in [Runs -> {"idle", "running", "archived", "passed", "failed"}]
          /\ owners \subseteq Runs /\ pending \subseteq Runs /\ inflight \subseteq Runs
          /\ completed \subseteq Runs /\ sourceValid \subseteq Runs /\ receipts \subseteq Runs
OneInflightBackup == Cardinality(inflight) <= 1
CompletedReceipt == receipts \subseteq completed
SourceBoundReceipt == receipts \subseteq sourceValid
Spec == Init /\ [][Next]_vars
====
