---- MODULE RestoreIsolation ----
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Runs, ExclusiveLock, RejectOrphans, HonorPending, IsolatedTarget, VerifiedReceipt
VARIABLES phase, owners, present, pending, inflight, target, verified, receipts, sourceTouched
vars == <<phase, owners, present, pending, inflight, target, verified, receipts, sourceTouched>>
Active == {"locked", "requested", "created", "restored", "verified", "cleaned"}
Init == /\ phase = [r \in Runs |-> "idle"] /\ owners = {} /\ present = {} /\ pending = {} /\ inflight = {}
        /\ target = [r \in Runs |-> "none"] /\ verified = {} /\ receipts = {}
        /\ sourceTouched = FALSE
Acquire(r) == /\ phase[r] = "idle"
              /\ (~ExclusiveLock \/ owners = {})
              /\ (~RejectOrphans \/ present = {})
              /\ (~HonorPending \/ pending = {})
              /\ phase' = [phase EXCEPT ![r] = "locked"]
              /\ owners' = owners \cup {r}
              /\ UNCHANGED <<present, pending, inflight, target, verified, receipts, sourceTouched>>
RequestCreate(r) == /\ phase[r] = "locked" /\ r \in owners
                    /\ (~HonorPending \/ pending = {})
                    /\ phase' = [phase EXCEPT ![r] = "requested"]
                    /\ pending' = pending \cup {r} /\ inflight' = inflight \cup {r}
                    /\ target' \in {[target EXCEPT ![r] = "isolated"]} \cup
                                   (IF IsolatedTarget THEN {} ELSE {[target EXCEPT ![r] = "production"]})
                    /\ UNCHANGED <<owners, present, verified, receipts, sourceTouched>>
CompleteCreate(r) == /\ r \in inflight
                     /\ phase' = IF phase[r] = "failed" THEN phase ELSE [phase EXCEPT ![r] = "created"]
                     /\ present' = present \cup {r} /\ inflight' = inflight \ {r}
                     /\ UNCHANGED <<owners, pending, target, verified, receipts, sourceTouched>>
Restore(r) == /\ phase[r] = "created" /\ r \in owners
              /\ phase' = [phase EXCEPT ![r] = "restored"]
              /\ sourceTouched' = (sourceTouched \/ target[r] = "production")
              /\ UNCHANGED <<owners, present, pending, inflight, target, verified, receipts>>
Verify(r) == /\ phase[r] = "restored" /\ r \in owners
             /\ phase' = [phase EXCEPT ![r] = "verified"]
             /\ verified' = verified \cup {r}
             /\ UNCHANGED <<owners, present, pending, inflight, target, receipts, sourceTouched>>
Cleanup(r) == /\ phase[r] = "verified" /\ r \in owners
              /\ phase' = [phase EXCEPT ![r] = "cleaned"]
              /\ present' = present \ {r} /\ pending' = pending \ {r}
              /\ UNCHANGED <<owners, inflight, target, verified, receipts, sourceTouched>>
Publish(r) == /\ phase[r] \in (IF VerifiedReceipt THEN {"cleaned"} ELSE Active)
              /\ r \in owners
              /\ phase' = [phase EXCEPT ![r] = "passed"]
              /\ receipts' = receipts \cup {r} /\ owners' = owners \ {r}
              /\ UNCHANGED <<present, pending, inflight, target, verified, sourceTouched>>
Crash(r) == /\ phase[r] \in Active /\ r \in owners
            /\ phase' = [phase EXCEPT ![r] = "failed"] /\ owners' = owners \ {r}
            /\ UNCHANGED <<present, pending, inflight, target, verified, receipts, sourceTouched>>
ResolveOrphan(r) == /\ phase[r] = "failed" /\ r \in pending /\ r \notin inflight /\ owners = {}
                   /\ present' = present \ {r} /\ pending' = pending \ {r}
                   /\ UNCHANGED <<phase, owners, inflight, target, verified, receipts, sourceTouched>>
Next == \E r \in Runs: Acquire(r) \/ RequestCreate(r) \/ CompleteCreate(r) \/ Restore(r) \/ Verify(r) \/ Cleanup(r) \/ Publish(r) \/ Crash(r) \/ ResolveOrphan(r)
ExclusiveOwners == Cardinality(owners) <= 1
NoSourceMutation == ~sourceTouched
AtMostOneIsolate == Cardinality(present) <= 1
ReceiptSound == receipts \subseteq verified /\ receipts \cap present = {}
TypeOK == /\ phase \in [Runs -> {"idle", "locked", "requested", "created", "restored", "verified", "cleaned", "passed", "failed"}]
          /\ owners \subseteq Runs /\ present \subseteq Runs /\ pending \subseteq Runs /\ inflight \subseteq Runs /\ verified \subseteq Runs /\ receipts \subseteq Runs
          /\ target \in [Runs -> {"none", "isolated", "production"}] /\ sourceTouched \in BOOLEAN
Spec == Init /\ [][Next]_vars
====
