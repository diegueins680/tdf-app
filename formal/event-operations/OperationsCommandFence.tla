------------------------ MODULE OperationsCommandFence ------------------------
EXTENDS Naturals, FiniteSets
CONSTANTS AllowStale, AllowRevoked, LeakEffect
VARIABLES version, expected, done, authorized, commits, effects, stale, revoked
vars == <<version,expected,done,authorized,commits,effects,stale,revoked>>
Actors == {1,2}
Init == /\ version=0 /\ expected=[a \in Actors |-> 0] /\ done={}
        /\ authorized=Actors /\ commits=0 /\ effects=0
        /\ stale=FALSE /\ revoked=FALSE
Revoke(a) == /\ a \in authorized /\ authorized'=authorized\{a}
             /\ UNCHANGED <<version,expected,done,commits,effects,stale,revoked>>
Commit(a) == /\ a \notin done
             /\ (a \in authorized \/ AllowRevoked)
             /\ (expected[a]=version \/ AllowStale)
             /\ done'=done\cup{a} /\ version'=version+1
             /\ commits'=commits+1 /\ effects'=effects+1
             /\ stale'=(stale \/ expected[a]#version)
             /\ revoked'=(revoked \/ a \notin authorized)
             /\ UNCHANGED <<expected,authorized>>
Reject(a) == /\ a \notin done
             /\ (a \notin authorized \/ expected[a]#version)
             /\ done'=done\cup{a}
             /\ effects'=effects + (IF LeakEffect THEN 1 ELSE 0)
             /\ UNCHANGED <<version,expected,authorized,commits,stale,revoked>>
Next == \E a \in Actors : Revoke(a) \/ Commit(a) \/ Reject(a)
NoStaleCommit == ~stale
NoRevokedCommit == ~revoked
AtomicEvidence == effects=commits /\ version=commits
Spec == Init /\ [][Next]_vars
=============================================================================
