---- MODULE DisposableCleanup ----
EXTENDS Naturals, Sequences, FiniteSets
CONSTANTS GuardOriginal, GuardOrder, GuardReplay, GuardAbsence
VARIABLES live, phase, epoch, target, attempts, marker
vars == <<live, phase, epoch, target, attempts, marker>>
Disposables == {"canary", "database"}
Init == /\ live = Disposables \cup {"original"}
        /\ phase = "ready" /\ epoch = 1 /\ target = "none"
        /\ attempts = <<>> /\ marker = TRUE
Issue(t) == /\ phase = "ready" \/ (phase = "uncertain" /\ ~GuardReplay)
            /\ marker /\ Len(attempts) < 6 /\ t \in live
            /\ (t \in Disposables \/ ~GuardOriginal)
            /\ (t # "database" \/ "canary" \notin live \/ ~GuardOrder)
            /\ phase' = "pending" /\ target' = t
            /\ attempts' = Append(attempts, <<epoch, t>>)
            /\ UNCHANGED <<live, epoch, marker>>
Acknowledge == /\ phase = "pending" /\ live' = live \ {target}
               /\ phase' = "ready" /\ target' = "none"
               /\ UNCHANGED <<epoch, attempts, marker>>
LoseBefore == /\ phase = "pending" /\ phase' = "uncertain"
              /\ UNCHANGED <<live, epoch, target, attempts, marker>>
LoseAfter == /\ phase = "pending" /\ phase' = "uncertain"
             /\ live' = live \ {target}
             /\ UNCHANGED <<epoch, target, attempts, marker>>
RecordedNewBoot == /\ phase = "uncertain" /\ epoch < 3
                   /\ epoch' = epoch + 1 /\ phase' = "ready" /\ target' = "none"
                   /\ UNCHANGED <<live, attempts, marker>>
Release == /\ phase = "ready" /\ marker
           /\ (live \cap Disposables = {} \/ ~GuardAbsence)
           /\ marker' = FALSE /\ phase' = "done"
           /\ UNCHANGED <<live, epoch, target, attempts>>
Next == (\E t \in live: Issue(t)) \/ Acknowledge \/ LoseBefore \/ LoseAfter \/ RecordedNewBoot \/ Release
TypeOK == /\ live \subseteq {"original", "database", "canary"}
          /\ phase \in {"ready", "pending", "uncertain", "done"}
          /\ epoch \in 1..3 /\ target \in {"none", "original", "database", "canary"}
          /\ marker \in BOOLEAN /\ Len(attempts) <= 6
OriginalPreserved == "original" \in live
CanaryRemovedFirst == "database" \notin live => "canary" \notin live
NoSameEpochReplay == \A i,j \in 1..Len(attempts): i # j => attempts[i] # attempts[j]
MarkerRequiresAbsence == ~marker => live \cap Disposables = {}
Spec == Init /\ [][Next]_vars
====
