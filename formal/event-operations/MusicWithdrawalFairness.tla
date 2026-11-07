---- MODULE MusicWithdrawalFairness ----
\* MEDIA-MUSIC-RELEASE-001: finite due withdrawals, one permanently failing
\* version, batch size one, recurring worker, persisted last-attempt priority.
\* Queue order abstracts updated_at ordering between successive transactions.
\* No concurrent arrivals, clock regression, rescheduling or failed persistence.
EXTENDS Naturals, Sequences, FiniteSets
CONSTANT RotateFailure
VARIABLES queue, withdrawn
vars == <<queue, withdrawn>>
Versions == {"poison", "healthy1", "healthy2"}
Healthy == Versions \ {"poison"}
Queued == {queue[i] : i \in 1..Len(queue)}

Init == /\ queue = <<"poison", "healthy1", "healthy2">>
        /\ withdrawn = {}

Run == /\ Len(queue) > 0
       /\ IF Head(queue) = "poison"
             THEN /\ queue' = IF RotateFailure
                                 THEN Append(Tail(queue), Head(queue))
                                 ELSE queue
                  /\ UNCHANGED withdrawn
             ELSE /\ withdrawn' = withdrawn \cup {Head(queue)}
                  /\ queue' = Tail(queue)

TypeOK == /\ queue \in Seq(Versions)
          /\ withdrawn \subseteq Healthy
Partition == /\ Queued \cup withdrawn = Versions
             /\ Queued \cap withdrawn = {}
             /\ Cardinality(Queued) = Len(queue)
HealthyEventuallyWithdrawn == <> (Healthy \subseteq withdrawn)
Spec == Init /\ [][Run]_vars /\ WF_vars(Run)
====
