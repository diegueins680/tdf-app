---- MODULE LegacyInterruptedStop ----
EXTENDS Integers
CONSTANTS RequireBinding, RequireIntent, RequireStopRestriction, RequireRecoveryRestriction, RequireAcknowledgement,
          RequireCleanDatabase, PreserveUncertainty, RequireReconciliation
VARIABLE s
vars == <<s>>
Init == s = [phase |-> "initial", bound |-> FALSE, restricted |-> FALSE,
             intent |-> FALSE, submitted |-> FALSE, acknowledged |-> FALSE,
             exited |-> FALSE, pgClean |-> FALSE, captured |-> FALSE,
             reconciled |-> FALSE, outcomeKnown |-> FALSE, graceful |-> FALSE,
             invalidStop |-> FALSE, unrestrictedStop |-> FALSE,
             unrestrictedRecovery |-> FALSE, releasedEarly |-> FALSE]
Bind == /\ s.phase = "initial"
        /\ s' = [s EXCEPT !.bound = TRUE]
Restrict == /\ s.phase \in {"initial", "intent", "stopped", "captured", "uncertain"}
            /\ s' = [s EXCEPT !.restricted = TRUE]
Intent == /\ s.phase = "initial"
          /\ s' = [s EXCEPT !.phase = "intent", !.intent = RequireIntent]
Submit == /\ s.phase = "intent"
          /\ (s.bound \/ ~RequireBinding)
          /\ (s.restricted \/ ~RequireStopRestriction)
          /\ s' = [s EXCEPT !.phase = "submitted", !.submitted = TRUE,
                   !.invalidStop = ~s.bound,
                   !.unrestrictedStop = ~s.restricted]
StopReply == /\ s.phase = "submitted"
             /\ \E reply \in BOOLEAN:
                 s' = [s EXCEPT !.phase = IF reply THEN "stopped" ELSE "uncertain",
                                !.acknowledged = reply, !.exited = TRUE,
                                !.outcomeKnown = ~PreserveUncertainty,
                                !.graceful = ~PreserveUncertainty]
CleanDatabase == /\ s.phase \in {"stopped", "uncertain"}
                 /\ s' = [s EXCEPT !.pgClean = TRUE]
Capture == /\ s.phase \in {"stopped", "uncertain"}
           /\ (s.acknowledged \/ ~RequireAcknowledgement)
           /\ (s.pgClean \/ ~RequireCleanDatabase)
           /\ s.restricted
           /\ s' = [s EXCEPT !.phase = "captured", !.captured = TRUE]
\* A reboot destroys sampled live evidence; the separate abort model governs
\* durable reboot epochs. Here no recovered process starts until restriction
\* has been established again. No ambiguous stop command is retried.
Reboot == /\ s.phase \in {"captured", "uncertain"}
          /\ s.restricted
          /\ s' = [s EXCEPT !.restricted = FALSE]
Recover == /\ s.phase \in {"captured", "uncertain"}
           /\ (s.restricted \/ ~RequireRecoveryRestriction)
           /\ s' = [s EXCEPT !.phase = "recovered", !.unrestrictedRecovery = ~s.restricted]
Reconcile == /\ s.phase = "recovered"
             /\ s' = [s EXCEPT !.reconciled = TRUE, !.outcomeKnown = TRUE]
Release == /\ s.phase = "recovered"
           /\ (s.reconciled \/ ~RequireReconciliation)
           /\ s' = [s EXCEPT !.phase = "released", !.restricted = FALSE,
                            !.releasedEarly = ~s.reconciled]
Next == Bind \/ Restrict \/ Intent \/ Submit \/ StopReply \/ CleanDatabase \/ Capture
        \/ Reboot \/ Recover \/ Reconcile \/ Release
TypeOK == /\ s.phase \in {"initial", "intent", "submitted", "stopped", "uncertain", "captured", "recovered", "released"}
          /\ \A key \in DOMAIN s \ {"phase"}: s[key] \in BOOLEAN
ExactLegacyBinding == ~s.invalidStop
IntentBeforeStop == s.submitted => s.intent
RestrictedStop == ~s.unrestrictedStop
AcknowledgedCapture == s.captured => s.acknowledged
CleanDatabaseCapture == s.captured => s.pgClean
NoInventedCompletion == /\ ~s.graceful /\ (s.outcomeKnown => s.reconciled)
RestrictedRecovery == ~s.unrestrictedRecovery
ReconciledRelease == ~s.releasedEarly
Spec == Init /\ [][Next]_vars
====
