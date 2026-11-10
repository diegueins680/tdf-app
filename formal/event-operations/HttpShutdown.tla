---- MODULE HttpShutdown ----
EXTENDS Naturals, FiniteSets
CONSTANTS Requests, DrainTicks, UnsafeOpenListener, UnsafeEarlyAbort, UnsafeSuccessOnFailure
VARIABLES startup, server, listener, signalled, active, accepted, completed, aborted,
          ticks, result, lateAccepted
vars == <<startup, server, listener, signalled, active, accepted, completed, aborted,
          ticks, result, lateAccepted>>
Init == /\ startup = "pending" /\ server = "serving" /\ listener = TRUE
        /\ signalled = FALSE /\ active = {} /\ accepted = {} /\ completed = {}
        /\ aborted = {} /\ ticks = 0 /\ result = "none" /\ lateAccepted = FALSE
StartDone == /\ result = "none" /\ startup = "pending" /\ startup' = "done"
             /\ UNCHANGED <<server, listener, signalled, active, accepted, completed,
                            aborted, ticks, result, lateAccepted>>
StartFails == /\ result = "none" /\ startup = "pending" /\ startup' = "failed"
              /\ UNCHANGED <<server, listener, signalled, active, accepted, completed,
                             aborted, ticks, result, lateAccepted>>
ServerFails == /\ result = "none" /\ server \in {"serving", "draining"}
               /\ server' = "failed" /\ listener' = FALSE
               /\ UNCHANGED <<startup, signalled, active, accepted, completed, aborted,
                              ticks, result, lateAccepted>>
Accept(r) == /\ result = "none" /\ listener /\ r \notin accepted
             /\ active' = active \cup {r} /\ accepted' = accepted \cup {r}
             /\ lateAccepted' = (lateAccepted \/ signalled)
             /\ UNCHANGED <<startup, server, listener, signalled, completed, aborted,
                            ticks, result>>
Complete(r) == /\ result = "none" /\ r \in active /\ server \in {"serving", "draining"}
               /\ active' = active \ {r} /\ completed' = completed \cup {r}
               /\ UNCHANGED <<startup, server, listener, signalled, accepted, aborted,
                              ticks, result, lateAccepted>>
Stop == /\ result = "none" /\ server = "serving"
        /\ server' = "draining" /\ signalled' = TRUE
        /\ listener' = UnsafeOpenListener
        /\ UNCHANGED <<startup, active, accepted, completed, aborted, ticks, result, lateAccepted>>
Tick == /\ result = "none" /\ server = "draining" /\ ticks < DrainTicks
        /\ ticks' = ticks + 1
        /\ UNCHANGED <<startup, server, listener, signalled, active, accepted, completed,
                       aborted, result, lateAccepted>>
FinishDrain == /\ result = "none" /\ server = "draining"
               /\ (active = {} \/ ticks = DrainTicks \/ UnsafeEarlyAbort)
               /\ server' = "stopped" /\ listener' = FALSE
               /\ aborted' = aborted \cup active /\ active' = {}
               /\ UNCHANGED <<startup, signalled, accepted, completed, ticks, result, lateAccepted>>
Supervise ==
  /\ result = "none" /\ (startup = "failed" \/ server \in {"failed", "stopped"})
  /\ LET failed == startup = "failed" \/ server = "failed" IN
       /\ result' = IF failed /\ ~UnsafeSuccessOnFailure THEN "failure" ELSE "success"
       /\ startup' = IF startup = "pending" THEN "cancelled" ELSE startup
       /\ server' = IF server \in {"serving", "draining"} THEN "cancelled" ELSE server
  /\ listener' = FALSE /\ aborted' = aborted \cup active /\ active' = {}
  /\ UNCHANGED <<signalled, accepted, completed, ticks, lateAccepted>>
Next == StartDone \/ StartFails \/ ServerFails \/ Stop \/ Tick \/ FinishDrain \/ Supervise
        \/ (\E r \in Requests : Accept(r) \/ Complete(r))
TypeOK == /\ startup \in {"pending", "done", "failed", "cancelled"}
          /\ server \in {"serving", "draining", "stopped", "failed", "cancelled"}
          /\ listener \in BOOLEAN /\ signalled \in BOOLEAN /\ lateAccepted \in BOOLEAN
          /\ active \subseteq Requests /\ accepted \subseteq Requests
          /\ completed \subseteq Requests /\ aborted \subseteq Requests
          /\ ticks \in 0..DrainTicks /\ result \in {"none", "success", "failure"}
NoNewAdmissionAfterStop == ~lateAccepted
NoEarlyAbort == aborted # {} => ticks = DrainTicks \/ startup = "failed" \/ server = "failed"
NoFailureSuccess == result = "success" => startup # "failed" /\ server # "failed"
TerminalCancelsStartup == result # "none" => startup # "pending"
RequestAccounting == /\ accepted = active \cup completed \cup aborted
                     /\ active \cap completed = {} /\ active \cap aborted = {}
                     /\ completed \cap aborted = {}
EventuallyTerminal == <>(result # "none")
Spec == Init /\ [][Next]_vars /\ WF_vars(Stop) /\ WF_vars(Tick)
        /\ WF_vars(FinishDrain) /\ WF_vars(Supervise)
====
