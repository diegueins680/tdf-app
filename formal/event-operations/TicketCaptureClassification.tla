---- MODULE TicketCaptureClassification ----
EXTENDS Naturals
CONSTANTS UnsafeErrorReadback, UnsafeSuccessResponse
VARIABLES phase, status, path, locked, otherRail
vars == <<phase, status, path, locked, otherRail>>
Statuses == {"MISSING", "UNKNOWN", "PENDING", "PARTIALLY_REFUNDED", "REFUNDED",
             "DECLINED", "FAILED", "FUTURE_STATUS"}
NoCharge == {"DECLINED", "FAILED"}
Init == /\ phase = "initial"
        /\ status = "MISSING"
        /\ path = "error"
        /\ locked = TRUE
        /\ otherRail = FALSE
Observe(s, p) ==
  /\ phase = "initial"
  /\ phase' = "observed"
  /\ status' = s
  /\ path' = p
  /\ locked' = IF p = "error" /\ UnsafeErrorReadback
                   THEN s \in {"MISSING", "PENDING"}
                   ELSE IF p = "response" /\ UnsafeSuccessResponse
                        THEN s = "PENDING"
                        ELSE s \notin NoCharge
  /\ UNCHANGED otherRail
StartOtherRail == /\ phase = "observed"
                  /\ ~locked
                  /\ phase' = "other-rail"
                  /\ otherRail' = TRUE
                  /\ UNCHANGED <<status, path, locked>>
Next == (\E s \in Statuses, p \in {"error", "response"}: Observe(s, p))
        \/ StartOtherRail
Spec == Init /\ [][Next]_vars
UnresolvedCaptureFenced == otherRail => status \in NoCharge
TypeOK == /\ phase \in {"initial", "observed", "other-rail"}
          /\ status \in Statuses
          /\ path \in {"error", "response"}
          /\ locked \in BOOLEAN /\ otherRail \in BOOLEAN
====
