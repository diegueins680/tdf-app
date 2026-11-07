---- MODULE InboundCapture ----
(* OPS-INBOUND-CAPTURE-001: messages reach a message table and a trigger      *)
(* decides which of them become an Operations event. The application stores   *)
(* a request from a customer with the direction "incoming"; older fixtures    *)
(* used "inbound" and "received". A row can also be the application's own     *)
(* outgoing message, the record of a deletion that arrived before its         *)
(* message, or a message imported from history. A message delivered again     *)
(* finds its row and inserts nothing.                                         *)
EXTENDS Naturals
CONSTANTS Messages, MaxDeliveries,
          AcceptApplicationDirection, SkipNonRequests, CaptureOnInsertOnly, RequireEnabled
VARIABLES kind, stored, events, deliveries, enabled, admitted
vars == <<kind, stored, events, deliveries, enabled, admitted>>

Kinds == {"request", "outgoing", "deletion", "history"}
Direction(k) == IF k = "outgoing" THEN "outgoing" ELSE "incoming"
Accepted(d) == d \in {"inbound", "received"} \/ (AcceptApplicationDirection /\ d = "incoming")
Captured(k) == Accepted(Direction(k)) /\ (SkipNonRequests => k = "request")

Init == /\ kind \in [Messages -> Kinds]
        /\ stored = [m \in Messages |-> FALSE]
        /\ events = [m \in Messages |-> 0]
        /\ deliveries = 0
        /\ enabled \in BOOLEAN
        /\ admitted = [m \in Messages |-> FALSE]

Deliver(m) ==
  /\ deliveries < MaxDeliveries /\ deliveries' = deliveries + 1
  /\ stored' = [stored EXCEPT ![m] = TRUE]
  /\ admitted' = [admitted EXCEPT ![m] =
       IF ~stored[m] THEN enabled /\ kind[m] = "request" ELSE @]
  /\ events' = [events EXCEPT ![m] =
       IF Captured(kind[m]) /\ (RequireEnabled => enabled)
          /\ (CaptureOnInsertOnly => ~stored[m]) THEN @ + 1 ELSE @]
  /\ UNCHANGED <<kind, enabled>>

Toggle == /\ enabled' = ~enabled
          /\ UNCHANGED <<kind, stored, events, deliveries, admitted>>

Next == (\E m \in Messages : Deliver(m)) \/ Toggle
Spec == Init /\ [][Next]_vars

TypeOK == /\ kind \in [Messages -> Kinds] /\ stored \in [Messages -> BOOLEAN]
          /\ events \in [Messages -> 0..MaxDeliveries]
          /\ enabled \in BOOLEAN /\ admitted \in [Messages -> BOOLEAN]

(* A request first stored while enabled has an event.                            *)
RequestsCaptured == \A m \in Messages : admitted[m] => events[m] >= 1
(* Nothing else has one.                                                     *)
OnlyRequestsCaptured == \A m \in Messages : kind[m] # "request" => events[m] = 0
(* A message has at most one event however often it is delivered.            *)
CapturedOnce == \A m \in Messages : events[m] <= 1
CaptureRespectsEnablement == \A m \in Messages : events[m] > 0 => admitted[m]
====
