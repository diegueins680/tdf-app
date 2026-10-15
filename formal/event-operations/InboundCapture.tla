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
          AcceptApplicationDirection, SkipNonRequests, CaptureOnInsertOnly
VARIABLES kind, stored, events, deliveries
vars == <<kind, stored, events, deliveries>>

Kinds == {"request", "outgoing", "deletion", "history"}
Direction(k) == IF k = "outgoing" THEN "outgoing" ELSE "incoming"
Accepted(d) == d \in {"inbound", "received"} \/ (AcceptApplicationDirection /\ d = "incoming")
Captured(k) == Accepted(Direction(k)) /\ (SkipNonRequests => k = "request")

Init == /\ kind \in [Messages -> Kinds]
        /\ stored = [m \in Messages |-> FALSE]
        /\ events = [m \in Messages |-> 0]
        /\ deliveries = 0

Deliver(m) ==
  /\ deliveries < MaxDeliveries /\ deliveries' = deliveries + 1
  /\ stored' = [stored EXCEPT ![m] = TRUE]
  /\ events' = [events EXCEPT ![m] =
       IF Captured(kind[m]) /\ (CaptureOnInsertOnly => ~stored[m]) THEN @ + 1 ELSE @]
  /\ UNCHANGED kind

Next == \E m \in Messages : Deliver(m)
Spec == Init /\ [][Next]_vars

TypeOK == /\ kind \in [Messages -> Kinds] /\ stored \in [Messages -> BOOLEAN]
          /\ events \in [Messages -> 0..MaxDeliveries]

(* A stored request from a customer has an event.                            *)
RequestsCaptured == \A m \in Messages : (stored[m] /\ kind[m] = "request") => events[m] >= 1
(* Nothing else has one.                                                     *)
OnlyRequestsCaptured == \A m \in Messages : kind[m] # "request" => events[m] = 0
(* A message has at most one event however often it is delivered.            *)
CapturedOnce == \A m \in Messages : events[m] <= 1
====
