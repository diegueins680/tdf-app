---- MODULE ProviderRetryAdmission ----
\* Slots represent fresh-charge intent lifecycles, not commerce attempt rows.
\* One intent may have separate create/capture operation attempts. Those
\* continuation receipts are outside this initial-admission abstraction.
EXTENDS Naturals, FiniteSets
CONSTANTS Keys, Payloads, MaxIntents,
          UnsafeAmbiguousRetry, UnsafeDuplicateKey, UnsafePayloadReplay,
          UnsafeClosedCreate, UnsafeLateNoCharge
VARIABLES count, status, receipts, responses, closed, countAtClose, captures
vars == <<count, status, receipts, responses, closed, countAtClose, captures>>
Slots == 1..MaxIntents
ReceiptType == [key : Keys, payload : Payloads, intent : Slots]
Active == {i \in 1..count : status[i] # "no_charge"}
Init == /\ count = 0
        /\ status = [i \in Slots |-> "unused"]
        /\ receipts = {} /\ responses = {}
        /\ closed = FALSE /\ countAtClose = 0 /\ captures = {}

Create(key, payload) ==
  /\ count < MaxIntents
  /\ ~closed \/ UnsafeClosedCreate
  /\ UnsafeDuplicateKey \/ ~\E r \in receipts : r.key = key
  /\ Active = {} \/ UnsafeAmbiguousRetry
  /\ count' = count + 1
  /\ status' = [status EXCEPT ![count + 1] = "active"]
  /\ receipts' = receipts \cup {[key |-> key, payload |-> payload, intent |-> count + 1]}
  /\ UNCHANGED <<responses, closed, countAtClose, captures>>

Replay(key, payload) ==
  /\ \E r \in receipts :
      /\ r.key = key
      /\ r.payload = payload \/ UnsafePayloadReplay
      /\ responses' = responses \cup {[key |-> key, payload |-> payload, intent |-> r.intent]}
  /\ UNCHANGED <<count, status, receipts, closed, countAtClose, captures>>

Ambiguous(i) ==
  /\ i \in 1..count /\ status[i] = "active"
  /\ status' = [status EXCEPT ![i] = "ambiguous"]
  /\ UNCHANGED <<count, receipts, responses, closed, countAtClose, captures>>

NoCharge(i) ==
  /\ i \in 1..count
  /\ status[i] \in {"active", "ambiguous"}
       \/ (UnsafeLateNoCharge /\ status[i] = "captured")
  /\ status' = [status EXCEPT ![i] = "no_charge"]
  /\ UNCHANGED <<count, receipts, responses, closed, countAtClose, captures>>

Capture(i) ==
  /\ i \in 1..count /\ ~closed
  /\ status[i] \in {"active", "ambiguous"}
  /\ status' = [status EXCEPT ![i] = "captured"]
  /\ captures' = captures \cup {i}
  /\ UNCHANGED <<count, receipts, responses, closed, countAtClose>>

Close == /\ ~closed /\ closed' = TRUE /\ countAtClose' = count
         /\ UNCHANGED <<count, status, receipts, responses, captures>>
Next == (\E key \in Keys, payload \in Payloads : Create(key, payload) \/ Replay(key, payload))
        \/ (\E i \in Slots : Ambiguous(i) \/ NoCharge(i) \/ Capture(i)) \/ Close
TypeOK == /\ count \in 0..MaxIntents
          /\ status \in [Slots -> {"unused", "active", "ambiguous", "no_charge", "captured"}]
          /\ receipts \subseteq ReceiptType /\ responses \subseteq ReceiptType
          /\ closed \in BOOLEAN /\ countAtClose \in 0..MaxIntents
          /\ captures \subseteq Slots
OneLiveIntent == Cardinality(Active) <= 1
ImmutableKey == \A a,b \in receipts : a.key = b.key => a = b
BoundReplay == responses \subseteq receipts
ClosedCreationDenied == closed => count = countAtClose
CapturedEvidenceRetained == \A i \in captures : status[i] = "captured"
AtMostOneCapture == Cardinality(captures) <= 1
Spec == Init /\ [][Next]_vars
====
