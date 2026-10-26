---------------------------- MODULE LedgerHeader ----------------------------
(* PAY-LEDGER-HEADER-001: one transaction, two abstract header values and two
   queued writers. A writer may retain its draft-era proposal across posting.
   Posting is atomic and the existing lifecycle never reopens a draft. Trusted
   triggers cannot be disabled by the actor. No SQL refinement/liveness claim. *)
EXTENDS Integers
CONSTANTS Writers, FreezePostedHeader
VARIABLES status, header, frozen, pending
vars == <<status, header, frozen, pending>>

Init ==
  /\ status = "draft"
  /\ header \in {0, 1}
  /\ frozen = -1
  /\ pending = [w \in Writers |-> -1]

Queue(w) ==
  /\ pending[w] = -1
  /\ pending' = [pending EXCEPT ![w] = 1 - header]
  /\ UNCHANGED <<status, header, frozen>>

Write(w) ==
  /\ pending[w] # -1
  /\ status = "draft" \/ ~FreezePostedHeader \/ pending[w] = header
  /\ header' = pending[w]
  /\ pending' = [pending EXCEPT ![w] = -1]
  /\ UNCHANGED <<status, frozen>>

Reject(w) ==
  /\ pending[w] # -1
  /\ status # "draft" /\ FreezePostedHeader /\ pending[w] # header
  /\ pending' = [pending EXCEPT ![w] = -1]
  /\ UNCHANGED <<status, header, frozen>>

Post ==
  /\ status = "draft"
  /\ status' = "posted"
  /\ frozen' = header
  /\ UNCHANGED pending
  /\ UNCHANGED header

Reverse ==
  /\ status = "posted"
  /\ status' = "reversed"
  /\ UNCHANGED <<header, frozen, pending>>

Next == (\E w \in Writers : Queue(w) \/ Write(w) \/ Reject(w)) \/ Post \/ Reverse
TypeOK ==
  /\ status \in {"draft", "posted", "reversed"}
  /\ header \in {0, 1}
  /\ frozen \in {-1, 0, 1}
  /\ pending \in [Writers -> {-1, 0, 1}]
PostedHeaderFrozen == status # "draft" => header = frozen
Spec == Init /\ [][Next]_vars
=============================================================================
