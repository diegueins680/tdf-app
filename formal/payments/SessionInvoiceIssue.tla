---- MODULE SessionInvoiceIssue ----
(***************************************************************************)
(* PAY-INVOICE-ISSUE-001. Requests to POST /invoices/{sessionId}/generate *)
(* for one session and one request fingerprint, each with an idempotency  *)
(* key, issue an SRI electronic invoice through a portal that is not      *)
(* idempotent (TDF.Invoice.SessionIssue, generateInvoiceForSession).      *)
(*                                                                         *)
(* A key's row in session_invoice_issue is                                 *)
(*   none       no reservation (never made, or released before SRI)        *)
(*   reserved   reserved, no stored response yet                           *)
(*   settled    answered; SRI not requested or reported "issued"           *)
(*   unsettled  answered with an SRI error or another status: the portal   *)
(*              may have issued the document                               *)
(* A row that is reserved or unsettled is an unresolved attempt.          *)
(*                                                                         *)
(* A request with an existing key replays or is refused (no SRI). A new   *)
(* key is refused while another key's attempt is unresolved, unless staff *)
(* verified the portal (sriPortalVerified). The reservation is an INSERT  *)
(* ... ON CONFLICT DO NOTHING; a reservation is released only before the  *)
(* SRI script starts. A request may stop at any point (crash).            *)
(*                                                                         *)
(* Lock = TRUE  : the sibling check and the reservation run in one        *)
(*                transaction after pg_advisory_xact_lock(session,        *)
(*                fingerprint) (reserveIssueGuarded with issueAdvisoryLock)*)
(* Lock = FALSE : the check and the reservation are separate steps         *)
(*                (negative control: the race found in review).           *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  Lock,
  \* @type: Set(Int);
  Requests,
  \* @type: Set(Int);
  Keys

VARIABLES
  \* @type: Int -> Str;
  row,          \* key -> none | reserved | settled | unsettled
  \* @type: Int -> Bool;
  rowVerified,  \* the reservation was made with portal verification
  \* @type: Int -> Int;
  runs,         \* SRI script starts under each key
  \* @type: Int -> Str;
  pc,           \* idle | checked | reserved | sri | done
  \* @type: Int -> Int;
  key,          \* the request's key (0 before it arrives)
  \* @type: Int -> Bool;
  verified      \* the request carries sriPortalVerified = true

vars == <<row, rowVerified, runs, pc, key, verified>>

Unresolved(k) == row[k] \in {"reserved", "unsettled"}

SiblingUnresolved(k) == \E k2 \in Keys : k2 /= k /\ Unresolved(k2)

\* The guard applies only to a new key and only without portal verification.
Refused(k, v) == ~v /\ SiblingUnresolved(k)

Init ==
  /\ row = [k \in Keys |-> "none"]
  /\ rowVerified = [k \in Keys |-> FALSE]
  /\ runs = [k \in Keys |-> 0]
  /\ pc = [r \in Requests |-> "idle"]
  /\ key = [r \in Requests |-> 0]
  /\ verified = [r \in Requests |-> FALSE]

Reserve(r, k, v) ==
  /\ row' = [row EXCEPT ![k] = "reserved"]
  /\ rowVerified' = [rowVerified EXCEPT ![k] = v]
  /\ pc' = [pc EXCEPT ![r] = "reserved"]

\* Lock = TRUE: lock, look up the key, check siblings and reserve atomically.
Arrive(r, k, v) ==
  /\ Lock
  /\ pc[r] = "idle"
  /\ key' = [key EXCEPT ![r] = k]
  /\ verified' = [verified EXCEPT ![r] = v]
  /\ IF row[k] /= "none" \/ Refused(k, v)
       THEN /\ pc' = [pc EXCEPT ![r] = "done"]   \* replay, 409 in progress, or 409 sibling
            /\ UNCHANGED <<row, rowVerified>>
       ELSE Reserve(r, k, v)
  /\ UNCHANGED runs

\* Lock = FALSE: the lookup and sibling check commit before the reservation.
Check(r, k, v) ==
  /\ ~Lock
  /\ pc[r] = "idle"
  /\ key' = [key EXCEPT ![r] = k]
  /\ verified' = [verified EXCEPT ![r] = v]
  /\ pc' = [pc EXCEPT ![r] = IF row[k] /= "none" \/ Refused(k, v) THEN "done" ELSE "checked"]
  /\ UNCHANGED <<row, rowVerified, runs>>

\* INSERT ... ON CONFLICT DO NOTHING: the loser observes the winner's row.
ReserveAfterCheck(r) ==
  /\ pc[r] = "checked"
  /\ IF row[key[r]] /= "none"
       THEN /\ pc' = [pc EXCEPT ![r] = "done"]
            /\ UNCHANGED <<row, rowVerified>>
       ELSE Reserve(r, key[r], verified[r])
  /\ UNCHANGED <<runs, key, verified>>

\* Creating or linking the invoice failed before SRI: releaseIssue.
Release(r) ==
  /\ pc[r] = "reserved"
  /\ row' = [row EXCEPT ![key[r]] = "none"]
  /\ rowVerified' = [rowVerified EXCEPT ![key[r]] = FALSE]
  /\ pc' = [pc EXCEPT ![r] = "done"]
  /\ UNCHANGED <<runs, key, verified>>

StartSri(r) ==
  /\ pc[r] = "reserved"
  /\ runs' = [runs EXCEPT ![key[r]] = @ + 1]
  /\ pc' = [pc EXCEPT ![r] = "sri"]
  /\ UNCHANGED <<row, rowVerified, key, verified>>

\* completeIssue stores the response; the portal outcome decides settlement.
Complete(r, outcome) ==
  /\ pc[r] = "sri"
  /\ row' = [row EXCEPT ![key[r]] = outcome]
  /\ pc' = [pc EXCEPT ![r] = "done"]
  /\ UNCHANGED <<rowVerified, runs, key, verified>>

Next ==
  \E r \in Requests :
    \/ \E k \in Keys, v \in BOOLEAN : Arrive(r, k, v) \/ Check(r, k, v)
    \/ ReserveAfterCheck(r)
    \/ Release(r)
    \/ StartSri(r)
    \/ \E outcome \in {"settled", "unsettled"} : Complete(r, outcome)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ row \in [Keys -> {"none", "reserved", "settled", "unsettled"}]
  /\ rowVerified \in [Keys -> BOOLEAN]
  /\ runs \in [Keys -> Nat]
  /\ pc \in [Requests -> {"idle", "checked", "reserved", "sri", "done"}]
  /\ key \in [Requests -> Keys \cup {0}]
  /\ verified \in [Requests -> BOOLEAN]

(* The SRI script runs at most once per key.                               *)
OneRunPerKey == \A k \in Keys : runs[k] <= 1

(* Two attempts that may each have issued the document are never open at  *)
(* the same time unless one of them was made after verifying the portal.   *)
NoBlindDuplicate ==
  \A k1, k2 \in Keys :
    (k1 /= k2 /\ Unresolved(k1) /\ Unresolved(k2)) => (rowVerified[k1] \/ rowVerified[k2])

Safe == OneRunPerKey /\ NoBlindDuplicate

Owns(r) == pc[r] \in {"reserved", "sri"}

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Lock
  /\ \A r \in Requests : pc[r] /= "checked"
  /\ \A r \in Requests : Owns(r) =>
       /\ key[r] \in Keys
       /\ row[key[r]] = "reserved"
       /\ rowVerified[key[r]] = verified[r]
       /\ runs[key[r]] = IF pc[r] = "sri" THEN 1 ELSE 0
  /\ \A r1, r2 \in Requests : (r1 /= r2 /\ Owns(r1) /\ Owns(r2)) => key[r1] /= key[r2]
  /\ \A k \in Keys : row[k] = "none" => runs[k] = 0
  /\ \A k \in Keys : row[k] = "reserved" => \E r \in Requests : Owns(r) /\ key[r] = k

CInitLock ==
  /\ Lock = TRUE
  /\ Requests \in SUBSET (1..4) /\ Requests /= {}
  /\ Keys \in SUBSET (1..4) /\ Keys /= {}

CInitNoLock ==
  /\ Lock = FALSE
  /\ Requests = {1, 2}
  /\ Keys = {1, 2}

====
