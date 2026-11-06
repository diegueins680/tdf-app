---- MODULE LogoutIntent ----
(***************************************************************************)
(* AUTH-LOGOUT-001. Browser logout intent (tdf-hq-ui/src/session/           *)
(* logoutIntent.ts). An explicit logout begins an intent with a fresh      *)
(* revision; the server call finishes it as confirmed or unconfirmed; only *)
(* fresh interactive authentication clears it, again with a fresh revision. *)
(* Auth callbacks and server responses carry the revision they started     *)
(* with and may arrive at any time, including after a later begin/clear.    *)
(*                                                                         *)
(* Fence = TRUE : finishLogoutIntent writes only when its revision is the   *)
(*                current one (the code).                                   *)
(* Fence = FALSE: any finish writes (negative control).                     *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  Fence,
  \* @type: Int;
  MaxRevisions

VARIABLES
  \* @type: Int;
  revision,     \* current revision (0 = initial)
  \* @type: Str;
  status,       \* cleared | pending | unconfirmed | confirmed
  \* @type: Int;
  issued,       \* revisions issued so far
  \* @type: Set(Int);
  inFlight,     \* revisions whose finish callback has not arrived yet
  \* @type: Bool;
  loggedOutSinceAuth  \* ghost: an explicit logout happened after the last fresh auth

vars == <<revision, status, issued, inFlight, loggedOutSinceAuth>>

HasIntent == status /= "cleared"

Init ==
  /\ revision = 0
  /\ status = "cleared"
  /\ issued = 0
  /\ inFlight = {}
  /\ loggedOutSinceAuth = FALSE

\* beginLogoutIntent: fresh revision, pending, callback in flight.
Begin ==
  /\ issued < MaxRevisions
  /\ issued' = issued + 1
  /\ revision' = issued + 1
  /\ status' = "pending"
  /\ inFlight' = inFlight \cup {issued + 1}
  /\ loggedOutSinceAuth' = TRUE

\* finishLogoutIntent(r, confirmed): a callback for revision r arrives.
Finish(r, confirmed) ==
  /\ r \in inFlight
  /\ inFlight' = inFlight \ {r}
  /\ IF ~Fence \/ r = revision
       THEN status' = IF confirmed THEN "confirmed" ELSE "unconfirmed"
       ELSE UNCHANGED status
  /\ UNCHANGED <<revision, issued, loggedOutSinceAuth>>

\* clearLogoutIntent: fresh interactive authentication; fresh revision so a
\* stale pre-logout callback can never own the new login.
Clear ==
  /\ issued < MaxRevisions
  /\ issued' = issued + 1
  /\ revision' = issued + 1
  /\ status' = "cleared"
  /\ loggedOutSinceAuth' = FALSE
  /\ UNCHANGED inFlight

Next == Begin \/ Clear \/ \E r \in inFlight, c \in BOOLEAN : Finish(r, c)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ revision \in Nat
  /\ status \in {"cleared", "pending", "unconfirmed", "confirmed"}
  /\ issued \in Nat
  /\ inFlight \in SUBSET (1..8)  \* literal bound for Apalache; MaxRevisions <= 8 in CInit
  /\ \A r \in inFlight : r <= issued
  /\ loggedOutSinceAuth \in BOOLEAN

(* Restoration is suppressed from an explicit logout until fresh auth, and  *)
(* no callback can mark a fresh login as logged out.                        *)
Suppressed == loggedOutSinceAuth <=> HasIntent

(* A revision is never reissued. *)
FreshRevisions == revision <= issued

Safe == Suppressed /\ FreshRevisions

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Fence
  /\ revision \in 0..issued
  /\ (revision = 0 => status = "cleared")
  \* The current revision awaits its callback exactly while the intent is pending.
  /\ (revision \in inFlight) <=> (status = "pending")

CInitFence == Fence = TRUE /\ MaxRevisions \in 1..8
CInitLegacy == Fence = FALSE /\ MaxRevisions = 3
====
