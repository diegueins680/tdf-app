---- MODULE CaptureAccounting ----
(***************************************************************************)
(* PAY-CAPTURE-RECON-001. A provider capture handler (Domo, course, event   *)
(* ticket, marketplace, service storefront) races a concurrent hold        *)
(* expiry. The handler loads its context, captures remotely, checks the     *)
(* hold time, and finally admits the payment under the checkout lock.      *)
(*                                                                         *)
(* Reconcile = FALSE : before 2026-11-01, a rejected admission only        *)
(*                     returned an error.                                   *)
(* Reconcile = TRUE  : recordVerifiedPaymentOrReconcile leaves an open      *)
(*                     reconciliation exception in the same transaction.    *)
(*                                                                         *)
(* Clocks are modelled as perfect: the time check observes exactly whether *)
(* the checkout has expired. The race remains between the time check and   *)
(* the locked admission.                                                    *)
(***************************************************************************)

CONSTANTS
  \* @type: Bool;
  Reconcile

VARIABLES
  \* @type: Str;
  checkout,     \* "open" | "expired" | "paid"
  \* @type: Str;
  pc,           \* "idle" | "loaded" | "captured" | "admit" | "done"
  \* @type: Bool;
  captured,     \* the provider holds the customer's money
  \* @type: Bool;
  verified,     \* recorded as a verified checkout payment
  \* @type: Bool;
  exception     \* an open reconciliation exception exists

vars == <<checkout, pc, captured, verified, exception>>

Init ==
  /\ checkout = "open"
  /\ pc = "idle"
  /\ captured = FALSE
  /\ verified = FALSE
  /\ exception = FALSE

\* Another request or worker expires the hold (domo_quote_expire_holds and
\* its siblings). Only an unpaid checkout expires.
Expire ==
  /\ checkout = "open"
  /\ checkout' = "expired"
  /\ UNCHANGED <<pc, captured, verified, exception>>

Load ==
  /\ pc = "idle"
  /\ checkout = "open"
  /\ pc' = "loaded"
  /\ UNCHANGED <<checkout, captured, verified, exception>>

CaptureRemote ==
  /\ pc = "loaded"
  /\ captured' = TRUE
  /\ pc' = "captured"
  /\ UNCHANGED <<checkout, verified, exception>>

\* The handler's hold-time check: an expired hold records an exception.
TimeCheck ==
  /\ pc = "captured"
  /\ IF checkout = "expired"
       THEN /\ exception' = TRUE
            /\ pc' = "done"
       ELSE /\ pc' = "admit"
            /\ UNCHANGED exception
  /\ UNCHANGED <<checkout, captured, verified>>

\* recordVerifiedPayment under FOR UPDATE: only an open checkout is admitted.
Admit ==
  /\ pc = "admit"
  /\ IF checkout = "open"
       THEN /\ checkout' = "paid"
            /\ verified' = TRUE
            /\ UNCHANGED exception
       ELSE /\ exception' = IF Reconcile THEN TRUE ELSE exception
            /\ UNCHANGED <<checkout, verified>>
  /\ pc' = "done"
  /\ UNCHANGED captured

Next == Expire \/ Load \/ CaptureRemote \/ TimeCheck \/ Admit

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ checkout \in {"open", "expired", "paid"}
  /\ pc \in {"idle", "loaded", "captured", "admit", "done"}
  /\ captured \in BOOLEAN
  /\ verified \in BOOLEAN
  /\ exception \in BOOLEAN

(* Every captured payment ends verified or under reconciliation, and a     *)
(* verified payment belongs to a paid checkout.                             *)
NoSilentCapture == (pc = "done" /\ captured) => (verified \/ exception)
VerifiedIsPaid == verified => checkout = "paid"
Safe == NoSilentCapture /\ VerifiedIsPaid

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ captured <=> pc \in {"captured", "admit", "done"}
  /\ pc \in {"idle", "loaded", "captured", "admit"} => ~verified
  /\ checkout = "paid" => verified
  /\ Reconcile

CInitReconcile == Reconcile = TRUE
CInitLegacy == Reconcile = FALSE
====
