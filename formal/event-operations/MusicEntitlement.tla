---- MODULE MusicEntitlement ----
(* PAY-MUSIC-DOWNLOAD-001: one music purchase order, its checkout, the       *)
(* entitlement the checkout trigger grants, and download authorizations      *)
(* identified by client request identifiers. Time is a bounded counter; a    *)
(* signed URL is usable for Lifetime from the moment it is signed from. A    *)
(* replayed request signs from its original authorization, so its URL ends   *)
(* with the first one.                                                       *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Requests, Limit, Lifetime, MaxTime, Never,
          RequireEvidence, StatusChangeOnly, UpsertGrant,
          EnforceLimit, ReplayWindow, ActiveOnly, SignFromOriginal
VARIABLES checkout, evidence, order, grants, entitlement,
          now, authorized, lastIssued, urlExpiry, issuedInactive
vars == <<checkout, evidence, order, grants, entitlement,
          now, authorized, lastIssued, urlExpiry, issuedInactive>>

Init == /\ checkout = "awaiting" /\ evidence = FALSE /\ order = "awaiting"
        /\ grants = 0 /\ entitlement = "none" /\ now = 0
        /\ authorized = [r \in Requests |-> Never]
        /\ lastIssued = [r \in Requests |-> Never]
        /\ urlExpiry = [r \in Requests |-> Never]
        /\ issuedInactive = FALSE

downloads == <<authorized, lastIssued, urlExpiry, issuedInactive>>

(* The provider confirmed the payment server to server and the result was    *)
(* bound to the attempt.                                                     *)
Verify == /\ ~evidence /\ evidence' = TRUE
          /\ UNCHANGED <<checkout, order, grants, entitlement, now, downloads>>

Grant == /\ grants' = IF UpsertGrant /\ grants > 0 THEN grants ELSE grants + 1
         /\ entitlement' = "active"

(* The checkout becomes paid; a database trigger demands the evidence and    *)
(* another one settles the order and grants the entitlement.                 *)
MarkPaid == /\ checkout = "awaiting" /\ (RequireEvidence => evidence)
            /\ checkout' = "paid" /\ order' = "paid" /\ Grant
            /\ UNCHANGED <<evidence, now, downloads>>

(* The same verified payment recorded again: the checkout row is written     *)
(* without changing status.                                                  *)
RecordAgain == /\ checkout = "paid"
               /\ IF StatusChangeOnly THEN UNCHANGED <<grants, entitlement>> ELSE Grant
               /\ UNCHANGED <<checkout, evidence, order, now, downloads>>

Refund == /\ checkout = "paid" /\ checkout' = "refunded" /\ order' = "refunded"
          /\ entitlement' = IF entitlement = "active" THEN "refunded" ELSE entitlement
          /\ UNCHANGED <<evidence, grants, now, downloads>>

Tick == /\ now < MaxTime /\ now' = now + 1
        /\ UNCHANGED <<checkout, evidence, order, grants, entitlement, downloads>>

Counted == {r \in Requests : authorized[r] # Never}

(* The URL answered for r: signed from the request's (possibly just         *)
(* recorded) authorization, or from the current instant when the guard is    *)
(* removed, and usable until one Lifetime later.                             *)
Issue(r) == /\ lastIssued' = [lastIssued EXCEPT ![r] = now]
            /\ urlExpiry' = [urlExpiry EXCEPT ![r] =
                 (IF SignFromOriginal THEN authorized'[r] ELSE now) + Lifetime]
            /\ issuedInactive' = (issuedInactive \/ entitlement # "active")

(* One authorization request. A new identifier is counted against the limit; *)
(* a known one is answered again without counting.                           *)
Authorize(r) ==
  /\ entitlement # "none" /\ (ActiveOnly => entitlement = "active")
  /\ \/ /\ authorized[r] = Never
        /\ EnforceLimit => Cardinality(Counted) < Limit
        /\ authorized' = [authorized EXCEPT ![r] = now] /\ Issue(r)
     \/ /\ authorized[r] # Never
        /\ ReplayWindow => now < authorized[r] + Lifetime
        /\ UNCHANGED authorized /\ Issue(r)
  /\ UNCHANGED <<checkout, evidence, order, grants, entitlement, now>>

Next == Verify \/ MarkPaid \/ RecordAgain \/ Refund \/ Tick \/ \E r \in Requests : Authorize(r)
Spec == Init /\ [][Next]_vars

Times == (0..MaxTime) \cup {Never}
TypeOK == /\ checkout \in {"awaiting", "paid", "refunded"} /\ evidence \in BOOLEAN
          /\ order \in {"awaiting", "paid", "refunded"} /\ grants \in 0..3
          /\ entitlement \in {"none", "active", "refunded"} /\ now \in 0..MaxTime
          /\ authorized \in [Requests -> Times] /\ lastIssued \in [Requests -> Times]
          /\ urlExpiry \in [Requests -> (0..(MaxTime + Lifetime)) \cup {Never}]
          /\ issuedInactive \in BOOLEAN

(* Paid only on verified provider evidence.                                  *)
PaidNeedsEvidence == checkout # "awaiting" => evidence
(* One entitlement per order, active only while the order is paid.           *)
OneEntitlement == /\ grants <= 1
                  /\ entitlement = "active" => order = "paid" /\ checkout = "paid"
                  /\ order = "paid" => entitlement = "active"
(* Counted downloads never exceed the entitlement limit.                     *)
LimitHolds == Cardinality(Counted) <= Limit
(* Every URL is issued inside the lifetime of its counted authorization.     *)
UrlsInsideLifetime ==
  \A r \in Requests : lastIssued[r] # Never => lastIssued[r] < authorized[r] + Lifetime
(* Every URL ends with its counted authorization: a replay never extends     *)
(* access beyond one lifetime.                                               *)
UrlsEndWithAuthorization ==
  \A r \in Requests : urlExpiry[r] # Never => urlExpiry[r] = authorized[r] + Lifetime
(* No URL is issued for an entitlement that is not active.                   *)
ActiveWhenIssued == ~issuedInactive
====
