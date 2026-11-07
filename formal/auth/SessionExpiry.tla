---- MODULE SessionExpiry ----
(***************************************************************************)
(* ID-SESSION-004. Bearer tokens in api_token and their validity window on  *)
(* the database clock (TDF.Auth.apiTokenWithinValidity,                     *)
(* TDF.ServerAuth.sessionValidityWindow, TDF.Config.validateSessionTokenTtl).*)
(*                                                                         *)
(* kind  interactive : password-login / google-login sessions              *)
(*       service     : service or custom-label tokens (may be unbounded)   *)
(*       reset       : password-reset tokens (never authenticate)          *)
(* expires = -1 means no expiry. Interactive sessions minted by login get  *)
(* expires = issued + max(1, Ttl); legacy interactive sessions that predate *)
(* enforcement exist from the start with no expiry and are not backfilled. *)
(* The database clock may move arbitrarily, including backwards.           *)
(*                                                                         *)
(* FailClosed = TRUE  : an interactive token without expiry is rejected    *)
(*                      (current code).                                    *)
(* FailClosed = FALSE : a token without expiry is accepted (negative       *)
(*                      control: the behaviour before enforcement).        *)
(* TtlChecked = TRUE  : startup refuses Ttl > CookieMaxAge (current code). *)
(* TtlChecked = FALSE : any positive Ttl (negative control).               *)
(***************************************************************************)
EXTENDS Integers

CONSTANTS
  \* @type: Bool;
  FailClosed,
  \* @type: Bool;
  TtlChecked,
  \* @type: Int;
  CookieMaxAge,
  \* @type: Int;
  Ttl,
  \* @type: Int;
  MaxTime,
  \* @type: Set(Int);
  TimeDomain,   \* values a clock reading is drawn from (TLC: 0..MaxTime; Apalache: Int)
  \* @type: Set(Int);
  Tokens,
  \* @type: Set(Int);
  Legacy

VARIABLES
  \* @type: Int;
  now,
  \* @type: Int -> Str;
  state,     \* absent | active | revoked
  \* @type: Int -> Str;
  kind,      \* interactive | service | reset
  \* @type: Int -> Int;
  issued,
  \* @type: Int -> Int;
  expires,   \* -1: no expiry
  \* @type: Int -> Int;
  lastUse    \* clock value of the last accepted use (-1: never)

vars == <<now, state, kind, issued, expires, lastUse>>

\* The configuration startup admits (validateSessionTokenTtl).
ConfigAdmitted == Ttl >= 1 /\ (TtlChecked => Ttl <= CookieMaxAge)

InTime(c) == c \in TimeDomain /\ 0 <= c /\ c <= MaxTime

Init ==
  /\ \E c \in TimeDomain : InTime(c) /\ now = c
  /\ state = [t \in Tokens |-> IF t \in Legacy THEN "active" ELSE "absent"]
  /\ kind = [t \in Tokens |-> IF t \in Legacy THEN "interactive" ELSE "service"]
  /\ issued = [t \in Tokens |-> -1]
  /\ expires = [t \in Tokens |-> -1]
  /\ lastUse = [t \in Tokens |-> -1]

\* The database clock moves to any value (NTP steps, replica differences).
Clock ==
  /\ \E c \in TimeDomain : InTime(c) /\ now' = c
  /\ UNCHANGED <<state, kind, issued, expires, lastUse>>

\* Password or Google login: sessionValidityWindow on the database clock.
MintInteractive(t) ==
  /\ state[t] = "absent"
  /\ state' = [state EXCEPT ![t] = "active"]
  /\ kind' = [kind EXCEPT ![t] = "interactive"]
  /\ issued' = [issued EXCEPT ![t] = now]
  /\ expires' = [expires EXCEPT ![t] = now + (IF Ttl >= 1 THEN Ttl ELSE 1)]
  /\ UNCHANGED <<now, lastUse>>

\* Service tokens (with or without expiry) and password-reset tokens (900 s).
MintOther(t) ==
  /\ state[t] = "absent"
  /\ \E k \in {"service", "reset"}, e \in TimeDomain \cup {-1} :
       /\ (e = -1 \/ InTime(e))
       /\ kind' = [kind EXCEPT ![t] = k]
       /\ expires' = [expires EXCEPT ![t] = IF k = "reset" THEN now + 900 ELSE e]
  /\ state' = [state EXCEPT ![t] = "active"]
  /\ issued' = [issued EXCEPT ![t] = now]
  /\ UNCHANGED <<now, lastUse>>

Revoke(t) ==
  /\ state[t] = "active"
  /\ state' = [state EXCEPT ![t] = "revoked"]
  /\ UNCHANGED <<now, kind, issued, expires, lastUse>>

WithinValidity(t) ==
  IF expires[t] /= -1 THEN now < expires[t]
  ELSE ~(FailClosed /\ kind[t] = "interactive")

Accepted(t) == state[t] = "active" /\ kind[t] /= "reset" /\ WithinValidity(t)

\* A request authenticates with token t (lookupUsernameFromToken and the
\* AuthProtect handler): only accepted tokens record a use.
Use(t) ==
  /\ Accepted(t)
  /\ lastUse' = [lastUse EXCEPT ![t] = now]
  /\ UNCHANGED <<now, state, kind, issued, expires>>

Next ==
  \/ Clock
  \/ \E t \in Tokens : MintInteractive(t) \/ MintOther(t) \/ Revoke(t) \/ Use(t)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ InTime(now)
  /\ state \in [Tokens -> {"absent", "active", "revoked"}]
  /\ kind \in [Tokens -> {"interactive", "service", "reset"}]
  /\ issued \in [Tokens -> Int]
  /\ expires \in [Tokens -> Int]
  /\ lastUse \in [Tokens -> Int]

(* Every accepted use of an interactive session fell inside its window.    *)
WithinWindowAtUse ==
  \A t \in Tokens :
    (kind[t] = "interactive" /\ lastUse[t] /= -1) => (expires[t] /= -1 /\ lastUse[t] < expires[t])

(* A legacy interactive session without an expiry is never accepted.       *)
LegacyInteractiveFailsClosed ==
  \A t \in Tokens : (kind[t] = "interactive" /\ expires[t] = -1) => lastUse[t] = -1

(* Every interactive window is positive and no longer than the cookie.     *)
TtlBoundedByCookie ==
  \A t \in Tokens :
    (kind[t] = "interactive" /\ expires[t] /= -1)
      => (expires[t] - issued[t] >= 1 /\ expires[t] - issued[t] <= CookieMaxAge)

(* Reset tokens never authenticate.                                        *)
ResetNeverAuthenticates == \A t \in Tokens : kind[t] = "reset" => lastUse[t] = -1

Safe == WithinWindowAtUse /\ LegacyInteractiveFailsClosed /\ TtlBoundedByCookie /\ ResetNeverAuthenticates

(* Reachability witness (expected to be violated): a freshly minted         *)
(* interactive session is accepted, so the proofs are not vacuous.          *)
NoMintedSessionUsed ==
  \A t \in Tokens : ~(kind[t] = "interactive" /\ expires[t] /= -1 /\ lastUse[t] /= -1)

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ FailClosed
  /\ TtlChecked
  /\ ConfigAdmitted
  /\ \A t \in Tokens :
       /\ state[t] = "absent" => (lastUse[t] = -1 /\ expires[t] = -1 /\ kind[t] /= "interactive")
       /\ (kind[t] = "interactive" /\ expires[t] /= -1) => expires[t] = issued[t] + Ttl

CInitCurrent ==
  /\ FailClosed = TRUE
  /\ TtlChecked = TRUE
  /\ CookieMaxAge \in 1..100000
  /\ Ttl \in 1..100000
  /\ Ttl <= CookieMaxAge
  /\ MaxTime \in 0..1000000
  /\ TimeDomain = Int
  /\ Tokens \in SUBSET (1..4) /\ Tokens /= {}
  /\ Legacy \in SUBSET Tokens

CInitNotFailClosed ==
  /\ FailClosed = FALSE
  /\ TtlChecked = TRUE
  /\ CookieMaxAge = 10
  /\ Ttl = 10
  /\ MaxTime = 3
  /\ TimeDomain = 0..3
  /\ Tokens = {1}
  /\ Legacy = {1}

====
