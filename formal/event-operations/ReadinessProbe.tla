---------------------------- MODULE ReadinessProbe ----------------------------
(* Database readiness probe over a connection pool (DEPLOY-READINESS-001):
   TDF.App.Readiness.databaseReady, one GET /health.

   The pool holds up to PoolSize idle connections. After PostgreSQL closes
   its sessions (restart, failover, terminated backends) some of them are
   stale: the first use fails and the pool destroys that connection. A probe
   attempt takes one connection: a stale one fails and is discarded; a live
   one, or a new one when the database is up, answers; when the database is
   down a new connection cannot be made and the attempt fails. The probe may
   make at most Attempts attempts (the code: the pool size plus one).

   Guard: Attempts > PoolSize. Negative configurations set Attempts to one
   (the previous code) and to the pool size itself (every pooled connection
   stale leaves no attempt for a live one); both fail ProbeReportsDatabase. *)
EXTENDS Naturals

CONSTANTS PoolSize, Attempts

VARIABLES stale, up, tries, result

None == "none"

Init ==
  /\ stale \in 0..PoolSize
  /\ up \in BOOLEAN
  /\ tries = 0
  /\ result = None

(* One attempt: a stale pooled connection fails and is destroyed; otherwise
   the answer is the database's state. The last attempt failing on a stale
   connection reports unavailable. *)
Attempt ==
  /\ result = None
  /\ tries < Attempts
  /\ tries' = tries + 1
  /\ IF stale > 0
       THEN /\ stale' = stale - 1
            /\ result' = (IF tries + 1 = Attempts THEN "unavailable" ELSE None)
       ELSE /\ stale' = stale
            /\ result' = (IF up THEN "ready" ELSE "unavailable")
  /\ UNCHANGED up

Done == result # None /\ UNCHANGED <<stale, up, tries, result>>

Next == Attempt \/ Done

Spec == Init /\ [][Next]_<<stale, up, tries, result>>

(* The probe never spends more than its attempts. *)
AttemptsBounded == tries <= Attempts

(* The answer is the database's state: ready exactly when PostgreSQL is up.
   Fails when Attempts <= PoolSize (a probe can be defeated by stale
   connections alone). *)
ProbeReportsDatabase ==
  result # None => (result = "ready") = up

=============================================================================
