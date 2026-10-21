---- MODULE InvoiceSubmission ----
(* EVT-TICKET-INVOICE-001: one tax document handled by leased workers and    *)
(* one external provider. The row holds status, the persisted                *)
(* submission-started mark and the lease; each worker step is one committed  *)
(* statement or one provider request.                                        *)
EXTENDS Naturals, FiniteSets, TLC
CONSTANTS Workers, MaxLeases, MaxRetries,
          MarkFenced, FinishFenced,
          UnknownIsUncertain, RetryOnlyRefused
VARIABLES status, started, lease, expired, issued,
          pc, token, seenStatus, seenStarted, result,
          provider, created, retries, everAuthorized
vars == <<status, started, lease, expired, issued, pc, token, seenStatus,
          seenStarted, result, provider, created, retries, everAuthorized>>

Statuses == {"pending", "submitted", "authorized", "rejected", "uncertain", "failed"}
Results == {"authorized", "processing", "rejected", "refused", "unknown"}

Init == /\ status = "pending" /\ started = FALSE /\ lease = 0 /\ expired = FALSE
        /\ issued = 0
        /\ pc = [w \in Workers |-> "idle"] /\ token = [w \in Workers |-> 0]
        /\ seenStatus = [w \in Workers |-> "pending"]
        /\ seenStarted = [w \in Workers |-> FALSE]
        /\ result = [w \in Workers |-> "unknown"]
        /\ provider = "none" /\ created = 0 /\ retries = 0 /\ everAuthorized = FALSE

row == <<status, started, lease, expired>>
world == <<provider, created, retries>>

(* Claim a due document whose lease is absent or expired.                    *)
Claim(w) ==
  /\ pc[w] = "idle" /\ status \in {"pending", "submitted"}
  /\ lease = 0 \/ expired
  /\ issued < MaxLeases
  /\ issued' = issued + 1 /\ lease' = issued + 1 /\ expired' = FALSE
  /\ token' = [token EXCEPT ![w] = issued + 1]
  /\ seenStatus' = [seenStatus EXCEPT ![w] = status]
  /\ seenStarted' = [seenStarted EXCEPT ![w] = started]
  /\ pc' = [pc EXCEPT ![w] = "claimed"]
  /\ UNCHANGED <<status, started, result, world, everAuthorized>>

Expire == /\ lease # 0 /\ ~expired /\ expired' = TRUE
          /\ UNCHANGED <<status, started, lease, issued, pc, token, seenStatus,
                         seenStarted, result, world, everAuthorized>>

(* Lease-fenced completion: a stale worker's result is discarded.            *)
Write(w, next) ==
  IF FinishFenced => lease = token[w]
    THEN /\ status' = next /\ lease' = 0 /\ expired' = FALSE
    ELSE UNCHANGED <<status, lease, expired>>

Decide(w) ==
  /\ pc[w] = "claimed"
  /\ \/ /\ seenStatus[w] = "submitted"
        /\ pc' = [pc EXCEPT ![w] = "polling"]
        /\ UNCHANGED <<row, everAuthorized>>
     \/ /\ seenStatus[w] = "pending" /\ seenStarted[w]
        (* An earlier submission may have reached the provider. The mark     *)
        (* below already prevents a resend; this makes the stall visible.    *)
        /\ Write(w, "uncertain") /\ UNCHANGED <<started, everAuthorized>>
        /\ pc' = [pc EXCEPT ![w] = "idle"]
     \/ /\ seenStatus[w] = "pending" /\ ~seenStarted[w]
        (* Persist that a request is about to be sent; only then send it.    *)
        /\ IF MarkFenced => (lease = token[w] /\ ~started)
             THEN started' = TRUE /\ pc' = [pc EXCEPT ![w] = "posting"]
             ELSE started' = started /\ pc' = [pc EXCEPT ![w] = "idle"]
        /\ UNCHANGED <<status, lease, expired, everAuthorized>>
  /\ UNCHANGED <<issued, token, seenStatus, seenStarted, result, world>>

(* The provider request. A refusal creates nothing; an unknown outcome may   *)
(* or may not have created the document.                                     *)
Post(w) ==
  /\ pc[w] = "posting"
  /\ \E r \in Results :
       /\ result' = [result EXCEPT ![w] = r]
       /\ \/ r = "refused" /\ UNCHANGED <<provider, created>>
          \/ r = "unknown" /\ UNCHANGED <<provider, created>>
          \/ /\ r # "refused"
             /\ created' = created + 1
             /\ provider' = IF r = "unknown" THEN "processing" ELSE r
  /\ pc' = [pc EXCEPT ![w] = "finishing"]
  /\ UNCHANGED <<row, issued, token, seenStatus, seenStarted, retries, everAuthorized>>

Poll(w) ==
  /\ pc[w] = "polling"
  /\ \E r \in {provider, "unknown"} :
       result' = [result EXCEPT ![w] = IF r = "none" THEN "unknown" ELSE r]
  /\ pc' = [pc EXCEPT ![w] = "finishing"]
  /\ UNCHANGED <<row, issued, token, seenStatus, seenStarted, world, everAuthorized>>

Finish(w) ==
  /\ pc[w] = "finishing"
  /\ LET polled == seenStatus[w] = "submitted"
         next == CASE result[w] = "authorized" -> "authorized"
                   [] result[w] = "processing" -> "submitted"
                   [] result[w] = "rejected" -> "rejected"
                   [] result[w] = "refused" -> IF polled THEN "submitted" ELSE "failed"
                   [] OTHER -> IF polled THEN "submitted"
                               ELSE IF UnknownIsUncertain THEN "uncertain" ELSE "failed"
     IN /\ Write(w, next)
        /\ everAuthorized' = (everAuthorized \/ status' = "authorized")
  /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<started, issued, token, seenStatus, seenStarted, result, world>>

(* The worker process stops at any point; nothing further is written.        *)
Crash(w) == /\ pc[w] # "idle" /\ pc' = [pc EXCEPT ![w] = "idle"]
            /\ UNCHANGED <<row, issued, token, seenStatus, seenStarted, result,
                           world, everAuthorized>>

(* An administrator resends a document the provider refused.                 *)
AdminRetry ==
  /\ retries < MaxRetries
  /\ status = "failed" \/ (~RetryOnlyRefused /\ status = "uncertain")
  /\ status' = "pending" /\ started' = FALSE /\ retries' = retries + 1
  /\ UNCHANGED <<lease, expired, issued, pc, token, seenStatus, seenStarted,
                 result, provider, created, everAuthorized>>

ProviderSettles ==
  /\ provider = "processing" /\ provider' \in {"authorized", "rejected"}
  /\ UNCHANGED <<row, issued, pc, token, seenStatus, seenStarted, result,
                 created, retries, everAuthorized>>

Next == \/ Expire \/ AdminRetry \/ ProviderSettles
        \/ \E w \in Workers : Claim(w) \/ Decide(w) \/ Post(w) \/ Poll(w)
                              \/ Finish(w) \/ Crash(w)

TypeOK == /\ status \in Statuses /\ started \in BOOLEAN /\ lease \in 0..MaxLeases
          /\ created \in Nat /\ retries \in 0..MaxRetries

(* The provider never holds two documents for one invoice.                   *)
AtMostOneProviderDocument == created <= 1

(* An authorized document is never reopened or overwritten.                  *)
AuthorizedIsFinal == everAuthorized => status = "authorized"

(* The recorded authorization is the provider's.                             *)
AuthorizedIsReal == status = "authorized" => provider = "authorized"

Spec == Init /\ [][Next]_vars
====
