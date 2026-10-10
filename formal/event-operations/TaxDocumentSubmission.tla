------------------------- MODULE TaxDocumentSubmission -------------------------
(* EVT-TICKET-INVOICE-001: the Datil worker never resends a submission whose  *)
(* outcome is unknown. One commerce_tax_document, two workers, expiring       *)
(* leases, crashes and three provider outcomes. Bounded design abstraction of *)
(* TDF.Invoice.Datil (claimSql, markSubmissionStarted, applyResult, finish)   *)
(* and retryFailedTicketTaxDocument; not a refinement proof. Each action is   *)
(* one committed transaction or one provider exchange.                        *)
EXTENDS Naturals
CONSTANTS
  MaxTokens, MaxRetries,
  \* Controlled mutations; the positive configuration sets all to FALSE.
  SkipStartMark,     \* POST without first persisting submitted_at
  UnguardedStart,    \* start mark trusts the claim-time snapshot, not submitted_at IS NULL
  LostAsRejected,    \* a created-but-unanswered POST is treated as refused
  RetryUncertain     \* staff "retry" also resets uncertain documents

Workers == {1, 2}
VARIABLES status, started, rowTok, expired, tok, held, pc, docs, retries
vars == <<status, started, rowTok, expired, tok, held, pc, docs, retries>>

Init ==
  /\ status = "pending" /\ started = FALSE /\ rowTok = 0 /\ expired = FALSE
  /\ tok = 0 /\ held = [w \in Workers |-> 0] /\ pc = [w \in Workers |-> "idle"]
  /\ docs = 0 /\ retries = 0

Fenced(w) == held[w] = rowTok

\* Lease-fenced completion; clears the lease like finish.
Finish(w, s) ==
  IF Fenced(w)
  THEN /\ status' = s /\ rowTok' = 0 /\ expired' = FALSE
  ELSE UNCHANGED <<status, rowTok, expired>>

Claim(w) ==
  /\ pc[w] = "idle" /\ status \in {"pending", "submitted"} /\ tok < MaxTokens
  /\ (rowTok = 0 \/ expired)
  /\ tok' = tok + 1 /\ rowTok' = tok + 1 /\ expired' = FALSE
  /\ held' = [held EXCEPT ![w] = tok + 1] /\ pc' = [pc EXCEPT ![w] = "claimed"]
  /\ UNCHANGED <<status, started, docs, retries>>

ExpireLease == /\ rowTok # 0 /\ ~expired /\ expired' = TRUE
               /\ UNCHANGED <<status, started, rowTok, tok, held, pc, docs, retries>>

\* A pending document whose submission already started is never resent.
MarkUncertain(w) ==
  /\ pc[w] = "claimed" /\ status = "pending" /\ started
  /\ Finish(w, "uncertain") /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<started, tok, held, docs, retries>>

StartSubmission(w) ==
  /\ pc[w] = "claimed" /\ status = "pending" /\ (~started \/ UnguardedStart)
  /\ IF SkipStartMark
     THEN /\ pc' = [pc EXCEPT ![w] = "posting"] /\ UNCHANGED started
     ELSE IF Fenced(w)
          THEN /\ started' = TRUE /\ pc' = [pc EXCEPT ![w] = "posting"]
          ELSE IF UnguardedStart
               THEN /\ started' = TRUE /\ pc' = [pc EXCEPT ![w] = "posting"]
               ELSE /\ UNCHANGED started /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<status, rowTok, expired, tok, held, docs, retries>>

\* The provider creates the document and the answer arrives.
PostAnswered(w) ==
  /\ pc[w] = "posting" /\ docs' = docs + 1
  /\ Finish(w, "authorized") /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<started, tok, held, retries>>

\* The provider creates the document but the answer is lost (5xx, timeout).
PostLost(w) ==
  /\ pc[w] = "posting" /\ docs' = docs + 1
  /\ Finish(w, IF LostAsRejected THEN "failed" ELSE "uncertain")
  /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<started, tok, held, retries>>

\* A 4xx refusal: nothing was created.
PostRefused(w) ==
  /\ pc[w] = "posting"
  /\ Finish(w, "failed") /\ pc' = [pc EXCEPT ![w] = "idle"]
  /\ UNCHANGED <<started, tok, held, docs, retries>>

Crash(w) == /\ pc[w] # "idle" /\ pc' = [pc EXCEPT ![w] = "idle"]
            /\ UNCHANGED <<status, started, rowTok, expired, tok, held, docs, retries>>

\* retryFailedTicketTaxDocument: failed -> pending, submitted_at cleared.
StaffRetry ==
  /\ retries < MaxRetries
  /\ (status = "failed" \/ (RetryUncertain /\ status = "uncertain"))
  /\ status' = "pending" /\ started' = FALSE /\ retries' = retries + 1
  /\ UNCHANGED <<rowTok, expired, tok, held, pc, docs>>

Next ==
  \/ ExpireLease \/ StaffRetry
  \/ \E w \in Workers :
       Claim(w) \/ MarkUncertain(w) \/ StartSubmission(w)
       \/ PostAnswered(w) \/ PostLost(w) \/ PostRefused(w) \/ Crash(w)

Spec == Init /\ [][Next]_vars

\* Exactly one fiscal document per order: never a second provider document.
AtMostOneDocument == docs <= 1
AuthorizedHasDocument == status = "authorized" => docs >= 1
=============================================================================
