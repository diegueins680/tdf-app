---------------------------- MODULE LedgerPosting ----------------------------
(* Posting of one commerce ledger transaction (SYS-MONEY-003;
   tdf-hq/sql/2026-10-12_ledger_posting_integrity.sql).

   One transaction row with a status, a multiset of entries per currency
   (abstracted as the per-currency balance and an entry count), and a
   reversing transaction that may exist. Writers add entries, post, try to
   reopen, reverse, or insert a transaction already posted. Balances are
   exact integers: the SQL check sums as numeric, never modular bigint.

   Guards, each removed by one negative configuration:
     EntriesFixed     a posted or reversed transaction takes no new entry
     PostValidated    posting requires at least one entry and a zero balance
                      in every currency
     NoReopen         a posted transaction cannot return to draft
     DraftOnly        a transaction is created as draft *)
EXTENDS Integers, FiniteSets

CONSTANTS EntriesFixed, PostValidated, NoReopen, DraftOnly, MaxEntries

Currencies == {"USD", "EUR"}
Amounts == {-2, -1, 1, 2}

VARIABLES
  status,      \* absent | draft | posted | reversed
  balance,     \* [Currencies -> Int], sum of entry amounts
  entries,     \* number of entries
  postedShape, \* the (balance, entries) pair at the last posting, or <<>>
  firstShape,  \* the pair at the first posting, or <<>>
  reverser     \* status of the transaction named by reversal_of: none | draft | posted

vars == <<status, balance, entries, postedShape, firstShape, reverser>>

TypeOK ==
  /\ status \in {"absent", "draft", "posted", "reversed"}
  /\ balance \in [Currencies -> Int]
  /\ entries \in 0..MaxEntries
  /\ reverser \in {"none", "draft", "posted"}

Init ==
  /\ status = "absent"
  /\ balance = [c \in Currencies |-> 0]
  /\ entries = 0
  /\ postedShape = <<>>
  /\ firstShape = <<>>
  /\ reverser = "none"

(* The writers create the row as draft; a direct insert as posted is refused
   when DraftOnly holds. *)
Create ==
  /\ status = "absent"
  /\ \/ status' = "draft"
     \/ ~DraftOnly /\ status' \in {"posted", "reversed"}
  /\ UNCHANGED <<balance, entries, postedShape, firstShape, reverser>>

AddEntry(c, a) ==
  /\ status \in {"draft", "posted", "reversed"}
  /\ EntriesFixed => status = "draft"
  /\ entries < MaxEntries
  /\ balance' = [balance EXCEPT ![c] = @ + a]
  /\ entries' = entries + 1
  /\ UNCHANGED <<status, postedShape, firstShape, reverser>>

Balanced == \A c \in Currencies : balance[c] = 0

Post ==
  /\ status = "draft"
  /\ PostValidated => (entries > 0 /\ Balanced)
  /\ status' = "posted"
  /\ postedShape' = <<balance, entries>>
  /\ firstShape' = IF firstShape = <<>> THEN <<balance, entries>> ELSE firstShape
  /\ UNCHANGED <<balance, entries, reverser>>

Reopen ==
  /\ status = "posted" /\ ~NoReopen
  /\ status' = "draft"
  /\ UNCHANGED <<balance, entries, postedShape, firstShape, reverser>>

(* The reversing transaction goes through its own lifecycle. *)
ReverserAdvance ==
  /\ reverser' = IF reverser = "none" THEN "draft" ELSE "posted"
  /\ reverser # "posted"
  /\ UNCHANGED <<status, balance, entries, postedShape, firstShape>>

Reverse ==
  /\ status = "posted" /\ reverser = "posted"
  /\ status' = "reversed"
  /\ UNCHANGED <<balance, entries, postedShape, firstShape, reverser>>

Next ==
  \/ Create
  \/ \E c \in Currencies, a \in Amounts : AddEntry(c, a)
  \/ Post \/ Reopen \/ ReverserAdvance \/ Reverse

Spec == Init /\ [][Next]_vars

(* A posted or reversed transaction has entries and balances to zero in
   every currency. *)
PostedBalanced == status \in {"posted", "reversed"} => (entries > 0 /\ Balanced)

(* The entries of a posted or reversed transaction are those it was posted
   with. *)
PostedImmutable == status \in {"posted", "reversed"} => postedShape = <<balance, entries>>

(* What a transaction was posted with is what it stays posted with: no
   second posting after a reopening. *)
PostedHistory == status \in {"posted", "reversed"} => postedShape = firstShape

(* Only a posted transaction with a posted reverser becomes reversed; the
   model keeps this by construction, stated for the reader. *)
ReversedByPosted == status = "reversed" => reverser = "posted"

=============================================================================
