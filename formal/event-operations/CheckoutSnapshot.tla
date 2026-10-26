--------------------------- MODULE CheckoutSnapshot ---------------------------
(* The line snapshot of one canonical checkout (PAY-CHECKOUT-002;
   tdf-hq/sql/2026-10-04_checkout_amount_correspondence.sql and
   tdf-hq/sql/2026-10-12_checkout_line_snapshot_fixed.sql).

   A creating transaction inserts the header with its payable total and its
   lines, then commits or aborts. Once committed, a later transaction may try
   to append lines (committed at once, after the deferred sum check, or
   aborted), and the header's total may be changed. Lines are a bag of
   amounts; the sum is exact.

   Guards, each removed by one negative configuration:
     LinesFixed       no line joins a committed checkout
     CommitValidated  a commit requires at least one line and an exact sum
     TermsImmutable   the total of a committed checkout cannot change *)
EXTENDS Integers, FiniteSets

CONSTANTS LinesFixed, CommitValidated, TermsImmutable, MaxLines

Amounts == {0, 1, 2}
Totals == 1..3

VARIABLES
  header,   \* absent | open | committed
  total,    \* payable total of the header
  lines,    \* [Amounts -> Nat], lines by amount
  snapshot, \* lines at commit, or <<>>
  terms,    \* total at commit, or 0
  late,     \* none | open: a later transaction appending lines
  pending   \* lines the late transaction has inserted, not yet committed

vars == <<header, total, lines, snapshot, terms, late, pending>>

Empty == [a \in Amounts |-> 0]

Count(b) == LET C[A \in SUBSET Amounts] ==
              IF A = {} THEN 0
              ELSE LET a == CHOOSE x \in A : TRUE IN b[a] + C[A \ {a}]
            IN C[Amounts]

Sum(b) == LET S[A \in SUBSET Amounts] ==
            IF A = {} THEN 0
            ELSE LET a == CHOOSE x \in A : TRUE IN a * b[a] + S[A \ {a}]
          IN S[Amounts]

Merged == [a \in Amounts |-> lines[a] + pending[a]]

TypeOK ==
  /\ header \in {"absent", "open", "committed"}
  /\ total \in Totals
  /\ lines \in [Amounts -> 0..MaxLines]
  /\ late \in {"none", "open"}
  /\ pending \in [Amounts -> 0..MaxLines]

Init ==
  /\ header = "absent"
  /\ total \in Totals
  /\ lines = Empty
  /\ snapshot = <<>>
  /\ terms = 0
  /\ late = "none"
  /\ pending = Empty

Create ==
  /\ header = "absent"
  /\ header' = "open"
  /\ UNCHANGED <<total, lines, snapshot, terms, late, pending>>

AddLine(a) ==
  /\ header = "open"
  /\ Count(lines) < MaxLines
  /\ lines' = [lines EXCEPT ![a] = @ + 1]
  /\ UNCHANGED <<header, total, snapshot, terms, late, pending>>

Valid(b) == Count(b) > 0 /\ Sum(b) = total

(* The deferred triggers run at commit. *)
Commit ==
  /\ header = "open"
  /\ CommitValidated => Valid(lines)
  /\ header' = "committed"
  /\ snapshot' = lines
  /\ terms' = total
  /\ UNCHANGED <<total, lines, late, pending>>

Abort ==
  /\ header = "open"
  /\ header' = "absent"
  /\ lines' = Empty
  /\ UNCHANGED <<total, snapshot, terms, late, pending>>

LateBegin ==
  /\ header = "committed" /\ late = "none"
  /\ late' = "open"
  /\ UNCHANGED <<header, total, lines, snapshot, terms, pending>>

(* The insert of a line into a checkout committed by an earlier transaction
   is refused when LinesFixed holds. *)
LateAdd(a) ==
  /\ late = "open" /\ ~LinesFixed
  /\ Count(Merged) < MaxLines
  /\ pending' = [pending EXCEPT ![a] = @ + 1]
  /\ UNCHANGED <<header, total, lines, snapshot, terms, late>>

LateCommit ==
  /\ late = "open"
  /\ CommitValidated => Valid(Merged)
  /\ lines' = Merged
  /\ late' = "none"
  /\ pending' = Empty
  /\ UNCHANGED <<header, total, snapshot, terms>>

LateAbort ==
  /\ late = "open"
  /\ late' = "none"
  /\ pending' = Empty
  /\ UNCHANGED <<header, total, lines, snapshot, terms>>

ChangeTotal(t) ==
  /\ header = "committed" /\ ~TermsImmutable
  /\ t # total
  /\ total' = t
  /\ UNCHANGED <<header, lines, snapshot, terms, late, pending>>

Next ==
  \/ Create
  \/ \E a \in Amounts : AddLine(a) \/ LateAdd(a)
  \/ Commit \/ Abort
  \/ LateBegin \/ LateCommit \/ LateAbort
  \/ \E t \in Totals : ChangeTotal(t)

Spec == Init /\ [][Next]_vars

(* A committed checkout has lines whose exact sum is its total. *)
CommittedSum == header = "committed" => Valid(lines)

(* The lines of a committed checkout are those it was committed with. *)
SnapshotFixed == header = "committed" => lines = snapshot

(* The total of a committed checkout is the one it was committed with. *)
TermsFixed == header = "committed" => total = terms

=============================================================================
