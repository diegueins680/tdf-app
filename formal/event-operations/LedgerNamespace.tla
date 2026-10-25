--------------------------- MODULE LedgerNamespace ---------------------------
(* Finite abstraction of SYS-MONEY-003 relation lookup. The actor can create
   shadows, but cannot change trusted-schema DDL or disable triggers. Existing
   LedgerPosting/MVCC models cover other posting rules; this model assumes one
   non-empty currency and abstracts balances to zero/nonzero. It does not prove
   PostgreSQL name resolution, ownership policy or implementation refinement. *)
EXTENDS Integers
CONSTANTS PostingNamespaceFixed, EntryNamespaceFixed
VARIABLES status, balance, entryShadow, parentShadow
vars == <<status, balance, entryShadow, parentShadow>>

Init ==
  /\ status = "draft"
  /\ balance \in {0, 1}
  /\ entryShadow = FALSE
  /\ parentShadow = FALSE

ShadowEntries ==
  /\ ~entryShadow
  /\ entryShadow' = TRUE
  /\ UNCHANGED <<status, balance, parentShadow>>

ShadowParent ==
  /\ ~parentShadow
  /\ parentShadow' = TRUE
  /\ UNCHANGED <<status, balance, entryShadow>>

Post ==
  /\ status = "draft"
  /\ balance = 0 \/ (~PostingNamespaceFixed /\ entryShadow)
  /\ status' = "posted"
  /\ UNCHANGED <<balance, entryShadow, parentShadow>>

MutateEntry ==
  /\ balance = 0
  /\ status = "draft" \/ (~EntryNamespaceFixed /\ parentShadow)
  /\ balance' = 1
  /\ UNCHANGED <<status, entryShadow, parentShadow>>

Next == ShadowEntries \/ ShadowParent \/ Post \/ MutateEntry
TypeOK ==
  /\ status \in {"draft", "posted"}
  /\ balance \in {0, 1}
  /\ entryShadow \in BOOLEAN
  /\ parentShadow \in BOOLEAN
PostedBalanced == status = "posted" => balance = 0
Spec == Init /\ [][Next]_vars
=============================================================================
