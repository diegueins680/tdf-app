---- MODULE PublicKnowledgeRead ----
(* PRIV-RAG-001: one index hit and one course, with concurrent committed edits.
   SQL statement snapshots are atomic; rank and metadata read are separate.
   Raw index text is deliberately private even when its source says course.
   Switches are negative controls, not application feature flags. Collision
   classification is an abstract predicate, not a proof of normalization. *)
EXTENDS Naturals
CONSTANTS FilterSource, RenderCurrent, DropMissing, ExcludeRaw, RejectAmbiguous
VARIABLES phase, source, present, metadata, selectedMetadata,
          presentAtRead, metadataAtRead, pending, output, ambiguous, ambiguousAtRead
vars == <<phase, source, present, metadata, selectedMetadata,
          presentAtRead, metadataAtRead, pending, output, ambiguous, ambiguousAtRead>>
PublicValues == {"public-v1", "public-v2"}
NoDocument == "none"
PrivateText == "private-index-text"
Payloads == PublicValues \cup {NoDocument, PrivateText}

Init ==
  /\ phase = "rank"
  /\ source \in {"course", "private"}
  /\ present \in BOOLEAN
  /\ ambiguous \in BOOLEAN
  /\ ambiguousAtRead = FALSE
  /\ metadata \in PublicValues
  /\ selectedMetadata = NoDocument
  /\ presentAtRead = FALSE
  /\ metadataAtRead = NoDocument
  /\ pending = NoDocument
  /\ output = NoDocument

Rank ==
  /\ phase = "rank"
  /\ LET eligible == present /\ (~FilterSource \/ source = "course")
                        /\ ~ambiguous IN
       /\ phase' = IF eligible THEN "read" ELSE "done"
       /\ selectedMetadata' = IF eligible THEN metadata ELSE NoDocument
  /\ UNCHANGED <<source, present, metadata, presentAtRead, metadataAtRead,
                 pending, output, ambiguous, ambiguousAtRead>>

ReadMetadata ==
  /\ phase = "read"
  /\ phase' = "deliver"
  /\ presentAtRead' = present
  /\ ambiguousAtRead' = ambiguous
  /\ metadataAtRead' = IF present THEN metadata ELSE NoDocument
  /\ pending' = IF ambiguous /\ RejectAmbiguous THEN NoDocument
                 ELSE IF ~present /\ DropMissing THEN NoDocument
                 ELSE IF ~ExcludeRaw THEN PrivateText
                 ELSE IF present /\ RenderCurrent THEN metadata
                 ELSE selectedMetadata
  /\ UNCHANGED <<source, present, metadata, selectedMetadata, output, ambiguous>>

Deliver ==
  /\ phase = "deliver"
  /\ phase' = "done"
  /\ output' = pending
  /\ UNCHANGED <<source, present, metadata, selectedMetadata,
                 presentAtRead, metadataAtRead, pending, ambiguous, ambiguousAtRead>>

Edit(value) ==
  /\ present
  /\ metadata' = value
  /\ UNCHANGED <<phase, source, present, selectedMetadata,
                 presentAtRead, metadataAtRead, pending, output, ambiguous, ambiguousAtRead>>

Delete ==
  /\ present
  /\ present' = FALSE
  /\ UNCHANGED <<phase, source, metadata, selectedMetadata,
                 presentAtRead, metadataAtRead, pending, output, ambiguous, ambiguousAtRead>>

ChangeCollision ==
  /\ ambiguous' = ~ambiguous
  /\ UNCHANGED <<phase, source, present, metadata, selectedMetadata,
                 presentAtRead, metadataAtRead, pending, output, ambiguousAtRead>>

Next == Rank \/ ReadMetadata \/ Deliver \/ Delete \/ ChangeCollision
        \/ (\E value \in PublicValues : Edit(value))
Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ phase \in {"rank", "read", "deliver", "done"}
  /\ source \in {"course", "private"}
  /\ present \in BOOLEAN
  /\ ambiguous \in BOOLEAN /\ ambiguousAtRead \in BOOLEAN
  /\ metadata \in PublicValues
  /\ selectedMetadata \in PublicValues \cup {NoDocument}
  /\ presentAtRead \in BOOLEAN
  /\ metadataAtRead \in PublicValues \cup {NoDocument}
  /\ pending \in Payloads
  /\ output \in Payloads
AllowedSource == output # NoDocument => source = "course"
NoPrivateText == output # PrivateText
ExistingAtRead == output # NoDocument => presentAtRead
CurrentMetadata == output # NoDocument => output = metadataAtRead
NoAmbiguousRead == output # NoDocument => ~ambiguousAtRead
====
