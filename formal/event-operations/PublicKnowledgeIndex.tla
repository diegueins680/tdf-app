---- MODULE PublicKnowledgeIndex ----
(* PRIV-RAG-002: one refresh of a captured public corpus. Document identities
   abstract their fields; a historic index may already contain private data. *)
EXTENDS Naturals, FiniteSets
CONSTANTS PublicOnly, ClearEmpty, PreserveFailure, PurgeLegacy
VARIABLES phase, corpus, collected, emitted, index, original, outcome
vars == <<phase, corpus, collected, emitted, index, original, outcome>>
Public == {"course"}
Private == {"studio-note", "booking"}
Documents == Public \cup Private

Init ==
  /\ phase = "cleanup"
  /\ corpus \in SUBSET Public
  /\ collected = {}
  /\ emitted = {}
  /\ index = Documents
  /\ original = index
  /\ outcome = "pending"

Cleanup ==
  /\ phase = "cleanup"
  /\ index' = IF PurgeLegacy THEN index \cap Public ELSE index
  /\ phase' = "collect"
  /\ UNCHANGED <<corpus, collected, emitted, original, outcome>>

Collect ==
  /\ phase = "collect"
  /\ collected' = IF PublicOnly THEN corpus ELSE corpus \cup Private
  /\ phase' = "embed"
  /\ UNCHANGED <<corpus, emitted, index, original, outcome>>

Empty ==
  /\ phase = "embed" /\ collected = {}
  /\ index' = IF ClearEmpty THEN {} ELSE index
  /\ phase' = "done" /\ outcome' = "success"
  /\ UNCHANGED <<corpus, collected, emitted, original>>

Embed ==
  /\ phase = "embed" /\ collected # {}
  /\ emitted' = collected
  /\ phase' = "result"
  /\ UNCHANGED <<corpus, collected, index, original, outcome>>

Publish ==
  /\ phase = "result"
  /\ index' = collected
  /\ phase' = "done" /\ outcome' = "success"
  /\ UNCHANGED <<corpus, collected, emitted, original>>

Reject ==
  /\ phase \in {"collect", "result"}
  /\ index' = IF PreserveFailure THEN index ELSE {}
  /\ phase' = "done" /\ outcome' = "failure"
  /\ UNCHANGED <<corpus, collected, emitted, original>>

Next == Cleanup \/ Collect \/ Empty \/ Embed \/ Publish \/ Reject
Spec == Init /\ [][Next]_vars
TypeOK ==
  /\ phase \in {"cleanup", "collect", "embed", "result", "done"}
  /\ corpus \subseteq Public
  /\ collected \subseteq Documents /\ emitted \subseteq Documents
  /\ index \subseteq Documents /\ original \subseteq Documents
  /\ outcome \in {"pending", "success", "failure"}
NoLegacyAfterCleanup == phase # "cleanup" => index \subseteq Public
NoPrivateEmbedding == emitted \subseteq Public
SuccessfulReplacement == outcome = "success" => index = corpus
FailedRefreshPreservesIndex == outcome = "failure" => index = original \cap Public
EmptySkipsProvider == collected = {} => emitted = {}
====
