---- MODULE PublicKnowledgeIndex ----
(* PRIV-RAG-002: one refresh of a captured public corpus. Document identities
   abstract their fields; a historic index may already contain private data. *)
EXTENDS Naturals, FiniteSets
CONSTANTS PublicOnly, ClearEmpty, PreserveFailure, PurgeLegacy, RejectAmbiguous
VARIABLES phase, corpus, collected, emitted, index, original, outcome, ambiguous
vars == <<phase, corpus, collected, emitted, index, original, outcome, ambiguous>>
Public == {"course"}
Private == {"studio-note", "booking"}
Documents == Public \cup Private
EligibleCorpus == IF ambiguous THEN {} ELSE corpus

Init ==
  /\ phase = "cleanup"
  /\ corpus \in SUBSET Public
  /\ ambiguous \in BOOLEAN
  /\ collected = {}
  /\ emitted = {}
  /\ index = Documents
  /\ original = index
  /\ outcome = "pending"

Cleanup ==
  /\ phase = "cleanup"
  /\ index' = IF PurgeLegacy THEN index \cap Public ELSE index
  /\ phase' = "collect"
  /\ UNCHANGED <<corpus, collected, emitted, original, outcome, ambiguous>>

Collect ==
  /\ phase = "collect"
  /\ collected' = IF PublicOnly
                   THEN IF RejectAmbiguous THEN EligibleCorpus ELSE corpus
                   ELSE corpus \cup Private
  /\ phase' = "embed"
  /\ UNCHANGED <<corpus, emitted, index, original, outcome, ambiguous>>

Empty ==
  /\ phase = "embed" /\ collected = {}
  /\ index' = IF ClearEmpty THEN {} ELSE index
  /\ phase' = "done" /\ outcome' = "success"
  /\ UNCHANGED <<corpus, collected, emitted, original, ambiguous>>

Embed ==
  /\ phase = "embed" /\ collected # {}
  /\ emitted' = collected
  /\ phase' = "result"
  /\ UNCHANGED <<corpus, collected, index, original, outcome, ambiguous>>

Publish ==
  /\ phase = "result"
  /\ index' = collected
  /\ phase' = "done" /\ outcome' = "success"
  /\ UNCHANGED <<corpus, collected, emitted, original, ambiguous>>

Reject ==
  /\ phase \in {"collect", "result"}
  /\ index' = IF PreserveFailure THEN index ELSE {}
  /\ phase' = "done" /\ outcome' = "failure"
  /\ UNCHANGED <<corpus, collected, emitted, original, ambiguous>>

Next == Cleanup \/ Collect \/ Empty \/ Embed \/ Publish \/ Reject
Spec == Init /\ [][Next]_vars
TypeOK ==
  /\ phase \in {"cleanup", "collect", "embed", "result", "done"}
  /\ corpus \subseteq Public
  /\ ambiguous \in BOOLEAN
  /\ collected \subseteq Documents /\ emitted \subseteq Documents
  /\ index \subseteq Documents /\ original \subseteq Documents
  /\ outcome \in {"pending", "success", "failure"}
NoLegacyAfterCleanup == phase # "cleanup" => index \subseteq Public
NoPrivateEmbedding == emitted \subseteq Public
SuccessfulReplacement == outcome = "success" => index = EligibleCorpus
FailedRefreshPreservesIndex == outcome = "failure" => index = original \cap Public
EmptySkipsProvider == collected = {} => emitted = {}
NoAmbiguousEmbedding == ambiguous => emitted = {}
====
