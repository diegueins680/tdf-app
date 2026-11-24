---- MODULE DirectoryEmbeddingIndex ----
(***************************************************************************)
(* DIR-SEMANTIC-SEARCH-001. Embeddings of public directory documents *)
(* (TDF.Directory.SemanticSearch, TDF.Directory.SemanticIndex and the      *)
(* migration 2026-11-21_directory_semantic_search.sql).                    *)
(*                                                                         *)
(* A document is public or hidden and has a text version. Owners and       *)
(* moderators publish, hide and edit at any time. A background sweep       *)
(* claims a public document (reading its text), sends the text to the      *)
(* provider outside any transaction, and later writes the vector. A        *)
(* search returns documents and may use their embeddings for ranking.      *)
(*                                                                         *)
(* GuardedWrite = TRUE : the write stores the vector only if the document  *)
(*                       is still public with the claimed text, under a    *)
(*                       row lock (writeDocumentEmbeddingSql).             *)
(* DeleteOnChange = TRUE : hiding or editing a document deletes its        *)
(*                       embedding in the same transaction (trigger).      *)
(* JoinOnly = TRUE     : search starts from the public projection and      *)
(*                       joins embeddings onto it (semanticSearchSql).     *)
(* HashChecked = TRUE  : an embedding is used only with the hash of the    *)
(*                       current text (similaritySql).                     *)
(* Each FALSE is a negative control.                                       *)
(*                                                                         *)
(* Each action is one database transaction and is atomic here. That the    *)
(* row lock makes the guarded write atomic with respect to a concurrent    *)
(* update of the same row is PostgreSQL behaviour, assumed and not proved. *)
(***************************************************************************)
EXTENDS Integers

CONSTANTS
  \* @type: Bool;
  GuardedWrite,
  \* @type: Bool;
  DeleteOnChange,
  \* @type: Bool;
  JoinOnly,
  \* @type: Bool;
  HashChecked,
  \* @type: Set(Int);
  Docs,
  \* @type: Int;
  MaxVersion

VARIABLES
  \* @type: Int -> Bool;
  public,
  \* @type: Int -> Int;
  version,      \* current text version, 0..MaxVersion
  \* @type: Int -> Int;
  emb,          \* version of the stored embedding, -1 when there is none
  \* @type: Int -> Set(Int);
  claims,       \* versions claimed by a sweep and not yet written
  \* @type: Int -> Set(Int);
  sent,         \* versions whose text reached the provider
  \* @type: Int -> Set(Int);
  wasPublic,    \* versions that were public at some instant
  \* @type: Bool;
  servedHidden, \* a search returned a hidden document
  \* @type: Bool;
  usedStale     \* a search ranked with an embedding of other text

vars == <<public, version, emb, claims, sent, wasPublic, servedHidden, usedStale>>

\* Text versions are bounded by a literal so that the type sets are constant.
Bound == 3
Versions == {v \in 0..Bound : v <= MaxVersion}

Init ==
  /\ public = [d \in Docs |-> FALSE]
  /\ version = [d \in Docs |-> 0]
  /\ emb = [d \in Docs |-> -1]
  /\ claims = [d \in Docs |-> {}]
  /\ sent = [d \in Docs |-> {}]
  /\ wasPublic = [d \in Docs |-> {}]
  /\ servedHidden = FALSE
  /\ usedStale = FALSE

Publish(d) ==
  /\ ~public[d]
  /\ public' = [public EXCEPT ![d] = TRUE]
  /\ wasPublic' = [wasPublic EXCEPT ![d] = @ \union {version[d]}]
  /\ UNCHANGED <<version, emb, claims, sent, servedHidden, usedStale>>

Hide(d) ==
  /\ public[d]
  /\ public' = [public EXCEPT ![d] = FALSE]
  /\ emb' = IF DeleteOnChange THEN [emb EXCEPT ![d] = -1] ELSE emb
  /\ UNCHANGED <<version, claims, sent, wasPublic, servedHidden, usedStale>>

Edit(d) ==
  /\ version[d] < MaxVersion
  /\ version' = [version EXCEPT ![d] = @ + 1]
  /\ emb' = IF DeleteOnChange THEN [emb EXCEPT ![d] = -1] ELSE emb
  /\ wasPublic' = IF public[d]
                    THEN [wasPublic EXCEPT ![d] = @ \union {version[d] + 1}]
                    ELSE wasPublic
  /\ UNCHANGED <<public, claims, sent, servedHidden, usedStale>>

\* The sweep reads a public document without a current embedding; its text
\* then goes to the provider.
Claim(d) ==
  /\ public[d]
  /\ emb[d] /= version[d]
  /\ claims' = [claims EXCEPT ![d] = @ \union {version[d]}]
  /\ sent' = [sent EXCEPT ![d] = @ \union {version[d]}]
  /\ UNCHANGED <<public, version, emb, wasPublic, servedHidden, usedStale>>

Write(d, v) ==
  /\ v \in claims[d]
  /\ claims' = [claims EXCEPT ![d] = @ \ {v}]
  /\ emb' = IF GuardedWrite /\ ~(public[d] /\ version[d] = v)
              THEN emb
              ELSE [emb EXCEPT ![d] = v]
  /\ UNCHANGED <<public, version, sent, wasPublic, servedHidden, usedStale>>

\* Provider failure or a stopped process: the claim is lost.
Abandon(d, v) ==
  /\ v \in claims[d]
  /\ claims' = [claims EXCEPT ![d] = @ \ {v}]
  /\ UNCHANGED <<public, version, emb, sent, wasPublic, servedHidden, usedStale>>

\* Backstop of the sweep.
Purge(d) ==
  /\ emb[d] /= -1
  /\ ~(public[d] /\ emb[d] = version[d])
  /\ emb' = [emb EXCEPT ![d] = -1]
  /\ UNCHANGED <<public, version, claims, sent, wasPublic, servedHidden, usedStale>>

Returned(d) == public[d] \/ (~JoinOnly /\ emb[d] /= -1)
Ranked(d) == emb[d] /= -1 /\ (HashChecked => emb[d] = version[d])

Search(d) ==
  /\ Returned(d)
  /\ servedHidden' = (servedHidden \/ ~public[d])
  /\ usedStale' = (usedStale \/ (Ranked(d) /\ emb[d] /= version[d]))
  /\ UNCHANGED <<public, version, emb, claims, sent, wasPublic>>

Next ==
  \E d \in Docs :
    \/ Publish(d) \/ Hide(d) \/ Edit(d) \/ Claim(d) \/ Purge(d) \/ Search(d)
    \/ \E v \in 0..Bound : Write(d, v) \/ Abandon(d, v)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ MaxVersion \in 0..Bound
  /\ public \in [Docs -> BOOLEAN]
  /\ version \in [Docs -> 0..Bound]
  /\ emb \in [Docs -> -1..Bound]
  /\ claims \in [Docs -> SUBSET (0..Bound)]
  /\ sent \in [Docs -> SUBSET (0..Bound)]
  /\ wasPublic \in [Docs -> SUBSET (0..Bound)]
  /\ servedHidden \in BOOLEAN
  /\ usedStale \in BOOLEAN
  /\ \A d \in Docs :
       /\ version[d] <= MaxVersion
       /\ emb[d] <= MaxVersion
       /\ \A v \in claims[d] \union sent[d] \union wasPublic[d] : v <= MaxVersion

(* I1: a hidden document is never returned.                                *)
NeverServed == ~servedHidden

(* I2: every stored embedding belongs to a public document and to its      *)
(* current text.                                                           *)
NeverIndexed == \A d \in Docs : emb[d] /= -1 => (public[d] /\ emb[d] = version[d])

(* I3: an embedding of other text never contributes to a ranking.          *)
NeverStale == ~usedStale

(* I4: only text that was public reached the provider.                     *)
SentOnlyPublic == \A d \in Docs : sent[d] \subseteq wasPublic[d]

Safe == NeverServed /\ NeverIndexed /\ NeverStale /\ SentOnlyPublic

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ GuardedWrite /\ DeleteOnChange /\ JoinOnly /\ HashChecked
  /\ \A d \in Docs :
       /\ public[d] => version[d] \in wasPublic[d]
       /\ claims[d] \subseteq sent[d]

CInitGuarded ==
  /\ GuardedWrite = TRUE /\ DeleteOnChange = TRUE /\ JoinOnly = TRUE /\ HashChecked = TRUE
  /\ Docs \in SUBSET (1..3) /\ Docs /= {}
  /\ MaxVersion \in 1..3

CInitUnguardedWrite ==
  /\ GuardedWrite = FALSE /\ DeleteOnChange = TRUE /\ JoinOnly = TRUE /\ HashChecked = TRUE
  /\ Docs = {1}
  /\ MaxVersion = 1

====
