---- MODULE DiscoverAffinity ----
(***************************************************************************)
(* SOCIAL-DISCOVER-AFFINITY-001. One request for the people in Discover    *)
(* (formal/system/social-discover-affinity.md).                            *)
(*                                                                         *)
(* The request first decides which evidence it may use, the units of each  *)
(* candidate and the candidates that lead, then emits the candidates one   *)
(* by one, always the one whose key precedes every candidate still         *)
(* waiting. That is the order a correct sort by the key produces; the      *)
(* model does not cover the sorting routine itself.                        *)
(*                                                                         *)
(* `eligible` is the candidate set of the previous function and baseKey    *)
(* with the id its order (shared interest, daily rotation, id). Both are   *)
(* arbitrary: the model does not describe how they are computed, only that *)
(* this change leaves them alone. A post in `usable` is top-level, not     *)
(* hidden and readable by the actor. `reacted` are the posts the actor     *)
(* interacted with inside the window, `otherReacted` those anybody else    *)
(* interacted with. Units are 0, 1 or 2 for no, one or several evidence    *)
(* posts, standing for min(reactions + 2 * replies, 20). Half stands for   *)
(* page_size div 2.                                                        *)
(*                                                                         *)
(* Switches for negative controls:                                         *)
(*   HonourOptOut = FALSE     the opt-out is ignored                       *)
(*   FilterEvidence = FALSE   hidden or unreadable posts count             *)
(*   OwnEvidenceOnly = FALSE  other parties' interactions count            *)
(*   CapPromotion = FALSE     every candidate with units leads             *)
(***************************************************************************)
EXTENDS Integers, FiniteSets

CONSTANTS
  \* @type: Set(Int);
  Parties,
  \* @type: Set(Int);
  Posts,
  \* @type: Set(Int);
  KeyValues,
  \* @type: Int;
  Half,
  \* @type: Bool;
  Gate,
  \* @type: Bool;
  HonourOptOut,
  \* @type: Bool;
  FilterEvidence,
  \* @type: Bool;
  OwnEvidenceOnly,
  \* @type: Bool;
  CapPromotion

VARIABLES
  \* @type: Set(Int);
  eligible,
  \* @type: Int -> Int;
  baseKey,
  \* @type: Int -> Int;
  author,
  \* @type: Set(Int);
  usable,
  \* @type: Set(Int);
  reacted,
  \* @type: Set(Int);
  otherReacted,
  \* @type: Bool;
  optOut,
  \* @type: Str;
  phase,
  \* @type: Set(Int);
  evidence,
  \* @type: Int -> Int;
  units,
  \* @type: Set(Int);
  promoted,
  \* @type: Int -> Int;
  pos,
  \* @type: Int;
  n

(* Most candidates the configurations and constant initialisers describe. *)
MaxParties == 4

vars == <<eligible, baseKey, author, usable, reacted, otherReacted, optOut, phase, evidence,
          units, promoted, pos, n>>

(* Posts whose interactions may count for a candidate.                     *)
Own == IF OwnEvidenceOnly THEN reacted ELSE reacted \cup otherReacted
EvidenceRead ==
  {q \in (IF FilterEvidence THEN Own \cap usable ELSE Own) : author[q] \in eligible}
Permitted == Gate /\ (HonourOptOut => ~optOut)
EvidenceOf == IF Permitted THEN EvidenceRead ELSE {}

UnitsOf(p) ==
  IF \E q, r \in EvidenceOf : q # r /\ author[q] = p /\ author[r] = p THEN 2
  ELSE IF \E q \in EvidenceOf : author[q] = p THEN 1
  ELSE 0

(* The candidates that lead under units u: those with units that have      *)
(* fewer than Half candidates of more units (or equal units, smaller id).  *)
\* @type: (Int -> Int) => Set(Int);
PromotedOf(u) ==
  LET W == {i \in eligible : u[i] > 0} IN
  IF CapPromotion
    THEN {i \in W : Cardinality({j \in W : u[j] > u[i] \/ (u[j] = u[i] /\ j < i)}) < Half}
    ELSE W

AffBefore(i, j) == units[i] > units[j] \/ (units[i] = units[j] /\ i < j)
LegacyBefore(i, j) == baseKey[i] > baseKey[j] \/ (baseKey[i] = baseKey[j] /\ i < j)
Before(i, j) ==
  \/ i \in promoted /\ j \notin promoted
  \/ i \in promoted /\ j \in promoted /\ AffBefore(i, j)
  \/ i \notin promoted /\ j \notin promoted /\ LegacyBefore(i, j)

Init ==
  /\ eligible \in SUBSET Parties
  /\ baseKey \in [Parties -> KeyValues]
  /\ author \in [Posts -> Parties]
  /\ usable \in SUBSET Posts
  /\ reacted \in SUBSET Posts
  /\ otherReacted \in SUBSET Posts
  /\ optOut \in BOOLEAN
  /\ phase = "start"
  /\ evidence = {}
  /\ units = [p \in Parties |-> 0]
  /\ promoted = {}
  /\ pos = [p \in Parties |-> 0]
  /\ n = 0

Decide ==
  /\ phase = "start"
  /\ phase' = "rank"
  /\ evidence' = EvidenceOf
  /\ units' = [p \in Parties |-> UnitsOf(p)]
  /\ promoted' = PromotedOf([p \in Parties |-> UnitsOf(p)])
  /\ UNCHANGED <<eligible, baseKey, author, usable, reacted, otherReacted, optOut, pos, n>>

Emit(i) ==
  /\ phase = "rank"
  /\ i \in eligible
  /\ pos[i] = 0
  /\ \A j \in eligible : (j # i /\ pos[j] = 0) => Before(i, j)
  /\ pos' = [pos EXCEPT ![i] = n + 1]
  /\ n' = n + 1
  /\ UNCHANGED <<eligible, baseKey, author, usable, reacted, otherReacted, optOut, phase,
                 evidence, units, promoted>>

Finish ==
  /\ phase = "rank"
  /\ \A i \in eligible : pos[i] > 0
  /\ phase' = "done"
  /\ UNCHANGED <<eligible, baseKey, author, usable, reacted, otherReacted, optOut, evidence,
                 units, promoted, pos, n>>

Next == Decide \/ (\E i \in Parties : Emit(i)) \/ Finish

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ eligible \in SUBSET Parties
  /\ baseKey \in [Parties -> KeyValues]
  /\ author \in [Posts -> Parties]
  /\ usable \in SUBSET Posts
  /\ reacted \in SUBSET Posts
  /\ otherReacted \in SUBSET Posts
  /\ optOut \in BOOLEAN
  /\ phase \in {"start", "rank", "done"}
  /\ evidence \in SUBSET Posts
  /\ units \in [Parties -> 0..2]
  /\ promoted \in SUBSET Parties
  /\ pos \in [Parties -> 0..MaxParties]
  /\ n \in 0..MaxParties

Emitted(i) == pos[i] > 0

(* Position of a candidate in the previous order.                          *)
LegacyPos(i) == 1 + Cardinality({j \in eligible : LegacyBefore(j, i)})

(* The actor has no usable interaction with any candidate.                 *)
NoEvidence == \A q \in reacted \cap usable : author[q] \notin eligible

(* P2 and P3. Only candidates are returned or lead. Evidence is a post the *)
(* actor interacted with that is top-level, not hidden and readable; a     *)
(* candidate has units only from such a post of its own.                   *)
Exclusion ==
  /\ \A i \in Parties : Emitted(i) => i \in eligible
  /\ promoted \subseteq eligible
  /\ evidence \subseteq (reacted \cap usable)
  /\ \A p \in Parties : units[p] > 0 => \E q \in reacted \cap usable : author[q] = p

(* P2. The finished response holds every candidate exactly once.           *)
Permutation ==
  phase = "done" =>
    /\ \A i \in eligible : pos[i] >= 1 /\ pos[i] <= n
    /\ \A i, j \in eligible : i # j => pos[i] # pos[j]
    /\ \A k \in 1..MaxParties : k <= n => \E i \in eligible : pos[i] = k

(* P1. Gate off, opted out, or no usable evidence: the previous order, and *)
(* no behaviour used for an actor who opted out.                           *)
Refinement ==
  /\ (phase # "start" /\ (~Gate \/ optOut \/ NoEvidence)) =>
       /\ promoted = {}
       /\ \A i, j \in eligible : (Emitted(i) /\ Emitted(j) /\ pos[i] < pos[j]) => LegacyBefore(i, j)
  /\ optOut => evidence = {}

(* P5. The order of any two returned candidates is fixed by the key alone. *)
Deterministic ==
  \A i, j \in eligible :
    (i # j /\ Emitted(i) /\ Emitted(j)) => (pos[i] < pos[j] <=> Before(i, j))

(* P4. At most Half candidates lead, each with units; every other          *)
(* candidate stands at most Half places below its previous position, and   *)
(* never above it.                                                         *)
Displacement ==
  /\ Cardinality(promoted) <= Half
  /\ \A i \in promoted : units[i] > 0
  /\ \A i \in eligible :
       (Emitted(i) /\ i \notin promoted) =>
         (pos[i] >= LegacyPos(i) /\ pos[i] <= LegacyPos(i) + Half)

Safe == Exclusion /\ Permutation /\ Refinement /\ Deterministic /\ Displacement

(* Inductive strengthening, for the implemented switches only.             *)
IndInv ==
  /\ TypeOK
  /\ HonourOptOut /\ FilterEvidence /\ OwnEvidenceOnly /\ CapPromotion
  /\ Half >= 0
  /\ phase = "start" =>
       /\ n = 0 /\ evidence = {} /\ promoted = {}
       /\ \A p \in Parties : pos[p] = 0 /\ units[p] = 0
  /\ phase # "start" =>
       /\ evidence = EvidenceOf
       /\ \A p \in Parties : units[p] = UnitsOf(p)
       /\ promoted = PromotedOf(units)
  /\ \A i \in Parties : pos[i] <= n /\ (Emitted(i) => i \in eligible)
  /\ \A i, j \in eligible : (i # j /\ Emitted(i) /\ Emitted(j)) => pos[i] # pos[j]
  /\ \A k \in 1..MaxParties : k <= n => \E i \in eligible : pos[i] = k
  /\ \A i, j \in eligible : (Emitted(i) /\ ~Emitted(j)) => Before(i, j)
  /\ \A i, j \in eligible : (Emitted(i) /\ Emitted(j) /\ pos[i] < pos[j]) => Before(i, j)
  /\ phase = "done" => \A i \in eligible : Emitted(i)

(* Constant initialisers for Apalache (--cinit): up to four candidates,    *)
(* three posts, three values of the previous key, and Half 0, 1 or 2       *)
(* (pages of 1 to 5). `eligible` ranges over every subset.                 *)
CInitShape ==
  /\ Posts = 1..3
  /\ KeyValues = 0..2
  /\ Gate \in BOOLEAN

CInitProof ==
  /\ CInitShape /\ Parties = 1..4 /\ Half \in 0..2
  /\ HonourOptOut = TRUE /\ FilterEvidence = TRUE /\ OwnEvidenceOnly = TRUE /\ CapPromotion = TRUE

CInitOptOutIgnored ==
  /\ CInitShape /\ Parties = 1..3 /\ Half = 1
  /\ HonourOptOut = FALSE /\ FilterEvidence = TRUE /\ OwnEvidenceOnly = TRUE /\ CapPromotion = TRUE

CInitUnfilteredEvidence ==
  /\ CInitShape /\ Parties = 1..3 /\ Half = 1
  /\ HonourOptOut = TRUE /\ FilterEvidence = FALSE /\ OwnEvidenceOnly = TRUE /\ CapPromotion = TRUE

CInitThirdPartyEvidence ==
  /\ CInitShape /\ Parties = 1..3 /\ Half = 1
  /\ HonourOptOut = TRUE /\ FilterEvidence = TRUE /\ OwnEvidenceOnly = FALSE /\ CapPromotion = TRUE

CInitNoCap ==
  /\ CInitShape /\ Parties = 1..3 /\ Half = 1
  /\ HonourOptOut = TRUE /\ FilterEvidence = TRUE /\ OwnEvidenceOnly = TRUE /\ CapPromotion = FALSE
====
