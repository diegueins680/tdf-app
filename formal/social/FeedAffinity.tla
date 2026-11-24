---- MODULE FeedAffinity ----
(***************************************************************************)
(* COMMUNITY-FEED-AFFINITY-001. One request for the "hot" order of a fan   *)
(* club feed (formal/system/feed-affinity-ranking.md).                     *)
(*                                                                         *)
(* The request first decides its mode and the evidence it may use, then    *)
(* emits the unpinned items one by one, always the item whose key precedes *)
(* every item still waiting. That is the order a correct sort by the key   *)
(* produces; the model does not cover the sorting routine itself.          *)
(*                                                                         *)
(* Scores are abstract. hot[i] stands for the previous score               *)
(* reactions / age^0.8 and base[i] for (reactions + 1) / age^0.8; the      *)
(* model lets them vary independently, which only adds behaviours. The     *)
(* affinity is Floor, Floor + 1 or Floor + 2 for no, one or several        *)
(* evidence items, standing for the range 1..3 of the implementation. The  *)
(* item id stands for the tie-break (kind, created_at, id), which is       *)
(* unique per item.                                                        *)
(*                                                                         *)
(* Switches for negative controls:                                         *)
(*   Floor = 0              an author without evidence scores nothing      *)
(*   TieBreak = FALSE       equal scores are emitted in any order          *)
(*   FilterEvidence = FALSE evidence is read before the visibility filter  *)
(*   HonourOptOut = FALSE   the opt-out is ignored                         *)
(***************************************************************************)
EXTENDS Integers, FiniteSets

CONSTANTS
  \* @type: Set(Int);
  Items,
  \* @type: Int;
  Viewer,
  \* @type: Set(Int);
  Authors,
  \* @type: Set(Int);
  HotValues,
  \* @type: Set(Int);
  BaseValues,
  \* @type: Bool;
  Gate,
  \* @type: Int;
  Floor,
  \* @type: Bool;
  TieBreak,
  \* @type: Bool;
  FilterEvidence,
  \* @type: Bool;
  HonourOptOut

VARIABLES
  \* @type: Int -> Int;
  author,
  \* @type: Int -> Int;
  hot,
  \* @type: Int -> Int;
  base,
  \* @type: Set(Int);
  visible,
  \* @type: Set(Int);
  hidden,
  \* @type: Set(Int);
  reacted,
  \* @type: Bool;
  optOut,
  \* @type: Str;
  phase,
  \* @type: Str;
  mode,
  \* @type: Set(Int);
  evidence,
  \* @type: Int -> Int;
  aff,
  \* @type: Int -> Int;
  pos,
  \* @type: Int;
  n

(* Largest feed the configurations and constant initialisers describe. *)
MaxItems == 4

vars == <<author, hot, base, visible, hidden, reacted, optOut, phase, mode, evidence, aff, pos, n>>

(* Items whose interactions may count: readable by the viewer, not hidden, *)
(* and not the viewer's own.                                               *)
Usable == {i \in (reacted \cap visible) \ hidden : author[i] # Viewer}
EvidenceRead == IF FilterEvidence THEN Usable ELSE {i \in reacted : author[i] # Viewer}
Permitted == Gate /\ (HonourOptOut => ~optOut)
EvidenceOf == IF Permitted THEN EvidenceRead ELSE {}
ModeOf == IF Permitted /\ EvidenceRead # {} THEN "personalised" ELSE "fallback"

(* Affinity to author a from the evidence the request may use.             *)
AffOf(a) ==
  Floor + (IF \E i, j \in EvidenceOf : i # j /\ author[i] = a /\ author[j] = a THEN 2
           ELSE IF \E i \in EvidenceOf : author[i] = a THEN 1
           ELSE 0)

(* k * b for k in 0..3, kept linear for the solver. *)
Times(k, b) == IF k <= 0 THEN 0 ELSE IF k = 1 THEN b ELSE IF k = 2 THEN b + b ELSE b + b + b

Score(i) == IF mode = "personalised" THEN Times(aff[author[i]], base[i]) ELSE hot[i]

Before(i, j) == Score(i) > Score(j) \/ (Score(i) = Score(j) /\ TieBreak /\ i > j)
LegacyBefore(i, j) == hot[i] > hot[j] \/ (hot[i] = hot[j] /\ i > j)

Init ==
  /\ author \in [Items -> Authors]
  /\ hot \in [Items -> HotValues]
  /\ base \in [Items -> BaseValues]
  /\ visible \in SUBSET Items
  /\ hidden \in SUBSET Items
  /\ reacted \in SUBSET Items
  /\ optOut \in BOOLEAN
  /\ phase = "start"
  /\ mode = "fallback"
  /\ evidence = {}
  /\ aff = [a \in Authors |-> Floor]
  /\ pos = [i \in Items |-> 0]
  /\ n = 0

Decide ==
  /\ phase = "start"
  /\ phase' = "rank"
  /\ mode' = ModeOf
  /\ evidence' = EvidenceOf
  /\ aff' = [a \in Authors |-> AffOf(a)]
  /\ UNCHANGED <<author, hot, base, visible, hidden, reacted, optOut, pos, n>>

Emit(i) ==
  /\ phase = "rank"
  /\ i \in visible
  /\ pos[i] = 0
  /\ \A j \in visible :
       (j # i /\ pos[j] = 0) => IF TieBreak THEN Before(i, j) ELSE Score(i) >= Score(j)
  /\ pos' = [pos EXCEPT ![i] = n + 1]
  /\ n' = n + 1
  /\ UNCHANGED <<author, hot, base, visible, hidden, reacted, optOut, phase, mode, evidence, aff>>

Finish ==
  /\ phase = "rank"
  /\ \A i \in visible : pos[i] > 0
  /\ phase' = "done"
  /\ UNCHANGED <<author, hot, base, visible, hidden, reacted, optOut, mode, evidence, aff, pos, n>>

Next == Decide \/ (\E i \in Items : Emit(i)) \/ Finish

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ author \in [Items -> Authors]
  /\ hot \in [Items -> HotValues]
  /\ base \in [Items -> BaseValues]
  /\ visible \in SUBSET Items
  /\ hidden \in SUBSET Items
  /\ reacted \in SUBSET Items
  /\ optOut \in BOOLEAN
  /\ phase \in {"start", "rank", "done"}
  /\ mode \in {"fallback", "personalised"}
  /\ evidence \in SUBSET Items
  /\ aff \in [Authors -> 0..3]
  /\ pos \in [Items -> 0..MaxItems]
  /\ n \in 0..MaxItems

Emitted(i) == pos[i] > 0

(* P5. Nothing outside the visible set is returned, and no hidden or       *)
(* unreadable item is evidence.                                            *)
Exclusion ==
  /\ \A i \in Items : Emitted(i) => i \in visible
  /\ evidence \subseteq (visible \ hidden)

(* P1. The finished response holds every visible item exactly once.        *)
Permutation ==
  phase = "done" =>
    /\ \A i \in visible : pos[i] >= 1 /\ pos[i] <= n
    /\ \A i, j \in visible : i # j => pos[i] # pos[j]
    /\ \A k \in 1..MaxItems : k <= n => \E i \in visible : pos[i] = k

(* P3. Gate off, opted out, or nothing usable as evidence: the previous    *)
(* order, and no behaviour used for a viewer who opted out.                *)
Refinement ==
  /\ (phase # "start" /\ (~Gate \/ optOut \/ Usable = {})) =>
       /\ mode = "fallback"
       /\ \A i, j \in visible : (Emitted(i) /\ Emitted(j) /\ pos[i] < pos[j]) => LegacyBefore(i, j)
  /\ optOut => evidence = {}

(* P4. The order of any two returned items is fixed by the key alone.      *)
Deterministic ==
  \A i, j \in visible :
    (i # j /\ Emitted(i) /\ Emitted(j)) => (pos[i] < pos[j] <=> Before(i, j))

(* P2. An item is preceded only by items whose base score is at least a    *)
(* third of its own.                                                       *)
Displacement ==
  mode = "personalised" =>
    \A i, j \in visible :
      (Emitted(i) /\ Emitted(j) /\ pos[i] < pos[j]) => Times(3, base[i]) >= base[j]

Safe == Exclusion /\ Permutation /\ Refinement /\ Deterministic /\ Displacement

(* Inductive strengthening, for the implemented switches only. It keeps    *)
(* only the range of the affinity, so the order properties hold for every  *)
(* affinity in 1..3, however it was derived.                               *)
IndInv ==
  /\ TypeOK
  /\ Floor = 1 /\ TieBreak /\ FilterEvidence /\ HonourOptOut
  /\ \A v \in BaseValues : v >= 1
  /\ \A v \in HotValues : v >= 0
  /\ \A a \in Authors : aff[a] >= 1 /\ aff[a] <= 3
  /\ phase = "start" => (n = 0 /\ mode = "fallback" /\ evidence = {} /\ \A i \in Items : pos[i] = 0)
  /\ phase # "start" => (mode = ModeOf /\ evidence = EvidenceOf)
  /\ \A i \in Items : pos[i] <= n /\ (Emitted(i) => i \in visible)
  /\ \A i, j \in visible : (i # j /\ Emitted(i) /\ Emitted(j)) => pos[i] # pos[j]
  /\ \A k \in 1..MaxItems : k <= n => \E i \in visible : pos[i] = k
  /\ \A i, j \in visible : (Emitted(i) /\ ~Emitted(j)) => Before(i, j)
  /\ \A i, j \in visible : (Emitted(i) /\ Emitted(j) /\ pos[i] < pos[j]) => Before(i, j)
  /\ phase = "done" => \A i \in visible : Emitted(i)

(* Constant initialisers for Apalache (--cinit): four candidate items by   *)
(* three authors, scores 0..6 and base scores 1..12. `visible` ranges over *)
(* every subset, so feeds of zero to four items are covered.               *)
CInitShape ==
  /\ Viewer = 1
  /\ Authors = 1..3
  /\ HotValues = 0..6
  /\ BaseValues = 1..12
  /\ Gate \in BOOLEAN

CInitProof ==
  /\ CInitShape /\ Items = 1..4
  /\ Floor = 1 /\ TieBreak = TRUE /\ FilterEvidence = TRUE /\ HonourOptOut = TRUE

CInitFloorZero ==
  /\ CInitShape /\ Items = 1..3
  /\ Floor = 0 /\ TieBreak = TRUE /\ FilterEvidence = TRUE /\ HonourOptOut = TRUE

CInitNoTieBreak ==
  /\ CInitShape /\ Items = 1..3
  /\ Floor = 1 /\ TieBreak = FALSE /\ FilterEvidence = TRUE /\ HonourOptOut = TRUE

CInitUnfilteredEvidence ==
  /\ CInitShape /\ Items = 1..3
  /\ Floor = 1 /\ TieBreak = TRUE /\ FilterEvidence = FALSE /\ HonourOptOut = TRUE

CInitOptOutIgnored ==
  /\ CInitShape /\ Items = 1..3
  /\ Floor = 1 /\ TieBreak = TRUE /\ FilterEvidence = TRUE /\ HonourOptOut = FALSE
====
