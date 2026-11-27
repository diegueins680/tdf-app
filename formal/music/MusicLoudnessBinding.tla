---- MODULE MusicLoudnessBinding ----
(***************************************************************************)
(* MEDIA-MUSIC-LOUDNESS-001, invariants L1 and L2                          *)
(* (formal/system/music-loudness-check.md).                                *)
(*                                                                         *)
(* One track of one release version. Every upload of a different file is   *)
(* a new master asset; the newest one is the track's file (latest). A file *)
(* becomes usable when inspection promotes it. Workers measure a file they *)
(* downloaded and record the outcome in music_audio_measurement; uploads   *)
(* may arrive at any moment, including while a worker is measuring.        *)
(*                                                                         *)
(* row[f] is the state of the row of asset f that is not superseded        *)
(* ("superseded" when the last one was closed without a successor), and    *)
(* rowOf[f] is the file whose bytes produced a measured value. gen[f]      *)
(* counts attempts; a worker keeps the number it started with.             *)
(*                                                                         *)
(* Guards, each removed by one negative configuration:                     *)
(*   BindFile  a result is written to the row of the asset whose bytes     *)
(*             were measured (keyed by asset and SHA-256), not to "the     *)
(*             track's current file"                                       *)
(*   Derived   "checked" is computed on read from the newest file's row    *)
(*             (TDF.MusicRelease.Loudness.trackCheck), not stored          *)
(*   Fence     only the attempt that opened a pending row may close it     *)
(*             (music_finish_audio_measurement and the worker lease guard) *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  BindFile,
  \* @type: Bool;
  Derived,
  \* @type: Bool;
  Fence,
  \* @type: Set(Int);
  Files,
  \* @type: Set(Int);
  Workers,
  \* @type: Int;
  MaxGen

VARIABLES
  \* @type: Set(Int);
  uploaded,
  \* @type: Int;
  latest,      \* newest uploaded file, 0 before the first upload
  \* @type: Set(Int);
  usable,      \* inspected, promoted and immutable
  \* @type: Int -> Str;
  row,         \* none | pending | measured | failed | superseded
  \* @type: Int -> Int;
  rowOf,       \* file whose bytes a measured row describes, else 0
  \* @type: Int -> Int;
  gen,
  \* @type: Int -> Int;
  wFile,       \* file a worker is measuring, 0 when idle
  \* @type: Int -> Int;
  wGen,
  \* @type: Bool;
  mark,        \* stored "checked" mark of the track (used when ~Derived)
  \* @type: Bool;
  rewritten    \* a recorded result was overwritten by another result

vars == <<uploaded, latest, usable, row, rowOf, gen, wFile, wGen, mark, rewritten>>

Init ==
  /\ uploaded = {}
  /\ latest = 0
  /\ usable = {}
  /\ row = [f \in Files |-> "none"]
  /\ rowOf = [f \in Files |-> 0]
  /\ gen = [f \in Files |-> 0]
  /\ wFile = [w \in Workers |-> 0]
  /\ wGen = [w \in Workers |-> 0]
  /\ mark = FALSE
  /\ rewritten = FALSE

\* A different file for the track: a new asset, now the newest.
Upload(f) ==
  /\ f \notin uploaded
  /\ uploaded' = uploaded \cup {f}
  /\ latest' = f
  /\ UNCHANGED <<usable, row, rowOf, gen, wFile, wGen, mark, rewritten>>

Promote(f) ==
  /\ f \in uploaded
  /\ usable' = usable \cup {f}
  /\ UNCHANGED <<uploaded, latest, row, rowOf, gen, wFile, wGen, mark, rewritten>>

\* The upload re-bind trigger re-points an asset that was not promoted.
Rebind(f) ==
  /\ f \in uploaded /\ f \notin usable
  /\ gen[f] < MaxGen
  /\ gen' = [gen EXCEPT ![f] = @ + 1]
  /\ row' = [row EXCEPT ![f] = IF @ = "none" THEN "none" ELSE "superseded"]
  /\ rowOf' = [rowOf EXCEPT ![f] = 0]
  /\ UNCHANGED <<uploaded, latest, usable, wFile, wGen, mark, rewritten>>

\* music_begin_audio_measurement: close the open row, open a pending one.
Begin(w, f) ==
  /\ wFile[w] = 0
  /\ f \in uploaded
  /\ gen[f] < MaxGen
  /\ gen' = [gen EXCEPT ![f] = @ + 1]
  /\ row' = [row EXCEPT ![f] = "pending"]
  /\ rowOf' = [rowOf EXCEPT ![f] = 0]
  /\ wFile' = [wFile EXCEPT ![w] = f]
  /\ wGen' = [wGen EXCEPT ![w] = gen[f] + 1]
  /\ UNCHANGED <<uploaded, latest, usable, mark, rewritten>>

Target(w) == IF BindFile THEN wFile[w] ELSE latest

MayWrite(w) == Fence => (gen[wFile[w]] = wGen[w] /\ row[wFile[w]] = "pending")

Finish(w, outcome) ==
  /\ wFile[w] /= 0
  /\ MayWrite(w)
  /\ rewritten' = (rewritten \/ row[Target(w)] \in {"measured", "failed"})
  /\ row' = [row EXCEPT ![Target(w)] = outcome]
  /\ rowOf' = [rowOf EXCEPT ![Target(w)] = IF outcome = "measured" THEN wFile[w] ELSE 0]
  /\ mark' = (mark \/ outcome = "measured")
  /\ wFile' = [wFile EXCEPT ![w] = 0]
  /\ UNCHANGED <<uploaded, latest, usable, gen, wGen>>

\* Lease lost, cancelled or crashed: the worker stops without writing.
Abandon(w) ==
  /\ wFile[w] /= 0
  /\ wFile' = [wFile EXCEPT ![w] = 0]
  /\ UNCHANGED <<uploaded, latest, usable, row, rowOf, gen, wGen, mark, rewritten>>

Next ==
  \/ \E f \in Files : Upload(f) \/ Promote(f) \/ Rebind(f)
  \/ \E w \in Workers :
       \/ \E f \in Files : Begin(w, f)
       \/ \E outcome \in {"measured", "failed"} : Finish(w, outcome)
       \/ Abandon(w)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ uploaded \in SUBSET Files
  /\ latest \in Files \cup {0}
  /\ usable \in SUBSET Files
  /\ row \in [Files -> {"none", "pending", "measured", "failed", "superseded"}]
  /\ rowOf \in [Files -> Files \cup {0}]
  /\ gen \in [Files -> Nat] /\ \A f \in Files : gen[f] <= MaxGen
  /\ wFile \in [Workers -> Files \cup {0}]
  /\ wGen \in [Workers -> Nat] /\ \A w \in Workers : wGen[w] <= MaxGen
  /\ mark \in BOOLEAN
  /\ rewritten \in BOOLEAN

Checked ==
  IF Derived
    THEN latest /= 0 /\ latest \in usable /\ row[latest] = "measured"
    ELSE mark

(* L1: the track is shown as checked only from a measurement of the bytes  *)
(* of its newest usable file.                                              *)
BoundToFile ==
  Checked => (latest /= 0 /\ latest \in usable
              /\ row[latest] = "measured" /\ rowOf[latest] = latest)

(* L2: measured and failed rows are never overwritten by another result.   *)
NoRewrite == ~rewritten

Safe == BoundToFile /\ NoRewrite

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ BindFile /\ Derived /\ Fence
  /\ 0 \notin Files
  /\ \A f \in Files : row[f] = "measured" => rowOf[f] = f

CInitGuarded ==
  /\ BindFile = TRUE /\ Derived = TRUE /\ Fence = TRUE
  /\ Files \in SUBSET (1..4) /\ Files /= {}
  /\ Workers \in SUBSET (1..3) /\ Workers /= {}
  /\ MaxGen \in 1..6

CInitUnbound ==
  /\ BindFile = FALSE /\ Derived = TRUE /\ Fence = TRUE
  /\ Files = {1, 2} /\ Workers = {1, 2} /\ MaxGen = 2

CInitStoredMark ==
  /\ BindFile = TRUE /\ Derived = FALSE /\ Fence = TRUE
  /\ Files = {1, 2} /\ Workers = {1, 2} /\ MaxGen = 2

CInitUnfenced ==
  /\ BindFile = TRUE /\ Derived = TRUE /\ Fence = FALSE
  /\ Files = {1, 2} /\ Workers = {1, 2} /\ MaxGen = 2

====
