---- MODULE ArtistSimilarity ----
(***************************************************************************)
(* COMMUNITY-ARTIST-SIMILARITY-001. The job that stores the artist          *)
(* similarity, the requests that read it and the follows and unfollows      *)
(* that commit while it runs (formal/system/artist-similarity.md).          *)
(*                                                                         *)
(* Signals are abstracted to follows and the similarity to the relation     *)
(* "at least two distinct fans follow both". The model does not depend on   *)
(* the weights: what it checks is which signal rows a stored relation was   *)
(* computed from, and how old they are when a request uses them.            *)
(*                                                                         *)
(* A run starts when it is due, every Period ticks, and reads the signals   *)
(* one artist at a time while follows and unfollows commit. It ends within  *)
(* Duration ticks by publishing one complete generation, or by failing.     *)
(*                                                                         *)
(* Snapshot = TRUE  : REPEATABLE READ; every read sees the signals as they  *)
(*                    were when the run started (runSimilarityGeneration).  *)
(* Snapshot = FALSE : each read sees the rows committed at that moment.     *)
(* AgeGuard = TRUE  : a request uses a generation only while its snapshot   *)
(*                    is at most MaxAge old (generationSimilaritySql).      *)
(* AgeGuard = FALSE : a request uses the newest generation whatever its age.*)
(* Failures         : whether a run may fail and leave the old generation.  *)
(***************************************************************************)
EXTENDS Integers

CONSTANTS
  \* @type: Set(Int);
  Fans,
  \* @type: Set(Int);
  Artists,
  \* @type: Bool;
  Snapshot,
  \* @type: Bool;
  AgeGuard,
  \* @type: Bool;
  Failures,
  \* @type: Int;
  Period,
  \* @type: Int;
  Duration,
  \* @type: Int;
  MaxAge,
  \* @type: Int;
  MaxTime,
  \* @type: Set(Int);
  SteadyFans         \* follow every artist from the start and never change

VARIABLES
  \* @type: Set(<<Int, Int>>);
  follows,           \* committed fan_follow rows: <<fan, artist>>
  \* @type: Int;
  now,
  \* @type: Int;
  nextDue,           \* when the next run starts
  \* @type: Bool;
  running,
  \* @type: Int;
  snapAt,            \* start of the current run
  \* @type: Set(<<Int, Int>>);
  snapFollows,       \* ghost: the rows committed when the current run started
  \* @type: Set(Int);
  toRead,            \* artists the current run has not read yet
  \* @type: Set(<<Int, Int>>);
  readFollows,       \* rows the current run has read
  \* @type: Bool;
  hasGen,
  \* @type: Set(<<Int, Int>>);
  tableSim,          \* stored relation of the newest generation
  \* @type: Int;
  tableSnapAt,       \* its snapshot_at
  \* @type: Set(<<Int, Int>>);
  tableSnapFollows,  \* ghost: the rows committed at its snapshot_at
  \* @type: Bool;
  servedFromTable,   \* the last request used the stored relation
  \* @type: Set(<<Int, Int>>);
  servedSim,         \* the relation the last request ranked with
  \* @type: Set(<<Int, Int>>);
  servedRef,         \* ghost: the reference on the snapshot it used
  \* @type: Int;
  servedAge          \* age of that snapshot at the request

vars == <<follows, now, nextDue, running, snapAt, snapFollows, toRead, readFollows,
          hasGen, tableSim, tableSnapAt, tableSnapFollows,
          servedFromTable, servedSim, servedRef, servedAge>>

Rows == Fans \X Artists
InitFollows == (SteadyFans \cap Fans) \X Artists
Mutable == (Fans \ SteadyFans) \X Artists
Pairs == Artists \X Artists

(* The reference: similarityFromWeights, abstracted. *)
\* @type: Set(<<Int, Int>>) => Set(<<Int, Int>>);
Similar(F) ==
  { p \in Pairs :
      /\ p[1] < p[2]
      /\ \E f1 \in Fans, f2 \in Fans :
           /\ f1 /= f2
           /\ <<f1, p[1]>> \in F /\ <<f1, p[2]>> \in F
           /\ <<f2, p[1]>> \in F /\ <<f2, p[2]>> \in F }

Init ==
  /\ follows = InitFollows
  /\ now = 0
  /\ nextDue = 0
  /\ running = FALSE
  /\ snapAt = 0
  /\ snapFollows = {}
  /\ toRead = {}
  /\ readFollows = {}
  /\ hasGen = FALSE
  /\ tableSim = {}
  /\ tableSnapAt = 0
  /\ tableSnapFollows = {}
  /\ servedFromTable = FALSE
  /\ servedSim = {}
  /\ servedRef = {}
  /\ servedAge = 0

job == <<nextDue, running, snapAt, snapFollows, toRead, readFollows>>
table == <<hasGen, tableSim, tableSnapAt, tableSnapFollows>>
served == <<servedFromTable, servedSim, servedRef, servedAge>>

\* followArtist / unfollowArtist commit at any moment.
Follow(f, a) ==
  /\ <<f, a>> \in Mutable
  /\ follows' = follows \cup {<<f, a>>}
  /\ UNCHANGED <<now, job, table, served>>

Unfollow(f, a) ==
  /\ <<f, a>> \in Mutable
  /\ follows' = follows \ {<<f, a>>}
  /\ UNCHANGED <<now, job, table, served>>

\* A due run starts before time passes, and a run ends within Duration.
Tick ==
  /\ now < MaxTime
  /\ ~(~running /\ now = nextDue)
  /\ running => now - snapAt < Duration
  /\ now' = now + 1
  /\ UNCHANGED <<follows, job, table, served>>

Start ==
  /\ ~running
  /\ now = nextDue
  /\ running' = TRUE
  /\ snapAt' = now
  /\ snapFollows' = follows
  /\ toRead' = Artists
  /\ readFollows' = {}
  /\ nextDue' = now + Period
  /\ UNCHANGED <<follows, now, table, served>>

\* One of the signal reads of loadSignalSimilarity.
ReadArtist(a) ==
  /\ running
  /\ a \in toRead
  /\ toRead' = toRead \ {a}
  /\ readFollows' = readFollows \cup
       { r \in (IF Snapshot THEN snapFollows ELSE follows) : r[2] = a }
  /\ UNCHANGED <<follows, now, nextDue, running, snapAt, snapFollows, table, served>>

\* The commit: generation, norms and pairs appear together.
Publish ==
  /\ running
  /\ toRead = {}
  /\ running' = FALSE
  /\ hasGen' = TRUE
  /\ tableSim' = Similar(readFollows)
  /\ tableSnapAt' = snapAt
  /\ tableSnapFollows' = snapFollows
  /\ UNCHANGED <<follows, now, nextDue, snapAt, snapFollows, toRead, readFollows, served>>

\* The run rolls back; the previous generation stays.
Fail ==
  /\ Failures
  /\ running
  /\ running' = FALSE
  /\ UNCHANGED <<follows, now, nextDue, snapAt, snapFollows, toRead, readFollows,
                 table, served>>

\* loadStoredSimilarity: one statement, so one view of the table.
Request ==
  LET usable == hasGen /\ (AgeGuard => now - tableSnapAt <= MaxAge) IN
  /\ servedFromTable' = usable
  /\ servedSim' = IF usable THEN tableSim ELSE {}
  /\ servedRef' = IF usable THEN Similar(tableSnapFollows) ELSE {}
  /\ servedAge' = IF usable THEN now - tableSnapAt ELSE 0
  /\ UNCHANGED <<follows, now, job, table>>

Next ==
  \/ Tick \/ Start \/ Publish \/ Fail \/ Request
  \/ \E a \in Artists : ReadArtist(a)
  \/ \E f \in Fans, a \in Artists : Follow(f, a) \/ Unfollow(f, a)

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ follows \in SUBSET Rows
  /\ now \in Nat
  /\ nextDue \in Nat
  /\ running \in BOOLEAN
  /\ snapAt \in Nat
  /\ snapFollows \in SUBSET Rows
  /\ toRead \in SUBSET Artists
  /\ readFollows \in SUBSET Rows
  /\ hasGen \in BOOLEAN
  /\ tableSim \in SUBSET Pairs
  /\ tableSnapAt \in Nat
  /\ tableSnapFollows \in SUBSET Rows
  /\ servedFromTable \in BOOLEAN
  /\ servedSim \in SUBSET Pairs
  /\ servedRef \in SUBSET Pairs
  /\ servedAge \in Int

(* Refinement: the stored relation is the reference on the rows committed   *)
(* at its snapshot_at.                                                      *)
TableRefinesReference == hasGen => tableSim = Similar(tableSnapFollows)

(* A request that used the table ranked with the reference of a snapshot    *)
(* at most MaxAge old.                                                      *)
ServedRefinesReference ==
  servedFromTable => (servedSim = servedRef /\ servedAge >= 0 /\ servedAge <= MaxAge)

(* While no run fails, the newest generation is at most one period and one  *)
(* run old.                                                                 *)
FreshWhileRunsSucceed ==
  (~Failures /\ hasGen) => now - tableSnapAt <= Period + Duration

Safe == TableRefinesReference /\ ServedRefinesReference /\ FreshWhileRunsSucceed

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Snapshot
  /\ AgeGuard
  /\ Duration >= 0 /\ Duration < Period /\ MaxAge >= 0
  /\ now <= nextDue
  /\ snapAt <= now
  /\ running => (nextDue = snapAt + Period /\ now - snapAt <= Duration)
  \* What the run has read is the snapshot restricted to the artists read.
  /\ running => readFollows = { r \in snapFollows : r[2] \notin toRead }
  /\ hasGen => tableSnapAt <= now
  \* Without failures the newest generation belongs to the latest finished run.
  /\ (~Failures /\ hasGen /\ ~running) => nextDue = tableSnapAt + Period
  /\ (~Failures /\ hasGen /\ running) => snapAt = tableSnapAt + Period

CInitShared ==
  /\ Fans \in SUBSET (1..3) /\ Artists \in SUBSET (1..3)
  /\ Period \in 1..6 /\ Duration \in 0..5 /\ Duration < Period
  /\ MaxAge \in 0..12
  /\ MaxTime \in 0..40
  /\ SteadyFans \in SUBSET (1..3)

\* The implementation, with and without failing runs.
CInitImplementation ==
  /\ CInitShared
  /\ Snapshot = TRUE /\ AgeGuard = TRUE
  /\ Failures \in BOOLEAN

\* Negative control: reads at different instants instead of one snapshot.
CInitNoSnapshot ==
  /\ Fans = {1, 2} /\ Artists = {1, 2}
  /\ Snapshot = FALSE /\ AgeGuard = TRUE /\ Failures = FALSE
  /\ Period = 2 /\ Duration = 1 /\ MaxAge = 4 /\ MaxTime = 2
  /\ SteadyFans = {1}

\* Negative control: failing runs and no age guard on the request.
CInitNoAgeGuard ==
  /\ Fans = {1, 2} /\ Artists = {1, 2}
  /\ Snapshot = TRUE /\ AgeGuard = FALSE /\ Failures = TRUE
  /\ Period = 3 /\ Duration = 1 /\ MaxAge = 1 /\ MaxTime = 6
  /\ SteadyFans = {1}
====
