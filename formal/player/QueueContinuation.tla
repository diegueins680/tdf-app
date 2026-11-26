--------------------------- MODULE QueueContinuation ---------------------------
(***************************************************************************)
(* MEDIA-MUSIC-CONTINUATION-001 (formal/system/queue-continuation.md).     *)
(*                                                                         *)
(* When the web player queue ends, the client walks the artists the server *)
(* recommended, in the server's order, and appends at most one recording   *)
(* per artist that is not queued yet, up to a limit.                       *)
(*                                                                         *)
(* The server response is abstract: a set of served artists and a strict   *)
(* order on all artists (rank). Which artists the server may serve is the  *)
(* subject of COMMUNITY-ARTIST-SIMILARITY-001; this model shows that the   *)
(* client adds nothing to that set and keeps its order.                    *)
(*                                                                         *)
(* Abstraction: the client takes the FIRST eligible track of an artist;    *)
(* the model lets it take ANY eligible recording, so the invariants hold   *)
(* for every choice. Determinism of the choice is tested, not modelled.    *)
(*                                                                         *)
(* QueueFilter = FALSE is the negative control: without the check against  *)
(* the queue a queued recording is appended again.                         *)
(***************************************************************************)
EXTENDS Integers

CONSTANTS
  \* @type: Set(Int);
  Artists,
  \* @type: Int;
  ArtistCount,
  \* @type: Set(Int);
  Recordings,
  \* @type: Int;
  Limit,
  \* @type: Bool;
  QueueFilter

VARIABLES
  \* @type: Set(Int);
  served,
  \* @type: Int -> Int;
  rank,
  \* @type: Int -> Set(Int);
  catalog,
  \* @type: Set(Int);
  queue,
  \* @type: Int;
  cursor,
  \* @type: Int;
  count,
  \* @type: Set({slot: Int, artist: Int, rec: Int});
  appended

vars == <<served, rank, catalog, queue, cursor, count, appended>>

RankOK ==
  /\ rank \in [Artists -> 1..ArtistCount]
  /\ \A a \in Artists: \A b \in Artists: rank[a] = rank[b] => a = b

Init ==
  /\ served \in SUBSET Artists
  /\ RankOK
  /\ catalog \in [Artists -> SUBSET Recordings]
  /\ queue \in SUBSET Recordings
  /\ cursor = 1
  /\ count = 0
  /\ appended = {}

Taken == {x.rec: x \in appended}

\* @type: (Int, Int) => Bool;
Eligible(a, r) ==
  /\ r \in catalog[a]
  /\ r \notin Taken
  /\ (QueueFilter => r \notin queue)

\* Visit the artist at the cursor.
Visit ==
  /\ cursor <= ArtistCount
  /\ count < Limit
  /\ \E a \in Artists:
       /\ rank[a] = cursor
       /\ \/ /\ a \in served
             /\ \E r \in Recordings:
                  /\ Eligible(a, r)
                  /\ appended' = appended \union {[slot |-> count + 1, artist |-> a, rec |-> r]}
                  /\ count' = count + 1
          \/ /\ (a \notin served \/ \A r \in Recordings: ~Eligible(a, r))
             /\ UNCHANGED <<appended, count>>
  /\ cursor' = cursor + 1
  /\ UNCHANGED <<served, rank, catalog, queue>>

\* A rank without an artist cannot occur when RankOK holds on ArtistCount artists;
\* the step keeps the walk total if the constants disagree.
SkipEmptyRank ==
  /\ cursor <= ArtistCount
  /\ count < Limit
  /\ \A a \in Artists: rank[a] # cursor
  /\ cursor' = cursor + 1
  /\ UNCHANGED <<served, rank, catalog, queue, count, appended>>

Next == Visit \/ SkipEmptyRank

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ served \in SUBSET Artists
  /\ RankOK
  /\ catalog \in [Artists -> SUBSET Recordings]
  /\ queue \in SUBSET Recordings
  /\ cursor \in 1..(ArtistCount + 1)
  /\ count \in 0..ArtistCount
  /\ appended \in SUBSET [slot: 1..ArtistCount, artist: Artists, rec: Recordings]

\* Every appended recording belongs to an artist the server returned.
Subset == \A x \in appended: x.artist \in served /\ x.rec \in catalog[x.artist]

\* The appended artists follow the server order; hence one track per artist.
Order == \A x \in appended: \A y \in appended: x.slot < y.slot => rank[x.artist] < rank[y.artist]

\* Nothing queued is appended and nothing is appended twice.
NoRepeat ==
  /\ \A x \in appended: x.rec \notin queue
  /\ \A x \in appended: \A y \in appended: x.rec = y.rec => x = y

Bound == count <= Limit /\ \A x \in appended: x.slot <= Limit

Safe == Subset /\ Order /\ NoRepeat /\ Bound

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Limit >= 0
  /\ \A x \in appended: x.slot >= 1 /\ x.slot <= count /\ rank[x.artist] < cursor
  /\ \A x \in appended: \A y \in appended: x.slot = y.slot => x = y

CInitShared ==
  /\ Artists = {1, 2, 3}
  /\ ArtistCount = 3
  /\ Recordings = {1, 2, 3, 4}
  /\ Limit \in 0..3

CInitImplementation == CInitShared /\ QueueFilter = TRUE

CInitNoQueueFilter == CInitShared /\ QueueFilter = FALSE
================================================================================
