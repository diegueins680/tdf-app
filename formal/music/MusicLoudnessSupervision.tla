---- MODULE MusicLoudnessSupervision ----
(***************************************************************************)
(* MEDIA-MUSIC-LOUDNESS-001, invariant L3                                  *)
(* (formal/system/music-loudness-check.md; scripts/measure-music-          *)
(* loudness.pl).                                                           *)
(*                                                                         *)
(* One run of the wrapper around one child process. The child is not       *)
(* trusted: it may exit at any moment with any status and any output, or   *)
(* hang and ignore TERM. The wrapper counts seconds while the child is     *)
(* alive, sends TERM at Limit and KILL at Limit + Grace, reaps the child   *)
(* and then prints exactly one result.                                     *)
(*                                                                         *)
(* This model is about the supervision only. It assumes what the operating *)
(* system guarantees (KILL ends a process) and says nothing about FFmpeg.  *)
(*                                                                         *)
(* Guards, each removed by one negative configuration:                     *)
(*   Kill       KILL follows an ignored TERM after Grace                   *)
(*   Reap       a result is printed only after the child is gone           *)
(*   Strict     "measured" requires a clean exit, a complete in-range      *)
(*              summary and no timeout                                     *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  \* @type: Bool;
  Kill,
  \* @type: Bool;
  Reap,
  \* @type: Bool;
  Strict,
  \* @type: Int;
  Limit,
  \* @type: Int;
  Grace

VARIABLES
  \* @type: Bool;
  alive,       \* the child process exists
  \* @type: Bool;
  hangs,       \* this child never exits by itself and ignores TERM
  \* @type: Int;
  elapsed,     \* seconds counted while waiting for the child
  \* @type: Bool;
  termSent,
  \* @type: Bool;
  timedOut,
  \* @type: Str;
  exit,        \* running | ok | error | missing | killed
  \* @type: Str;
  output,      \* none | valid | silent | range | garbage
  \* @type: Str;
  result,      \* none, or the one result printed
  \* @type: Int;
  printed      \* results printed

vars == <<alive, hangs, elapsed, termSent, timedOut, exit, output, result, printed>>

Results ==
  {"measured", "timeout", "decode_error", "tool_unavailable", "no_summary",
   "unparseable", "silent", "out_of_range"}

Init ==
  /\ alive = TRUE
  /\ hangs \in BOOLEAN
  /\ elapsed = 0
  /\ termSent = FALSE
  /\ timedOut = FALSE
  /\ exit = "running"
  /\ output = "none"
  /\ result = "none"
  /\ printed = 0

Done == printed > 0

\* The child ends by itself, in any way, with any output.
ChildExit ==
  /\ alive /\ ~hangs
  /\ alive' = FALSE
  /\ exit' \in {"ok", "error", "missing"}
  /\ output' \in {"none", "valid", "silent", "range", "garbage"}
  /\ UNCHANGED <<hangs, elapsed, termSent, timedOut, result, printed>>

TermDue == elapsed = Limit /\ ~termSent
KillDue == elapsed = Limit + Grace /\ termSent

\* With Kill = FALSE nothing bounds the wait (negative control).
Tick ==
  /\ alive /\ ~Done
  /\ ~TermDue
  /\ ~(Kill /\ KillDue)
  /\ elapsed' = elapsed + 1
  /\ UNCHANGED <<alive, hangs, termSent, timedOut, exit, output, result, printed>>

\* A child that does not hang obeys TERM; one that hangs ignores it.
SendTerm ==
  /\ alive /\ TermDue
  /\ termSent' = TRUE
  /\ timedOut' = TRUE
  /\ alive' = hangs
  /\ exit' = IF hangs THEN exit ELSE "killed"
  /\ UNCHANGED <<hangs, elapsed, output, result, printed>>

SendKill ==
  /\ Kill
  /\ alive /\ KillDue
  /\ alive' = FALSE
  /\ exit' = "killed"
  /\ UNCHANGED <<hangs, elapsed, termSent, timedOut, output, result, printed>>

Classify ==
  IF timedOut THEN "timeout"
  ELSE IF exit = "missing" THEN "tool_unavailable"
  ELSE IF exit /= "ok" THEN "decode_error"
  ELSE IF output = "none" THEN "no_summary"
  ELSE IF output = "garbage" THEN "unparseable"
  ELSE IF output = "silent" THEN "silent"
  ELSE IF output = "range" THEN "out_of_range"
  ELSE "measured"

Emit ==
  /\ ~Done
  /\ Reap => ~alive
  /\ result' = IF Strict THEN Classify ELSE "measured"
  /\ printed' = printed + 1
  /\ UNCHANGED <<alive, hangs, elapsed, termSent, timedOut, exit, output>>

Next == ChildExit \/ Tick \/ SendTerm \/ SendKill \/ Emit

Spec == Init /\ [][Next]_vars

TypeOK ==
  /\ alive \in BOOLEAN /\ hangs \in BOOLEAN
  /\ elapsed \in Nat /\ elapsed <= Limit + Grace + 1
  /\ termSent \in BOOLEAN /\ timedOut \in BOOLEAN
  /\ exit \in {"running", "ok", "error", "missing", "killed"}
  /\ output \in {"none", "valid", "silent", "range", "garbage"}
  /\ result \in Results \cup {"none"}
  /\ printed \in 0..1

(* The wait is bounded by the limit plus the kill grace.                   *)
Bounded == elapsed <= Limit + Grace

(* Exactly one result once done, never with a child left behind.           *)
OneResult ==
  /\ printed <= 1
  /\ Done <=> result /= "none"
  /\ Done => ~alive

(* "measured" only from a clean exit with a complete in-range summary.     *)
Honest ==
  result = "measured" => (exit = "ok" /\ output = "valid" /\ ~timedOut)

(* While not done, some step is enabled: the run cannot stall.             *)
CanStep ==
  \/ (alive /\ ~hangs)                     \* ChildExit
  \/ (alive /\ ~TermDue /\ ~KillDue)       \* Tick
  \/ (alive /\ TermDue)                    \* SendTerm
  \/ (alive /\ KillDue)                    \* SendKill
  \/ ~alive                                \* Emit
NoStall == ~Done => CanStep

Safe == Bounded /\ OneResult /\ Honest /\ NoStall

IndInv ==
  /\ TypeOK
  /\ Safe
  /\ Kill /\ Reap /\ Strict
  /\ Limit >= 1 /\ Grace >= 1
  /\ alive => exit = "running"
  /\ exit = "running" => (alive /\ output = "none")
  /\ termSent => (elapsed >= Limit /\ timedOut)
  /\ ~termSent => elapsed <= Limit
  /\ timedOut => termSent
  /\ (alive /\ termSent) => hangs
  /\ exit = "killed" => timedOut
  /\ Done => result = Classify

CInitGuarded ==
  /\ Kill = TRUE /\ Reap = TRUE /\ Strict = TRUE
  /\ Limit \in 1..900 /\ Grace \in 1..60

CInitNoKill ==
  /\ Kill = FALSE /\ Reap = TRUE /\ Strict = TRUE /\ Limit = 2 /\ Grace = 1

CInitNoReap ==
  /\ Kill = TRUE /\ Reap = FALSE /\ Strict = TRUE /\ Limit = 2 /\ Grace = 1

CInitTrusting ==
  /\ Kill = TRUE /\ Reap = TRUE /\ Strict = FALSE /\ Limit = 2 /\ Grace = 1

====
