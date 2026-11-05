# Owned heartbeat shutdown

Requirement: `MEDIA-MUSIC-ASSET-001`. Narrow obligation:
`MEDIA-WORKER-HEARTBEAT-STOP-001`.

The worker must stop and reap the heartbeat's native sleep or PostgreSQL client
before heartbeat shutdown returns. A successful worker-shell exit does not
establish that inherited stdout and stderr have drained.

Hosted job `125601931722` on inventory head `dc2e1425c` recorded a worker that
exited successfully after about 0.22 seconds while both output streams remained
open beyond the existing 30-second completion deadline. Its output reported a
successful `validate_release` job. The log did not capture the descriptor holder;
the live process identity and exact signal schedule remain unknown. The inventory
handler regressions had not run when this earlier worker check failed.

A controlled fault using the actual shell helpers lets the first group TERM
reach the heartbeat leader while missing its native child. This models a fork
overlapping group-signal delivery. The original worker then exits zero with open
streams. Both native-sleep and blocked-native-SQL cases must fail the original
implementation and finish successfully with the correction. One hundred ordinary
helper trials on macOS Bash 3.2 all completed; that does not reproduce or exclude
the live Linux race.

The heartbeat's signal callback sets a stopping flag and signals its owned native
child. Waiting and reaping happen outside the callback, including after a signal
interrupts `wait`; nested trap/wait handling is avoided for Bash 3.2 compatibility.
The flag is checked after recording a new child PID so a stop delivered during
startup cannot leave that child running. Both sleep and the native PostgreSQL
client use this supervision.

The existing SQL helper remains the authority for lease guards, transaction
options and values. Only its heartbeat execution path changes. That path must
preserve stdin explicitly: asynchronous commands with job control disabled would
otherwise read `/dev/null`. Normal and supervised paths must send identical SQL
and arguments and preserve success and failure statuses. Intentional shutdown
must not be misreported as lost lease ownership.

The existing 30-second completion deadline and the requirement to observe stream
close remain unchanged. Tests use disposable native-process doubles and synthetic
SQL, never provider credentials or a network database. Their cleanup targets only
the owned fixture groups. Existing real PostgreSQL worker and lease-fencing tests
remain required; focused local runs do not replace hosted Linux qualification.

Assurance is `TESTED` under these controlled conditions. It assumes ordinary
signal delivery/progress for the native child and ownership of the recorded
child PID during its active wait. It does not prove behavior for unkillable OS
tasks, arbitrary subprocess trees, PID reuse during shutdown, every signal
interleaving, or the identity of the descriptor holder in the original CI run.
