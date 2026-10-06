# Sampled host process admission: DEPLOY-JOURNAL-001

`host-process-admission.py` complements the separate scheduler and Docker source
checks. Its reviewed policy is `ops/hetzner/host-process-policy.json`. It does not
stop processes, inspect application data or establish a continuous writer fence.
The coordinator remains responsible for admitting the exact Docker container IDs,
checking scheduler policy, retaining storage descriptors and stopping writers.

The Linux collector requires root and a unified cgroup hierarchy. Each process is
read through its open proc directory, with parent/start-time and cgroup resampled
before acceptance. Kernel threads and zombies cannot supply an application writer
and are omitted. A disappearing task is omitted only when its PID pathname is
also gone; a live task with a missing executable or script rejects the sample.
The collector opens each executable through the deliberate procfs magic link,
hashes its bounded bytes, and checks that the task still references the same
inode, size and modification/change timestamps. A per-sample cache shares hashes
only for identical file identities. Descriptors close on success and rejection.

Exact canonical Docker cgroups must belong to the caller-admitted ID set. Other
processes must match reviewed service-unit components extracted from their
cgroup, executable path/hash and invocation
class. Unknown services, interpreters and executable versions reject. The known
unattended-upgrades interpreter additionally requires its exact shutdown-waiter
script/flag shape and script hash. Root's user manager and PAM helper have explicit
invocation shapes, so the generic systemd executor is not broadly admitted.
Raw arguments are examined locally and never returned or committed.

The observer itself is trusted coordinator code. Its parent chain must be complete
and acyclic. PID1 and exact SSH executables are the only admitted unscoped process
classes, and they must belong to that chain. An unrelated unscoped SSH process therefore rejects admission; the reviewed
ssh.service class remains trusted. This does not certify commands that an already trusted operator
may perform later. Two complete observations must have identical PID/start-time,
parent, cgroup and admitted executable/invocation identities. The public receipt
contains counts and a hash of canonical compact sorted policy JSON plus newline;
it contains no PID list, arguments, environment or process contents.

The initial executable fingerprints are sampled evidence, including processes
still using pre-update deleted binaries. They are not installed-package
attestations. Kernel, procfs, Docker, systemd, the reviewed OS and operator behavior
remain trusted. Hashes of executable files do not attest process memory, shared
libraries, transitive configuration or open descriptors. Known OS workers are
assumed not to mutate TDF application stores. Arbitrary root tampering, a new
short-lived process entirely between observations, later process activity and
all continuous-exclusion claims remain outside this boundary. The receipt states
those limitations; the coordinator must repeat admission at effect boundaries.
An invalid policy cannot be repaired by learning whatever processes are present.

Thirteen deterministic tests check class/ancestry/container denial, interpreter
forms, changed process identity, proc stat/cgroup parsing, and real temporary-file
collector seams for executable replacement, missing live executables, vanished
tasks and descriptor cleanup. These fake proc directories supplement a real
read-only production sample. A controlled inert child check also requires the
actual sample to fail with the child present and pass when only that row is
removed; the helper-owned child is terminated and reaped. This is empirical
classification evidence, not a kernel or implementation refinement proof. The
release coordinator, crash recovery and production rollout remain incomplete.
