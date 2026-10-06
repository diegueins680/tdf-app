# Reviewed host schedulers: DEPLOY-JOURNAL-001

`ops/hetzner/host-scheduler-admission.py` checks a separately reviewed OS scheduling
policy before the future coordinator establishes its application writer fence.
It performs no lifecycle operation and cannot authorize a deployment. The
canonical policy is `ops/hetzner/host-scheduler-policy.json`; it is deliberately
pinned rather than learned or relaxed during admission.

The October6 observation covered16 cron files (including the yearly placeholder),
52 system unit definitions and two root-user timer/service definitions. Every
recorded fragment, drop-in and cron file matched its installed package checksum.
That supports an explicit trust assumption about the installed OS packages; it
is not a proof of their behavior, their package database, or transitive program
configuration. The provenance observation is separate from runtime admission.

Admission requires root, only reviewed running system services, exactly the
reviewed loaded system timers plus the TDF backup timer, and an empty `at` queue.
This helper rejects a backup service reported as running. The separate
WriterFence requires it to be inactive, checks the TDF unit configuration and
stops its timer. The root user manager is examined through
its fixed `/run/user/0` bus. Its complete loaded timer set and reviewed timer and
service definitions must match. Other active user managers are rejected. This
covers that manager's timers; arbitrary socket-activated workers and other
processes still require the separate process inventory.

Cron enumeration covers system crontab/anacrontab, hourly/daily/weekly/monthly/
yearly scripts, cron.d and all user crontabs. Unknown or changed files reject.
Files must be root-owned, single-link regular files, at most1MiB, with no group or
world write access, and are opened without symlink ancestors. Cron directories
are root-owned without group/world write, except the canonical crontab spool's
root:crontab1730 layout. Directory identities and names are sampled again to
reject observed replacement. This does not prevent a privileged cron writer from
acting between observations.

System and root-user unit fragments, ordered drop-in lists, content hashes,
loaded state and absence of pending reload/transient configuration must match.
The closing observation repeats cron, queue, unit names **and all unit rows**.
The policy hash covers canonical compact sorted JSON with a trailing newline,
not the indented file bytes. A returned hash means those observations matched; it does not reserve
resources, stop schedulers, exclude processes, or establish continuous writer
exclusion. The coordinator must repeat admission at effect boundaries and
separately admit processes, retained mounts, canonical Docker writers, deadlines
and recovery. Kernel, systemd, root OS behavior and absence of noncooperating
privileged changes remain environmental assumptions.

A package update or new scheduler can legitimately invalidate this policy. Review
its behavior and provenance, update the canonical policy through normal review,
and rerun controls. Never overwrite the policy with whatever a failing host
happens to report. Ten deterministic tests cover positive scope and rejection of
unknown services/timers, active backups, queued jobs, cron and unit changes,
unexamined user managers and root-user timer/configuration drift. These mock OS
observations; a real read-only production sample is separate evidence. This
boundary adds no formal refinement or liveness claim.
