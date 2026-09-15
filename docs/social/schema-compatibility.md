# Complete schema compatibility fixture

This qualification PR depends on the inactive API/client PR #367. It changes tests,
CI and evidence only. All social runtime gates and deployment suppression remain as
in the parent; do not merge or activate the stack.

The harness creates its own private database and accepts no existing database URL.
It restores the repository's schema-only production baseline captured 2026-08-14,
loads its synthetic catalog fixture, and applies the **102 registered migrations**.
It then applies both additive social SQL files, exercises real complete-table
columns/constraints, pauses the social runtime, reapplies, and checks preserved data.

Actual native result: PostgreSQL **16.10**, pgvector available, passed twice including
the final portable harness. Evidence: `evidence/schema-compatibility-final.txt`.
The baseline dump was generated from PostgreSQL 17; native 16 acceptance establishes
compatibility with this fixture, not equivalence to the live database. CI uses a
private `pgvector/pgvector:pg17` container. That actual schema step passed at
`b689f88e2` in run [34991068129](https://github.com/diegueins680/tdf-app/actions/runs/34991068129);
job/step metadata is committed separately from the native result.

```sh
# Docker (same script as CI; no shared ports exposed):
bash scripts/social/test-schema-compatibility.sh
# Installed native PostgreSQL with pgvector (private loopback port/cluster):
TDF_SOCIAL_SCHEMA_NATIVE=1 bash scripts/social/test-schema-compatibility.sh
# Override TDF_SOCIAL_PG_BIN when native binaries are elsewhere.
```

Assertions cover disabled defaults; bilateral consent and block denial; public
preferences; feed membership on actual columns; original source edits and a
backdated insertion; repeated publication batches; preserved immutable positions;
reapplied migration defaults; byte-equal command records/publication snapshots
across pause; and zero block/closure/orphan/order reconciliation violations.
Synthetic identities, usernames and unusable password strings are fixtures only.

This fixture does not start old and new application servers together, make legacy
DM/media/notifications obey blocks, prove index-build lock times under concurrent
production traffic, or validate live data distribution. Application rollback after
activation still requires every legacy deny guard; preserving SQL rows alone does
not prevent an old binary from bypassing them. Destructive cleanup remains deferred.
