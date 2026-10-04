# Legacy direct-message write boundary — 2026-09-15

**Inactive, incomplete cutover. Do not activate the social runtime.** Depends on
session PR #382 and its stack. This migration makes existing `chat_message` writers
obey canonical consent/block/lifecycle policy once required, including writers from
an older application version. Legacy thread/history reads and HTTP error mapping
are not repaired by a database write trigger. Those remain activation blockers.

## Audit and selected compatibility behavior

`TDF.Server.ensureCanChatWith` currently allows administrators or reciprocal
`PartyFollow` rows. `chatSendMessage` checks this in a different transaction from
its INSERT. Reciprocal rows can be manufactured by legacy social flows, and no
new block is checked on that write. Existing `ChatThread` and `ChatMessage` remain
the authoritative thread/message tables; no new message store, broker or delivery
service is introduced. Thread identities remain owned by chat; follows never become
new consent. No message is copied into a social projection.

The additive migration installs a shared SQL predicate and a BEFORE INSERT/UPDATE
trigger on the existing message table. It locks both Party rows and credential rows
in canonical order **before** deciding whether to use legacy compatibility. This
also serializes a first block that creates the pair with a simultaneous message.
For protected pairs it requires current active account eligibility, a thread
participant sender, bilateral independent consent and no block in either direction.
The application uses PostgreSQL READ COMMITTED. The trigger rejects any other
isolation mode with `0A000/social_dm_requires_read_committed`; a long-lived
REPEATABLE READ snapshot cannot safely realize this boundary merely by waiting on
row locks. Both REPEATABLE READ and SERIALIZABLE denial are tested.
The database does not provide an administrator bypass. Unblock does not restore
consent. Updates and upserts use the same check; deletion remains available for
existing moderation/data removal.

Compatibility rules:

1. Before any social activation, with no canonical pair/tombstone and no social
   closure, old application permission behavior is preserved. This does not certify
   old mutual follows as valid new consent.
2. A canonical pair or social closure always imposes its authoritative restriction,
   even if the UI/process/runtime flag is off. Pair history is retained; an empty
   disconnected pair cannot become an ungoverned legacy pair again.
3. A new `social_v2_runtime.activated_once` boolean defaults false. Activation sets
   it permanently true. An already-enabled fixture is backfilled true. Ordinary
   attempts to clear it are normalized to true; DELETE/TRUNCATE of the runtime row
   are rejected. A missing runtime row makes the predicate require policy. A pause
   sets `enabled=false` but does not erase this memory or restore legacy access.
4. Once activated, all DM writes require canonical consent. Existing conversations
   without that consent need the explicit connection flow; no consent backfill is
   performed. Do not activate until UI/API/history behavior makes this transition
   usable and every remaining privacy surface has been qualified.

A message that wins the account locks may commit before a competing block. If the
block commits first, the waiting insert is denied with SQLSTATE `42501` and stable
code `social_dm_not_permitted`. The old HTTP handler does not map that code to a
friendly 403 yet; it must be updated before activation. Old and new applications
can share the additive schema for writes, but **old history readers remain unsafe**.
This is write-path compatibility evidence, not full old/new application qualification.

## Research-to-decision

[PostgreSQL 16 trigger behavior](https://www.postgresql.org/docs/16/trigger-definition.html)
(accessed 2026-09-15; page update date unavailable) describes row triggers and their
participation in the statement's transaction. [Transaction isolation](https://www.postgresql.org/docs/16/transaction-iso.html)
(accessed the same date) describes statement visibility and conflicts. TDF inference:
a transactionally checked existing-table trigger is a small compatibility fence for
old writers. Reject a flag-off fallback that forgets stored blocks; reject relying
only on the new endpoint when legacy INSERTs remain available. This adds lock/query
cost even before activation; no production latency claim is made. Deployment must
retain the guards and account for lock acquisition duration on real data.

## Executable evidence and limits

`LegacyDm.tla` models two consent owners, pair existence, blocking, closure,
activation/pause and the observed INSERT decision. **410 generated / 101 distinct
states, depth 8**; authority, consent integrity, activation memory and fair completion
passed with pinned TLC distribution 1.7.2 (reports 2.17), Java 17.0.12. Two unsafe
configurations each produce the exact `AuthorityAtInsert` counterexample: forgetting
policy while paused, and substituting legacy permission for independent consent.
The raw logs are in `evidence/dm-models/`.

`generate-dm-cases.py` reads actual observed `Insert` edges from the exported checked
DOT graph and emits **27 SQL cases**. Each case runs against the real trigger inside
a transaction rolled back afterward, preserving the never-reset activation rule
between production transactions. No generated expected permission comes from SQL.

| Requirement | Model | Implementation | Actual test |
|---|---|---|---|
| S-DM-CONSENT | AuthorityAtInsert / Consent, Disconnect, Insert | social_v2_dm_allowed + trigger | 27 generated observations; bilateral sends, outsider denial, unblock requires new consent |
| S-DM-PAUSE | AuthorityAtInsert, ActivationMemory / Activate, Pause | activated_once trigger; pair/closure predicate | disable/reset/delete/truncate attempts; blocked retry after pause and reapply |
| S-DM-REVOKE | atomic Insert versus Block/Close | sorted Party/credential locks, current SQL policy | real block-first and send-first lock barriers; inactive credential and closed account |
| S-DM-STALE | AuthorityAtInsert / Insert after Block/Disconnect | every INSERT/UPDATE, including upsert, rechecks | blocked edit/upsert denied; old direct INSERT denied |
| S-DM-PROGRESS | fair Insert eventually done | lock-holder completion and database availability assumed | actual waiting sessions observed; both race orders terminate |

The model abstracts the INSERT policy boundary atomically; concrete locking is
qualified by PostgreSQL races, not proven by this small model. It does not model
all lock graphs, multi-thread transactions, catalog-role revocation, token sessions,
message contents or external notification delivery. It assumes thread participant
identities are immutable through supported APIs and database owners do not disable
triggers, replace functions, change replication mode or restore obsolete authority.
Session identity remains the application's trust boundary, separately modeled in
#382. The trigger cannot authenticate a client merely from `sender_party_id`.
Legacy message retry deduplication is not added: its contract has no request key.

Actual fixture results (terminal trailing whitespace normalized in copied logs):

- Without the new trigger, generated **case 2 failed**: the old direct INSERT
  accepted a socially closed participant. Exit 3 is the expected observed regression,
  not a passing run. `evidence/dm-writes-before.txt`.
- With the migration, all **27 generated cases** and the targeted SQL assertions
  passed, including both observed block/send lock orders. Four intended messages
  and eleven command records remain after pause/reapply. `evidence/dm-writes.txt`.
- Complete schema-only baseline + **102 registered migrations**, synthetic old
  thread/message writes, new guard application/reapplication and preserved-write
  pause passed on private PostgreSQL **16.10**. Original message preserved; blocked
  legacy retry denied; feed/consent/publication reconciliation still passed.
  `evidence/dm-full-schema.txt`. Hosted PostgreSQL 17 results are tracked separately.
- No real production experiment, full-app/mobile journey or message-read privacy
  claim. Fixture INSERT cost: five accounts, five warmups and 50 samples per mode,
  server-side timing within each batch (network/commit time excluded). Predeclared
  added p95 limit: **10 ms**. Before trigger p50/p95 **0.052/0.110 ms**, inactive
  trigger **0.487/0.903 ms**, active trigger **1.301/1.615 ms**; largest p95 increase
  **1.505 ms**. Passed on this fixture; sequential batches and tiny data do not
  establish production/high-degree latency or contention. `evidence/dm-benchmark.txt`.
- Catalog audit passed with the inherited internal `SessionAccess` classification;
  **21 workflow/scope tests passed**. Hosted checks are recorded separately.

```sh
TLA_JAR=/path/to/tla2tools-1.7.2.jar bash scripts/social/check-dm-model.sh
java -cp /path/to/tla2tools-1.7.2.jar tlc2.TLC -workers 1 -deadlock \
  -metadir /tmp/dm-states -dump dot,actionlabels /tmp/dm.dot \
  -config formal/social/LegacyDm.cfg formal/social/LegacyDm.tla
python3 scripts/social/generate-dm-cases.py /tmp/dm.dot /tmp/dm-model-cases.sql
diff -u scripts/social/dm-model-cases.sql /tmp/dm-model-cases.sql
bash scripts/social/test-dm-writes.sh
# Private native fallback; no existing database URL is accepted:
TDF_SOCIAL_NATIVE=1 bash scripts/social/test-dm-writes.sh
TDF_SOCIAL_NATIVE=1 TDF_SOCIAL_DM_BENCHMARK=1 bash scripts/social/test-dm-writes.sh
TDF_SOCIAL_SCHEMA_NATIVE=1 bash scripts/social/test-schema-compatibility.sh
# Expected failure: old unguarded write, same generated assertions:
TDF_SOCIAL_NATIVE=1 TDF_SOCIAL_DM_UNSAFE_CONTROL=1 bash scripts/social/test-dm-writes.sh
```

## Migration, reconciliation and rollback

Apply `2026-09-15_social_v2_dm_write_boundary.sql` after foundation/read-model
migrations and existing chat schema, in a separately approved migration window.
It is **not registered for automatic production boot migrations**. The single-row
activation backfill and function/trigger replacement are idempotent and transactional;
there is no historical message rewrite. Locks for ALTER/TRIGGER creation require a
measured deployment window. Existing clients keep their request/response contracts.

Pause with the existing social pause script. Preserve messages, pair/command
history, closure preferences and activated_once. Rolling the application back while
retaining this write guard preserves its database denials; rolling back to an old
history reader is still unsafe for full privacy. Do not drop the guard or erase the
activation latch to obtain legacy access. Removing additive structures before any
activation/pair/closure writes could be considered separately; no destructive down
migration is supplied or described as reversible after activation. The tested
rollback is preserved-data application pause with enforcement retained.

Monitor aggregate `42501`/stable-code counts and transaction latency/lock waits,
without logging message bodies or relationship identities. A high denial rate can
mean a stale client needs the consent flow; it is not proof of abuse. Keep existing
report/review/appeal authorities. Read/filter integration, API error mapping, rollout
lock-duration measurement and all wider handoff blockers remain required.

## Hosted verification snapshot

[Social CI](https://github.com/diegueins680/tdf-app/actions/runs/35011873153)
completed successfully at implementation `efc827f7628afe084db173be627d0d7f71d0142a`.
It ran the DM model and both negative controls, regenerated the observed INSERT
cases, executed the DM migration/races on PostgreSQL 17, and passed the complete
schema fixture. Existing relationship/feed/session models and client checks also
passed. Exact step results: `evidence/dm-ci-efc827f76.json`. [Full CI](https://github.com/diegueins680/tdf-app/actions/runs/35011873154) also
completed successfully at that SHA, including backend build/tests, the actual
session HTTP fixture, merch runtime, automatic migrations and booking concurrency.
Exact metadata: `evidence/dm-full-ci-efc827f76.json`. Later evidence-only commits
must not be confused with checks of this implementation SHA.
