# Executable interaction invariants

This specification maps directly to implemented SQL functions and real PostgreSQL
properties; it is not an assertion of unbounded correctness. The existing social
session/pair models remain authoritative for token revocation and bilateral blocks.

Let T be a registered, unretired target with a currently resolvable source, U a
live authenticated actor, C a comment, and R(T,U) its optional single reaction slot.

| Transition | Preconditions | Result and executable evidence |
| --- | --- | --- |
| React(T,U,k) | Source readable, no bilateral block; non-null k additionally requires domain write grant and published selectable catalog type | R(T,U)=k or absent for null; PK(target,actor); each type total equals authoritative rows. `schema-properties.sql`, `test-concurrency.py` |
| Create(T,U,parent,body,key) | Current comment policy; valid source/session; optional parent belongs to T; bounded body/mentions | Immutable target/root/parent; deeper child retains root. Repeated actor/key+payload produces the same ID; changed payload conflicts. `command-properties.sql`, `legacy-properties.sql`, concurrency runner |
| Edit(C,U,version) | Visible, own comment, current access, expected version | Version increments; body/mentions validated atomically; no ownership/root changes. `command-properties.sql` |
| Delete(C,U,version) | Own visible/hidden comment, current access, expected version | Erase body/mentions/legacy presentation; deleted tombstone retains all descendants and identity. `command-properties.sql`, `moderation-properties.sql`, `legacy-properties.sql`, concurrency runner |
| Hide(C,U) | Content management authority | Hidden body available only through scoped moderation; replies retain a structural placeholder. Never grants ability to edit another author. `moderation-properties.sql`, `entity-properties.sql` |
| Remove(C,U) | Strict current moderator role | Erase body and archived source body; append immutable audit; resolve reports. `moderation-properties.sql` |
| Block(U,V) | Current session, expected pair version | Both directions deny interaction; clear canonical and legacy follow/consent; include actor-owned directory blocks with versioned removal. Unblock never recreates grants or clears a peer-owned block. `policy-properties.sql`, `navigation-properties.sql`, concurrency runner |
| Notify(event,U) | Current target/actor/recipient access and preferences | Recipient priority/dedupe, bounded durable fanout, exact opaque destination; recheck on inbox read. `notification-properties.sql`, `navigation-properties.sql`, real HTTP/browser flow |
| DeleteSource(T) | Existing source-specific deletion authority | Retire target, erase bodies, remove reactions; preserve comment graph/audit and prohibit source-key reuse inheriting a thread. `entity-properties.sql` |
| Activate | First enabled transition under legacy write/table locks | Reject unresolved/null author identities atomically; preserve reconciled engagement identities and payloads exactly; fence old writers; irreversible activation history. Pause denies commands without old-path fallback. `legacy-properties.sql` |

For every readable projection, authorization and blocking precede pagination and
aggregation. Hidden/deleted author identities and bodies never appear in public
placeholders. Private event access depends on organizer/logistics authority; old
self-created invitations cannot grant it (`entity-properties.sql`).

Counters are transactional target/author/type/root projections. Filtered counts
use current author eligibility, not client-supplied numbers. Concurrent operations
lock current source/grants/accounts plus the target before rechecking permissions;
serialization/deadlock failures are retryable, never partial accepted effects.

Run `scripts/interactions/test-schema.sh` (1,200 generated reaction/counter states)
and `scripts/interactions/test-database.sh` (production-shaped migration, direct
function properties and multiconnection interleavings). Frontend model tests run
2,000 reaction transitions and 200 Unicode mention edits. `test-api.py` exercises
real bearer/session middleware. Browser/native tests cover rendered behavior,
which SQL properties alone cannot prove. CI runs the database suite on PostgreSQL
17; local evidence uses PostgreSQL 16 and all SQL suites also pass on a restored production PostgreSQL 17.8 schema. The tests cover selected bounded
interleavings, not every scheduler, infrastructure failure or future domain adapter.

Notification delivery models new activity separately from desired-state commands.
For `React(T,U,k)` with `R(T,U)=k`, the event count is unchanged. An edit snapshots
`newMentionIds = nextMentionIds - previousMentionIds` before replacing mentions.
Delivery selects the highest enabled reason in `mention > reply > comment`, never
letting a disabled high-priority reason erase an enabled lower-priority reason.
`notification-properties.sql` executes all eight preference assignments, existing
read-notification mention promotion, unchanged-mention silence, reaction no-ops,
new-actor aggregate refresh and completed-event replay. A persisted event watermark
prevents an older/replayed event from making a read notification unread again.

`moderation-properties.sql` proves directory blocks affect the same account list
and pair version, stale unblocks fail, opposite-direction blocks remain effective,
old follow grants stay revoked, and private report reasons are moderator-only.
These are state/authorization properties of the actual migration functions.

Withdrawal after lost write eligibility is verified against actual SQL and HTTP commands.
An accessible target permits removal only from the actor's own slot. Summary
choices become nonselectable, while the existing selected reaction remains
removable in both clients; restoring eligibility never restores a withdrawn slot.

Social blocking is not an authorization revocation for scoped enforcement.
`moderation-properties.sql` exercises an author blocking both owner and moderator:
normal lists remain filtered, privileged queues retain evidence and actual state,
redacted deep-link context remains reachable, owner hide/restore and moderator
report resolution/removal succeed, owner administrative removal remains denied,
and removed bodies are erased and audited. HTTP plus rendered web/native tests
exercise the same state boundary, including the owner's queue action.
