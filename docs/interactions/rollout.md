# Interaction rollout and recovery

Production schema migration is additive and separate from activation. The live
baseline was 115 reviewed migrations on PostgreSQL 17.8; eight existing social
compatibility migrations and fourteen interaction migrations are registered in
order. Existing social-v2 rollout gates remain disabled. No feature activation
is a schema-install side effect.

## Release gates

1. Merge through normal protected review/CI; build an immutable backend image.
   Apply only the exact merged manifest using the existing Hetzner release lane.
2. Capture a fresh encrypted/off-host production backup and rehearse its restore.
   Capture exact legacy row counts and parent/reaction validation in a read-only
   transaction; earlier table estimates are not conversion evidence.
3. Rehearse the complete manifest plus first activation on a restored isolated
   PostgreSQL 17 database. Verify cutover source/migrated counts agree, references
   and known IDs persist, counters match authoritative rows, and repeated
   activation is a no-op. Exercise pause and resume. Never send real notifications
   or provider requests from a rehearsal.
4. Stage schema with `RUN_MIGRATIONS=false` and install the new backend on every
   TDF writer before activation. Confirm source/target adapters, authentication,
   existing events/feed/records flows, credentials and webhook health. Do not
   touch Trader or the shared Fly database. Old Fly TDF writers stay fenced.
5. Deploy web and signed native releases. Configure existing Cloudflare association
   functions with the verified public certificates in implementation.md. Check
   HTTPS association responses without redirects and installed-app deep links.
6. In the existing deployment lease, enable `interaction_runtime.enabled` in a
   guarded transaction. The activation trigger locks all legacy engagement
   sources, validates/converts nonempty engagement and installs permanent write
   fencing atomically. Inspect `interaction_legacy_cutover`: source/migrated counts
   must match. No engagement rows are silently discarded. Keep source tables as
   read-only mapping evidence; author/moderator erasure scrubs their private body.
7. Verify owned canary accounts on real web/native clients: four reactions,
   comment/reply/edit/delete, bounded expansion, mentions, notifications and exact
   link focus; ownership/moderation, blocking, private-event access and denied
   legacy writes. Remove canary discussion bodies through author deletion.
8. Capture exact deployment digest/commit/ledger, checks, and recovery evidence.

## Recovery

Before first activation, keep the gate disabled and repair forward. Installation
alone does not modify legacy engagement or stop its existing writers.

After activation, `UPDATE interaction_runtime SET enabled=false WHERE singleton`
is the non-destructive emergency pause. New writes and delivery stop; canonical
read privacy and permanent legacy fences remain. Preserve the new backend and
all canonical tables/mappings/receipts/counters/audits. Never downgrade to an old
backend that can read frozen source bodies without current blocks. Never reset
`activated_once`, remove the fences, truncate interactions, reopen the old Fly
TDF role, or restore a stale snapshot over accepted writes. Fix forward and
resume by enabling the gate; the conversion ledger makes resumption idempotent.

Disaster recovery restores the whole current database and compatible immutable
application together using the existing backup runbook. A backup from before
activation is not a safe routine rollback once new interactions have been accepted.
External notification delivery is limited to the existing in-app inbox; no email
or push distribution is newly activated by this migration.
