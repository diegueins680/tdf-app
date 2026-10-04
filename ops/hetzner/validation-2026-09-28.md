# Hetzner cutover evidence — 2026-09-28

## Live transition — validation incomplete

The authorized web-first traffic and writer transition occurred on 2026-09-28.
Cutover validation remains **incomplete**: Google interactive login and
authenticated uploads have not passed their end-to-end gates. No waiver of
those gates is recorded here. Production
`https://www.tdfrecords.net` now uses `https://api.tdfrecords.net` on the dedicated
Hetzner CPX12. Cloudflare deployment
`4f1e7744-460e-46c7-bdd9-b47d6ae706ad` succeeded at 14:42:14 UTC with frontend
merge `7e7106b36e7ac427711c2f64af650527d619f9ce` from reviewed PR #467.
Both production API bindings changed; all sixteen other bindings were preserved.
The backend runs the reviewed video-ingestion release `cc244b1f86603055997b51379b297baebfd3e7ce`.
All six post-merge main workflows completed successfully.

| Final cutover check | Observed result |
| --- | --- |
| Old writers | TDF source role NOLOGIN; zero TDF connections; both old API machines stopped and cordoned |
| Shared Fly database | Retained running for Trader; four Trader connections observed after cleanup |
| Final export | 10,215,235 bytes; SHA256 `b94ed77e2de06f57be280691c5e197b5eecfbf19480798c38b21b30c20e52541` |
| Restore equivalence | All 616 original tables / 33,189 rows match by sorted row-content hash before migrations |
| Guarded schema | 115 ledger entries, 623 public tables; schema and emergency database-readiness checks passed |
| Assets | 62 uploaded files / 15,399,112 bytes preserved; all 37 referenced local image URLs returned HTTPS 200 |
| Final restricted canary | 64/64 reads passed; concurrency 8; median 165.69 ms, p95 279.26 ms; outbound access blocked |
| Live writer transition | Canary stopped, reader NOLOGIN, production role LOGIN with fresh credentials; live health and exact version passed |
| Browser | Directory search and video feed use the new API with HTTP 200; records page 11 images, zero broken |
| Event preview | `/directory/events/24` and canonical `/eventos/24` return 200; event-specific title and canonical metadata present |
| Payment callbacks | Existing PayPal and Stripe webhook URLs updated; IDs, secrets and subscribed event types preserved |
| Signature rejection | Invalid Stripe signature 400; invalid PayPal signature 401; no real charge created |
| Mail | SMTP STARTTLS and credential authentication passed without sending a message |
| Release leases | This migration's exact token removed on both databases; both lease tables empty |
| Obsolete staging | TDF staging API stopped and cordoned; its TDF-only database stopped; volumes retained |
| Backup | Daily logical timer active; first backup independently restored and schema-verified; hash-verified off-host copy retained |

The first production logical backup is 10,440,643 bytes, SHA256
`229c3b61a22a27865c2fda8e2ee62ed93fb0951547c4d5bf5038a03258faa868`.
Its separate verification database passed the 115-entry schema check before
being dropped. Native Hetzner backups are enabled; this does not assert a
post-cutover server snapshot has already completed.

New writes have been enabled. **Do not reopen the old Fly TDF role or route
traffic back to its stale database.** Application recovery must use the tested
compatible image against the current Hetzner database. A provider reversal
requires another write freeze and fresh reverse migration.

Older installed mobile builds still point at Fly and need an updated release;
the operator explicitly accepted that downtime. Google interactive login and
authenticated uploads have not been exercised end to end after cutover.
No real payment was made. Database emergency-access checks are not evidence
of a manual login. Video ingestion was disabled at cutover and was activated in the verified
follow-up below. Event discovery and automatic publication remain disabled. Event PRs #460/#463/#464
remain separate review and rollout work.

The operator explicitly authorized retaining Trader and the shared Fly database.
Hetzner's account-specific estimate is USD 16.788/month including backups and
IPv4, **not a realized total-bill saving**. Retained Fly storage and the shared
running database continue to incur charges; total spend can temporarily increase.

## Video-ingestion activation follow-up — 15:27 UTC

The operator reported that the two latest official uploads were still missing.
The deployed importer had neither a configured YouTube source nor its global
switch enabled. The authenticated administration API verified the official
`UCx9Jpaw_XDrMtIdzWYlU51g` channel as TDF Records and approved source 137 for
the existing recordings collection. No role or permission grants were changed.

A dry run completed with 38 eligible videos and one review-only item. After
enabling hourly synchronization, the full import completed: two resources
created, 36 refreshed, and one item retained for review. Its known-resource
pass found all 38 verified resources unchanged. A repeat of the same execution
key returned the completed run without applying it again.

The first two public recordings are now `0W4KdgkQD5w` (Llama Este Pez part 2)
and `0hYDXQ5hWfo` (part 1), ordered by YouTube publication time. Both are
available and their provider thumbnails return HTTP 200 / image/jpeg.
The next hourly slot is 16:00 UTC; full reconciliation is weekly on Sunday.
The two removed legacy uploads retain their unavailable status.

## Earlier isolated rehearsal

The following observations describe the rehearsal before production cutover.
The production credentials and live public routes described above supersede
this rehearsal's restricted state.

| Check | Observed result |
| --- | --- |
| Exact release | `cc244b1f86603055997b51379b297baebfd3e7ce`, merged reviewed PR #462 |
| Release image | `diegueins680/tdf-hq@sha256:01bc0553e541cb6ab94550efd9e95b27642ca0624a0243870d0cfe646a4c1420` |
| Recovery image | `diegueins680/tdf-hq@sha256:88331e60cda3dd8e9004e9c3996c7005e786f32752b31c51dd7b9912046fd332` |
| PostgreSQL image | `pgvector/pgvector@sha256:3e8b3adfd27b5707128f60956f62a793c3c9326ea8cfaf0eab7adccb5d700b21` |
| PostgreSQL compatibility | Source 17.2 → restored 17.8; vector 0.8.1 retained |
| Database restore | Custom archive restored without errors; 616 original public tables |
| Guarded schema | Preflight, 115 migrations, checksum ledger, and schema verifier passed |
| Assets | 62 unique uploaded files preserved from both API volumes; generated registry seeded from release |
| Public TLS and health | Trusted certificate; `/health` returns database/status `ok` |
| Public data exposure | `/records/feed` returns 503 through the rehearsal edge |
| Internal feed check | 64/64 HTTP 200 responses, concurrency 8, canonical-origin CORS |
| Server-local latency | Median 190.77 ms, p95 286.34 ms, maximum 374.14 ms |
| Network isolation | API attached only to internal quarantine network; public-IP egress probe failed as required |
| Recovery drill | Healthy on `b0713647b2232a77eb021e4428ed541a196de62b`, then healthy again on target release |
| Idle resources | API ~18 MiB, database ~95 MiB, edge ~13 MiB; host ~1.3 GiB available, zero swap used |
| Local release regression suite | 82 tests passed |
| Owned URL migration | 34 asset URLs, 3 canonical event images, 1 radio URL; repeat run changed zero rows |
| Frontend API-base tests | 5 tests passed |

The RSVP image relation is a derived view: the migration updates its canonical
event metadata and verifies the projection. An initial rehearsal attempted
to update that view and correctly rolled the entire transaction back. The
corrected transaction and its idempotent replay both passed; historical
cutover-source metadata remains untouched.

The HTTPS edge was updated to the current Caddy 2.11.4 release, pinned as
`caddy@sha256:6aeddd44c3078b0f9a35206472a11420648a79c184603ef95957d0a20044cb2b`.
Its [release notes](https://github.com/caddyserver/caddy/releases/tag/v2.11.4)
include security fixes. The API and database retained their tested images.

These are bounded read checks, not an authentication/payment transaction
test, peak-load certification, or proof of multi-region availability.
The rehearsal role cannot write business records; expected worker permission
errors are logged. Only two catalog usage-counter columns are writable for
startup validation. Production provider credentials were not copied.

## DNS and transition history

The existing eleven editable Webador records were backed up and compared
before adding the previously absent `api` A record, TTL 900. The new hostname
points to the dedicated Hetzner host. Website, wildcard, apex, and email
records were preserved in the submitted form. The new address was verified
against authoritative DNS and via normal HTTPS resolution.

The supported Fly staging Machine update was rejected with a billing error,
in addition to the earlier 403 on release creation. This prevents testing
the forwarding path for installed mobile clients, whose release builds use
`tdf-hq.fly.dev`. The staging machine configuration was snapshotted before
the attempt and verified unchanged afterward; production was not touched.
The operator subsequently authorized a web-first cutover and explicitly
accepted downtime for older installed mobile clients until updated.

Private database archives, asset tarballs, credentials, machine snapshots,
and unsanitized logs are intentionally outside this repository. The operator
holds those files locally and in the root-only restore directory on the
dedicated host. The repeatable deployment and recovery procedure is in [README.md](README.md).
