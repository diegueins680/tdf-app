# Hetzner restore evidence — 2026-09-28

## Result

An isolated CPX12 restore serves trusted HTTPS health/version checks at
`https://api.tdfrecords.net`. Production remains on Fly. The production Pages
environment still uses `https://tdf-hq.fly.dev`; no live traffic or database
writes have been moved.

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

## DNS and cutover status

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
dedicated host. Full cutover still requires the gates in [README.md](README.md).
