# MEMORY.md

## Workspace

- This repo uses `AGENTS.md`, `SOUL.md`, `USER.md`, daily `memory/YYYY-MM-DD.md`, and now `AI_WORKFLOW.md` as the main continuity and onboarding surface for AI work.
- `scripts/continuous-improvement-loop.codex.json` currently targets `main`, so unattended loop runs should be treated as high-risk unless copied to a branch-scoped config first.
- Marketplace Stripe safety commit `876413b721049da71d66a8d67c0162f97c72bef8` was deployed to production as Fly release `v2049` on 2026-07-12 after applying the 2026-07-07 and 2026-07-12 marketplace migrations with old writers quiesced. Production keeps `RUN_MIGRATIONS=false`; the `tdf-hq-db` volume was expanded from 3 GB to 5 GB during the cutover.
- On 2026-07-14, the marketplace orders admin route in `tdf-hq` had to be moved ahead of the public marketplace capture so `/marketplace/orders` would not be mistaken for a listing id. The corresponding server composition order in `tdf-hq/src/TDF/Server.hs` must stay aligned with `tdf-hq/src/TDF/API.hs`.
- Production notification schema drift was repaired on 2026-07-12 with the narrow `tdf-hq/sql/2026-07-12_notification_table.sql` migration. Do not apply the broader `tdf-hq/sql/2026-05-25_content_engagement.sql` as part of Stripe or ticketing work.
- The 2026-07-13 ticketing/event-discovery release lane is intentionally guarded and commit-based. Production is currently a partial release: discovery schema exists and ran once with zero imported records, but ticket runtime schema and Stripe Fly secrets are still missing. Do not push/release until main-branch auto-deploy risk is resolved, `EVENT_DISCOVERY_ENABLED=false` is effective, and `STRIPE_SECRET_KEY`/`STRIPE_WEBHOOK_SECRET` are set.
- Event discovery was expanded on 2026-07-30 to explicit country-aware city subscriptions, six-hour per-source runs, Ticketmaster + Buen Plan + structured venue feeds, and canonical multi-source purchase options. Production additionally requires `tdf-hq/sql/2026-07-30_event_city_subscriptions.sql`; keep the master switch off through the guarded migration/backend rollout, and treat Buen Plan as independently disableable because its public endpoint is undocumented.

## Preferences

- Keep workflow guidance concrete and repo-specific.
- Prefer small automation that catches setup drift early over more prompt text.
- Para el trabajo de préstamo de Domo del Pululahua, todo material usado para pedir, sustentar, negociar o dar seguimiento al financiamiento debe entregarse en español y adaptado al contexto ecuatoriano. Si la investigación fuente está en inglés, traducir y localizar el entregable final a español ecuatoriano.
- Para operaciones del préstamo de Domo del Pululahua, usar los flujos ya creados en `docs/venue-manager/domo-loan-packet/04-proformas-uso-de-fondos/` para proformas firmadas y en `docs/venue-manager/domo-loan-packet/07-respuestas-bancos-cooperativas/` para llamadas a bancos/cooperativas. No contactar bancos o proveedores ni enviar documentos privados sin confirmación explícita del operador y sin canal seguro confirmado.

## Promoted From Short-Term Memory (2026-08-04)

<!-- openclaw-memory-promotion:memory:memory/2026-07-28.md:3:6 -->
- Added browser-side magnet-link and `.torrent` audio playback to the HQ radio widget using a lazy-loaded WebTorrent 2.8.5 browser bundle and scoped service worker. Uploaded torrent metadata stays session-only; magnet stations persist like other custom stations. Torrent playback selects the largest browser-compatible audio file and closes its P2P session when stopped or switched. - Replaced the dense inline social-event form with a guided collaborative creator at `/social/eventos/nuevo`.... [score=0.805 recalls=0 avg=0.620 source=memory/2026-07-28.md:3-6]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:8:8 -->
- Added canonical multi-source event matching so ticketing platforms can attach [score=0.805 recalls=0 avg=0.620 source=memory/2026-07-30.md:8-8]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:16:17 -->
- `/configuracion/fuentes-eventos` for enabling providers and registering city-bound HTTPS iCalendar/JSON venue feeds. [score=0.805 recalls=0 avg=0.620 source=memory/2026-07-30.md:16-17]

## Promoted From Short-Term Memory (2026-08-05)

<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:4:4 -->
- Buen Plan Ecuador, and structured venue-owned iCalendar/JSON feeds. [score=0.836 recalls=0 avg=0.620 source=memory/2026-07-30.md:4-4]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:11:11 -->
- Added country-aware event-city/subscription APIs and mobile UI. The Events tab [score=0.836 recalls=0 avg=0.620 source=memory/2026-07-30.md:11-11]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:14:15 -->
- Added mobile multi-platform purchase choices to event detail.; Added strict-admin source management APIs and the HQ page at [score=0.836 recalls=0 avg=0.620 source=memory/2026-07-30.md:14-15]
<!-- openclaw-memory-promotion:memory:memory/2026-07-29.md:3:6 -->
- Investigated the live `/feedback` authentication error shown at 10:06. PR #138 (`54009732c`) merged at 10:09 and Cloudflare deployed the corrected frontend at 10:11; the deployed bundle now includes session cookies, resolves the Fly API base, and does not prefill non-email usernames such as `admin`. - Production Fly still runs backend commit `393bf4fc9708a051409dbef3606564eb2e446933` from July 15, so anonymous `POST /feedback` still returns `401 Missing or invalid auth token`. An invalid `consent=false` probe confirmed this without inserting feedback or sending email.... [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-29.md:3-6]
<!-- openclaw-memory-promotion:memory:memory/2026-07-29.md:7:8 -->
- Fly resolved the image correctly but rejected the canary update before applying it because the `diego-saa` organization requires billing information. Both production Machines were rechecked afterward and remain started on the prior digest `sha256:4664e57e556a75057732f6c971d9aca98bf73e524ec86c8b26573dbd587db273`; no production state changed. After billing is added, resume the one-Machine-at-a-time rollout, explicitly preserve `EVENT_DISCOVERY_ENABLED=true`, verify the canary directly, and retain the prior digest for rollback. Do not use the generic releaser unchanged because it stages event discovery to `false`.... [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-29.md:7-8]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:3:3 -->
- Implemented city-subscription-based event discovery across Ticketmaster, [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-30.md:3-3]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:5:5 -->
- Event discovery now runs in isolated six-hour provider slots with a PostgreSQL [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-30.md:5-5]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:6:7 -->
- leader lock, per-source health/error tracking, a 24-hour circuit-breaker cooldown after repeated failures, and a two-run missing-event grace period. [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-30.md:6-7]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:9:10 -->
- separate purchase options without duplicating the public event. Ticketmaster, Buen Plan, and venue sources use configurable priorities. [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-30.md:9-10]
<!-- openclaw-memory-promotion:memory:memory/2026-07-30.md:12:13 -->
- defaults to subscribed cities, supports Explore-all and saved-event scopes, and lets users add/remove global cities by ISO-2 country code. [score=0.804 recalls=0 avg=0.620 source=memory/2026-07-30.md:12-13]
