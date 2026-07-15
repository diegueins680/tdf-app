# MEMORY.md

## Workspace

- This repo uses `AGENTS.md`, `SOUL.md`, `USER.md`, daily `memory/YYYY-MM-DD.md`, and now `AI_WORKFLOW.md` as the main continuity and onboarding surface for AI work.
- `scripts/continuous-improvement-loop.codex.json` currently targets `main`, so unattended loop runs should be treated as high-risk unless copied to a branch-scoped config first.
- Marketplace Stripe safety commit `876413b721049da71d66a8d67c0162f97c72bef8` was deployed to production as Fly release `v2049` on 2026-07-12 after applying the 2026-07-07 and 2026-07-12 marketplace migrations with old writers quiesced. Production keeps `RUN_MIGRATIONS=false`; the `tdf-hq-db` volume was expanded from 3 GB to 5 GB during the cutover.
- On 2026-07-14, the marketplace orders admin route in `tdf-hq` had to be moved ahead of the public marketplace capture so `/marketplace/orders` would not be mistaken for a listing id. The corresponding server composition order in `tdf-hq/src/TDF/Server.hs` must stay aligned with `tdf-hq/src/TDF/API.hs`.
- Production notification schema drift was repaired on 2026-07-12 with the narrow `tdf-hq/sql/2026-07-12_notification_table.sql` migration. Do not apply the broader `tdf-hq/sql/2026-05-25_content_engagement.sql` as part of Stripe or ticketing work.
- The 2026-07-13 ticketing/event-discovery release lane is intentionally guarded and commit-based. Production is currently a partial release: discovery schema exists and ran once with zero imported records, but ticket runtime schema and Stripe Fly secrets are still missing. Do not push/release until main-branch auto-deploy risk is resolved, `EVENT_DISCOVERY_ENABLED=false` is effective, and `STRIPE_SECRET_KEY`/`STRIPE_WEBHOOK_SECRET` are set.

## Preferences

- Keep workflow guidance concrete and repo-specific.
- Prefer small automation that catches setup drift early over more prompt text.
- Para el trabajo de préstamo de Domo del Pululahua, todo material usado para pedir, sustentar, negociar o dar seguimiento al financiamiento debe entregarse en español y adaptado al contexto ecuatoriano. Si la investigación fuente está en inglés, traducir y localizar el entregable final a español ecuatoriano.
- Para operaciones del préstamo de Domo del Pululahua, usar los flujos ya creados en `docs/venue-manager/domo-loan-packet/04-proformas-uso-de-fondos/` para proformas firmadas y en `docs/venue-manager/domo-loan-packet/07-respuestas-bancos-cooperativas/` para llamadas a bancos/cooperativas. No contactar bancos o proveedores ni enviar documentos privados sin confirmación explícita del operador y sin canal seguro confirmado.
