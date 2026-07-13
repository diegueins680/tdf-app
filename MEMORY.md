# MEMORY.md

## Workspace

- This repo uses `AGENTS.md`, `SOUL.md`, `USER.md`, daily `memory/YYYY-MM-DD.md`, and now `AI_WORKFLOW.md` as the main continuity and onboarding surface for AI work.
- `scripts/continuous-improvement-loop.codex.json` currently targets `main`, so unattended loop runs should be treated as high-risk unless copied to a branch-scoped config first.
- Marketplace Stripe safety commit `876413b721049da71d66a8d67c0162f97c72bef8` was deployed to production as Fly release `v2049` on 2026-07-12 after applying the 2026-07-07 and 2026-07-12 marketplace migrations with old writers quiesced. Production keeps `RUN_MIGRATIONS=false`; the `tdf-hq-db` volume was expanded from 3 GB to 5 GB during the cutover.
- Production still has unrelated schema drift for the missing `notification` relation. Audit and apply the broader `tdf-hq/sql/2026-05-25_content_engagement.sql` separately; do not treat it as part of the completed Stripe migration.

## Preferences

- Keep workflow guidance concrete and repo-specific.
- Prefer small automation that catches setup drift early over more prompt text.
- Para el trabajo de préstamo de Domo del Pululahua, todo material usado para pedir, sustentar, negociar o dar seguimiento al financiamiento debe entregarse en español y adaptado al contexto ecuatoriano. Si la investigación fuente está en inglés, traducir y localizar el entregable final a español ecuatoriano.
- Para operaciones del préstamo de Domo del Pululahua, usar los flujos ya creados en `docs/venue-manager/domo-loan-packet/04-proformas-uso-de-fondos/` para proformas firmadas y en `docs/venue-manager/domo-loan-packet/07-respuestas-bancos-cooperativas/` para llamadas a bancos/cooperativas. No contactar bancos o proveedores ni enviar documentos privados sin confirmación explícita del operador y sin canal seguro confirmado.
