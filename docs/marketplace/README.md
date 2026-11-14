# Marketplace de demanda y oferta

Evolución de Comunidad y anuncios hacia un marketplace bilateral: quien busca publica (banda para
una boda, venue para una fecha, tecladista para una banda), quienes ofrecen responden con precio y
condiciones, el autor compara y elige, y (Fase 2) la oferta aceptada se convierte en acuerdo con
depósito, pago en la app y comisión para TDF. Decisión: [ADR 0134](../adr/0134-marketplace-demand-offers.md).

| Documento | Contenido |
| --- | --- |
| [`00-auditoria.md`](00-auditoria.md) | Estado actual del directorio, comercio e interacciones; brechas priorizadas |
| [`01-plan-por-fases.md`](01-plan-por-fases.md) | Fases 0–5, alcance, criterios de aceptación, orden de PRs |
| [`02-especificacion-funcional.md`](02-especificacion-funcional.md) | Actores, anuncios, ofertas y negociación, acuerdo, reputación, métricas |
| [`03-diseno-de-datos.md`](03-diseno-de-datos.md) | Modelo, máquina de estados, migraciones, contrato OpenAPI |
| [`04-mockups/index.html`](04-mockups/index.html) | Seis pantallas web/móvil de la Fase 1 (y referencia de Fase 2) |
| [`manual-gates.md`](manual-gates.md) | Verificaciones que requieren infraestructura o revisión humana antes de habilitar |

## Fase 1 — qué se implementó

- SQL `tdf-hq/sql/2026-11-08_marketplace_offers.sql` (+ rollback, ledger): intención por categoría,
  cupos/modo de precio/fecha límite/términos del anuncio, columnas de negociación y nuevos estados de
  la oferta, tabla `classified_offer_round`, tipos de notificación `marketplace.*`, flag
  `marketplace.offers`.
- `TDF.Directory.Policy`: estados `countered_by_author`, `countered_by_applicant`, `expired`;
  reglas de turno, alternancia, tope de rondas, cupos y cierre (I18–I20), con `PolicySpec`.
- `TDF.Server.Directory`: creación de anuncios con cupos/modo de precio/términos; ofertas con monto,
  términos y vencimiento; `GET /directory/applications` (mis ofertas), `GET
  /directory/applications/{id}` (detalle con rondas), `POST /directory/applications/{id}/counter`;
  aceptación atómica con cupos, cierre de ofertas al cubrir/cerrar el anuncio, notificaciones,
  `offerStats` públicos, filtro `intent` en búsqueda.
- OpenAPI `directory.yaml` / `api.yaml` y cliente web generado (`tdf-hq-ui/src/api/generated/types.ts`).
- UI web: `features/marketplace/{offers.ts,OfferCard,OffersBoard,MyOffersPanel,OfferForm}`;
  `DirectoryManagePage` (pestaña "Mis ofertas", tablero de ofertas, campos de cupos/precio/términos
  al publicar), `DirectoryPublicDetailPage` (intención, modo de precio, cupos, estadísticas, CTA
  "Enviar oferta"), `DirectorySearchPage` (filtro e insignia de intención); i18n es/en
  (`marketplace.*`, `directorySearch.intent*`).
- Pruebas: `PolicySpec` (Hspec), `offers.test.ts` y `OfferCard.test.tsx` (Jest),
  `e2e/web/marketplace-negotiation.spec.mjs` (Playwright, `npm run test:marketplace-negotiation`).
- Verificación de esquema del release (`scripts/lib/production-release.mjs`) y modelo formal
  (`docs/music-directory/formal-model.yaml`, gate estático `check-state-machines` en verde).

## Cómo probar localmente

```bash
# Backend
cd tdf-hq && stack build && stack test --test-arguments '--match "music directory formal invariants"'
psql "$DATABASE_URL" -f sql/2026-11-08_marketplace_offers.sql
# Flag de negociación (sandbox/staging ya quedan en TRUE por la migración)
# Web
cd ../tdf-hq-ui && npm test -- src/features/marketplace
cd .. && npm run test:marketplace-negotiation
python3 scripts/check-state-machines.py --static
```

Recorrido manual: publicar un anuncio "Busco banda" con precio sugerido → con otra cuenta enviar
oferta desde `/clasificados/:slug` → como autor abrir "Ofertas", ordenar por precio, contraofertar →
como oferente ver "Mis ofertas", aceptar → el anuncio pasa a cubierto y las demás ofertas quedan
rechazadas con motivo.
