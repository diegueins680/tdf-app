# Marketplace de demanda y oferta — Plan por fases

Fecha: 2026-11-08. Depende de `00-auditoria.md`. Cada fase se entrega como uno o varios PRs
pequeños detrás de un feature flag, con migración + rollback + entrada en
`scripts/production-migrations.json`, contrato OpenAPI actualizado, tests y evidencia en
`docs/marketplace/`.

## Principios

1. Reutilizar `classified`, `classified_application`, `directory_invitation`, el DM, las
   notificaciones, las taxonomías y el stack de pagos. Ninguna identidad, chat ni catálogo nuevo.
2. Cambios aditivos y compatibles: las filas existentes siguen siendo válidas sin reescribirlas.
3. Todo detrás de gate: `marketplace.offers` (Fase 1), `marketplace.agreements` (Fase 2),
   `marketplace.reputation` (Fase 3), `marketplace.matching` (Fase 4). Los pagos nunca se habilitan
   por fusionar código.
4. Métricas desde el día uno (PostHog + `directory_audit_event`): anuncios publicados por intención,
   ofertas por anuncio, tiempo a primera oferta, tasa de aceptación, acuerdos, GMV, take rate,
   Relaciones Activadas Semanales (RAS).

## Fase 0 — Auditoría y decisión (hecha)

Entregables: `00-auditoria.md`, este plan, `02-especificacion-funcional.md`,
`03-diseno-de-datos.md`, `04-mockups/`, ADR 0134. Criterio de aceptación: el equipo puede estimar
y empezar la Fase 1 sin preguntas abiertas de alcance.

## Fase 1 — Demanda y ofertas negociables (sin pago)

Alcance:
- `classified_category.intent` (`seeking` | `offering`) sembrado para las 16 categorías; el anuncio
  hereda la intención de su categoría y la expone en búsqueda y detalle.
- `classified.slots` (cupos, por defecto 1), `classified.offer_deadline_at`, `classified.price_mode`
  (`open` | `suggested` | `fixed`) sobre el presupuesto ya existente.
- Ofertas negociables sobre `classified_application`: nueva tabla `classified_offer_round` (cadena
  de propuestas/contraofertas por postulación) y columnas `terms`, `current_amount_minor`,
  `current_round`, `offer_expires_at`, `last_actor_role` en la postulación. Estados nuevos de
  postulación: `countered_by_author`, `countered_by_applicant`.
- Reglas de aceptación: aceptar una oferta la marca `accepted`, decrementa cupos; al agotarse los
  cupos el anuncio pasa a `filled` y las ofertas abiertas pasan a `rejected` con motivo
  `slots_filled`, con notificación.
- Endpoints: `GET /directory/applications` (mis ofertas, como oferente), `POST
  /directory/applications/{id}/counter` (contraoferta de cualquiera de las dos partes), `GET
  /directory/applications/{id}` (detalle con rondas), `GET /directory/classifieds/{id}/applications`
  enriquecido (orden por precio/reputación, resumen). `PublicClassified` expone `intent`, `slots`,
  `slotsRemaining`, `priceMode`, `offerDeadlineAt`, `offerStats` (conteo y rango agregado, sin
  identificar oferentes).
- UI web: formulario de oferta con monto/moneda/términos en `/clasificados/:slug`; tablero de
  comparación de ofertas para el autor (ordenar por precio, reputación, fecha; preseleccionar,
  contraofertar, aceptar); pestaña "Mis ofertas" para el oferente con la cadena de negociación y
  botones aceptar/contraofertar/retirar; badge de intención en tarjetas de búsqueda; filtro
  `intent` en `/buscar`.
- Notificaciones: nueva oferta, contraoferta, aceptación, rechazo por cupos, oferta por vencer.
- Formal: extender `formal-model.yaml` (I18 negociación alternada, I19 contabilidad de cupos, I20
  confidencialidad agregada de ofertas) y `PolicySpec`.

Criterios de aceptación:
- Recorrido Playwright: publicar búsqueda con precio sugerido → oferente contraoferta → autor
  contraoferta → oferente acepta → anuncio `filled` → DM abierto.
- Anuncios y postulaciones previos a la migración se listan y operan sin cambios.
- `stack test` y `npm test` en verde; cobertura de transiciones exhaustiva (producto cartesiano).

Estimación: 3–4 semanas (1 backend, 1 frontend, 0,5 QA).

## Fase 2 — Acuerdo, pago y comisión

Entrega en dos partes (decisión del 2026-11-12):

- **Parte A — acuerdo (ADR 0135, implementada):** `marketplace_agreement` creado al aceptar una
  oferta con monto, partes según la intención, comisión (10 %) y depósito (30 %) congelados desde
  `marketplace_agreement_pricing`, estados y autoridad por parte, cierre con interacción verificada
  para reseñas, cancelación que libera el cupo, pestaña «Acuerdos». Sin cobro: el acuerdo nace
  `confirmed` y `off_platform`.
- **Parte B — pagos (pendiente):** checkout del depósito y del saldo, vencimiento del depósito,
  liquidación al proveedor, reembolsos según la política congelada y panel de disputas.

Alcance:
- Tabla `marketplace_agreement` creada al aceptar una oferta cuando el gate está activo: partes,
  anuncio, postulación, monto, moneda, términos congelados (JSON versionado), `commission_bps`,
  `commission_minor`, `deposit_minor`, `balance_minor`, política de cancelación, estados
  `pending_deposit → confirmed → in_progress → balance_due → completed | cancelled | disputed |
  expired`.
- Checkout: reutilizar `CheckoutStore` con `domain_order_kind='marketplace_agreement'`,
  `platform_fee_minor = commission_minor`, `DepositPolicy` para depósito mínimo/umbral; proveedor
  según capacidades (Stripe primero, luego Datafast/PayPhone/PlaceToPay por gate y país).
- Liquidación: v1 manual verificada por operaciones (reusar evidencia de pago manual y
  `commerce` de órdenes) con registro en ledger; v2 Stripe Connect cuando el perfil tenga cuenta
  conectada (patrón de `ArtistTipStore`).
- Cancelaciones y reembolsos: reusar `RefundStore` y reconciliación; política por categoría
  (ventanas y porcentajes) en el registro de features.
- UI: pantalla de acuerdo (resumen, términos, depósito/saldo, estado), checkout enlazado,
  seguimiento `/acuerdos/:id`, panel de operaciones para liquidaciones.
- Formal: ADR 0135 (acuerdo y comisión), invariantes contables (depósito + saldo = total;
  comisión ≤ total; estados de pago nunca implican servicio prestado).

Criterios: depósito cobrado en sandbox Stripe; comisión registrada en ledger; reembolso parcial
según política; ningún endpoint de pago activo sin gate.

Estimación: 4–6 semanas.

## Fase 3 — Reputación y verificación del marketplace

- Productor de `directory_interaction` (`interaction_kind='marketplace_agreement'`, `completed`
  + `verified_at`) al completar un acuerdo pagado → habilita `review-eligibility` bidireccional.
- Agregados por perfil (`directory_profile_reputation`): rating medio, conteo de reseñas,
  trabajos completados, tasa y tiempo de respuesta a ofertas, cancelaciones.
- Insignias: verificado (ya existe), pago habilitado, venue verificado; exposición en
  `PublicDirectoryProfile`, tarjetas de búsqueda y comparación de ofertas.
- Recordatorios post-evento para reseñar; moderación de reseñas ya existente.

Estimación: 2–3 semanas.

## Fase 4 — Oferta enriquecida, disponibilidad y matching proactivo

- Calendario de disponibilidad por perfil (reusar `directory_profile_location` y un
  `directory_profile_availability` simple) y filtro por fecha en búsqueda.
- Alertas a perfiles compatibles cuando se publica una búsqueda (reusar trigger de búsquedas
  guardadas con un perfil "implícito" por profesión/instrumento/ciudad).
- Sugerencias de precio a partir de ofertas aceptadas (mediana por categoría/ciudad).
- Plantillas de anuncio por caso de uso (boda, festival, sesión, residencia).

Estimación: 3–4 semanas.

## Fase 5 — Paridad móvil e internacionalización

- Pantallas Expo: búsqueda, detalle, ofertar, mis ofertas, acuerdo; cliente generado desde el
  mismo OpenAPI; registro en `feature-registry` con `mobilePresentation`.
- Zonas horarias en términos de fecha/hora; formato de moneda por locale; idiomas adicionales.

Estimación: 4 semanas.

## Dependencias y orden de PRs (Fase 1)

1. `feat(directory): intent and slots on classifieds` — SQL + rollback + ledger + OpenAPI +
   handlers + tests.
2. `feat(directory): negotiable offers` — `classified_offer_round`, estados, endpoints, Policy,
   PolicySpec, formal-model.
3. `feat(ui): offer form, comparison board, my offers` — React + i18n + Vitest.
4. `test(e2e): marketplace negotiation journey` — Playwright.
5. `docs: marketplace phase 1 evidence` — capturas, manual gates.

## Riesgos de calendario

- Build Haskell lento y CI con PostgreSQL: agrupar migraciones por PR.
- Liquidación a terceros depende de capacidades legales/fiscales por país: Fase 2 arranca con
  liquidación manual.
- Mobile depende del submódulo y de EAS: no bloquea Fases 1–4.
