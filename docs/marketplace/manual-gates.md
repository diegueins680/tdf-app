# Marketplace — compuertas manuales antes de habilitar en producción

Lo que no se pudo verificar en el entorno donde se escribió la Fase 1 y debe cerrarse por una
persona con acceso a la infraestructura. Ninguna compuerta se considera cerrada por fusionar código.

## Compilación y pruebas que requieren toolchain

| Compuerta | Comando | Estado |
| --- | --- | --- |
| Build Haskell y `PolicySpec` | `cd tdf-hq && stack build && stack test` | Pendiente: el entorno de autoría no tenía GHC/stack; `check-state-machines.py --static` sí pasa |
| Clientes generados idénticos (web y móvil) | `npm run generate:api && node scripts/check-generated-api.mjs` | Pendiente: `tdf-hq-ui/src/api/generated/types.ts` se editó a mano siguiendo la salida de openapi-typescript; regenerar y confirmar que el diff es vacío; el cliente móvil (`tdf-mobile/src/api/generated/types.ts`) necesita el submódulo |
| Jest web (`features/marketplace`, páginas del directorio) | `cd tdf-hq-ui && npm test` | Pendiente: sin `node_modules` ni red en el entorno de autoría |
| Playwright `marketplace-negotiation` | `npm run test:marketplace-negotiation` | Pendiente |
| Migración contra fixture de producción | `bash scripts/test-automatic-migrations-production-schema.sh` | Pendiente; confirma el reemplazo del `CHECK` de `classified_application.status`, el widening de tipos de notificación y la verificación de esquema añadida en `production-release.mjs` |
| `introducedBy` del ledger | `scripts/production-migrations.json` | Tras el squash-merge, reemplazar por el SHA de merge (ver `docs/automatic-production-migrations.md`) |

## Datos previos

1. Ejecutar la consulta de diagnóstico de la migración (duplicados de ofertas activas por anuncio y
   oferente). Si hay filas, decidir caso por caso (retirar la más antigua) antes de habilitar la
   negociación.
2. Revisar anuncios publicados con `budget_max_minor` y `budget_negotiable = FALSE` y
   `budget_min_minor <> budget_max_minor`: el backfill los deja en `suggested`; confirmar con los
   autores si alguno debía ser `fixed`.

## Recorrido funcional en producción (con el flag apagado y luego encendido)

Con `marketplace.offers = FALSE` en producción, todo lo previo sigue funcionando y además:
- `/buscar` muestra la insignia Busca/Ofrece y el filtro de intención.
- `/clasificados/:slug` muestra modo de precio, cupos y estadísticas; el formulario de oferta acepta
  monto y términos.
- El autor ve el tablero de ofertas; aceptar consume cupos; `POST …/counter` responde 503.

Con el flag encendido: contraofertas en ambos sentidos, alternancia estricta, vencimiento y
notificaciones `marketplace.offer.countered`.

Casos a ejecutar con dos cuentas de prueba (autor y oferente):
1. Publicar "Busco banda" con precio sugerido 1 200 USD, 1 cupo, fecha límite en 10 días.
2. Oferente A oferta 1 300; oferente B oferta 1 100. Público ve "2 ofertas" sin rango (< 3).
3. Autor ordena por precio y por reputación; contraoferta a A por 1 250 → A ve "Te toca responder".
4. A acepta → anuncio `filled`, B pasa a `rejected(slots_filled)` y recibe notificación.
5. A retira la oferta aceptada → cupo liberado, anuncio vuelve a `published`.
6. Publicar "Venue busca banda" con precio fijo 400: la oferta solo acepta 400 y no hay botón de
   contraoferta; otro monto devuelve 409.
7. Dejar vencer una oferta (`offer_expires_at`) y comprobar que la lectura la muestra `expired` y la
   transición devuelve 409.
8. Pausar un anuncio con ofertas abiertas: las ofertas siguen abiertas; no se admiten nuevas.

## Moderación y contenido

- Los mensajes de ronda (`classified_offer_round.message`) pasan por `validatePrivateMessage` pero
  no por la cola de moderación: añadir reporte de ofertas (`directory/reports` con
  `targetKind=application`) antes de abrir el flag al público general.
- Revisar textos de notificaciones y correos (si el gate de email está activo) con el equipo de
  comunidad.

## Observabilidad

Eventos analíticos nuevos: `offer_countered`, `offer_accepted` (además de `application_submitted`,
`match_converted`). Crear el tablero de la Fase 1 (ofertas por anuncio, tiempo a primera oferta,
tasa de aceptación, RAS) en PostHog antes de la campaña.
