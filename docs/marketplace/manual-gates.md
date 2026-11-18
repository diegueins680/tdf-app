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
| `introducedBy` del ledger | `scripts/production-migrations.json` | Apunta a `5901003` (commit que fijó el SQL tras la revisión: allowlist analítica y arista filled→published). El repo fusiona con merge commit, así que se conserva; si se hiciera squash, reemplazar por el SHA de merge (ver `docs/automatic-production-migrations.md`) y actualizar `scripts/__tests__/catalog-list-audit.test.mjs` |
| Gates de contrato y gobierno | `compiled-api-surface.json`, `requirements.json` (`MKT-DEMAND-OFFER-001`), `catalog-list-decisions.json` (decisión sucesora del manifiesto, 247 entradas) | Reconciliados en `8ee6c7a` y `16b362c`; CI `pull_request` no se dispara con pushes de la sesión: relanzar con `workflow_dispatch` o con un push propio |

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

## Fase 2, parte A (ADR 0135)

| Gate | Dónde | Qué verificar |
| --- | --- | --- |
| Tarifa vigente | `marketplace_agreement_pricing` | La regla por defecto es 10 % de comisión y 30 % de depósito; cambiarla es un `UPDATE` (desactivar la fila y crear otra), nunca un despliegue |
| Recorrido con dos cuentas | sandbox/staging (`marketplace.agreements` TRUE) | Aceptar una oferta con monto crea el acuerdo; el proveedor lo inicia; el comprador lo completa; ambos pueden reseñarse; cancelar libera el cupo |
| Producción | `revenue_feature_flag` | `marketplace.agreements` sigue en FALSE hasta decidir el lanzamiento; con el gate apagado no se crean acuerdos y las ofertas funcionan como en la Fase 1 |
| `introducedBy` del ledger | `scripts/production-migrations.json` | Apunta al commit que introdujo `2026-11-12_marketplace_agreements.sql`; fusionar con merge commit |

## Fase 2, parte B1 (ADR 0136)

| Gate | Dónde | Qué verificar |
| --- | --- | --- |
| Gate apagado | `revenue_feature_flag` | `marketplace.agreement_payments` es FALSE en producción, staging y sandbox; no se enciende hasta que exista un flujo de proveedor que reparta el cobro (B2) y su evidencia de capacidad esté registrada |
| Autorización de split de PayPhone | Formulario de PayPhone (docs.payphone.app/split-de-pagos) | Requiere Botón, Cajita o Link operando en producción; pedir la modalidad «al día siguiente»; los datos legales y las declaraciones los firma TDF |
| Comisión del procesador | `marketplace_payment_fee_rule` | PayPhone 5,75 % a cargo del proveedor del servicio por defecto; confirmar el porcentaje contratado y quién la asume antes de encender el gate (es un `UPDATE`) |
| Datos del receptor | `marketplace_payout_account.identifier` | Cédula, RUC o teléfono guardados en claro; revisar con privacidad si deben cifrarse antes de abrir el registro de cuentas a usuarios |
| Vencimiento del depósito | `marketplace_agreement.deposit_due_at` | Vence al cargar los acuerdos de un participante y cada cinco minutos en el job del servidor; revisar en los logs que no haya `[Marketplace][AgreementExpiry] pass failed` repetidos |
| Dinero sin aplicar | `directory_audit_event` | Una fila `agreement.payment.unapplied` significa que llegó dinero verificado para un cobro que ya no estaba pendiente: hay que devolverlo a mano |
| `introducedBy` del ledger | `scripts/production-migrations.json` | Apunta al commit que introdujo `2026-11-12_marketplace_agreement_payments.sql`; fusionar con merge commit |

## Fase 2, parte B2b-1: Botón de PayPhone con split (ADR 0136)

Nada de esto se probó contra PayPhone. Antes de registrar evidencia hay que verificar en el modo de
pruebas de PayPhone, con dos cuentas y un receptor real:

| Supuesto del código | Cómo verificarlo |
| --- | --- |
| La consulta de estado por referencia (`GET /api/Sale/client/{clientTransactionId}`) encuentra también las transacciones del Botón | PayPhone la documenta en API Sale sin limitarla a ese producto. Pagar una vez y consultarla con la referencia `TDFC-`; si no la encuentra, el cobro nunca se confirma y PayPhone lo revierte: habría que tomar el `id` de la URL de respuesta o de la notificación externa |
| Antes del pago, esa consulta responde con un estado HTTP de error | Consultar la referencia recién preparada; un 2xx con otro formato dejaría la consulta en revisión |
| Confirm (`{id: transactionId, clientTxId}`) responde con `transactionId`, `clientTransactionId`, `amount` en centavos, `currency` y `statusCode` | Comparar con la respuesta real; el `paymentId` de Prepare es opaco (texto) y solo identifica la preparación |
| PayPhone acepta `transferTo` con `Amount` numérico en centavos | Revisar en PayPhone Developer que el receptor recibió `provider_amount_minor` |
| `payWithCard` está en `payphonetodoesposible.com` o `payphone.app` | Si es otro dominio, ampliar `safePayPhoneRedirect` (backend y web) |
| PayPhone añade `id` y `clientTransactionId` a la URL de respuesta sin romper `tdf_checkout` | La página de retorno ya corta lo que siga a un segundo `?` |
| La consulta del worker confirma antes de cinco minutos | `COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED=true` y flag `checkout.provider_query_recovery`. Las referencias `TDFC-` se consultan cada minuto, y una respuesta aprobada se confirma en el mismo intento, (`providerQueryRetryDelaySeconds`) durante los 24 intentos, unos 24 minutos; el presupuesto compartido de una consulta por proveedor cada diez segundos sostiene ese ritmo con seis cobros simultáneos y el plazo con unos 25. Medir el tiempo entre pago y confirmación |
| PayPhone no permite pagar un formulario del Botón cargado más de diez minutos después de prepararlo | TDF solo entrega la página durante diez minutos desde la preparación (`loadOperation`); el formulario vive diez minutos desde que se carga, así que el pago ocurre a más tardar a los veinte minutos y el worker lo confirma dentro de sus 24 minutos. Abrir un enlace guardado de `payWithCard` pasada esa ventana debería fallar en PayPhone |
| El Botón solo se ofrece con el worker de consultas activo | `PaymentAvailability` exige `COMMERCE_PROVIDER_QUERY_RECOVERY_ENABLED=true`, las tablas de recuperación y `checkout.provider_query_recovery` con la cuenta lista. Un intento del Botón que nadie paga termina en revisión al agotar los 24 intentos; mientras tanto el depósito no vence |

Configuración: `PAYPHONE_CODING_PASSWORD` (exactamente 32 caracteres, de PayPhone Developer) además de
`PAYPHONE_TOKEN`, `PAYPHONE_STORE_ID` y `PAYPHONE_RESPONSE_URL`. Sin la contraseña el método `card`
de PayPhone no se ofrece.

Evidencia de capacidades, solo después de la verificación y de la autorización de split de PayPhone
(una fila por capacidad; `sandbox_verified` en staging, `production_verified` en producción):

```sql
INSERT INTO commerce_provider_capability
  (provider_account_id, payment_method, capability, verification_status, source_reference, verified_at)
SELECT account.id, 'card', capability, 'sandbox_verified',
       'https://docs.payphone.app/split-de-pagos#<referencia-de-la-prueba>', now()
FROM commerce_provider_account account
CROSS JOIN unnest(ARRAY['one_time','server_verification','connected_accounts',
                        'split_settlement','seller_payouts']) AS capability
WHERE account.provider = 'payphone' AND account.environment = 'sandbox'
ON CONFLICT (provider_account_id, payment_method, capability)
DO UPDATE SET verification_status = EXCLUDED.verification_status,
              source_reference = EXCLUDED.source_reference,
              verified_at = EXCLUDED.verified_at, updated_at = now();
```

El gate `marketplace.agreement_payments` sigue en FALSE hasta completar esta tabla.

