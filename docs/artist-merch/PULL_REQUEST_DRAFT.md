# Draft PR: Artist merch storefronts

## Problema y objetivo

TDF podía exhibir perfiles y operar otros tipos de comercio, pero “las bandas pueden vender merch” no era todavía una capacidad real. Este PR introduce un dominio específico de merch físico integrado con perfiles, comunidad y checkout canónico, con rollout cerrado y honesto.

## Estado anterior

Existían perfiles/directorio/comunidad, marketplace de activos, checkouts Datafast/PayPal/manual y administración. No existían tienda, producto físico, variante/SKU, stock, storefront ni orden de merch; la documentación la trataba como futura.

## Arquitectura

- Entidades `merch_*` para vendedor, catálogo, imágenes, inventario, orden, fulfillment, issues, reviews, settlement, outbox, analítica y auditoría.
- Reutilización de `party`, `directory_profile`, permisos/PartySelector, `/assets/serve` y `commerce_checkout_*`.
- Tokens opacos hasheados para carrito/orden y snapshots inmutables.
- API Servant/OpenAPI y clientes TypeScript regenerados.
- UI web completa para comprador/vendedor/staff, incluida cancelación sin pagar, triage de incidencias y pedidos filtrables con totales/exportación sin PII; móvil para compra, seguimiento, soporte y operación esencial.
- Dependencia móvil revisable: [TDF-mobile#49](https://github.com/diegueins680/TDF-mobile/pull/49).

## UX

Storefront integrado al perfil y marketplace; mensajes explícitos para revisión, pago pendiente, proveedor no disponible, stock cambiado y permisos. El panel de pedidos permite filtrar fulfillment, resume solo importes autorizados y exporta CSV operativo sin PII ni fórmulas ejecutables. Diseño responsive, controles táctiles y semántica accesible. Español/inglés en superficies nuevas. El móvil deriva configuración compleja al panel web responsive.

## Datos y estados

Producto: `draft → pending_review → published/sold_out/paused/rejected/archived`. Pago, orden, reserva, fulfillment, shipment, refund, disputa y settlement permanecen separados. Comisión general 1000 bps sobre producto después de descuento; override auditable, incluido 0%.

Incidencias: vendedor puede revisar/responder/resolver casos operativos; cancelación pagada, refund, disputa y fraude solo se escalan a staff. Cerrar el caso nunca muta por implicación pago, refund o settlement.

## Seguridad y privacidad

Autorización backend por store/acción; elegibilidad reclamada/verificada; locks/constraints contra sobreventa; idempotencia con fingerprint; `paid` solo por evidencia server-side; archivos decodificados/reencodados y object keys generados; referencias HTTPS/durables; analítica sin PII; auditoría append-only; settlement con doble control; flags cerrados y kill switch `merch.checkout.runtime_ready`.

## Migración

- Apply: `tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql`
- Rollback: `tdf-hq/sql/2026-09-07_artist_merch_storefronts_rollback.sql`
- Rehearsal: `scripts/test-artist-merch-storefronts-migration.sh`

Rollback se niega si existe evidencia comercial. No hay conversión automática de assets; los tests usan datos sintéticos.

## Pruebas ejecutadas

- Migración PostgreSQL aislada: PASS, incluida reejecución, concurrencia, expiración, pago, comisión y rollback.
- Runtime handlers + HTTP Servant + PostgreSQL 16 temporal: `./scripts/test-artist-merch-runtime.sh` PASS 1/1 y agregado a `backend-quality`. Cubre autenticación, aplicación/aprobación de piloto a 0%, catálogo público, checkout invitado, cálculo server-side, recuperación idempotente de carrito convertido, conflicto de fingerprint, falso retorno de navegador, cero intentos de pago, capability privada, cancelación/liberación de stock, permisos/finanzas, aislamiento de vendedor, validación del filtro de pedidos y triage seller/admin.
- Backend Haskell: PASS, 2.487/2.487 ejemplos sobre la integración final con `main`.
- Reglas focalizadas posteriores: PASS 8/8 coincidencias `merch`.
- Build web: PASS; presupuesto inicial JS PASS (416.063 bytes gzip).
- Web y móvil typecheck: PASS.
- Jest web merch API + exportación CSV: PASS 10/10.
- Jest móvil deep links: PASS 2/2.
- Android nativo API 36.1: `app:assembleDebug` PASS (481 tareas), APK instalado y runtime verificado con API sintética local. Pasaron deep link público, catálogo, producto, aviso de piloto y bloqueo accesible de compra (`enabled=false`); sin excepciones TDF en logcat. Capturas reales obtenidas localmente.
- iOS nativo: Xcode 16.2 / iOS 18.3 Simulator / iPhone 16 `x86_64`. CocoaPods quedó sincronizado con las dependencias Expo declaradas; `pod install --deployment` y el build Release sin firma terminaron PASS. El `.app` se instaló y verificaron deep links de catálogo, storefront y producto contra fixture local read-only; el CTA de compra permaneció deshabilitado, solo se registraron GET y no hubo logs `error`/`fault` de TDF. Expo Updates se apagó solo en el artefacto de prueba para fijar el bundle local. Capturas reales versionadas en `docs/artist-merch/media/`. Dispositivo físico no ejecutado.
- Playwright Chromium desktop/Pixel 7: PASS 4/4, con capturas de runtime adjuntas al reporte.
- Axe en recorridos públicos: PASS, sin impactos serios/críticos.
- Regresión móvil global: 319/320 en una corrida simultánea; la única prueba con timeout pasó aislada 15/15.
- Regresión web global: la suite preexistente de administración de cursos falló durante la corrida simultánea; queda por repetir en CI/aislamiento. Las pruebas específicas de merch pasaron.
- OpenAPI YAML + regeneración web/móvil: PASS.
- Verificador del manifiesto de release: PASS 47/47 sobre el `main` final.
- Feature generation: PASS; auditoría conserva un fallo preexistente no relacionado en `/reputation/consents`.
- Auditoría de listas/catálogos: PASS, 1.007/1.007 candidatos clasificados; prueba determinista PASS 1/1. La fixture móvil read-only está revisada como estructura técnica sintética y no como autoridad de capacidades o permisos.
- Gate remoto del PR: PASS 17/17 en la corrida que incluyó explícitamente el runtime HTTP autenticado dentro de `backend-quality`, además de migraciones, contratos, UI, móvil, E2E y auditoría de listas. El estado del HEAD vigente debe consultarse en GitHub; los previews automáticos no se consideran staging ni producción.

No se ejecutó dispositivo físico ni integración real con proveedor de pagos. La verificación iOS realizada corresponde exclusivamente a un Simulator local con API sintética; no equivale a TestFlight, staging o producción.

## Configuración y staging

Todos los flags permanecen `false`. Staging debe configurar de forma independiente storefront, solicitud, catálogo, checkout runtime, proveedor, reviews, notificaciones y experimentos. Sin credenciales sandbox y evidencia de adapter, checkout debe continuar cerrado.

## Riesgos

- Los adapters específicos de pago/refund de merch todavía no están expuestos.
- Falta validar workers/outbox y rate limiting en staging.
- Falta repetir en staging el E2E HTTP autenticado que ya pasó localmente y completar validación manual con lector/zoom.
- Definición fiscal, contractual y de protección al consumidor pendiente.

## Rollout y rollback

Local sintético → staging → prueba interna con dos adultos/roles → piloto cerrado → validación operativa/legal/contable → progresivo → GA. Rollback inicial por flags; migración destructiva solo antes de datos comerciales. Ver `docs/artist-merch/OPERATIONS.md`.

## Checklist bloqueante

- [ ] Adapters de pago/refund/reconcile verificados en sandbox.
- [x] E2E HTTP local de comprador invitado/vendedor/staff y cross-tenant con PostgreSQL 16 temporal.
- [ ] Repetición E2E en staging con observabilidad y personas/roles separados.
- [ ] Responsive/WCAG/teclado/lector/zoom con evidencia.
- [ ] Observabilidad y soporte on-call.
- [ ] Vendedor formal, facturación, impuestos/retenciones.
- [ ] Acuerdo vendedor, privacidad, consumidor, IP y productos prohibidos.
- [ ] Flujo de evidencia final de settlement y conciliación.

## Alcance diferido

Descargas digitales, multicurrency/países, cart multi-vendedor, couriers automáticos, payouts automáticos, chat nuevo y reviews públicas hasta cerrar moderación.
