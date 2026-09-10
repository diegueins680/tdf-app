# Threat model de comercio multi-vendedor

Este documento es técnico, no una certificación. El alcance es el MVP Ecuador/USD con pagos deshabilitados por defecto.

## Activos y fronteras

Activos críticos: control de tienda, catálogo/precios, stock, evidencia de pago, datos de entrega, reembolsos, comisiones, settlements, imágenes y tokens privados. Las fronteras son navegador/app ↔ API, API ↔ PostgreSQL/archivos, API ↔ proveedor y staff ↔ controles administrativos.

## Amenazas y controles

| Amenaza | Control implementado | Riesgo/acción restante |
|---|---|---|
| Takeover o acceso entre vendedores | Perfil reclamado/verificado, autorización backend por store+acción, owner y permisos delimitados, pruebas negativas SQL y HTTP local con dos vendedores | Repetir E2E con dos cuentas en staging y alertas activas |
| Manipulación de precio | Payload de checkout no acepta precios; joins server-side y snapshots inmutables | Revisión externa del adapter antes de activar |
| Sobreventa/carrera | Locks de variante, constraint de contadores, reserva atómica y prueba concurrente | Alertar reservas atascadas/expirador |
| Doble orden/cargo | Idempotencia con fingerprint en solicitud/producto/invitación/checkout/issue; checkout canónico | Cada adapter debe reutilizar intento y clave en reintentos |
| Falso retorno de navegador | Trigger rechaza `paid` sin intento exitoso y evidencia server-side verificada | Verificar firma/replay de cada webhook en staging |
| Replay o webhook forjado | Infraestructura canónica de provider events y evidencia; provider flags cerrados | Merch aún no expone adapters específicos; no habilitar runtime |
| Enumeración de órdenes/PII | UUID + lookup token hasheado en header; 404 uniforme; snapshots no aparecen en analítica | Añadir rate limits de borde y rotación/revocación operativa |
| Insider/refund/settlement fraud | Refund canónico ligado a caso staff, asignaciones/saldo bajo lock, idempotencia exacta, aprobación por otra identidad, auditoría append-only, órdenes elegibles sin PII y evidencia privada de settlement; aprobar no ejecuta dinero | Adapter refund sandbox, revisión humana y conciliación contable en staging siguen pendientes |
| Disputa o chargeback inventado | La API de merch solo lee `commerce_dispute`; no ofrece creación ni decisión, y la proyección no muta pago/fulfillment/settlement | Verificar firma, replay e ingestión real de cada proveedor en staging |
| Archivo malicioso | MIME/extensión coincidentes, límite 10 MB/40 MP, decode+reencode, nombre generado, rutas server-side, moderación previa a publicación | Integrar scanner/moderación operativa y retención/borrado |
| XSS/SSRF/path traversal | Texto controlado, React escaping, slugs/SKU validados, object keys generados, URLs de tienda limitadas a `/assets/serve/merch/`, tracking solo HTTPS | CSP y proxy/CDN se validan en staging |
| Spam/abuso | Directorio usa consentimiento, follow/contacto y reporte/bloqueo existentes; sin chat nuevo | Rate limits y revisión de abuso E2E pendientes |
| Fuga en logs/analítica | Tabla analítica rechaza email/teléfono/dirección/tarjeta/token/tracking; eventos cliente usan IDs y estados | Auditar configuración real de PostHog/retención/consentimiento |
| País/moneda falsamente soportados | Backend rechaza fuera de EC/USD y capability lo declara | Validación legal/tributaria antes de ampliar |

## Controles de despliegue

- Todos los flags se insertan `false` por entorno.
- `merch.checkout.runtime_ready` es un kill switch adicional; nunca se activa solo por existir credenciales.
- Datafast, PayPal y transferencia tienen flags separados y requieren configuración completa.
- `merch.refunds` y `merch.disputes` son flags independientes y también nacen en `false`. Habilitar el primero solo permite preparar/aprobar evidencia; no habilita ejecución de proveedor.
- Payouts automáticos permanecen `false` y no existe ruta para activarlos.
- Registrar evidencia de liquidación solo documenta una transferencia ejecutada fuera de TDF: no llama bancos ni proveedores. El archivo se guarda fuera de `/assets/serve`, se reencoda y no tiene endpoint público de descarga.
- La migración de rollback se niega si ya hay órdenes, refunds, disputas o settlements.
- Los logs y capturas de staging deben usar exclusivamente identidades y direcciones sintéticas.

## Validaciones pendientes de seguridad

- DAST, CSP/CORS/cookies/CSRF y rate-limit en un deployment de staging real.
- Pruebas de replay/firma y consulta autoritativa con sandbox Datafast/PayPal.
- Ejecución de refund `approved → processing → succeeded/failed` mediante adapter sandbox, con conciliación y recuperación; hoy no hay ruta merch que la realice.
- Ingestión de `commerce_dispute` desde webhooks/consultas verificadas y ejercicio operativo de plazos/respuestas; hoy solo se probó una evidencia sintética canónica.
- Revisión manual independiente de comprobantes, acceso al volumen privado y conciliación de settlement en staging.
- Repetición en staging del E2E cross-tenant local ya aprobado con vendedor A, vendedor B, comprador invitado y staff.
- Revisión WCAG con lector de pantalla, zoom 200/400% y teclado sobre build servido.
