# Tiendas de merch para artistas — diagnóstico y alcance

Estado del corte: 2026-09-10. Rama base `main`, SHA inicial `269f3784121b8bdca378ec516d5cb446cc818e39`; integración final sobre `32d618a6e8704d82105c00c584d94f9c28ad13ea`.

## Diagnóstico inicial verificado

El `HEAD` inicial ya contenía perfiles públicos, directorio, seguimiento y solicitudes de colaboración; marketplace de venta/alquiler de activos; el checkout canónico `commerce_checkout_*`; adaptadores Datafast, PayPal y revisión manual utilizados por otros dominios; órdenes, permisos y administración. Las referencias a una tienda de merch para artistas seguían marcándola como futura y no existían entidades de catálogo físico específicas, storefront, rutas ni clientes de merch.

La solución amplía esas fuentes de verdad. No convierte `asset`, equipos del estudio, tickets, servicios ni música en productos de merch y no crea un checkout o una comunidad paralelos.

## Matriz de reutilización y brechas

| Área | Reutilizado | Extensión incorporada | Estado |
|---|---|---|---|
| Identidad | `party`, cuentas, perfiles y managers del directorio | Elegibilidad reclamada/verificada y propietario primario | Implementado; migración verificada |
| Comunidad | Perfil, directorio, follow, contacto/colaboración, reporte/bloqueo existentes | Enlaces contextuales desde tienda y producto | Integrado; no se creó chat nuevo |
| Comercio | `commerce_checkout_session`, line items, intentos y evidencia | Orden, snapshots y reserva de stock de merch | Implementado; proveedor externo bloqueado |
| Catálogo | Búsqueda y marketplace general | Tiendas, productos, variantes, SKU, imágenes y stock específicos | Implementado |
| Entrega | Estados y patrones de órdenes existentes | Zonas Ecuador, retiro, envío, tracking y timeline | Implementado para operación manual |
| Finanzas | Checkout, reembolso, disputa, evidencia y conciliación canónicos | 10%, override por vendedor, autorización dual de refund, proyección read-only de disputa y comprobante privado de settlement manual | Implementado y verificado localmente con datos sintéticos; ejecución de pagos/refunds reales deshabilitada |
| Archivos | Servicio durable `/assets/serve` | Reencode JPEG, tamaños responsive, checksum, alt y moderación | Implementado; revisión humana sigue requerida |
| Clientes | OpenAPI y generadores existentes | Tipos y APIs web/móvil regenerados | Verificado por typecheck |
| UX web | React, MUI, routing y analítica existentes | Descubrimiento, storefront, producto, carrito, tracking, seller y admin | Implementado; runtime visual responsive verificado, staging pendiente |
| UX móvil | Expo Router y feature registry existentes | Compra completa hasta orden pendiente y fulfillment esencial | Implementado; runtime público verificado en Android API 36.1 e iOS 18.3 Simulator; dispositivo físico pendiente |
| Pagos | Adaptadores canónicos de otros dominios | Flags y vínculo de la orden de merch al checkout canónico | La iniciación específica por proveedor se mantiene cerrada |
| Posventa | Estados financieros canónicos | Cancelación inmediata sin pagar, issues, triage vendedor/staff, solicitud/asignación/aprobación de refund y monitoreo de disputas | Implementado/verificado hasta autorización; ejecución refund e ingestión real de disputas por proveedor diferidas |

## Alcance del incremento

- Vendedores: solicitud desde un perfil administrado, revisión del piloto, propietario automático, colaboradores buscados por nombre/artista/username y seis permisos separados.
- Catálogo: prendas, vinilo, CD, cassette, póster, accesorios, ediciones limitadas, bundles y extensión `other`; variantes, stock con versión optimista, preventa, bajo pedido, límites, publicación y revisión.
- Storefront: URL `/tienda/{slug}`, integración con perfil/directorio y marketplace, SEO/OG, relacionados y favoritos.
- Compra: carrito de un solo vendedor, invitado, cálculo servidor, snapshots, reserva atómica de veinte minutos, idempotencia y tracking mediante token opaco.
- Operación: retiro o envío nacional, políticas versionadas, panel de órdenes con filtro/totales/exportación sin PII, preparación, tracking, cancelación segura sin pagar, cola de incidencias con escalamiento, reembolsos canónicos con doble control sin ejecución de proveedor, disputas canónicas de solo lectura y settlements manuales con comprobante privado.
- Rollout: todos los flags se crean en `false`; `merch.checkout.runtime_ready` funciona como kill switch adicional.

## Estado honesto

La tienda no está lanzada. La implementación disponible permite probar localmente el dominio, los contratos y los recorridos, pero no habilita cobros. No debe activarse `merch.checkout.runtime_ready` hasta que un adaptador específico de merch para Datafast, PayPal o pago manual complete en staging creación/captura, webhook o revisión independiente, reembolso y conciliación. Tampoco se habilitarán vendedores externos antes de cerrar la lista legal, contable y operativa de [OPERATIONS.md](./OPERATIONS.md).

## Alcance diferido explícito

- Descargas digitales, licencias y DRM.
- Payouts automáticos.
- Couriers automáticos.
- Carrito multi-vendedor; cada vendedor conserva carrito y orden independientes.
- Chat nuevo en tiempo real.
- Reseñas públicas hasta completar moderación y criterios de salida.
- Disponibilidad fuera de Ecuador o moneda distinta de USD.
- Ejecución de refund e ingestión de dispute por proveedor, además del acceso excepcional auditado a comprobantes privados; quedan pendientes del adapter sandbox y del diseño operativo de retención.
