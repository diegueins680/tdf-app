# Plan de abstraccion (mantener funciones, habilitar uso fan-artista/label)

Objetivo: en lugar de reetiquetar o eliminar funciones, crear una capa de configuracion y tipos que permita usar la misma logica para casos fan-artista/label (divulgacion, marketplace, streaming) sin perder la generalidad de CRM, reservas, facturacion, inventario y pipelines.

## Principios
- No romper: conservar modelos y endpoints; sumar tipos/metadata para especializar.
- Configurable: vocabulario, tipos de item/evento/orden y etapas de pipeline definibles por entorno.
- Reusable: la UI consume schemas de tipos y los muestra con labels configurables.

## Fase 0 — taxonomia y labels configurables
- Crear tablas/JSON de configuracion (seed por entorno) con:
  - Tipos de party (fan, artista, staff, cliente genérico).
  - Tipos de producto/servicio (merch, ticket, drop digital, sesión, servicio).
  - Tipos de reserva/evento (live, listening, booking, clase).
  - Estados y etapas de pipeline (teaser, pre-save, entrega, etc.) parametrizables.
  - Copy/labels por módulo para UI (sin hardcodear strings).
- UI: leer labels desde configuración; fallback a los actuales para compatibilidad.

## Fase 1 — modelo de datos extensible
- Añadir campos de metadata (JSONB) y enums suaves en:
  - Products/paquetes: `kind`, `perks`, `usageRules` para servir membresías o servicios.
  - Bookings/reservas: `eventType`, `capacity`, `hostPartyId` (artista/label) y `attendees` (ids).
  - Invoices/ordenes: `orderType` (merch/ticket/servicio), `lineItemKind`, `tipAmount` opcional.
  - Inventory: `inventoryType` (merch/backline/consumible), `shippingState` opcional.
  - Pipelines: `pipelineType` y `stageConfigId` para reutilizar columnas/etapas.
- Mantener campos actuales; no renombrar tablas; agregar índices donde corresponda.

## Fase 2 — capa de mapeo fan-artista (config, no hardcode)
- Definir presets de configuración “fan-artista/label”:
  - Paquetes como “membresías/perks”.
  - Reservas como “eventos/lives” con cupos y check-in.
  - Invoices como “órdenes” (merch/tickets/drops/propinas).
  - Inventario como “merch/logística”.
  - Pipeline como “campañas de lanzamiento”.
- Exponer estos presets vía feature flag; UI selecciona preset pero puede mostrar el vocabulario genérico según entorno.

## Fase 3 — UX adaptable
- UI Web/Móvil: componentes leen schema de tipos y labels; solo muestran acciones válidas según `kind` o `pipelineType`.
- Fan Hub: mostrar perks, calendario y tienda usando los mismos endpoints con filtros por `kind`.
- Label/Backoffice: dashboards parametrizados por `orderType`, `eventType`, `pipelineType`.
- Check-in offline: reutilizar reservas/eventos con `eventType=fan-experience`.

## Fase 4 — migracion y compatibilidad
- Seeds de configuración por entorno (dev/stage/prod) sin tocar datos existentes.
- Scripts de backfill no destructivos: rellenar `kind` y `eventType` con valores por defecto que preserven el comportamiento actual.
- Flags para activar el preset fan-artista gradualmente.

## Medicion
- Métricas parametrizadas por tipos:
  - Eventos: RSVP/check-in por `eventType`.
  - Marketplace: GMV y conversión por `orderType` y `product kind`.
  - Campañas: progreso por `pipelineType` y etapa.
