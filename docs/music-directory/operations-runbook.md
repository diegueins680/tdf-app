# Runbook de despliegue, validación y rollback

## Preflight

1. Confirmar SHA de rama/imagen, worktree limpio y submódulo móvil fijado.
2. Ejecutar lint OpenAPI y regenerar ambos clientes; el diff posterior debe ser vacío.
3. Ejecutar pruebas backend, web, móvil, SQL, E2E, accesibilidad, SEO y carga.
4. Tomar backup nuevo y verificar restaurabilidad según el runbook de producción existente.
5. Ejecutar backfill seco y revisar conteos/ambigüedades sin PII en artefactos.
6. Confirmar que las rutas aún no reciben tráfico público y que no se requieren PostGIS, geocoder,
   email o push para arrancar.

## Orden de despliegue

1. Migración expand.
2. Seeds idempotentes.
3. Backend compatible con writers viejos.
4. Backfill seco; aprobación humana; backfill aplicado.
5. Web y móvil con contrato generado.
6. Activar lectura pública para porcentaje interno, luego Quito y finalmente general.
7. Activar mutaciones autenticadas después de revisar moderación, edad y rate limits.

## Smoke tests

- Anónimo busca Quito y recibe solo contenido permitido, sin PII/coordenadas privadas.
- Pestañas combinada/categoría/mapa comparten cursor y filtros.
- Cuenta crea dos perfiles y solo puede editar los que administra.
- Perfil con dos profesiones aparece en ambos filtros sin duplicarse.
- Publicar anuncio multi-ciudad; otra cuenta postula; autor acepta y abre un único DM contextual.
- Guardar búsqueda y ejecutar dos veces el mismo match produce una alerta.
- Evento/venue publicado se lee sin token; borrador/suspendido no.
- Cerrar anuncio como cubierto lo retira de resultados activos.

## Observabilidad

Dashboards: p50/p95/p99, errores por código, cero resultados, tasa de click, contactos, primera
respuesta, conversiones, backlog de moderación, rate-limit hits, alertas deduplicadas y lag del índice.
Logs estructurados usan correlation ID y nunca incluyen consulta libre, PII, evidencia ni coordenadas.

## Rollback

- Retirar las rutas del tráfico público en edge/router y ocultar navegación mediante el artefacto
  anterior; no asumir un runtime feature flag inexistente.
- Revertir web/móvil al artefacto anterior.
- Revertir backend manteniendo tablas nuevas (expand compatible).
- No borrar writes. Archivar por `backfill_run_id` si el backfill fue incorrecto.
- Usar rollback SQL destructivo solo en un entorno sin writes confirmados y después de backup.
- Investigar y reconciliar antes de reactivar; no reintentar jobs idempotentes con payload distinto.
