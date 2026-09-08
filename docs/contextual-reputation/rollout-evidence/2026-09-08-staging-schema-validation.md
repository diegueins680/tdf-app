# Evidencia técnica — validación de esquema en staging

- **Fecha:** 2026-09-08
- **Ambiente:** `tdf-hq-studio-audit-staging`
- **Release:** Fly v16
- **Código integrado:** `714060ffcc1db56448f6d3cb82e055e0085af4e7`
- **Alcance:** compatibilidad histórica de `public.notification.notif_type`.

## Resultado

La ejecución controlada e idempotente de migraciones revisadas concluyó con
verificación de esquema aprobada. La migración
`2026-09-08_notification_notif_type_text_compatibility` quedó registrada en
`public.tdf_schema_migration` con el checksum
`e495d71e1f6553735351d58edd65afbb4cb0a5aaa841112937c3b068faeec204`.

La comprobación de solo lectura posterior confirmó:

- `public.notification.notif_type` es `text NOT NULL`.
- `/health` respondió `{ "db": "ok", "status": "ok" }`.
- `CONTEXTUAL_REPUTATION_ENABLED=false`.
- `REPUTATION_AGGREGATION_WORKER_ENABLED=false`.
- `REPUTATION_AGGREGATION_MODE=simulation`.
- `AUTO_APPLY_PRODUCTION_MIGRATIONS=false` fue restaurado tras la validación.

El volumen histórico de staging permanece preservado junto con sus snapshots;
la máquina anterior se retiró únicamente después de crear y verificar esa ruta
de recuperación.

## Límite de esta evidencia

Esta evidencia valida únicamente la migración y el arranque de staging. No
aprueba G0, no provisiona cola/DLQ/dashboards/on-call, no valida accesibilidad
ni moderación, no publica copy legal y no autoriza un piloto o visibilidad
pública de reputación.
