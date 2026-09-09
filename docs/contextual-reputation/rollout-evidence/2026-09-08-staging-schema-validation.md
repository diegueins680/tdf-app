# Evidencia técnica — validación de esquema en staging

- **Fecha:** 2026-09-08
- **Ambiente:** `tdf-hq-studio-audit-staging`
- **Release:** Fly v18 (`l8w0PB4QQoOwMTBXMJk9j9B0`)
- **Código integrado:** `7f60f6ec03171a1b6e558bf8247f59e5914ea8f0`
- **Imagen multi-plataforma:** `sha256:9165e11128a7adfe8794ed1fb8089e1fe05e6c588f84ed89570518913fd7ff9a`
- **Imagen amd64 seleccionada por Fly:** `sha256:32df7500d4868dfe6c66cd04cde67fb8fd5c48204c0f8696b73da1335bda4017`
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
- `REPUTATION_AGGREGATION_ENVIRONMENT=staging`.
- `REPUTATION_AGGREGATION_MODE=simulation`.
- `RUN_MIGRATIONS=false`; las migraciones ORM permanecen desactivadas.
- `AUTO_APPLY_PRODUCTION_MIGRATIONS=false`; los arranques ordinarios no ejecutan
  trabajo de esquema.
- Antes del rollout, el `release_command` habilitó temporalmente únicamente el
  ejecutor limitado por checksum con `TDF_MIGRATION_PRECHECK_ONLY=true`, confirmó
  que las 88 migraciones ya estaban aplicadas, aprobó la verificación completa
  de esquema y terminó correctamente.
- CORS aceptó únicamente el origen web exacto de staging y rechazó
  `https://evil.example` con HTTP 400 sin cabecera de autorización de origen.

Antes del release 18 se crearon snapshots nuevos del volumen de API
(`vs_91lMXgA2jawwCj0w4l7RZ`, digest `42b7c3a0ce571ba9d0234526c0208fe53638b4d98409bee1022d4c1afc684760`)
y del volumen de base de datos (`vs_a496agR5OG9tMLyAM9m3NkK`, digest
`49892605b0d9de22879af874296070afa37f25d4a7fc39ade11dbd7d35feff74`). El volumen activo
`vol_vdej5owg087momw4` conservó sus 13 archivos existentes y la tabla de
evidencias continuó vacía. El volumen histórico separado y sus snapshots
permanecen preservados.

## Límite de esta evidencia

Esta evidencia valida únicamente la migración y el arranque de staging. No
aprueba G0, no provisiona cola/DLQ/dashboards/on-call, no valida accesibilidad
ni moderación, no publica copy legal y no autoriza un piloto o visibilidad
pública de reputación.
