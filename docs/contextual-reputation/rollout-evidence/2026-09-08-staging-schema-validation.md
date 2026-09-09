# Evidencia técnica — validación de esquema en staging

- **Fecha:** 2026-09-08
- **Ambiente:** `tdf-hq-studio-audit-staging`
- **Release:** Fly v17 (`VRl7314XXwvlkH3omPlby76Q`)
- **Código integrado:** `714060ffcc1db56448f6d3cb82e055e0085af4e7`
- **Imagen amd64:** `sha256:38318e0b6c1c6d64748de83715b25a16214f5006f62f6beab0808ff5e712f99f`
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
- `AUTO_APPLY_PRODUCTION_MIGRATIONS=true`; el arranque vuelve a verificar la
  lista revisada y limitada por checksum, y confirmó que todo ya estaba aplicado.
- CORS aceptó únicamente el origen web exacto de staging y rechazó
  `https://evil.example` con HTTP 400 sin cabecera de autorización de origen.

Antes de reconciliar la configuración se crearon snapshots nuevos del volumen
de API (`vs_YnmGJkoMzALLUxQKXzoK`) y del volumen de base de datos
(`vs_oZ7ywx8JY17fnxN975LJVvO`). El volumen activo
`vol_vdej5owg087momw4` conservó sus 13 archivos existentes y la tabla de
evidencias continuó vacía. El volumen histórico separado y sus snapshots
permanecen preservados.

## Límite de esta evidencia

Esta evidencia valida únicamente la migración y el arranque de staging. No
aprueba G0, no provisiona cola/DLQ/dashboards/on-call, no valida accesibilidad
ni moderación, no publica copy legal y no autoriza un piloto o visibilidad
pública de reputación.
