# API y configuración operativa

Fecha: 2026-09-12. La API Servant usa el prefijo `/music`; las rutas protegidas heredan la autenticación TDF y vuelven a comprobar artista verificado, rol administrativo o permiso de equipo en el servidor.

## Superficie HTTP

- Público: `GET /music/releases`, `GET /music/releases/:slug`, `GET /music/assets/:assetId/access` y `POST /music/playback-events`.
- Autoría: listado propio en `GET /music/studio/releases`, creación idempotente, autosave con control optimista, contenido canónico, términos, validación, transiciones, comentarios y correcciones versionadas.
- Carga: creación/binding multipart, firma por parte, registro idempotente de ETag/hash, finalización, confirmación y cancelación.
- Biblioteca: favoritos, playlists/ítems reordenables e historial de reproducción.
- Confianza y métricas: reportes de infracción, acciones administrativas y analítica por release/periodo.
- Comercio: órdenes, Datafast, PayPal, entitlements, descargas compradas y descargas gratuitas autorizadas.
- DDEX: registro verificado de partes/DPID, creación/listado de exportaciones y descarga exacta del paquete inmutable.

Las operaciones que crean efectos externos o repetibles exigen `Idempotency-Key` (8–200 caracteres ASCII visibles). Las rutas territoriales solo aceptan `CF-IPCountry` cuando `MUSIC_TRUST_CF_IPCOUNTRY=true`; el origen debe estar inaccesible directamente y debe eliminar o sobrescribir cualquier cabecera del cliente. Sin esa garantía solo se descubre/autoriza `Worldwide`.

Contratos que deben conservarse al generar clientes:

- partes externas: `partyId` y `partyKind` (además de `clientRef`, `tdfPartyId`, nombres e identificadores);
- evidencia multipart: `byteSize`, `etag`, `sha256`; confirmación final: `etag`;
- analítica del player: `eventId`, `sessionId`, `sequenceNumber`, `eventType`, `releaseVersionId`, `recordingId`, contadores, fecha y `metadata`.

El parser rechaza campos desconocidos. Estos nombres tienen pruebas de regresión en `TDF.MusicRelease.ContentSpec` y el E2E los consume mediante HTTP real.

Desde la migración `2026-09-15_music_version_parties.sql`, `parties` incluye
también colaboradores sin crédito/split. El PUT reemplaza las pertenencias;
el cliente debe reenviar el conjunto que desea conservar y reutilizar sus
`partyId`. Con `2026-09-15_music_party_details.sql` se editan nombres e
identificadores exclusivamente dentro de la versión, sin emitir códigos. La
respuesta agrega `detailsSource` (`legacy_observed`/`user_provided`); el cliente
no lo envía ni establece estados de verificación. Para partes vinculadas ya
existentes, enviar solo `partyId`, no también `tdfPartyId`.

## Correcciones

`POST /music/releases/:releaseId/versions/:versionId/corrections` exige
release.edit y clave idempotente vinculada a la fuente. Reutilizar la clave
con otra fuente devuelve 409. Grafos inválidos devuelven 422 con
`errors[{code,fieldPath,message}]` después del rollback, sin SQL ni locators.
Conflictos de serialización/deadlock permiten reintentar con la misma clave.
Ver [códigos, contrato y migración](correction-concurrency.md).

## Banderas

La migración crea apagadas, por ambiente, `music_releases.authoring`, `music_releases.processing`, `music_releases.public`, `music_releases.commerce` y `music_releases.ddex_export`. Su orden de activación está en la guía de despliegue. Ninguna variable de entorno sustituye estas puertas de base de datos.

## Variables sin secretos

La plantilla canónica es `tdf-hq/config/default.env.example`. Para música requiere:

- S3-compatible: `MUSIC_S3_ENDPOINT`, `MUSIC_S3_REGION`, credencial de API/worker y buckets separados `QUARANTINE`, `MASTER`, `DERIVATIVE`, `DDEX`;
- límites/edge: `MUSIC_MASTER_MAX_BYTES`, `MUSIC_TRUST_CF_IPCOUNTRY`;
- worker: `DATABASE_URL`, `MUSIC_WORKER_ID`, `MUSIC_WORKER_POLL_SECONDS`;
- DDEX: `MUSIC_DDEX_RENDER_BIN`, `MUSIC_DDEX_SCHEMA_DIR`; la descarga del XSD exige `DDEX_LICENSE_ACCEPTED=true` de forma puntual, no en el runtime;
- pago: configuración existente de Datafast/PayPal y `COMMERCE_EVENT_ENCRYPTION_KEY`.

No guardar valores reales en el repositorio. Fuera de local/test, base de datos y object storage deben usar TLS; las credenciales de API, worker y CI deben ser diferentes y de privilegio mínimo.

## Dependencias de migración

Aplicar antes: `init_schema.sql`, `2026-07-12_notification_table.sql`, `2026-08-05_artist_enrichment.sql`, `2026-08-13_unified_checkout_core.sql` y `2026-09-04_access_request_notification_types.sql`. Después aplicar `2026-09-11_music_release_platform.sql`. El escaneo legado se ejecuta aparte y es reanudable; no activa ninguna bandera.

La API actual requiere después `2026-09-15_music_preview_ranges.sql` y
`2026-09-15_music_version_parties.sql` y `2026-09-15_music_party_details.sql`;
consultar [migración de datos y rollback protegido](versioned-party-details.md).

Después se requieren `2026-09-16_music_correction_asset_graph.sql` y
`2026-09-16_music_correction_concurrency.sql`, en ese orden, para copias
multinivel y asignación concurrente segura. Ver [recuperación](correction-concurrency.md).

Finalmente aplicar `2026-09-16_music_resource_graph_validation.sql` antes de
reanudar autoría/revisión. `/validate` informa errores por campo y `/transition`
devuelve 422 para requisitos editoriales incumplidos, sin consumir la clave
idempotente ni escribir un snapshot. Ambos contratos están en OpenAPI y tipos
generados. La nueva vista operativa detecta referencias inválidas legadas,
sin corregirlas ni suspender publicaciones automáticamente. Ver
[códigos, orden, diagnóstico y rollback](resource-graph-validation.md).

Para API/renderer/worker v5 aplicar después
`2026-09-16_music_ddex_operations.sql`. DDEX devuelve 422 por campo ante
`invalid_export_state`, `initial_export_missing` o `takedown_not_due`, además
de los requisitos editoriales existentes. Reutilizar una clave de exportación
con otra operación/remitente/destinatario devuelve 409. No se añaden secretos,
variables ni formas de respuesta. Ver [operaciones y rollback](ddex-operation-lifecycle.md).

ADR-018 reúne export y job en una transacción sin SQL adicional. La clave del
actor también queda ligada a la versión; reutilizarla en otra versión devuelve
409. Otra clave para el mismo export natural o registro incompatible también
devuelve 409, sin filas nuevas. Replays idénticos conservan la respuesta y pueden
reponer únicamente un job ausente de un export queued. Ver
[concurrencia, errores y recuperación](ddex-atomic-enqueue.md).

ADR-019 requiere `2026-09-16_music_playback_identity.sql`, después de las
migraciones anteriores. Ambos POST de playback devuelven 409 con
`code=playback_identity_conflict` cuando se reutiliza evento/sesión/secuencia
con otra identidad o contenido; un replay exacto conserva 200 vacío. OpenAPI
incluye las dos rutas. Desplegar API y UI coordinadas, con ingesta pausada:
la UI antigua no rota sesiones al cambiar de cuenta. Sin variables nuevas.
Ver [diagnóstico legado, despliegue y rollback](playback-identity.md).
