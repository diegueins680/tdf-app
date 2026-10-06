# Arquitectura, modelo y decisiones

## ADR-001: el catálogo canónico es la verdad

Estado: aceptada, 2026-09-12.

`music_release` conserva identidad y URL; `music_release_version` conserva cada revisión. Pistas, grabaciones, créditos, identificadores, partes, derechos, splits, disponibilidad y assets pertenecen a una versión. Una aprobación congela `immutable_snapshot` y `snapshot_sha256`. DDEX es una proyección asociada a ese hash, no una tabla maestra.

Consecuencia: corregir un publicado crea una versión relacionada por `correction_of_version_id`/`replaces_version_id`; clona grabaciones, identificadores, créditos, derechos, splits y el grafo lógico de assets, pero referencia exactamente los mismos bytes inmutables. La aceptación legal debe renovarse. No se altera el historial y la URL del release permanece estable.

## ADR-002: recursos y procesamiento portables

La API expone una interfaz S3-compatible con firmas SigV4 de alcance mínimo. Una carga multipart va a un bucket de cuarentena con clave UUID. El worker descarga, vuelve a calcular SHA-256, inspecciona el tipo real y genera salidas reproducibles. Solo después promueve los mismos bytes al bucket privado de máster/original y marca ubicación + checksum como inmutables. Streaming, previews, portadas y DDEX viven en buckets/clases separados.

Pipeline de audio v1:

- entrada: PCM WAV/AIFF, FLAC o ALAC; 16/24/32 bit, 44.1–192 kHz, 1–8 canales, máximo configurable (8 GiB por defecto);
- inspección: `ffprobe`; integridad por decodificación y SHA-256 antes/después;
- medición EBU R128/true peak y normalización en derivados, nunca en el máster;
- AAC-LC en M4A a 96/160/256 kbps, estéreo 48 kHz y `faststart`; FLAC y preview AAC de 30 s;
- manifiesto versionado con hashes, tamaño, mediciones y procedencia.

AAC/M4A es la base por compatibilidad de navegador y costo; FLAC queda como alternativa autorizable, no como primera elección automática. La capa de assets admite nuevos roles sin convertir video/letras/booklets en alcance de esta fase.

## ADR-003: autorización y publicación

`music_can(actor, artist, permission)` exige artista verificado y propietario, admin de equipo o permiso específico. Las mutaciones vuelven a comprobar permisos en backend. Las transiciones están enumeradas por `music_valid_transition`; triggers bloquean saltos, dobles publicaciones y edición destructiva.

`music_publish_due` y `music_withdraw_due` bloquean filas, aplican el cambio y actualizan la única versión pública en una transacción. `music_public_release` solo contiene `published` después del embargo; las APIs de catálogo/página exigen además una regla vigente en el territorio antes de revelar metadatos. `music_public_asset_accessible(asset_id, territory)` es el único gate público de firma: vuelve a resolver versión, vigencia, territorio y política para portada, thumbnail, stream o preview, y nunca autoriza un máster aunque se conozca su UUID. Favoritos, playlists e historial reciben el mismo territorio confiable: no permiten agregar contenido no disponible y marcan una entrada previamente guardada como no disponible al cambiar de región. Los assets permitidos se firman por cinco minutos; el territorio proviene de `CF-IPCountry` solo cuando el origen está cerrado a Cloudflare, y en otro caso las reglas específicas fallan cerradas.

Cada hito editorial relevante y cada reporte de infracción inserta una notificación en el centro global para el artista/equipo autorizado; `ready_for_review` y las infracciones también alcanzan a administradores. Si la tabla general usa una restricción de tipos, la migración conserva su expresión exacta, añade el namespace `music_release_*` y la restaura durante rollback. El destino solo conduce al workspace protegido; el backend vuelve a validar permisos.

## ADR-004: player único

`PlayerProvider` posee un solo `HTMLAudioElement` en el shell. El shell y el grafo de rutas se cargan como una unidad diferida, pero después permanecen montados por encima de toda navegación; cada página sigue siendo un chunk independiente. Cola, posición y preferencias se persisten con límites; Media Session, teclado, foco, mensajes accesibles, repeat/shuffle, calidad y recuperación de red se centralizan. La cola persiste `assetId` y renueva la autorización breve al seleccionar o reintentar una pista, por lo que un álbum largo no depende de URLs obtenidas al principio. Favoritos, playlists reordenables e historial sincronizado alimentan el mismo motor. El radio legado permanece detrás de compatibilidad mientras se validan todos sus consumidores.

## ADR-005: comercio canónico

`music_purchase_order` enlaza una regla/version/asset, siempre en unidades menores enteras. Se abre `commerce_checkout_session` con dominio `music_download`; solo la evidencia del proveedor verificada por el servidor puede llevarlo a paid. Triggers crean un único entitlement y lo revocan en refund. Descargas usan request UUID, límite, auditoría y URL breve. No hay regalías, suscripciones ni liquidación automática.

## Modelo lógico resumido

ADR-008 (2026-09-15): las partes del borrador no dependen de tener un crédito
o split. `music_release_version_party` conserva la pertenencia explícita y la
copia en correcciones; ver [migración y límites](version-party-membership.md).
Ese corte versionó pertenencias. ADR-009 (2026-09-15) añade datos locales de
partes y snapshot de aprobación v2, sin mutar el directorio global; ver
[nombres/identificadores y migración](versioned-party-details.md).

ADR-010 (2026-09-15): el adaptador ERN consume partes/créditos del snapshot
v2, agrupa roles por persona y preserva el ámbito de cada grabación; no infiere
créditos desde nombres de display. Ver [mapeo y límites](ddex-versioned-credits.md).

ADR-011 (2026-09-16 UTC): copiar recursos por profundidad explícita al crear
correcciones, abortando grafos incompletos o cíclicos sin debilitar integridad.
Ver [migración reversible y regresión multinivel](chained-corrections.md).

ADR-012 (2026-09-16 UTC): serializar la asignación entre fuentes de un release,
vincular claves a su fuente y devolver errores conocidos después del rollback.
Ver [concurrencia, privacidad y evidencia](correction-concurrency.md).

ADR-013 (2026-09-16 UTC): validar pertenencia y procedencia de recursos antes
de revisión/aprobación/publicación y nuevas exportaciones, sin reescribir
snapshots legados. Ver [barrera SQL/API y saneamiento](resource-graph-validation.md).

ADR-014 (2026-09-16 UTC): repetir requisitos DDEX al generar y confirmar,
coordinando las comprobaciones y escrituras con locks de versión/lease/registro;
conservar paquetes ya validados en recuperación. Ver [worker y límites](ddex-queued-validation.md).

ADR-015 (2026-09-16): separar el bundle offline de revisión de la entrega
Cloud Storage; versionar manifiesto y adaptador, corregir el deal gratuito,
validar XSD fijados y copiar exclusivamente recursos referenciados. Publicar
ZIP reproducible sin sobrescrituras; la aceptación del receptor sigue pendiente.
Ver [decisión, pruebas y operación](ddex-offline-bundle.md).

ADR-016 (2026-09-16): generar nombres de XML/recursos desde el identificador
proporcionado y las referencias técnicas; verificar concordancia en el builder.
Manifiesto v3 incluye `messageFile` y mantiene explícitamente entrega no realizada.
Ver [nombres, compatibilidad y límites](ddex-file-naming.md).

ADR-017 (2026-09-16): compartir requisitos por operación entre API, renderer y
worker; mantener la identidad propietaria de TrackRelease entre exportaciones.
Updates/retiros exigen antecedente compatible de ambas contrapartes y producto;
un retiro desde suspensión conserva la aprobación y snapshot previos.
Ver [migración, legado y límites](ddex-operation-lifecycle.md).

ADR-018 (2026-09-16): una transacción para validación/export/job, locks por
actor-clave y versión, replay ligado al cuerpo completo y recuperación acotada
de exports queued sin job. Sin nueva migración ni cambio de renderer/worker.
Ver [atomicidad, invariantes y operación](ddex-atomic-enqueue.md).

ADR-019 (2026-09-16): ligar eventos/sesiones de reproducción a su identidad,
rotarlas al cambiar de cuenta, exigir replay exacto y confirmar evento/historial
en una transacción. Eventos atrasados no retroceden posición. Una vista permite
diagnosticar sesiones legadas mezcladas sin reescribirlas. Ver
[migración, contrato 409 y límites antifraude](playback-identity.md).

```text
music_release 1---n music_release_version 1---n music_release_track n---1 music_recording
                           |  |  |  |  |
                           |  |  |  |  +--- music_asset / upload_session / processing_job
                           |  |  |  +------ music_credit --- music_party --- party_identifier
                           |  |  +--------- rights_declaration --- rights_split
                           |  +------------ availability_rule --- purchase_order --- entitlement
                           +--------------- identifiers / terms / comments / audit / DDEX export
```

UUID es la clave interna; ISRC/UPC/EAN/GRid/ISNI/IPI/DPID son valores proporcionados con `provenance` y `verification_status`. La aplicación valida sintaxis pero no emite códigos. `music_ddex_party_registry` exige evidencia verificable para un DPID operativo.

## Analítica

Los eventos llevan versión, UUID y secuencia idempotente. Un play elegible requiere 30 s continuos escuchados, o 80% para pistas menores de 30 s; seeks y automatización no suman. Se deduplican UUID/secuencia, se marcan ráfagas y patrones básicos, y se materializan métricas diarias por versión/pista/territorio. Son métricas de producto, no contabilidad certificada de regalías.

El dashboard filtra periodos y desglosa pista/territorio. Los territorios con menos de cinco oyentes agregados se omiten, salvo `ZZ` (territorio desconocido); los únicos agregados por dimensión pueden solaparse y no deben interpretarse como audiencia certificada.

## ADR-006: infracciones y respuesta

Los usuarios autenticados pueden crear reportes idempotentes sobre releases públicos. El personal estricto de TDF mueve un reporte por `received → triage → investigating → actioned/dismissed`, siempre con notas. Marcarlo `actioned` puede suspender atómicamente la versión publicada; la vista, búsqueda canónica y nuevas firmas dejan de exponerla de inmediato. Las firmas ya emitidas conservan como máximo su TTL de cinco minutos y requieren purga CDN si el incidente exige invalidación más rápida.

## Compatibilidad y backfill

La migración no borra tablas ni rutas legadas. `music_scan_legacy_release_sanitation(cursor,lote)` recorre `artist_release` por clave estable y hace upsert idempotente en `music_legacy_sanitation_item`; la vista `music_legacy_release_sanitation_queue` presenta el trabajo pendiente. El escáner solo inventaría datos si intentara deducir el tipo, máster, derechos, créditos o política de acceso, por lo que los marca como faltantes y no crea un release canónico. Un operador debe sanearlos, crear el release con evidencia real y enlazarlo antes de marcar `backfilled`. Las UI y APIs nuevas permanecen apagadas hasta activar su bandera por cohorte.

## ADR-007: reservas renovables y escrituras protegidas por intento

Aceptada, 2026-09-14. Se reutilizan `locked_at`, `locked_by` y `attempt_count`;
no requiere migración. `SKIP LOCKED` sólo decide quién reclama inicialmente.
El worker renueva la reserva periódicamente; cada escritura verifica propietario,
intento, estado y expiración bajo el bloqueo de la misma fila y en la misma
transacción. Reutilizar un nombre de worker no concede autoridad a un intento
anterior. Se bloquea primero la versión con `FOR NO KEY UPDATE` y después el
job; ese orden serializa escrituras de trabajos hermanos sin competir con los
`KEY SHARE` de las claves foráneas. La prueba concurrente detectó un deadlock
con el orden contrario y bloqueo `FOR UPDATE`. Pérdida de conexión/reserva falla cerrada.

Las salidas nuevas llevan su hash en la clave de objeto. Así un PUT en vuelo
puede dejar un objeto para reconciliación, pero no sobrescribir un artefacto de
contenido diferente que ya quedó registrado. No se promete atomicidad PostgreSQL–S3
ni se borra automáticamente un objeto al perder la reserva: podría pertenecer a
otro intento. Las rutas existentes permanecen válidas.

La señal de pérdida de reserva y `TERM` interrumpen la espera asíncrona y detienen
el grupo del comando activo. El supervisor propaga la señal y el contenedor usa
un proceso init para recoger descendientes. Límites SQL y HTTP evitan esperas
ilimitadas; tiempos/limpieza se documentan en el runbook.

Fuentes primarias consultadas el 2026-09-14:
[bloqueos de PostgreSQL](https://www.postgresql.org/docs/17/explicit-locking.html),
[consistencia y bloqueo de filas](https://www.postgresql.org/docs/17/applevel-consistency.html)
y [señales/espera de Bash](https://www.gnu.org/software/bash/manual/html_node/Signals.html).
La elección del init se contrastó con el [proyecto Tini](https://github.com/krallin/tini)
y su [paquete en Debian bookworm](https://packages.debian.org/source/bookworm/tini).
