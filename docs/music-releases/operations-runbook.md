# Runbook operativo

## Identidad de analítica

Un 409 `playback_identity_conflict` no es un fallo transitorio de red: revisar
cambio de identidad, reutilización de secuencia o alteración del cuerpo. No
reintentar con UUID aleatorio para eludir el control. Al cambiar de cuenta,
rotar la sesión. Para legado consultar `music_playback_session_sanitation`
administrativamente, sin borrar evidencia ni atribuir cuentas por inferencia.
SQL/API/UI requieren despliegue coordinado con ingesta pausada; rollback
restaura el comportamiento vulnerable y exige mantenerla pausada. Ver
[procedimiento, atomicidad y límites](playback-identity.md).

## Preparación

1. Aplicar la migración aditiva en staging y comprobar `music_%`/triggers/flags.
   Para API/worker con previews configurables, aplicar después `2026-09-15_music_preview_ranges.sql`; ver [contrato, legado y rollback](preview-ranges.md). No arrancar este worker contra el esquema anterior.
2. Crear buckets privados y políticas por prefijo; comprobar las capacidades reales de versioning/retención + backup en máster y DDEX, lifecycle corto en cuarentena. R2 no soporta las APIs S3 de versioning/Object Lock; sus bucket locks propios requieren un ensayo específico de reintentos y recuperación (ver matriz de infraestructura).
3. Configurar las variables de `default.env.example`; nunca compartir el principal del worker con el navegador.
4. Tras revisar la licencia: `DDEX_LICENSE_ACCEPTED=yes ./scripts/fetch-ddex-ern432-schema.sh /ruta/ern432` y montar esa ruta read-only.
5. Construir el worker con `docker build -f tdf-hq/Dockerfile.music-worker .`; ejecutarlo como proceso separado. Empezar con una instancia en staging; antes de ampliar, probar dos instancias contra el storage elegido, incluidos fallos y transferencias largas. La reserva ya tiene renovación y control por intento; esto no sustituye el ensayo de carga ni la validación S3. No mezclar workers anteriores sin esta protección con la versión nueva.

Antes de conectar esa imagen a datos, ejecutar `npm run test:music-worker-image -- REFERENCIA_LOCAL`;
ver [prueba Linux aislada](linux-worker-image.md). Comprueba los scripts de la
imagen contra el checkout, usa sus ejecutables reales y no necesita red ni
credenciales. La construcción usa el manifiesto reducido `tdf-hq/music-worker/`
con los mismos módulos Haskell y Stack limitado a dos trabajos; mantener su
contrato alineado ejecutando `node --test scripts/__tests__/music-worker-build.test.mjs`.

Para probar esa imagen con almacenamiento HTTPS y PostgreSQL desechables,
ejecutar `npm run test:music-linux-integration`. Ver
[topología, requisitos y evidencia](linux-worker-integration.md). El worker
ejecuta los archivos incluidos en la imagen; la API sigue en el host. Este modo
no necesita la base PostgreSQL habitual ni buckets/credenciales de TDF y no
certifica CDN ni un despliegue completo del backend en Linux.

Antes de habilitar una cohorte, ejecutar el escenario API sobre una base desechable con un binario recién compilado:

```sh
TDF_MUSIC_API_E2E_BACKEND_EXE=/ruta/al/tdf-hq-exe \
TDF_MUSIC_API_E2E_PASSWORD='valor-sintetico-local-de-16-caracteres' \
./scripts/test-music-release-api-e2e.sh
```

El timeout de bootstrap es 600 s y puede ajustarse con `TDF_MUSIC_API_E2E_BOOT_TIMEOUT_SECONDS`. El harness se niega a reutilizar el puerto o una base existente, crea identidades sintéticas, imprime el log del backend si falla y siempre elimina servidor/base/runtime al salir. Inyecta directamente solo el resultado técnico sintético que normalmente producirían storage/worker; no simula ni intercepta handlers HTTP.

Ejecutar también `npm run test:music-worker-runtime`. Requiere PostgreSQL local en
`127.0.0.1:5432`, permiso `CREATEDB` para el usuario del sistema, Node, Bash,
FFmpeg/ffprobe, psql, jq, shasum, xmllint, zip y unzip en `PATH`. Crea una base
con nombre aleatorio, aplica las migraciones reales y elimina sólo esa base y
su directorio temporal al terminar. Usa audio/arte generados por FFmpeg y una
sustitución explícita de `curl` para inyectar fallos de transporte; no prueba
SigV4, multipart, HTTP, TLS, CORS ni CDN. Verifica el SQL del worker, promoción,
hashes, reintentos, dead letters, rollback y transiciones editoriales.
Para repetir sólo las carreras y las señales: `npm run test:music-worker-runtime -- --concurrency-only`.

Para medir etapas sin imprimir argumentos/SQL/secretos, activar temporalmente
`MUSIC_WORKER_DIAGNOSTICS=true`. SQL y procesos hijos emiten tiempos de resolución
de un segundo a stderr; no cambian los timeouts ni la reserva. El harness ahora
espera a que desaparezca su sesión de creación antes de borrar su base aleatoria,
incluso si `createdb` agotó tiempo. Ver [diagnóstico y evidencia](worker-regression-diagnostics.md).

Para comprobar el firmador de producción con transferencias reales, ejecutar
`npm run test:music-s3-local` después de instalar la imagen fijada según
[la guía S3 local](local-s3-integration.md). Usa un servidor local desechable,
HTTPS verificado, CORS, rangos, multipart y acceso privado. Es complementario:
ese comando también prueba el cargador de storage del worker, pero no conecta
la API/PostgreSQL/FFmpeg ni prueba la CDN. El modo
`npm run test:music-api-s3-local` sí conecta handlers HTTP, PostgreSQL y FFmpeg
con ese storage, sin insertar activos ficticios. Compilar primero el backend
y consultar la evidencia de su última ejecución en la guía; la existencia del
comando no implica que haya pasado. Sus pagos/reembolsos siguen siendo fixtures
canónicos, no llamadas al proveedor.

El worker de una iteración requiere Bash (`bash -n` para validarlo; no ejecutarlo
con `sh`). Sus pipelines usan `pipefail` y se invocan fuera de condiciones `if`
para que el primer fallo termine el procesamiento. La trampa `EXIT` conserva el
error y programa el reintento; al agotar intentos actualiza la versión a
`validation_failed`. Si falla también PostgreSQL al registrar el error, la
reserva queda recuperable al expirar. El éxito del job y la validación final
se confirman en una transacción: `ready_for_review` sólo cuando no hay errores
de envío; en otro caso queda `validation_failed` con errores por campo.

La reserva usa `MUSIC_WORKER_LEASE_SECONDS=900` y renovación cada
`MUSIC_WORKER_HEARTBEAT_SECONDS=30`; todas las instancias de una cola deben usar
el mismo TTL y el heartbeat no debe superar un tercio del TTL. Cada transacción
bloquea primero la versión (`NO KEY UPDATE`) y después el job, y comprueba estado `running`, propietario, número de intento y
vigencia antes de escribir. Una reserva vencida no puede resucitarse. La cuenta
SQL necesita permiso `TEMP` sobre su base para la función temporal de control.
Los bloqueos SQL esperan como máximo 5 s y cada sentencia 30 s; configurar además
`connect_timeout=10` en `DATABASE_URL`. La pérdida de renovación detiene el comando
activo y deja que otro intento recupere el trabajo al vencer la reserva.

`TERM` se propaga desde el supervisor a la iteración, que detiene el grupo de
procesos de la transferencia/transcodificación y registra el fallo si todavía
posee la reserva. El contenedor usa `tini` para recoger procesos huérfanos. Un
`SIGKILL` no ejecuta limpieza: esperar la expiración y reconciliar temporales y
objetos sobrantes. La confirmación de fallo y su estado DDEX comparten transacción;
el refresco editorial posterior vuelve a comprobar el número de intento/estado.
Si ese refresco falla, la evidencia de fallo y el reintento no se deshacen.

Las transferencias tienen 10 s para conectar y hasta
`MUSIC_WORKER_TRANSFER_TIMEOUT_SECONDS=3600` por intento (ajustable hasta 86400 s
según tamaño de máster y red). Los derivados/paquetes nuevos incluyen SHA-256 en
la clave. Las referencias anteriores siguen funcionando sin migración. Una
transferencia ya aceptada por storage puede terminar después de perder la reserva;
su resultado no puede registrar metadatos ni cambiar bytes de otro hash. Los
originales conservan su ruta estable y sólo se suben tras comprobar el hash del
original; un intento antiguo sólo puede volver a enviar esos mismos bytes.

Se envía SQL mediante `psql -f -`, manteniendo valores separados con `-v` y
literales `:'nombre'`. `psql -c` no realiza esa sustitución. Fuentes consultadas
el 2026-09-14: [documentación de psql](https://www.postgresql.org/docs/18/app-psql.html)
y [reglas de errexit de Bash](https://www.gnu.org/s/bash/manual/html_node/The-Set-Builtin.html).

Verificar después el shell persistente, responsive y accesible en los proyectos Playwright configurados. Elegir un puerto libre evita reutilizar por accidente un Vite de otro checkout:

```sh
PLAYWRIGHT_PORT=4187 \
PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-music-playwright \
npx playwright test e2e/web/music-player.spec.mjs
```

Este spec usa API y media sintéticas en el navegador para aislar la mecánica UI. Debe pasar junto con el E2E HTTP anterior; no lo sustituye.

## Monitoreo mínimo

Alertar por jobs `dead_letter`, reservas `running` cuyo `locked_at` no se renueva durante el TTL configurado, edad del job más antiguo, uploads expirados, objetos de cuarentena sin sesión, fallos/ratio de rebuffer, errores de firma, scheduler atrasado, webhooks sin binding, órdenes paid sin entitlement y DDEX `validation_failed`/`failed`. Un job largo con heartbeat vigente no está vencido. Comparar hash/tamaño en restauraciones.

## Reprocesar

No editar un asset ready. Verificar causa y checksum del original. Para un fallo reintentable, pasar el job a `retry`, limpiar lease, conservar `attempt_count` y usar el mismo `job_key`. Si cambian codec/pipeline, escribir prefijo y `pipelineVersion` nuevos; no sobrescribir derivados anteriores. Un dead-letter exige ticket y actor en auditoría.

## Retirada o infracción

1. Registrar reporte/evidencia desde el release; personal autorizado hace triage con notas y puede marcarlo `actioned` suspendiendo atómicamente la versión publicada.
2. Para retirada programada, fijar UTC + zona original y `takedown_scheduled`.
3. `music_withdraw_due` retira una sola vez, limpia la versión pública y deja historial.
4. La vista pública y las nuevas firmas dejan de autorizar al suspender/retirar. Purgar caché CDN y búsqueda/feed; una firma ya emitida puede vivir hasta cinco minutos.
5. Si hubo entrega DDEX, generar takedown al mismo receptor después de una entrega inicial válida.
6. Conservar máster/evidencia según política legal; no borrarlos como parte del hide público.

## Cuarentena y huérfanos

Promoción, derivados y DDEX usan ahora [multipart del worker](worker-multipart.md)
desde el umbral configurado. Configurar permisos de aborto y lifecycle de partes
abandonadas en todos los buckets de destino. Los reintentos de jobs no recuperan
un checkpoint multipart durable. TERM intenta abortar; SIGKILL o una respuesta
perdida requieren conciliación/lifecycle. No habilitar 8 GiB solo por pasar los
fixtures pequeños del harness.

Abortar multipart cancelados/expirados. El worker elimina el objeto de cuarentena después de promover el original; una eliminación fallida deja advertencia y debe reconciliarse por `music_upload_session.quarantine_object_key`. No borrar objetos sin comprobar que no existan en `music_asset` y que superen la ventana de seguridad.

El inventario de objetos debe incluir también prefijos de derivados y DDEX: un
PUT completado seguido de rollback/pérdida de reserva puede dejar un objeto sin
fila. No existe una transacción distribuida entre PostgreSQL y S3. Retener estos
objetos al menos durante TTL + máximo de transferencia/reintentos + margen antes
de reconciliarlos; no aplicar un lifecycle ciego a prefijos que contienen assets
registrados. El ensayo local no valida todavía esa limpieza con un proveedor real.

## DDEX

Encolado atómico: desplegar todas las instancias de API coherentes. Un replay
idéntico puede reparar un export `queued` sin job, pero nunca reinicia intentos
existentes ni paquetes terminales. 409 por clave/cuerpo, export natural duplicado,
registro incompatible o vínculo de job exige revisar la causa; no generar claves
nuevas repetidamente. Ver [inventario, invariantes y rollback](ddex-atomic-enqueue.md).

Adaptador actual v5: aplicar `2026-09-16_music_ddex_operations.sql` y desplegar
API/renderer/worker juntos, con generación pausada. `initial_export_missing`
requiere comprobar producto y ambas contrapartes, además del adaptador del
paquete inicial; no reenviar un alta a un receptor legado para eludir el error.
`takedown_not_due` exige esperar la retirada programada: este mensaje sin deals
es inmediato. Un 409 de idempotencia exige recuperar la solicitud original o
usar una clave nueva para una operación realmente distinta. Ver
[continuidad de identidad, inventario legado y rollback](ddex-operation-lifecycle.md).

Actualización v4/manifiesto v3: leer `messageFile` para localizar el XML dentro
del ZIP, ya no `release.xml`. El builder verifica correspondencia entre nombres,
identificador principal y referencias técnicas. Un error `resources.fileName`
indica discrepancia o mezcla de versiones; desplegar renderer/builder coherentes,
no renombrar archivos manualmente. Exports históricos se descargan sin cambios.
Ver [operación y compatibilidad](ddex-file-naming.md).

El adaptador v3 genera un **bundle offline TDF**, no una entrega Cloud Storage
1.8.1. El manifiesto v2 declara `packageFormat`, `deliveryPerformed:false`,
`targetChoreography` y `messageCreatedAt`; sustituye las claves ambiguas
`choreography` y `generatedAt` del manifiesto anterior. Ningún paquete histórico
se reescribe. Ver [contrato, hashes XSD y evidencia](ddex-offline-bundle.md).
La hora de generación operativa sigue en la fila de exportación, no en el ZIP.
Si falta un XSD fijado, cambia su hash o falta un recurso referenciado, fallar
cerrado: no cambiar hashes ni relajar validación para desbloquear el job.

El worker actualizado repite requisitos al generar y confirmar paquetes;
requiere las migraciones hasta `music_ddex_operations` y desplegar
su imagen nueva. Ante `ddex_preconditions_failed`, revisar los errores por
campo de `validation_report` y la vigencia/identidad de los registros DDEX.
No editar snapshots aprobados ni reiniciar intentos indiscriminadamente.
Un fallo final puede dejar recursos privados para reconciliación; no borrarlos
automáticamente. Ver [reintentos, despliegue y límites](ddex-queued-validation.md).

Un error de campo se corrige en una nueva versión canónica. No parchear el XML. Una exportación valid permanece inmutable y se vuelve a descargar por su mismo `package_asset_id`/SHA-256. Si el hash oficial del XSD cambia, detener exports, investigar release notes, actualizar matriz/adaptador/fixtures y habilitar de nuevo solo después de validación.

Si el worker cayó después de confirmar `music_ddex_export.status='valid'` y antes
de cerrar el job, el reintento recupera ese mismo hash sin volver a renderizar,
subir ni cambiar referencias. La exportación debe pertenecer a la versión del
job, también al registrar un fallo. La suite aislada original usa artefactos
sintéticos; la integración opcional `--ddex-schema-dir` añade generación real,
ZIP/S3 y descarga idéntica tras recuperación. Ninguna verifica DPID reales,
aceptación de un receptor ni entrega Cloud Storage.
