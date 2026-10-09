# Despliegue, rollback y verificación

Identidad de analítica (ADR-019, 2026-09-16): aplicar
`2026-09-16_music_playback_identity.sql` después de las migraciones listadas
abajo y antes de reabrir ambos endpoints de ingesta. Pausar/drenar peticiones,
desplegar todas las réplicas API y la UI que rota sesiones; sin imagen nueva
del worker ni backfill destructivo. Rollback independiente mediante
`2026-09-16_music_playback_identity_rollback.sql`, conservando datos e ingesta
pausada porque restaura el defecto. Ver [evidencia actual y límites](playback-identity.md).

Atomicidad DDEX (2026-09-16): API confirma export y job juntos, serializa por
actor/clave y versión, y recupera huérfanos queued mediante replay. No hay nueva
migración ni imagen worker: requiere el esquema hasta music_ddex_operations y
la imagen v5 existente. Pausar solicitudes durante el reemplazo de todas las
APIs; no mezclar escritores antiguos. Rollback de código sin borrar datos y
manteniendo solicitudes apagadas si vuelve la ventana de dos transacciones.
Ver [evidencia y límites del corte](ddex-atomic-enqueue.md).
Verificación de este corte: build API, Node **17/17**, formal general sin
errores/críticos (351 warnings), imagen **8/8** e integración **18/18**, código 0.
La API pasó fallos inyectados antes/después del job, cuatro replays con barrera
de locks observada, recuperación queued y regresión de los tres paquetes reales.
Sin SQL nuevo, despliegue, cambios de renderer/worker ni verificación de UI manual.

Operaciones DDEX (2026-09-16): adaptador v5, identidad TrackRelease estable y
validación común en API/renderer/worker. Aplicar
`2026-09-16_music_ddex_operations.sql` después de resource_graph_validation,
sin backfill. Pausar DDEX y desplegar los tres componentes coherentes; rollback
de esta función antes de revertir las migraciones anteriores, manteniendo
generación apagada. Ver [evidencia y límites](ddex-operation-lifecycle.md).
Resultado final: Haskell **56/56**, builder **16/16**, Node **17/17**, worker
**22/22**, imagen **8/8**, integración API/HTTPS/S3 **18/18**, migración con
rollback/reaplicación y cinco XML contra XSD, todos código 0. La integración
comprueba tres ZIP reales de alta/update/retiro, identidad estable y recuperación
exacta. La primera corrida falló por un campo incorrecto del fixture, corregido
sin relajar la API; ver ADR-017. Sin despliegue, proveedor remoto ni UI manual.

Nombres DDEX (2026-09-16): adaptador v4 y manifiesto v3 con `messageFile`.
XML y recursos usan el identificador proporcionado y referencias técnicas;
el builder rechaza discrepancias. Sin SQL nuevo; recompilar API/renderer y
worker completo, con DDEX pausado durante sustitución. Paquetes históricos
intactos; no mezclar renderer/builder de versiones distintas.
Resultado: Haskell **55/55**, builder **16/16**, Node **17/17**, worker dirigido
**2/2**, imagen **8/8**, integración API/HTTPS/S3 **18/18** y cinco XML contra
XSD oficial; código 0. Nombres, descarga privada y recuperación verificados
con renderer/builder reales. Sin despliegue ni entrega remota.
Ver [evidencia y límites de entrega](ddex-file-naming.md).

Continuación del paquete offline (2026-09-16): adaptador v3 y manifiesto v2;
deal gratuito bajo demanda con fin de vigencia, XSD fijados en runtime,
recursos referenciados exclusivamente y ZIP reproducible. No añade SQL;
recompilar API/renderer e imagen worker sobre las migraciones ya indicadas.
Mantener DDEX pausado durante el cambio, sin mezclar workers anteriores.
Un rollback no debe reabrir generación con el mapeo erróneo de suscripción;
conservar exports históricos y permitir su descarga sin regenerarlos.
Resultado final: Haskell **53/53**, empaquetador **11/11**, Node **17/17**,
worker dirigido **3/3**, Linux **8/8** y API/HTTPS/S3 **18/18**, código 0;
cinco fixtures XML y XML del ZIP real validan contra XSD oficial local.
Recuperación y descarga idéntica verificadas; sin despliegue ni activación.
Ver [evidencia actual y límites de coreografía](ddex-offline-bundle.md).

Continuación del worker DDEX (2026-09-16 UTC): la imagen nueva revalida jobs
encolados antes de generar y antes de confirmar `valid`, incluida vigencia de
remitente/destinatario y hash del snapshot. No añade migración; requiere las
ya indicadas abajo. Pausar reclamación durante el cambio y no mezclar workers
anteriores. Rollback a una imagen anterior exige mantener DDEX pausado.
Resultados finales: worker **22/22**, Node dirigido **16/16**, Linux **8/8**
y API/HTTPS/S3 **18/18**, todos código 0. XML real valida XSD localmente;
el ensayo de confirmación/revocación usa dobles DDEX explícitos, no certifica
paquete ERN completo. Sin despliegue ni activación de banderas.
Ver [evidencia y límites](ddex-queued-validation.md).

Validación editorial del grafo (2026-09-16 UTC): añadir
`2026-09-16_music_resource_graph_validation.sql` después de concurrency.
Diez clases de referencias inválidas bloquean revisión/aprobación/programación
y nuevas exportaciones; diagnóstico legado sin reescribir snapshots.
Suite SQL y rollback/reaplicación correctos, Haskell **51/51**, integración
**8/8 Linux + 18/18 API/HTTPS/S3**, typecheck web y tipos móviles correctos.
No suspende automáticamente contenido legado. La revalidación de jobs DDEX
encolados requiere además la imagen posterior indicada arriba. Sin despliegue; ver
[evidencia, orden y límites](resource-graph-validation.md).

Correcciones concurrentes (2026-09-16 UTC): añadir
`2026-09-16_music_correction_concurrency.sql` después de asset_graph y antes
de reabrir autoría; rollback en orden inverso. API con claves vinculadas a la
fuente y errores de grafo accionables. Regresión PostgreSQL/rollback/
reaplicación aprobados, Haskell 51/51 y compilaciones API/renderer/imagen
correctas. Integración final **8/8 Linux + 18/18 API/HTTPS/S3**, incluido
reintento concurrente, 409 por otra fuente y 422 con rollback completo.
Typecheck web completo y de tipos móviles correcto; XML local válido.
Sin despliegue; ver [evidencia y límites](correction-concurrency.md).

Continuación de correcciones (2026-09-16 UTC): nueva migración
`2026-09-16_music_correction_asset_graph.sql`, reversible y sin backfill,
copia recursos multinivel por profundidad. La suite de migración pasó
reproducción del fallo original, cinco generaciones, rechazo atómico de
grafos inválidos y rollback/reaplicación. La integración terminó con
**8/8 Linux y 18/18 API/HTTPS/S3**, incluida tercera versión por HTTP y
reintento idempotente; el XML resultante valida XSD localmente. No hubo
despliegue. Ver [evidencia y límites](chained-corrections.md).

Continuación de créditos ERN: API y renderer consumen el snapshot aprobado v2,
con errores por campo para roles no soportados. Requiere recompilar ambos y
reconstruir la imagen worker; no añade migración ni activa exportación real.
Ver [evidencia actual, fallos y límites](ddex-versioned-credits.md).
Resultado final de ese corte: Haskell **46/46**, Node **3/3**, **5 XML fixtures
+ 1 XML API** válidos contra XSD oficial, **8/8 Linux** y **18/18 API/S3**.
No equivale a paquete DDEX completo. En ese corte se descubrió un fallo al
clonar padres multinivel y se aisló el fixture negativo DDEX en una corrección
directa del original. La reparación posterior se documenta arriba.

Integración posterior del worker Linux (2026-09-15 UTC): **8/8 escenarios de
imagen**, **18/18 API/S3** y **20/20 tests dirigidos**, todos código 0. Los jobs
de audio, portada y preview corrieron en la imagen real con PostgreSQL
desechable (`verify-full`) y S3 HTTPS; la API permanece en el host. Se aplicaron
las migraciones existentes solo al fixture, sin nueva migración ni despliegue.
Ver [evidencia, fallos previos y límites](linux-worker-integration.md).
La repetición de compatibilidad con el worker del host (`--with-api`) pasó
también **18/18, código 0**, sin omisiones.

Continuación Linux (2026-09-15 UTC): imagen completa construida y probada,
**8/8 escenarios de contenedor** y **13/13 tests dirigidos**, código 0.
Se aisló el manifiesto de compilación del renderer sin bifurcar sus fuentes,
se fijaron bases/snapshot y se verificaron checksums, procesamiento real y TERM.
ID local `sha256:401d6874b59bd8337d70a187e09a6429e5dcfc7c5ea1a7cd3d3204a381d7a5cc`.
Ver [evidencia, fuentes y límites de la imagen Linux](linux-worker-image.md).
No hubo migración ni despliegue en esa entrega de construcción. La integración
posterior indicada arriba cubre el worker conectado; quedan API Linux completa
y las puertas de proveedor/CDN/pagos/operación.

Última continuación local (2026-09-15 UTC): **worker 21/21**, **API/S3 18/18** y
**unitarias dirigidas 10/10**, todos código 0 con el worker final. Incluyen
promoción multipart real del máster, publicación/descarga/retiro y TERM corregido.
Se cerró la regresión editorial sin ampliar límites. Los fallos anteriores se
conservan abajo como históricos; ver [diagnóstico y hashes finales](worker-regression-diagnostics.md).
No hubo despliegue, migración nueva, commit ni PR en esta continuación.

## Secuencia de despliegue

1. Crear backup y restaurarlo en staging; pausar autoría/revisión/programación y reclamación de jobs DDEX. Aplicar, en orden, `2026-09-11_music_release_platform.sql`, `2026-09-15_music_preview_ranges.sql`, `2026-09-15_music_version_parties.sql`, `2026-09-15_music_party_details.sql`, `2026-09-16_music_correction_asset_graph.sql`, `2026-09-16_music_correction_concurrency.sql`, `2026-09-16_music_resource_graph_validation.sql` y `2026-09-16_music_ddex_operations.sql`. Coordinar esquema con API/worker; revisar previews, diagnóstico de grafos legados y exportaciones encoladas antes de reanudar. El backfill de pertenencias no deduce vínculos de partes huérfanas; el de datos de partes marca observaciones legadas, no modifica snapshots aprobados. Ver [procedimiento de partes](versioned-party-details.md), [validación y saneamiento](resource-graph-validation.md) y [compatibilidad de exportaciones](ddex-operation-lifecycle.md).
2. Ejecutar `DATABASE_URL=... ./scripts/scan-music-legacy-releases.sh`; si se interrumpe, reanudar con el último `TDF_MUSIC_LEGACY_CURSOR` impreso. Revisar la cola antes de exponer el catálogo nuevo.
3. Después de que este cambio tenga un commit real, añadir la migración a `scripts/production-migrations.json` con ese SHA de introducción. No se añadió un SHA ficticio.
4. Desplegar API/UI con todas las banderas apagadas; desplegar worker sin jobs.
5. Verificar S3 multipart, promoción de original, ranges/CORS/TTL, pagos sandbox y retiro con datos sintéticos.
6. Activar en orden para cohorte interna: `authoring` → `processing` → `public` → `commerce`. DDEX permanece apagado hasta DPID/licencia/XSD/contrato receptor.
7. Observar al menos una ventana operacional; ampliar cohorte. Mantener rutas/player legado.

## Rollback

- Código: apagar banderas primero y volver a la imagen anterior; nuevas tablas son aditivas.
- Jobs: detener supervisor y esperar que cierre su iteración; no borrar cola. Si hubo terminación forzada, esperar el TTL configurado (15 min por defecto) y el límite de transferencias en vuelo antes de reconciliar objetos. No mezclar workers antiguos sin control de intento con los nuevos. Volver a procesar solo con la versión compatible del pipeline.
- Publicación: para error de contenido, suspender/retirar mediante estado, nunca mutar snapshot.
- Datos: `2026-09-11_music_release_platform_rollback.sql` solo opera si no hay datos; con datos, conservar schema y revertir código. El test prueba tanto el bloqueo poblado como el rollback vacío.
- Pertenencias de colaboradores: conservar tabla al volver código y mantener autoría apagada; `2026-09-15_music_version_parties_rollback.sql` rechaza eliminar vínculos sin crédito/split que no se podrían reconstruir.
- Datos versionados de partes: conservar columnas al volver código; `2026-09-15_music_party_details_rollback.sql` rechaza eliminar datos aportados, observaciones no reconstruibles o aprobaciones v2. No mezclar escritores antiguos/nuevos; revisar [el orden inverso de recuperación](versioned-party-details.md).
- Operaciones DDEX: con generación pausada y componentes compatibles restaurados, ejecutar `2026-09-16_music_ddex_operations_rollback.sql` antes de resource_graph_validation. Conserva paquetes; no habilitar generación con identidades inestables.
- Grafo editorial: después de operaciones DDEX, revertir `2026-09-16_music_resource_graph_validation_rollback.sql`, después concurrency y asset_graph si corresponde. Restaura funciones sin borrar datos, pero retira la barrera temprana; mantener autoría/revisión pausadas. Revisar [saneamiento legado y jobs pendientes](resource-graph-validation.md).
- Correcciones concurrentes: `2026-09-16_music_correction_concurrency_rollback.sql` restaura el asignador previo y reintroduce la carrera; mantener correcciones apagadas hasta reaplicar.
- Correcciones multinivel: con autoría pausada, `2026-09-16_music_correction_asset_graph_rollback.sql` restaura la función previa sin borrar datos. Reintroduce el fallo anterior; mantener correcciones apagadas hasta reaplicar la reparación.
- Storage: no borrar másteres al volver código; conservar manifest/hash. Lifecycle de cuarentena puede seguir.

## Evidencia ejecutada hasta el 2026-09-14

| Comando | Resultado |
|---|---|
| `stack exec -- runhaskell -isrc -itest test/MusicReleaseSpecMain.hs` | 34 ejemplos, 0 fallos; incluye contratos JSON de partes, multipart y eventos |
| `stack test --fast` | correcto: 2.577 ejemplos, 0 fallos; recompiló y enlazó el runner, `tdf-hq-exe` y `tdf-ddex-render` después de reconciliar módulos/bounds, e incluye la mutación idempotente de reacciones que desbloqueó el target global |
| `TDF_MUSIC_API_E2E_BACKEND_EXE=... TDF_MUSIC_API_E2E_PASSWORD=... ./scripts/test-music-release-api-e2e.sh` | correcto contra handlers HTTP y PostgreSQL desechable reales: permisos, multipart, editorial, scheduler, publicación/activos/biblioteca por territorio, player, compra/entitlement/descarga/reembolso, corrección y retiro |
| `npm run build` (pasada anterior al endurecimiento de confirmación multipart) | correcto: TypeScript, Vite y presupuesto; 5 preloads y 376 903 bytes gzip iniciales de 419 840 permitidos. No certifica los cambios posteriores de confirmación multipart |
| Jest dirigido a player, acciones de release, Studio, feed, notificaciones y SHA incremental | 7 suites, 30 tests, 0 fallos |
| `PLAYWRIGHT_PORT=4187 PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-music-playwright-final-pass npx playwright test e2e/web/music-player.spec.mjs` | 5/5: Chromium desktop, Pixel 7, tableta Chromium, Firefox y WebKit; una instancia de audio conserva identidad/cola/estado al navegar, controles táctiles, teclado, calidad, aleatorio/repetición y axe sin violaciones serias/críticas en el player |
| ESLint dirigido a App, API, player, Studio, páginas públicas/feed y utilidades musicales (`--max-warnings=0`) | correcto |
| `stack build tdf-hq:exe:tdf-hq-exe --fast` en este checkout | correcto: 196/196 módulos, enlace e instalación local del ejecutable; se corrigió el `Just` faltante de `emrPartyId` en SocialEvents y se recompilaron sin advertencias propias los módulos musicales modificados |
| `stack build tdf-hq:exe:tdf-ddex-render --fast` | correcto: 4/4, enlace e instalación local; sin advertencias propias después de normalizar nombres locales |
| scripts sintéticos de audio y artwork | correctos; el host emitió advertencias de locale y ambos scripts terminaron con código 0 |
| `node scripts/test-music-release-worker-runtime.mjs` | Pasada final: 20/20 escenarios, código 0. PostgreSQL con migraciones reales, audio/arte FFmpeg, GET/PUT fallidos, checksums, cuarentena, promoción/reproceso sin duplicados, rollback, dead letter, revisión única, recuperación/aislamiento DDEX y los siete casos concurrentes/de apagado; transporte sustituido explícitamente para inyección de fallos |
| `node scripts/test-music-release-worker-runtime.mjs --concurrency-only` | 7 escenarios, código 0, repetidos con TTL de 30 s durante 65 s: cierre/reintento de trabajos hermanos, renovación durante más de dos TTL, intento vencido con nombre de worker reutilizado, PUT en vuelo sin promoción tras perder reserva, cancelación, TERM directo y TERM a través del supervisor; PostgreSQL/procesos reales, transporte sustituido |
| `node scripts/test-music-release-worker-runtime.mjs '--only=valid DDEX'` | 1 escenario, código 0: dos reintentos conservan exactamente fila/referencias/hash/bytes sin renderer ni PUT; un job de otra versión no modifica la exportación ni siquiera al fallar. Estado `valid` y artefactos sembrados explícitamente como sintéticos: no prueba XSD/DPID ni genera un paquete conforme |
| paquete DDEX con audio/arte sintéticos y XSD oficial local | XML `xmllint --nonet`, ZIP, recursos y manifiesto correctos: ERN 4.3.2, Audio 2.3.1, AVS 011, DD-ERN-432, Cloud Storage 1.8.1 |
| `sh -n` de 11 scripts, `node --check` del E2E y `git diff --check` | correcto |
| `./scripts/test-music-release-platform-migration.sh` sobre PostgreSQL 16 | correcto: schema/rollback, permisos, gates territoriales de arte/audio, denegación de máster, publicación/reemplazo/retiro, corrección clonada, playlists, infracciones, pagos, analítica y DDEX |
| `docker build --check -f tdf-hq/Dockerfile.music-worker .` | correcto; no equivale a construir/publicar la imagen |
| `bash -n scripts/run-music-release-worker-once.sh`, `node --check` del test de runtime y su fixture, `git diff --check` | correctos después de las correcciones del worker; el worker de una iteración requiere Bash |
| `node scripts/test-music-s3-local.mjs` | 10/10 escenarios, código 0: HTTPS con certificado validado, firmador Haskell de producción, multipart/reanudación/ETag/SHA-256, acceso privado, URLs alteradas/vencidas, rangos, CORS y aborto contra MinIO local real; sin mocks del transporte. Limpieza del contenedor, objetos y credenciales sintéticas completada |
| Jest dirigido a `musicStorageResponse`, `musicReleases.upload` y `MusicReleaseStudioPage` | 3 suites, 14 tests, 0 fallos, código 0; confirma que HTTP 200 con `<Error>` no confirma ni cancela una sesión reanudable, acepta XML válido y rechaza ETags ambiguos/inconsistentes. Emitió advertencia de handles abiertos al terminar; no se usó `--forceExit` |
| Jest de los dos tests nuevos con `--detectOpenHandles` | 2 suites, 12 tests, 0 fallos, código 0; cerró sin aviso de handles abiertos. No demuestra la causa del aviso de la corrida que incluía Studio |
| ESLint de `musicReleases.ts`, `musicStorageResponse.ts` y sus dos tests nuevos (`--max-warnings=0`) | correcto, código 0 |
| `npm run build --workspace=tdf-hq-ui` después del endurecimiento multipart | **No completado**: se detuvo exclusivamente su proceso TypeScript con SIGTERM tras más de 12 minutos; salida 143. La máquina reportó carga 304; el proceso seguía activo y no había emitido error de tipos. Vite y el presupuesto no llegaron a ejecutarse. Repetir en un host con capacidad disponible; no contar como verde |
| Repetición de `npm run build --workspace=tdf-hq-ui` en la siguiente continuación | **Correcto**, código 0: TypeScript, 12.456 módulos transformados por Vite y presupuesto de 5 preloads / 376.939 bytes gzip de JS inicial. Conserva advertencia de chunks mayores de 500 kB. Esta pasada sí incluye la confirmación multipart endurecida |

La evidencia anterior es local. El E2E de API no intercepta la API, pero inserta activos técnicos sintéticos en la base después de probar el estado multipart; no está conectado al bucket local del nuevo test S3. Por tanto ese E2E no prueba transferencia de bytes, CORS/ranges ni callbacks reales de proveedor. La nueva suite S3 prueba estas operaciones de almacenamiento por HTTPS de forma independiente, sin arrancar API/worker/navegador. El E2E Playwright es complementario: intercepta únicamente su frontera HTTP con fixtures inequívocamente sintéticos y reemplaza el motor multimedia del navegador para probar de forma determinista el shell/UI; no se presenta como validación de storage, streaming real ni de los handlers, cubiertos por las pruebas separadas.

La preparación S3 tuvo fallos previos no contados como verdes: Docker Hub rechazó la imagen, el puente de prueba inicialmente leía Unicode como Latin-1, y una corrida se interrumpió por `ECONNRESET` tras cinco escenarios al reutilizar conexiones. Se obtuvo la imagen de Quay fijada por digest, se corrigió la lectura UTF-8 y se aislaron las conexiones HTTPS. La corrida final pasó los diez escenarios sin reintentos de red que oculten fallos. [Reproducción y límites](local-s3-integration.md).

Se reforzó también `uploadMusicAsset`: la confirmación requiere un `CompleteMultipartUploadResult` XML válido con un único ETag directo y coherente con la cabecera, cuando exista. Un HTTP 200 con `<Error>` se rechaza incluso si tiene ETag en cabecera, sin llamar al endpoint de confirmación. El mensaje muestra únicamente un código de error seguro, nunca los detalles arbitrarios del proveedor. Las pruebas de este gate usan respuestas sintéticas y no deben confundirse con la suite de transferencias reales. La suite musical Haskell se repitió: 34 ejemplos, 0 fallos.

SHA-256 del código mantenido sin cambios durante esa pasada final:

```text
a53bea235f6d86c75ec4552f125e6dd51275ec36df9830d668e89b7d6a3e5574  scripts/test-music-s3-local.mjs
55e8f6c3268ba11ee998becd9c799c807fc3120183f09231def2af138a104175  tdf-hq/test/MusicS3ProbeMain.hs
```

### Inventario remoto de almacenamiento: 2026-09-14

Consultas de solo lectura, sin leer/subir objetos, crear credenciales ni modificar recursos:

- `flyctl storage list --org personal` encontró **Tigris**, bucket `broken-thunder-8987`. `flyctl storage status broken-thunder-8987` devolvió `Status: created`, `Public: False`. Su pertenencia al inventario de la organización no demuestra que esté reservado para TDF/staging; no se encontraron referencias a ese nombre en la configuración de TDF revisada.
- Acceso administrativo de Fly: sesión existente en `/Users/diegosaa/.fly/config.yml`, campo `access_token`. La invocación directa inicial indicó que no había token; cargar esa credencial existente en `FLY_API_TOKEN` únicamente para el proceso de consulta permitió verificar el inventario. No se imprimió ni copió su valor. GitHub también tiene un secreto de repositorio denominado `FLY_API_TOKEN`; no se intentó recuperar su valor.
- `tdf-hq-studio-audit-staging`: máquina activa en `gru`, volumen cifrado `tdf_staging_clean_20260908` montado en `/data`; también existe el volumen `tdf_studio_audit_staging_data`. La configuración remota tiene `HQ_ASSETS_DIR=/app/assets` y `TDF_INTERNAL_FEEDBACK_UPLOAD_ROOT=/data/audit-evidence`, coherente con `fly.studio-audit-staging.toml`. La lista de secretos contiene únicamente `DATABASE_URL`; no hay credenciales S3 ni variables de buckets en la configuración consultada.
- `tdf-hq`: volúmenes `tdf_assets` montados en `/data/assets`, máquinas activas en `ord` y `lax` (no confundir con `primary_region=gru` del TOML local). Hay secretos `DRIVE_CLIENT_ID`, `DRIVE_CLIENT_SECRET`, `DRIVE_REFRESH_TOKEN` y `DRIVE_UPLOAD_FOLDER_ID` para la integración Drive existente; no hay secretos S3/R2/AWS ni variables de buckets en la configuración consultada.
- No se encontraron credenciales de almacenamiento musical en los archivos locales `.env` revisados, el entorno del proceso, los nombres de secretos del repositorio GitHub ni los entornos `Preview`, `Production` y `production-the-dream-factory/tdf`. Los cuatro nombres `tdf-music-*` del archivo de ejemplo no son recursos verificados.

**Pendiente:** identificar el uso del bucket privado existente y disponer de credenciales S3 limitadas al ámbito de pruebas. Acceso administrativo de Fly no equivale a acceso S3 del worker. No se comprobó una configuración CDN para audio ni se ejecutaron transferencias, pruebas de CORS/ranges/retención o despliegues. No reutilizar este bucket para fixtures hasta confirmar su destino y aislamiento.

La ejecución del worker contra el esquema real reveló defectos que los tests
aislados de FFmpeg y el E2E con assets insertados no cubrían: parámetros `psql`
sin sustituir dentro de `-c`, errores ignorados por `set -e` dentro de `if`, una
escritura a `music_recording.technical_metadata` (columna inexistente) y la
transición prohibida `processing → draft`. Quedaron corregidos en el worker:
SQL por stdin con parámetros escapados, Bash con `pipefail` y terminación al
primer fallo, manifiesto técnico en el asset y cierre editorial mediante la
validación completa. La imagen incorpora Bash y Perl para `shasum`. Estas
correcciones no requieren modificar el esquema ni relajar sus restricciones.
El ensayo del worker usa archivos reales y coteja sus SHA-256 con `music_asset`,
pero reemplaza `curl` por un transporte de archivos con fallos controlados. No
constituye evidencia de autenticación S3, multipart reanudable, CORS, HTTP ranges
ni reproducción de esos archivos en navegador.

La ampliación de concurrencia encontró un deadlock entre trabajos del mismo
release; se corrigió bloqueando primero la versión con `NO KEY UPDATE` y luego
el job. La ejecución dirigida de siete casos pasó después de esa corrección.
Ejecuciones posteriores con TTL de prueba de 6 s fallaron por timeout de inicio
y por ausencia de renovación observada, bajo carga local superior a 200. No se
cuentan esas ejecuciones como aprobadas. El arranque ahora extrae todos los
campos del job con una sola lectura JSON, y el ensayo usa TTL de 30 s durante
65 s; el TTL operativo predeterminado sigue siendo 900 s. La siguiente ejecución
integral pasó los 13 casos de pipeline/DDEX y se detuvo por timeout SQL de 5 s
entre trabajos hermanos, no por deadlock. La prueba ahora exige registrar y
recuperar ese fallo reintentable, conserva el rechazo explícito de deadlocks y
comprueba un único cierre editorial. Los siete casos dirigidos volvieron a pasar
con código 0. Finalmente se ejecutó de nuevo toda la suite con el código congelado:
20/20 escenarios, código 0 y limpieza de la base/directorio temporales completada.

Código del worker y harness mantenido sin cambios durante la pasada final
(SHA-256, 2026-09-14; no sustituye un commit):

```text
ab18d0921fb5b793fceee5a973dc31cda6e64c572ff7abcf307a50e95883ea2a  scripts/run-music-release-worker-once.sh
521e52bb3b059c86c17765100957bacbfcf66d1575978b710bc6dc22e916cc5b  scripts/run-music-release-worker.sh
25b9ef034fb7f0465f8e325491885d93ba13f6b6898572afc7f8366fa0d05583  scripts/test-music-release-worker-runtime.mjs
1b816316017ca5081325de224176d3f48deb3d60d6ae46db230f052df8c3bb46  test/fixtures/music-worker/curl.mjs
```

## No verificado / puertas de producción

La continuación de [persistencia de Studio](studio-persistence.md) impide
aceptar términos, enviar o cargar tras un guardado fallido, serializa las
operaciones del formulario y cancela el debounce al guardar manualmente.
La suite dirigida final pasó 11/11 y el build frontend pasó sobre el último
ajuste. Añade un caso de navegador para crear un single privado, recuperar
un guardado rechazado por derechos incompletos y comprobar persistencia tras
recargar; repetición final 15/15 navegador, 18/18 API/S3 y 8/8 Linux.
La revisión de red dejó pendientes 403 de la radio del shell en catálogos
administrativos de géneros/países; Studio usa la ruta pública de géneros.
Este corte de UI no añadió migraciones ni despliegue. La continuación de
[pertenencia de colaboradores](version-party-membership.md) añade una migración
para conservar partes sin créditos/splits; pasó build API, migración/rollback,
35/35 Haskell y repetición 15/15 navegador + 18/18 API/S3 + 8/8 Linux.
Su primera matriz 12/15 también está conservada. La continuación de
[datos versionados de partes](versioned-party-details.md) implementa nombres,
identificadores y aprobación v2 con bitácora; sus resultados son independientes.
Pasaron build API/UI, migración/rollback ampliados, Haskell 35/35, Jest 9/9 y
matriz final 15/15 navegador + 18/18 API/S3 + 8/8 Linux, sin skips/reintentos.
El fallo inicial de consulta de auditoría del test se conserva en el informe.
Carga/revisión completa por UI y representación DDEX completa de colaboradores
siguen pendientes; no equivale a aceptación completa del flujo editorial.

La integración de navegador del 2026-09-15 cerró la brecha de API real local:
**10/10** casos Playwright conectados a API/PostgreSQL/MinIO HTTPS, **18/18**
escenarios API/S3 y **8/8** comprobaciones de imagen Linux, código 0. Pasaron
también 53/53 tests Jest, 3/3 comprobaciones CORS y los builds de API/frontend.
Se corrigieron calidad FLAC, prioridad de audio completo, retorno del login,
CORS idempotente, radio duplicada y estado/caché de biblioteca por cuenta.
Sin migraciones nuevas ni despliegue; recursos de prueba limpiados. Véase
[evidencia, fallos previos y límites](real-browser-integration.md). Esta prueba
usa almacenamiento local y pagos canónicos sintéticos, no CDN ni proveedores
de pago externos; la confianza TLS del navegador usa una excepción local
para el certificado autofirmado. Creación/carga/revisión por UI y pruebas
manuales en dispositivos físicos siguen pendientes.

La continuación de controles compactos expone calidad/repetición/aleatorio/
volumen en teléfono y tablet usando el mismo motor, con foco modal y objetivos
táctiles. Corrige también atajos que interferían con controles y estado de
carga en pausa. Matriz local 25/25 (20 nativos + 5 con motor sustituido); tras
el último ajuste de listboxes pasaron 10/10 casos dirigidos en los cinco
proyectos y 25/25 tests unitarios. No se cuentan los otros 15 casos como
repetidos después de ese ajuste. Véase [evidencia por versión y límites](compact-player-controls.md).
La limitación previa de controles compactos ocultos queda atendida por este
panel; la API real local en navegador se verificó en la entrega descrita arriba.
Continúan pendientes dispositivos físicos y CDN.
El build final (TypeScript/Vite/presupuesto) pasó con código 0, 376939 bytes
gzip iniciales y 5 preloads; permanece la advertencia de chunks grandes.
No requiere migración ni se desplegó esta entrega.

La entrega de reproducción nativa del 2026-09-15 añadió pruebas con AAC/FLAC
sintéticos producidos por el pipeline real y corrigió repetición, respuestas
tardías de `play()` y posición/progreso atribuidos a otra pista durante su
autorización. Pasaron 12/12 tests Jest y 20/20 casos Playwright (15 nativos,
5 con motor sustituido), sin skips ni reintentos en la pasada final. También
pasó el build frontend final (TypeScript/Vite/presupuesto), código 0; permanece
el warning de chunks grandes, sin modificar el presupuesto. No requiere
migración. Se preservan el player legado y los cambios concurrentes. Véase
[evidencia, fallos previos y límites](native-player-verification.md); no acredita
dispositivos físicos, controles móviles ocultos, API real en navegador ni CDN.

La entrega de previews configurables añadió la migración
`2026-09-15_music_preview_ranges.sql`. Pasaron el E2E HTTP/S3 ampliado (11/11),
audio sintético (2/2), contrato/reprocesamiento/rollback PostgreSQL dirigido,
Jest (5/5), Stack, build frontend, TypeScript repetido, ESLint y Dockerfile check.
Dos corridas completas del worker fallaron en concurrencia durante carga alta;
la repetición aislada pasó 7/7 sin ampliar TTL ni timeouts. No existe una pasada
única verde de los 21 escenarios nuevos. Detalle y hashes en
[previews configurables](preview-ranges.md).

La nueva integración API/S3 detectó un 404 para másteres comprados: los originales
del worker tienen estado `valid`, no `ready`. Se ajustaron los dos handlers de
descarga privada para aceptar únicamente originales validados, inmutables y
fuera de cuarentena, con versión coincidente. El predicado compartido pasó
14 casos contra PostgreSQL de solo lectura; el lector XML del test pasó 3 casos.
No requieren migración. Las dos primeras pasadas integradas fallaron y no se
cuentan como verdes. La repetición final con el backend recompilado pasó
**11/11 escenarios, código 0**, incluidos los diez casos S3 y el flujo completo
API/PostgreSQL/FFmpeg, descarga intacta, aislamiento del comprador, reembolso,
corrección y retiro. Se comprobó la limpieza de contenedor/base temporales.
Véase [el detalle, límites y hashes](local-s3-integration.md).

- La compilación integral pendiente tras la interrupción fue cerrada por la repetición exitosa registrada arriba; continúa la advertencia de chunks grandes, sin aumentar el presupuesto.
- Dispositivos físicos, lector de pantalla humano, reproducción larga con red degradada y streams/CDN reales. Playwright sí cubre los cinco proyectos headless configurados, emulación táctil, navegación SPA, teclado y axe.
- Buckets/CDN del proveedor remoto, sus CORS/ranges, KMS, versioning, backup/restore y latencia Ecuador/LATAM. S3 local por HTTPS pasó tanto las pruebas independientes como el flujo integrado API/worker descrito arriba; no extrapolarlos a Tigris ni a una CDN.
- Datafast/PayPal sandbox o reales, callbacks/webhooks externos, reembolso/conciliación contra proveedor y reglas tributarias/contables. El E2E sí prueba que evidencia canónica `paid/refunded` crea y revoca exactamente un entitlement.
- DPID autorizado, licencia DDEX, perfil contractual de un DSP o entrega directa.
- Carga y concurrencia a escala; antifraude avanzado; retención/consentimiento aprobados legalmente.
- Worker: renovación y control de propietario/intento probados localmente con TTL de 30 s durante 65 s y recuperación de una reserva envejecida 16 min. Falta un soak test con TTL de producción, carga sostenida, imagen Linux completa y S3 real; `build --check` no construye la imagen ni verifica `tini` en ejecución.
- Retención de storage: no asumir S3 versioning/Object Lock en R2. Sus bucket locks propios y los PUT reintentados contra originales retenidos no han sido probados; resolver esa compatibilidad y demostrar restauración antes de producción.
- Objetos grandes: el worker ahora selecciona multipart para promoción, derivados y exportaciones desde 64 MiB, con checksum por parte y aborto. Ver [contrato y evidencia](worker-multipart.md). Sigue pendiente demostrar archivos de varios GiB, retención/lifecycle del proveedor y memoria/disco del pipeline completo; los fixtures pequeños no certifican esos límites.
- Previews: el rango canónico ya está conectado a Studio, `audio-v2`, jobs `create_preview` y filtros de catálogo/acceso. La repetición HTTP/S3 pasó 11/11 con corrección de rango, duración real de 1,75 s y rechazo del preview anterior. Los previews legados sin evidencia requieren revisión/corrección; no se reescriben publicados. Falta aceptación manual de los nuevos controles y verificación CDN. Ver [migración y pruebas](preview-ranges.md).
- Deploy/rollback real. No se desplegó ni se creó commit/PR. Playwright produjo artefactos automáticos temporales durante la depuración; no se creó ni añadió al repositorio una captura de aceptación manual.

Los bloqueos Haskell encontrados durante la verificación quedaron corregidos de forma acotada. En `TDF.Server.SocialEventsHandlers.hs`, el campo persistido `Text` ahora se expone como `Just (...)` en el DTO `Maybe Text`; además, la mutación de reacciones se extrajo a una transacción reutilizable que respeta `emrrActive` y crea evidencia `reaction_added` solo al insertar una reacción nueva. Al declarar correctamente los módulos del test, una recompilación limpia reveló además que `ServerSpec` no desestructuraba el endpoint final de `SessionAPI`; la prueba ahora selecciona explícitamente `reconcileProgress` y deja separado el canje de invitación. La configuración de prueba inicializa también el feature flag público en `False`. Finalmente, el cálculo del siguiente sábado dejó de depender de `head` y usa una selección total con fallback; la suite incluye la prueba indirecta del calendario. La suite global validó estos cambios.

`tdf-hq-test` enumera ahora los módulos locales que realmente compila y el bound `http2 >=5.0 && <5.4` incluye la versión 5.3.10 del resolver; las advertencias anteriores de módulos omitidos y dependencia fuera de rango ya no aparecen. Stack todavía advierte que el `.cabal` fue editado manualmente porque `package.yaml` está incompleto y el Cabal es hoy el manifiesto efectivo. No se regeneró ni eliminó ninguno a ciegas sobre cambios ajenos. El build global también expone warnings históricos en código no musical (principalmente nombres sombreados e imports/bindings sin usar); no se atribuyen ni se ocultaron como parte de esta entrega.
