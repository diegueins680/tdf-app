# Previews configurables

Implementación: 2026-09-14 (migración identificada con fecha UTC 2026-09-15).

## Contrato y procesamiento

Studio permite guardar `previewStartMs` y `previewDurationMs` por pista, en milisegundos enteros. Ambos vacíos mantienen selección automática: inicio 30 s si la grabación supera 90 s, inicio 0 en caso contrario, duración máxima 30 s limitada por la grabación. Una duración explícita sin inicio empieza en 0. Un inicio explícito exige duración positiva; el intervalo debe caber entero dentro de la duración inspeccionada. Un rango inválido no se recorta silenciosamente.

`audio-v2` conserva el máster intacto y genera el preview desde su derivado normalizado FLAC. El manifiesto registra tanto la petición como el rango efectivo. Reutilizar un directorio con otra fuente, versión de pipeline o petición de preview falla: se necesita un directorio nuevo. Las claves de objetos contienen versión de pipeline y SHA-256. El contenedor incluye el resolver compartido `music-preview-spec.jq` y el renderer `process-music-release-preview.sh`.

La cola del worker busca rangos sin preview coincidente únicamente en versiones editables y con FLAC normalizado disponible. Encola `create_preview` con un snapshot del rango, clave idempotente por fuente/rango y bitácora. No reprocesa máster, portada ni calidades completas. Un rango que cambia mientras el job corre produce un artefacto histórico, no un preview vigente; la siguiente iteración encola el rango actual. Los intentos, renovación de reserva, fencing, fallos y dead letters siguen el mecanismo existente.

`music_check_submission` bloquea revisión/aprobación cuando falta un preview vigente para un rango explícito o una política de escucha preview. La API filtra previews antiguos de release, favoritos, playlists e historial. Cada firma pública vuelve a comprobar rango, publicación, tiempo y territorio. Los archivos anteriores se conservan privados para auditoría; no se sobrescriben ni borran. Una URL ya firmada mantiene el TTL existente de hasta cinco minutos: no constituye revocación instantánea en CDN.

## Migración y rollback

Aplicar `tdf-hq/sql/2026-09-15_music_preview_ranges.sql` después de la migración musical base y antes de desplegar esta API/worker. Es aditiva: añade funciones y envuelve las dos reglas existentes; no reescribe filas ni archivos. Reaplicarla conserva las funciones base y no genera wrappers recursivos. No se ha incorporado al manifiesto productivo con un SHA ficticio ni aplicado en producción.

Mantener banderas apagadas durante el cambio y coordinar API/worker. Detener workers antes de rollback; `2026-09-15_music_preview_ranges_rollback.sql` rechaza jobs `create_preview` pendientes, en ejecución o reintentables. Drenarlos o cancelarlos mediante operación auditada, conservar sus filas y restaurar código compatible antes de reabrir acceso. El rollback conserva todos los activos y eventos. Al quitar el filtro de previews se recupera la semántica anterior: no reabrir catálogo con versiones afectadas sin revisión.

Contenido legado sin metadatos de rango no se declara conforme por inferencia: sus previews dejan de recibir nuevas firmas. Para publicados/aprobados, crear corrección; el worker nunca los migra automáticamente. En borradores, el FLAC normalizado permite regeneración; sin esa fuente, procesar el máster antes. Los jobs muertos no se reactivan automáticamente al guardar el mismo rango: resolver la causa y reintentar con el procedimiento operativo.

Inventariar los afectados antes de activar publicación (consulta administrativa de solo lectura, sin títulos ni claves de storage):

```sql
SELECT version.release_id, version.id AS version_id, version.state,
       asset.id AS preview_asset_id, asset.recording_id
FROM music_asset asset
JOIN music_release_version version ON version.id = asset.release_version_id
WHERE asset.asset_role = 'preview_audio'
  AND NOT music_preview_matches(asset.id)
ORDER BY version.state, version.id, asset.id;
```

Revisar especialmente versiones ya programadas: el gate también puede impedir su publicación. Resolverlas antes de abrir el scheduler; no desactivar la validación para completar una fecha.

## Verificación

Comandos reproducibles:

```sh
node --test scripts/__tests__/music-preview-pipeline.test.mjs
sh scripts/test-music-release-audio-pipeline.sh
node scripts/test-music-release-worker-runtime.mjs '--only=preview ranges'
node scripts/test-music-release-worker-runtime.mjs
npm test --workspace=tdf-hq-ui -- --runInBand src/utils/musicPreviewRange.test.ts src/pages/MusicReleaseStudioPage.test.ts
npm run test:music-api-s3-local
```

La prueba de audio usa tonos sintéticos distintos antes/después del segundo 2 y mide qué frecuencia aparece en el preview; también verifica duración, SHA del máster, reintento y rechazo de reutilización con otro rango. La prueba PostgreSQL/worker verifica cambio de rango, artefactos históricos intactos, resultado obsoleto, errores accionables, encolado idempotente, reaplicación y rollback bloqueado/permitido. Su transporte está sustituido explícitamente para inyección de fallos. El modo HTTP/S3 utiliza transporte real local y comprueba una corrección de preview por API, nueva duración, rechazo del activo anterior y catálogo sin previews obsoletos; no sustituye una prueba de navegador ni CDN remota.

El primer intento del fixture de tonos falló por escapes incorrectos del filtro de prueba, antes de procesar audio; corregido el fixture, los dos tests pasaron. Resultados definitivos de las suites completas: consultar la guía de despliegue; no asumir éxito por disponer del comando.

Evidencia de esta entrega:

- Audio real: 2/2 tests; frecuencia seleccionada, duración y conservación del máster. El script de regresión de audio completo también terminó con código 0.
- PostgreSQL/worker dirigido: pasó reaplicación, límites, cambio de rango, resultado obsoleto, alternancia explícito/automático y rollback. El bloqueo de rollback con jobs pendientes fue esperado y comprobado.
- API + PostgreSQL + S3 local + FFmpeg: 11/11, código 0, con cambio editorial de 1.500/1.250 ms a 2.500/1.750 ms. El nuevo preview fue servido con SHA/rangos correctos y duración inspeccionada de 1,75 s; el anterior recibió 404 y no apareció entre las fuentes del release. Descarga del máster, reembolso sintético, corrección y retiro continuaron pasando. El runner completó su limpieza.
- Backend: Stack recompiló el módulo de catálogo/biblioteca, enlazó e instaló el ejecutable; código 0.
- Frontend: build con 12.457 módulos, 5 preloads y 376.939 bytes gzip iniciales; código 0, con la advertencia existente de chunks grandes. La repetición de TypeScript después del ajuste final del guardado progresivo terminó con código 0, y los cinco tests de Studio/rangos volvieron a pasar. Ese ajuste impide serializar números inválidos como `null` y conserva el nuevo `updatedAt` de metadatos si falla la transacción posterior de contenido.
- ESLint dirigido y Dockerfile `build --check`: código 0. Este último no construye ni ejecuta la imagen Linux.
- La primera suite completa del worker falló en exclusividad del heartbeat: el contender ejecutó un trabajo cuando debía permanecer idle. Coincidió con carga alta y otros tests activos, pero no se atribuye la causa sin evidencia. Se añadieron diagnósticos de tiempo/estado del primer worker, sin relajar la aserción ni el TTL. No se cuenta esa pasada como verde.
- La segunda suite completa volvió a pasar el bloque funcional, pero se detuvo en el timeout de 30 s esperando los jobs hermanos concurrentes. El host reportó carga 494 inmediatamente después. Tampoco se cuenta como verde ni se ampliaron los límites del worker/harness.
- La repetición `node scripts/test-music-release-worker-runtime.mjs --concurrency-only` pasó **7/7, código 0**, con el código final: jobs hermanos, heartbeat durante 65 s con TTL 30 s, intento vencido, transferencia obsoleta, cancelación, TERM directo y supervisor. No convierte las dos ejecuciones completas fallidas en una pasada única de 21/21. Se conserva esa limitación de evidencia; el verde histórico de 20 escenarios tampoco certifica automáticamente el nuevo encolado.

Sin cambios productivos, commit, PR, capturas manuales ni pagos externos. El nuevo SQL sí requiere despliegue coordinado; no es una entrega sin migraciones.

SHA-256 del núcleo probado (no sustituyen un commit):

```text
d5b33d6b6d605e2a7aaad3f9b89c7804136d1b9b71e1b418fb4bc4a50e63b505  scripts/process-music-release-audio.sh
51d1add10638e1c328f604602ceee22251f9b7e4dcd2bc9504cb7ac51da11260  scripts/process-music-release-preview.sh
35e956ddb29b1ce19ffcb42c92d2fb6138c0033d81a88f3559b74f2334457e3c  scripts/music-preview-spec.jq
ab7fedd6310b3489bdbd8a4f8ff3d5fcb4f419e3b14d2aa9c223d85cfb867f42  scripts/run-music-release-worker-once.sh
5623f8135a1966e348585b3a2341d8e0735f1b9436a30dd931f5d60b88ff4329  tdf-hq/sql/2026-09-15_music_preview_ranges.sql
9c7a864cf0c290b6c81c3d0a0f972f686f8219e1e7edb2778ce9706a2e0a4b61  tdf-hq/sql/2026-09-15_music_preview_ranges_rollback.sql
dc4f59c8f41576bc427291f32463915e2891a3bf7dc73f0288c927f1a4dfd99c  tdf-hq/src/TDF/Server/MusicRelease.hs
80026e6e9dced718705787053a1200b4480e228bad6dffffd6ad8ea1d9923120  scripts/test-music-release-api-e2e.mjs
4043ffbbc1027dd44eb0ab8900f0d953476b81191229dadaca25b1d5cd10607b  scripts/lib/music-api-s3-probe.mjs
44749a86a12c55e5709ee293ffb0817e61f6f2d70ad2cee4bc8e919aad62a020  scripts/test-music-release-worker-runtime.mjs
```

## Referencias técnicas

Documentación oficial consultada el 2026-09-14: [FFmpeg, opciones `-ss` y `-t`](https://ffmpeg.org/ffmpeg.html#Main-options) describe el seek preciso al transcodificar y el límite de salida. El renderer usa esas opciones sobre FLAC decodificado, no stream copy. [Filtros de audio y `atrim`](https://ffmpeg.org/ffmpeg-filters.html#atrim) distingue timestamps de conteo de muestras; las pruebas verifican el resultado decodificado y toleran únicamente el encuadre del AAC (40 ms), no un cambio de rango editorial.
