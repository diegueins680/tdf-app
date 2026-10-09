# Créditos versionados en ERN 4.3.2

Fecha: 2026-09-15 Ecuador (verificaciones finales 2026-09-16 UTC).
Este corte amplía el adaptador; no certifica todos los
campos del catálogo, una entrega a DSP ni la coreografía de transporte.

## Auditoría y ADR-010

El renderer anterior creaba `PArtist` desde `display_artist`, lo reutilizaba
para todas las grabaciones y añadía `Contributor/Artist` sin leer créditos.
Los snapshots v2 ya conservaban partes, pero el exportador no los consumía.

Decisión: API y renderer comparten `parseErn432Credits` y su validación pura.
Leen `parties`, `credits` y `recordings` del snapshot aprobado v2, nunca los
nombres actuales del directorio. El renderer comprueba además que el hash de
versión coincide con el hash solicitado por la exportación y que los recursos
abarcan las grabaciones del snapshot. No se recalcula aquí el hash completo
del snapshot: sigue dependiendo de las protecciones de inmutabilidad de la DB.
No se cambia el esquema HTTP ni se requieren migraciones nuevas.

| Dato canónico | Representación |
|---|---|
| `main_artist`, `featured_artist`, `display_artist` | `DisplayArtist` secuenciado, roles `MainArtist`, `FeaturedArtist`, `Artist` |
| principales/invitados | `Contributor/Role/Value=Artist` |
| compositor, letrista, intérprete | `Composer`, `Lyricist`, `Performer` |
| productor, ingeniero, mezcla, mastering | `StudioProducer`, `Engineer`, `MixingEngineer`, `MasteringEngineer` |
| editorial | `MusicPublisher`; no se deduce propiedad ni porcentaje |
| sello explícito | `ReleaseLabelReference`; no se convierte en intérprete |
| ISNI, IPI Name Number, DPID proporcionados | `PartyId/ISNI`, `IpiNameNumber`, `DPID`, con sintaxis revalidada |

Los roles de una misma parte se agrupan en un único `Contributor` por
grabación. Se eliminan repeticiones de roles y se conserva un orden
determinista (`display_order`, identidad, rol, ámbito); no se agrupan personas
por nombre. Las referencias `P_<identidad interna>` son locales al mensaje,
no códigos oficiales. Los créditos a nivel release se aplican a sus pistas,
coherentemente con la validación canónica de composición; los específicos de
una grabación no pasan a sus hermanas ni al display del release.

`DisplayArtistName` sigue siendo el texto editorial explícito, distinto de
la identidad acreditada. No se divide un nombre compuesto ni se inventa un
artista principal a partir del texto. Se exige principal explícito para
release y pistas; roles de display contradictorios para la misma persona
bloquean la exportación. Si falta crédito de sello se conserva el sello
declarado en `label_name`, sin asignarle identidad o identificador oficial.

No se exportan nombres legales, enlaces a cuentas, evidencia privada ni
contactos sin crédito. El rol `other` y los identificadores propietarios sin
namespace bloquean con error; requieren un mapeo explícito futuro, no borrar
información correcta para conseguir un XML verde. Los roles internos no se
inyectan en extensiones arbitrarias. Los ISNI/IPI de fixtures son exclusivamente
sintéticos: una validación de sintaxis no acredita asignación ni titularidad.
El parser rechaza además identificadores con estado `invalid` o desconocido;
revalidar la sintaxis no rehabilita evidencia negativa. `unvalidated` puede
pasar únicamente si el tipo está soportado y su sintaxis se verifica localmente;
no se transforma por ello en `authority_verified` ni se modifica el snapshot.

## Fuentes oficiales y compatibilidad

Consultadas el 2026-09-15, antes de implementar:

- [Índice oficial de estándares](https://kb.ddex.net/reference-material/standards-specifications/): ERN 4.3.2, perfiles 2.3.1 para 4.3.1+, cloud 1.8.1.
- [Diccionario ERN 4.3.2: Contributor](https://service.ddex.net/dd/DD-ERN-432/dd/ddexC_Contributor.html).
- [Diccionario: DetailedPartyId](https://service.ddex.net/dd/DD-ERN-432/dd/ddexC_DetailedPartyId.html).
- [Perfiles 7.2: artistas de display y secuencia](https://ern-rp.ddex.net/electronic-release-notification-message-suite-part-2-release-profiles/7-rules-common-to-all-release-profiles/7.2-for-releases-and-resources/).
- [Perfiles 7.4: agrupación de roles y créditos de composición](https://ern-rp.ddex.net/electronic-release-notification-message-suite-part-2-release-profiles/7-rules-common-to-all-release-profiles/7.4-for-resources/).

Se mantiene la [matriz fijada](ddex-compatibility-matrix-2026-09-12.md): Audio
2.3.1, AVS 011 y diccionario DD-ERN-432, sin Business Profile de ERN 3. Los
roles se comprobaron contra el AVS incluido en el ZIP fijado, no contra una
edición `CURRENT` descargada silenciosamente. Se reutilizó el XSD oficial
local existente; no se aceptaron licencias nuevas ni enviaron datos externos.

## Operación y rollback

Requiere las cuatro migraciones ya documentadas, incluida `music_party_details`.
Recompilar API y `tdf-ddex-render` juntos; reconstruir la imagen worker con
el manifiesto reducido que ahora también declara `aeson`. La prueba de
paridad de componentes protege ambos manifiestos Cabal, sin regenerarlos.
Los nuevos manifiestos y reportes identifican `tdf-ern432-audio-v2`, separado
de la versión del estándar. No se reescriben manifiestos previos.

Mantener exportación apagada hasta verificar DPID/licencia/contrato real. Los
fixtures habilitan esa bandera solo en su base local desechable. La API
rechaza créditos no representables con 422 y errores por campo antes de crear
exportación/job; el worker vuelve a validar independientemente. Las versiones
legadas necesitan una corrección revisada y aprobada; no se fabrican partes
para snapshots v1. Los paquetes ya válidos siguen su recuperación exacta.

Rollback seguro: pausar creación/consumo de exportaciones, conservar paquetes,
jobs y evidencia, corregir hacia delante. No regresar al renderer anterior
para procesar créditos múltiples: produciría pérdida silenciosa. No hay SQL
de reversión nuevo ni se han modificado flags o datos de producción.

## Pruebas reproducibles y límites

```sh
cd tdf-hq
stack build tdf-hq:exe:tdf-ddex-render tdf-hq:exe:tdf-hq-exe --fast
stack exec -- runhaskell -isrc -itest test/MusicReleaseSpecMain.hs
```

Desde raíz:

```sh
sh scripts/test-music-ddex-credits.sh /ruta/al/xsd-oficial
node --test scripts/__tests__/music-worker-build.test.mjs
docker build --progress=plain -f tdf-hq/Dockerfile.music-worker -t tdf-music-worker:local-verification .
npm run test:music-worker-image
npm run test:music-linux-integration
```

El test XSD verifica hashes de ambos esquemas y no descarga ni acepta licencias.
Genera cinco XML sintéticos, valida localmente, comprueba ocho relaciones con
XPath y exige rechazo de un rol inventado. No genera archivos multimedia ni
afirma que estos XML solos sean paquetes entregables.

Evidencia ya ejecutada:

- Preflight 15 OK, 3 advertencias, 0 errores; main sucio preservado, sin pull,
  cambio de rama ni polling GitHub (su autenticación no era válida).
- Build API y renderer del host: código 0; advertencias previas del linker.
- Haskell dirigido final: 46 ejemplos, 0 fallos, código 0 (11 casos nuevos),
  incluida evidencia de identificador inválido pese a tener sintaxis correcta.
- Paridad/build worker Node: 3/3, código 0; sintaxis shell/Node y diff limpios.
- Cinco XML/XSD, ocho assertions XPath y rechazo de rol inválido: código 0.
  Artefactos finales en `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-ddex-credits.g2kJMJ`.
  Repetición tras el guard de identificadores inválidos: código 0, mismos hashes.
  SHA-256 `album.xml`: `ef78f4431ce164ebe17361ccc5c2107ebe18a7804e42e985ed1f185d100b390d`.
  SHA-256 `update.xml`: `104a23906f72aac149443a158cf75bdac16fbe62e8085e1edc61ffeee7e27755`.
- Primer intento XSD falló por el lock de Stack en sandbox; repetido con
  autorización. Worker-image e integración también se repitieron autorizados
  tras denegación del socket Docker.
- Primera integración conectada autorizada falló **en runtime** después de
  aprobar/crear la exportación: `SUM(bigint)` devuelve `numeric`, incompatible
  con el `Int64` de HeaderRow. Se añadió cast explícito `::bigint` al total,
  se recompiló y se reconstruyó Linux. No se sustituyó el renderer ni se
  amplió ningún timeout. Ese intento no llegó al retiro final y no es verde.
- Segundo intento autorizado bloqueado en preflight por motor Docker HTTP 500
  mientras Desktop decía `running`; no alcanzó API/DDEX. Reinicio autorizado
  `docker desktop restart` terminó código 0. La prueba terminó código 1 durante
  la indisponibilidad del socket; no confundir arranque de Desktop con prueba
  verde ni afirmar limpieza completa sin consultar el motor recuperado.
  Motor recuperado: Server 29.8.0. Inventario por etiqueta de esa corrida
  encontró exactamente sus contenedores `...00fcf4fb...-media` (Created) y
  `...00fcf4fb...-hashes` (Exited); ambos se retiraron explícitamente.

- Tercer intento: smoke 8/8, pero el renderer reveló otra incompatibilidad
  real (`42703`): `track.title_override` no existe. Se sustituyó por
  `recording.canonical_title`, como hace la API de catálogo. Se revisaron las
  demás columnas de header/pistas/portada contra la migración vigente.
  Ese intento tampoco es verde; el harness retiró sus recursos desechables.

Imagen final construida localmente, código 0:
`sha256:77460f8581e16d4904a0f93159b19254c0f2ee7bef65e66d170169f6164ed9e2`.
Preflight final **8/8**, código 0; SHA-256 del renderer Linux
`e02d6fc848ac89e65eab5ec01dc4c53b789c47fba3eef72483b547201012643a`.
Las imágenes intermedias `5aaaffe...` y `f03d44...` pasaron smoke pero son
anteriores a las correcciones SQL completas; no usarlas como entrega final.

SHA-256 del código final de este corte:

```text
ERN432.hs                         1fc79b39d5b60a45dd7784494370fb604b655e88d6cdeb3133be04532d076bba
MusicDdexRenderMain.hs             ebb037efbbd7eac45619efbd16254bb89f7dc8b8610211a68caea430235a7c4f
MusicReleaseDDEX.hs                84e04b895234bdaee1e55a7a539edb4deb7d7cbefa606cb695710173765aeabf
test-music-release-api-e2e.mjs     3f3cb171f19e3deaeab5feccd02daa8b34fd2cfcf98fbe7b5fd67a4adb453c57
build-ddex-ern432-package.sh       07e1f2e37fc5500d7c106b131536bf3e2d7f950d18da4354720d1b665a8aa8a6
run-music-release-worker-once.sh   d42e68227a2c27766d8da76b6b952e90a518812463ff00895814c66fb7005f5c
```

Sexta pasada, con el código/imagen finales: **18/18 escenarios API/HTTPS/S3 y
8/8 preflight Linux, código 0**. Incluye las dos nuevas comprobaciones DDEX
dentro del recorrido API: exportación aprobada e idempotente con un solo job,
renderer real que conserva el nombre aprobado tras cambiar el directorio, y
422 por campo para un crédito `other` aprobado sin crear exportación/job.
También terminó compra sintética, descarga del original intacto, devolución,
revocación y retiro. No contar estos asserts nuevos como 20 escenarios del
runner: el agregado sigue siendo 18, ampliado por dentro.

XML real de esta corrida validado localmente contra XSD oficial, código 0:
`/private/tmp/tdf-ddex-api-evidence-BbR1qA/release.xml`, SHA-256
`7b7b39706edad5fba49b87393a24589f88eab10bdafabd40f713198910f64be8`.
TSV de recursos del mismo directorio:
`297f0ee1fbc729f9d20af66a97bd533d9e9336cd36c3d00fff011d19a0a9827b`.
Son seis XML válidos comprobados en este corte: cinco fixtures puros y este
resultado API/DB final; los resultados intermedios se conservan por separado.
La exportación sigue `queued`: renderizar/validar XML no equivale a completar,
validar o entregar un paquete con todos sus archivos.

Limpieza final comprobada con el motor recuperado: cero contenedores con las
etiquetas `tdf.test=music-s3`, `tdf.music-linux-run`, `tdf.music-image-test` y
cero redes `tdf.music-linux-run`, consultas código 0. El harness retiró sus
datos/credenciales/certificados temporales; quedaron XML/TSV sintéticos e
imágenes locales. No se certifica rendimiento ni estabilidad del entorno a
partir de esta pasada; se conservan los fallos y el reinicio descritos arriba.

Cuarta pasada (imagen intermedia `532ff607...`, anterior al guard adicional de
identificadores): sí ejecutó API → aprobación → exportación idempotente →
renderer real, con nombre congelado e IPI/créditos correctos. XML retenido en
`/private/tmp/tdf-ddex-api-evidence-DA41eA/release.xml`, validado posteriormente
contra XSD oficial con código 0; SHA-256
`4f79e822ac2813017506aa46fd15e8921d619eb05f16cc7434118d97a9b88879`.
La corrida completa falló después al crear una corrección de esa corrección.

Quinta pasada con ambos binarios e imagen finales: preflight 8/8, pero MinIO
no llegó a healthy en los 60 intentos existentes (`ECONNRESET`, contenedor
running, sin OOM). No alcanzó API/DDEX. El harness limpió sus recursos; se
repitió sin ampliar límites, desactivar TLS ni omitir comprobaciones.

### Defecto encontrado en este corte: correcciones encadenadas

Actualización posterior: la migración del 16-sepUTC repara el orden y añade
regresión multinivel/rollback. Ver [evidencia actual](chained-corrections.md).
Lo siguiente conserva el diagnóstico y el alcance de la pasada ERN original.

`music_create_release_correction` clona assets en un bucle ordenado por
`parent_asset_id IS NOT NULL, created_at, id`, suponiendo dos niveles. Un
preview cuyo padre es otro derivado necesita orden topológico. Tras clonar,
los timestamps coinciden y el orden UUID puede situar el hijo antes del padre:
el mapa devuelve NULL y la restricción `music_asset_check1` aborta con 23514.
La transacción falla; no se debe quitar la restricción ni remitir el hijo a
un padre arbitrario para aparentar éxito. Requiere migración aditiva/reversible,
regresión multinivel y prueba de correcciones encadenadas. **No está arreglado
en este corte y el criterio global de correcciones no está satisfecho.**

La prueba negativa específica DDEX se aisló en una segunda corrección directa
del original publicado, manteniendo aprobación, mismo crédito `other`, 422 por
campo y ausencia de exportación/job. Esa fixture no demuestra la reparación
del defecto anterior. No se eliminó el fallo del historial de evidencia.

Siguen pendientes: paquete completo de este nuevo corte con descargas/checksums
del worker, aceptación de receptor, mapeo de todos los demás campos canónicos
(incluidos derechos, distintos deals y discos), concurrencia de aprobación,
UI editorial completa, RadioWidget 403 y las puertas remotas/CDN/pagos/operación.
En particular, el `dealListElement` heredado todavía fija `SubscriptionModel`:
la política interna `full` no demuestra que ese sea el modelo comercial del
receptor. Es una brecha semántica concreta, no cubierta por XSD, que exige
configuración/mapeo revisado y validación antes de habilitar exportación real.
Tampoco se valida todavía la coherencia editorial entre el texto completo
`DisplayArtistName` y cada nombre acreditado; XSD no demuestra esa coherencia.
No se repitieron navegador ni UI manual; no hubo commit, PR ni despliegue.
