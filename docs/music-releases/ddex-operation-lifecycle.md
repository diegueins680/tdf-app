# ADR-017 — Operaciones DDEX y continuidad de identidad

Fecha: 2026-09-16. Implementación local, sin despliegue ni entrega a terceros.

## Hallazgos

La API admitía un retiro desde `suspended`, pero el checker genérico rechazaba
ese estado incluso con aprobación y snapshot conservados. API y worker no
compartían la comprobación de operación; un job podía sobrevivir a un cambio
de estado. El antecedente se buscaba por release/destinatario sin remitente ni
identificador de producto. Además, `TrackRelease/ReleaseId/ProprietaryId`
dependía de `MessageId`, cambiando de identidad con cada exportación.
Un reintento con otra operación o contraparte devolvía silenciosamente la
respuesta previa. No se afirma haber reproducido todos esos defectos en una
versión anterior: se identificaron revisando las rutas y ahora tienen cobertura.

## Reglas oficiales y decisión

Fuentes primarias revisadas el 2026-09-16:

- [Takedowns](https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/takedowns/): se mantiene `NewReleaseMessage` sin deals para solicitar retirada; no se añade `PurgeReleaseMessage`.
- [Active deals](https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/active-deals-at-time-of-sending-an-ern/): las actualizaciones deben incluir los deals vigentes. El adaptador sigue limitado a una regla gratuita bajo demanda, no habilita nuevos modelos comerciales.
- [Update Indicator](https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/guidance-on-message-exchange-protocols-and-choreographies/update-indicator/): ERN 4 no usa ese campo. Una actualización sigue siendo una nueva declaración completa.

Se añade `music_check_ddex_operation(version,sender,recipient,operation)`, usado
por API, CLI y worker antes de generar y de confirmar. Conserva las validaciones
de metadatos, identificadores, recursos y grafo. Solo permite la excepción de
estado para retiro de una versión suspendida **ya aprobada y con snapshot**.
No convierte una suspensión de borrador en autorización de publicación.

Operaciones nuevas/updates requieren aprobación, programación o publicación;
update requiere corrección/reemplazo. Retiro requiere suspensión, reemplazo
pendiente, retirada programada ya vencida o retirada efectiva. Si la retirada
está programada para el futuro, se devuelve `takedown_not_due`: un mensaje sin
deals no puede fingir una retirada futura. No se altera el scheduler local.

Update/takedown requieren un paquete inicial validado del mismo release,
identificador de producto, remitente y destinatario con adaptador v5. Esto es
continuidad de **exportaciones offline**, no constancia de entrega, ingestión
ni aceptación del receptor. Los errores tienen ruta de campo y acción.

## Identificadores y compatibilidad

`tdf-ern432-audio-v5` asigna una identidad **propietaria TDF**, bajo el namespace
del remitente, a cada track release usando el identificador principal y el
ISRC proporcionados: `TDF-TRACK-<ReleaseId>-<ISRC>`. No es un código oficial
emitido, ni reemplaza UUID internos o el ISRC de la grabación. Permanece estable
al cambiar MessageId o clonar UUID de grabaciones en una corrección.
La selección del identificador principal usa el mismo orden en SQL y renderer:
GRid, UPC, EAN, y desempate por fecha/UUID. No depende del orden físico de filas.

Los paquetes anteriores conservan exactamente sus bytes y siguen descargándose.
No se usan automáticamente como antecedente del nuevo esquema de identidad:
generan `initial_export_missing` con aviso de revisión de legado. Antes de
operar un receptor real, revisar los TrackRelease IDs que haya recibido; no
reenviar ciegamente un alta ni renombrar IDs que ese receptor ya conozca.
Si hace falta conservar IDs antiguos por destinatario, requiere un adaptador
de compatibilidad explícito; esta entrega no lo inventa.

El manifiesto sigue en v3; las nuevas exportaciones registran adaptador v5.
Reutilizar la misma clave idempotente con otra operación/remitente/destinatario
ahora devuelve 409. No cambia la forma de respuesta ni los clientes generados.
La prueba conserva UUID/hash de snapshots y archivos previos tras update/retiro.

## Migración y rollout

Aplicar `2026-09-16_music_ddex_operations.sql` después de las migraciones
musicales existentes hasta `music_resource_graph_validation`. Añade dos funciones,
sin backfill ni escrituras sobre releases/exports. Aplicación repetible.
El fixture API instala la función antes de su upgrade deliberado del grafo
para verificar también ese caso de compatibilidad; no es el orden de producción.

Pausar reclamación DDEX y nuevas solicitudes, aplicar SQL, recompilar API y
renderer y desplegar la imagen worker coherente. Reanudar solo después de los
smokes. Workers anteriores no ejecutan el gate nuevo: no mezclarlos.

Rollback: pausar DDEX, restaurar API/renderer/worker compatibles y ejecutar
`2026-09-16_music_ddex_operations_rollback.sql` antes de revertir migraciones
anteriores. No elimina paquetes ni evidencia. Mantener generación apagada si
se vuelve al esquema anterior con identidades inestables; no se consideran
las validaciones antiguas una alternativa segura de producción.

Consulta de inventario legado, solo lectura y para personal autorizado:

```sql
SELECT id, release_version_id, operation, sender_registry_id, recipient_registry_id,
       validation_report->>'adapterVersion' AS adapter_version
FROM music_ddex_export
WHERE status='valid'
  AND validation_report->>'adapterVersion' IS DISTINCT FROM 'tdf-ern432-audio-v5';
```

## Evidencia

- Preflight: 15 OK, 3 advertencias, 0 errores; árbol sucio, gh inválido y loop
  apuntando a main. Trabajo ajeno conservado; no se inició loop.
- Haskell musical final: 56 ejemplos, 0 fallos. Se sustituyó `head` del nuevo
  test tras la advertencia del compilador; repetición final sin esa advertencia.
- Builder: 16/16; cinco XML (alta/EP/álbum/update/takedown) válidos contra XSD
  fijados, ocho aserciones semánticas y rol inválido rechazado. Evidencia
  `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-ddex-credits.zmHnk6`.
- Worker dirigido `--only=DDEX`: 2/2, código 0. Transporte sustituido
  explícitamente, no se presenta como prueba de S3.
- Suite de migraciones final: código 0, aplicación repetida, pruebas de
  operaciones sobre fixtures de estado, rollback/reaplicación y regresiones
  previas y rollback vacío. Estos fixtures no son paquetes conformes ni DPIDs reales.
- Suite completa del worker: 22/22, código 0; incluye los dos escenarios DDEX
  dirigidos, no se suman como casos distintos. Inyección de fallos de transporte.
- Pruebas Node dirigidas de integración/build/timing: 17/17, código 0.
- Preflight de imagen final: 8/8, código 0; fuentes/scripts coinciden por SHA,
  procesamiento de audio/arte real y TERM. No demuestra integración DB/S3.
- Build API y renderer nativo final: código 0. Hubo advertencias preexistentes
  de imports/sombreado/deprecación y del enlazador, no errores de compilación.
- Integración final: **8/8 preflight Linux + 18/18 API/HTTPS/S3**, código 0.
  Alta/update/retiro desde suspensión generan ZIP reales, con XSD oficial,
  recursos y checksums; rechazan acceso anónimo/ajeno y recuperan los mismos
  bytes tras una caída simulada después de confirmar el export. IDs de pista
  e hilo estables, MessageId distinto, snapshot de update conservado en retiro
  y hashes anteriores intactos. Rechazo de contrapartes distintas e idempotencia
  secuencial incluidos. No sumar los 8 del preflight separado como otros casos.
  Primera corrida terminó con código 1: alta/update y su recuperación pasaron,
  pero el nuevo test envió `comment` en la suspensión; la API rechazó ese campo
  desconocido con 400. Se corrigió el fixture a `reason`, sin relajar el contrato.
  Evidencia parcial: `/private/tmp/tdf-ddex-api-evidence-IaOmmD`; no cubre retiro.

Evidencia final conservada: `/private/tmp/tdf-ddex-api-evidence-iLedj4`.
SHA-256 de los tres ZIP:

```text
af22737889953d6775025c2231b1029dca8fb5ca71ac6ec631921769403f4ae1  package.zip
33b380307ca03b6038ddaa8cfd6766011d1e696ad3e6d783000d844c5e77fed5  update-78ce8f55-48e9-4441-ab8f-7ad103138a92.zip
5f8331b4ba07c9659ffc08c742b2e92688b4fca46b81581aae857995f0ff52b6  takedown-5d982224-14a3-4ba3-856d-3f149627471c.zip
```

Inspección final de manifiesto y hashes por CLI: adaptador v5, manifiesto v3,
`deliveryPerformed:false`, XSD y AVS fijados, `recipientAcceptance:not-verified`.
`git diff --check` y sintaxis shell/Node sin errores. La limpieza del runner
retiró sus contenedores/redes, objetos, certificado y credenciales sintéticos;
consultas finales por etiquetas devolvieron cero recursos. Imagen y ZIP retenidos.

Imagen final construida con código 0:
`sha256:c335ca0031529682a682efe9903af1b0d98c1ef5168edf2226bdc1860bb4669b`.
Renderer Linux: `c8e6e4e00703fd043d6bd831c91c38f50867a55c25f9500040c1f79beaeb87ec`.

SHA-256 de migración y rollback:

```text
7ee319838463bfdc07b459fad9d67f51ea5f7eda22c939b317cec03bb678d667  2026-09-16_music_ddex_operations.sql
9ce554f137c34e72bd301c9cdb67f4081ac3d32b90e3bb6c27a315df2144a733  2026-09-16_music_ddex_operations_rollback.sql
```

Fuentes finales SHA-256:

```text
0102ca1c2de600a6f57b7807fac86509d726fdf6467ba2174849576fc4b45d80  src/TDF/Server/MusicReleaseDDEX.hs
e70bf01308d7918735128deb87f309a35123c1764514f531e18f211355c561c1  app/MusicDdexRenderMain.hs
4dec7b8d3065fb925231203efe76181a645eb3701b3560b635eb364ece867677  src/TDF/MusicRelease/DDEX/ERN432.hs
9674d11b9ae0f8932047a35db3fc77908d65bc46b5a495425d2bc8a1758ba625  scripts/run-music-release-worker-once.sh
8cd6f2ccc3881d3715499144e1513dfe2fd61ac9f1577b53d18eda9c5c5c26bc  scripts/build-ddex-ern432-package.sh
```

Comandos: `sh scripts/test-music-release-platform-migration.sh`,
`node scripts/test-music-release-worker-runtime.mjs`, los comandos de
[ADR-015](ddex-offline-bundle.md#reproducir-la-verificación), y
`npm run test:music-linux-integration -- --ddex-schema-dir=/private/tmp/ddex-ern432-xsd/zip`.

La API esperó un lock de otra compilación de Stack; no se mataron procesos ni
se modificó trabajo ajeno. Docker no requirió reinicio. Sin pagos externos,
CDN/proveedor remoto, UI manual, commits, PR, despliegues o flags activados.
Siguen pendientes reglas integrales del perfil, coreografía/contrato/acuses,
compatibilidad de identidad con receptores legados y operación remota.
Pendiente al cerrar ADR-017: enqueue y job usaban escrituras separadas; su prueba
secuencial no demostraba atomicidad ni concurrencia. La continuación
[ADR-018](ddex-atomic-enqueue.md) aborda esa ventana; consultar allí su evidencia.
