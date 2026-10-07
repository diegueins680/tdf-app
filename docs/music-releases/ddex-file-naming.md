# ADR-016 — Nombres de archivos DDEX vinculados al mensaje

Continuación: [ADR-017](ddex-operation-lifecycle.md) conserva este naming y
manifiesto v3, añade operaciones coherentes e identidad TrackRelease estable
en el adaptador v5. La evidencia siguiente corresponde a la entrega v4.

Fecha: 2026-09-16. Implementación local; no desplegada. Continúa el
[bundle offline](ddex-offline-bundle.md), sin convertirlo en entrega a un DSP.

## Revisión y decisión

Se revisaron fuentes oficiales el 2026-09-16:

- [Cloud Storage 1.8.1, §5.3](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/5-release-by-release-profile/5.3-file-naming-conventions/): el XML usa el identificador del release; recursos pueden incluir la referencia técnica del mensaje. Label y jerarquía son opcionales y se omiten en este adaptador.
- [§5.2, servidor](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/5-release-by-release-profile/5.2-file-server-organisation/): el directorio incorpora prioridad acordada y fecha real de subida, con recursos en un subdirectorio. No se usa la fecha de generación del ZIP como una subida inexistente.
- [§5.1, coreografía](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/5-release-by-release-profile/5.1-choreography/): intercambio seguro y acuses requieren un receptor y acuerdos bilaterales. No los simula el exportador offline.
- [Diccionario ERN 4.3.2](https://service.ddex.net/dd/DD-ERN-432/dd/index.html): `TechnicalResourceDetailsReference` identifica los detalles técnicos dentro del mensaje. El adaptador interpreta el componente `TechnicalResourceId` de §5.3 como esta referencia del XML ERN 4.3.2; es una decisión de mapeo explícita que debe contrastarse con el contrato del receptor.

El adaptador anterior usaba `release.xml`, UUID internos en audio y `cover.jpg`.
Ahora `tdf-ern432-audio-v4` usa el identificador proporcionado/validado del
release, normalizado igual en XML y nombres, y la referencia técnica exacta:

```text
<ReleaseId>.xml
resources/<ReleaseId>_T1_SoundRecording.m4a
resources/<ReleaseId>_T2_SoundRecording.m4a
resources/<ReleaseId>_TArtwork_CoverArt.jpg
manifest.json
```

Los marcadores representan valores reales del modelo, no identificadores
emitidos por TDF. Para audio la numeración coincide con el orden del mensaje;
la portada usa `TArtwork`. No se infiere una jerarquía de discos a partir del
nombre del archivo. No cambia el modelo canónico, las URLs públicas ni los
objetos originales. El manifiesto TSV del renderer usa las mismas funciones
de nombres que el XML, evitando divergencias entre descarga y referencia.
El XML temporal y su clave privada interna S3 pueden seguir llamándose
`release.xml`; `messageFile` describe el nombre exportado dentro del ZIP.

## Contrato y seguridad

Manifiesto **v3**: añade `messageFile`; los lectores no deben asumir
`release.xml`. Declara el subconjunto de nombres validado en `validation.fileNaming`,
`serverLayout:not-performed`, `deliveryPerformed:false` y aceptación no verificada.
`packageFormat` sigue siendo `tdf-offline-review-bundle`. El manifiesto es TDF,
no se presenta como un mensaje normativo DDEX ni se anuncia conformidad completa.

El builder exige un único ICPN o GRid principal, seguro y normalizado. Cada
URI debe coincidir exactamente con release, referencia técnica, tipo y extensión.
Extensiones admitidas por el builder: audio m4a/wav/flac/aiff e imagen jpg/jpeg/png;
el renderer actual entrega m4a/jpg. Se rechazan rutas históricas genéricas,
identidades cruzadas, recursos ajenos al subconjunto y extensiones incompatibles.
Se conservan XSD fijados, rechazo de DTD/entidades/symlinks/traversal, copia
exclusiva de referencias, checksums y ZIP reproducible sin sobrescrituras.
Sintaxis/nombres no demuestran titularidad ni que un código esté asignado.

## Verificación de este corte

- Preflight: 15 OK, 3 advertencias, 0 errores; main sucio, autenticación gh
  inválida y configuración de loop apuntando a main. No se inició loop ni se
  modificó trabajo ajeno.
- Stack API y renderer: compilados, código 0. Suite musical: **55/55**.
- Builder: **16/16**, código 0; ICPN y GRid sintéticos, reproducibilidad,
  hashes, ausencia de recursos privados no referenciados y rechazo de nombres
  históricos, referencia técnica/release discrepantes, traversal, entidades,
  symlinks, falta de recursos, XSD alterado y carrera sin sobrescritura.
- Node dirigido: **17/17**, código 0; dos casos de limpieza usan dobles explícitos.
- Worker dirigido `node scripts/test-music-release-worker-runtime.mjs --only=DDEX`:
  **2/2**, código 0; recuperación inmutable de un export ya validado y
  revalidación de jobs encolados. Usa transporte sustituido explícitamente;
  no es evidencia de compatibilidad S3 ni se cuentan otros tests omitidos.
- Cinco fixtures ERN (single/EP/álbum/update/takedown) válidos contra XSD oficial,
  ocho aserciones semánticas y rol inválido rechazado, código 0. No son paquetes
  de entrega; evidencia en el directorio temporal
  `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-ddex-credits.82MgrQ`.
- Imagen construida: `sha256:bb98ffe45723907d95c9a180f2753c928ce86cfece8e542e955dfb600cbeadcb`.
- Integración final sobre esa imagen: **8/8 Linux y 18/18 API/HTTPS/S3**,
  código 0. El ZIP real usa `036000291452.xml` y recursos con `T1`/`TArtwork`
  coincidentes (identificador de fixture). XML validado localmente contra XSD,
  todos los hashes comprobados, acceso ajeno/anónimo denegado, recuperación de
  job confirmado y segunda descarga con bytes idénticos. No se sustituyeron
  renderer, builder ni transporte en este flujo; pagos/identidades son sintéticos.
- Sintaxis shell/Node y `git diff --check`: código 0. No hubo fallos de pruebas
  en este corte; la primera suite Haskell tenía 54 y la final 55 casos, builder
  15 y luego 16 al incorporar GRid. No sumar ambas corridas como tests distintos.
- Limpieza confirmada por etiquetas: cero contenedores/redes de integración y
  cero contenedores del preflight. El runner eliminó sus objetos, certificados
  y credenciales sintéticos; imagen y evidencia permanecen disponibles.

Evidencia retenida: `/private/tmp/tdf-ddex-api-evidence-gHnzHc/`.

```text
93ab611d54319ecde903a111132b8c58283ddfc142e14a2f3cc6bd4133fcc74f  package.zip
3e24c8ae638223e109777a39f275c3c4e513a9fec305035ee90b1862e6386663  release.xml (copia temporal del mensaje exportado)
5543a4e3f5bb902b59f179ea4decce80906ea5f38853d8ef15e2cac6778977a4  resources.tsv
353bfb2b43794fae0ba36658dd6d19b05ce55c43834d1069df9cbf589072d999  scripts/build-ddex-ern432-package.sh
b27e141ea3cb945aa4c1e4ec425f75b6da6c02bf0d662b735a7040795ead5b74  scripts/test-music-ddex-package.mjs
50c98dc646be95b210b86bb22aaff49683645dd37b45ce1adb7de606825c57ae  scripts/run-music-release-worker-once.sh
6ac5ad1f443cdec9b41cb01b26c69950a0e39e2ee0190f2fb2c4e80fbcbeeeae  tdf-hq/src/TDF/MusicRelease/DDEX/ERN432.hs
ab9e16cf7099e150042add1885bef84326694bcceb135113a5d12bbaadeb2b75  tdf-hq/app/MusicDdexRenderMain.hs
eed21d5f7b3b71706528ec261a74f8b0994ec938db1b333742126b1dd3145ed9  scripts/lib/music-api-s3-probe.mjs
da3ba71aa899e69405e7d06c166dadb3d8081ee79b48314d793867672efb075a  renderer Linux
```

Comandos reproducibles: los de [ADR-015](ddex-offline-bundle.md#reproducir-la-verificación).
La integración usa `--ddex-schema-dir=/private/tmp/ddex-ern432-xsd/zip` y
comprueba `messageFile`, nombres del renderer real, todos los hashes, XSD,
privacidad, descarga y recuperación. Ningún test descarga/acepta XSD por cuenta
del usuario. Los fixtures no son códigos oficiales ni pagos reales.

## Despliegue, compatibilidad y límites

Sin SQL/backfill nuevos. Requiere las migraciones musicales anteriores, API y
renderer recompilados, imagen worker actualizada y XSD revisados montados.
Pausar reclamación DDEX durante rollout; no mezclar renderer v3 y builder v4
porque los nombres anteriores fallan cerrados. Mantener una sola imagen coherente.
Los exports ya `valid` conservan archivos/hashes y se recuperan sin regenerar;
la descarga de paquetes antiguos no interpreta el nuevo manifiesto.
Rollback de código: pausar generación DDEX y restaurar imagen completa, nunca
reescribir paquetes históricos ni reabrir v2 con el deal de suscripción erróneo.

Quedan pendientes estructura de entrega en servidor, prioridad/acuses/contrato,
validación integral de reglas del perfil, paquetes reales update/takedown,
otros deals, varios GiB, proveedor/CDN y operación/rollback remotos. No hubo
despliegue, banderas habilitadas, pagos externos, commits/PR ni UI manual.
