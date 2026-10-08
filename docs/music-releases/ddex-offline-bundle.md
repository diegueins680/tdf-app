# ADR-015 — Bundle offline DDEX privado y reproducible

Fecha: 2026-09-16. Estado: implementado localmente; no desplegado. No cierra
todos los requisitos de exportación ni declara aceptación de un receptor.

Este documento conserva el corte v3/manifiesto v2. La continuación
[ADR-016](ddex-file-naming.md) implementa nombres vinculados al mensaje y
manifiesto v3; las limitaciones de entrega/perfil continúan explícitas.

## Hallazgos y decisión

El renderer anterior declaraba `SubscriptionModel` y `Stream`, aunque la
regla canónica exportable permitía escucha completa gratuita. Omitía el fin
de vigencia y el embargo en el comienzo del deal. El empaquetador copiaba
recursos no referenciados, dependía de metadatos de archivos/hora de ejecución
y declaraba Cloud Storage sin implementar su estructura de entrega.

El adaptador `tdf-ern432-audio-v3` exporta esa regla exclusivamente como
`FreeOfChargeModel` / `OnDemandStream`. Conserva créditos versionados,
incluye `EndDateTime` cuando existe y bloquea periodos inválidos. El inicio
respeta el máximo entre disponibilidad, publicación y embargo. El CLI repite
el gate canónico; ejecutarlo directamente no evita la comprobación de deals.
Preview, compra/descarga, reglas múltiples o por pista siguen bloqueadas.
Esto no concede ni infiere derechos comerciales para un DSP externo.

Fuentes oficiales revisadas el 2026-09-16:

- [AVS: modelos comerciales](https://service.ddex.net/dd/DD-AVS-CURRENT/dd/avs_CommercialModelType.html): gratuidad y suscripción son modelos distintos.
- [Uso recomendado de CommercialModelType/UseType](https://kb.ddex.net/implementing-each-standard/best-practices-for-all-ddex-standards/deals-and-commercial-aspects/recommended-use-of-commercialmodeltype-and-usetype-in-ern-4/): `Stream` es más amplio que escucha bajo demanda.
- [Cloud Storage 1.8.1](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/), [organización del servidor](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/5-release-by-release-profile/5.2-file-server-organisation/) y [nombres de archivos](https://ernccloud.ddex.net/electronic-release-notification-message-suite-part-3-choreographies-for-cloud-based-storage/5-release-by-release-profile/5.3-file-naming-conventions/): requieren estructura/nombres que este ZIP interno no implementa. No se inventa un timestamp de recepción en servidor.

Se mantiene [ERN 4.3.2 / Audio 2.3.1 / AVS 011 / DD-ERN-432](ddex-compatibility-matrix-2026-09-12.md).
Business Profile: ninguno. Cloud Storage 1.8.1 es objetivo pendiente, no una
certificación del ZIP. Tampoco se promete validación completa de todas las
reglas del perfil por el mero hecho de pasar XSD.

## Contrato del empaquetador

`scripts/build-ddex-ern432-package.sh` exige XML UTF-8 con la declaración
exacta del renderer, autor no vacío, hash canónico y dos XSD oficiales fijados:

```text
def25b4e72696c9bbc1fed84962acc3a9bae2bc92ef25f8393c99b362aa53a6a  release-notification.xsd
87e99fe74f57a640dce0d3247d16b3b52358562c1dbefc4617eb8a9b7360d943  allowed-value-sets.xsd
```

No es un importador XML genérico. Rechaza DTD/entidades antes de parsear,
esquemas modificados, symlinks, URIs remotas, traversal incluso codificado como
entidad XML y recursos ausentes. Copia solo archivos regulares referenciados.
Normaliza permisos, fechas y orden del ZIP; entradas idénticas producen bytes
idénticos con el mismo toolchain. No se garantiza igualdad entre versiones de ZIP.
Publicación por enlace atómico en el mismo filesystem: ni un destino existente
ni una carrera entre escritores permiten sobrescribir un paquete.

Manifiesto v2: formato `tdf-offline-review-bundle`, hashes/tamaños por archivo,
hash de contenido, snapshot, autor, versiones y validación XSD. Usa fecha del
mensaje congelado; la hora operativa permanece en PostgreSQL. Declara
`deliveryPerformed:false` y `recipientAcceptance:not-verified`. El archivo
`release.xml` y nombres internos de recursos no son los de la coreografía.
API y worker conservan XML/manifiesto/ZIP privados asociados a la versión;
una recuperación de job confirmado no regenera ni sustituye el paquete.

## Reproducir la verificación

Proporcionar localmente los XSD revisados/licenciados; los tests no descargan
esquemas ni aceptan licencias automáticamente:

```sh
node scripts/test-music-ddex-package.mjs /private/tmp/ddex-ern432-xsd/zip
sh scripts/test-music-ddex-credits.sh /private/tmp/ddex-ern432-xsd/zip
node --test scripts/__tests__/music-linux-integration.test.mjs scripts/__tests__/music-worker-build.test.mjs scripts/__tests__/music-worker-timing.test.mjs
node scripts/test-music-release-worker-runtime.mjs --only=DDEX
node scripts/test-music-release-worker-runtime.mjs --only='complete audio release'
docker build --progress=plain -f tdf-hq/Dockerfile.music-worker -t tdf-music-worker:local-verification .
npm run test:music-linux-integration -- --ddex-schema-dir=/private/tmp/ddex-ern432-xsd/zip
```

Compilación/pruebas Haskell desde `tdf-hq`:

```sh
stack build tdf-hq:exe:tdf-hq-exe tdf-hq:exe:tdf-ddex-render --fast
stack exec -- runhaskell -isrc -itest test/MusicReleaseSpecMain.hs
```

La opción de integración monta únicamente los dos XSD comprobados, en modo
solo lectura; no hereda rutas arbitrarias del host. Usa API, renderer, worker,
PostgreSQL TLS y MinIO HTTPS reales con archivos propios/sintéticos y permisos
aislados. Los identificadores son fixtures explícitos, no códigos emitidos ni
autoridades verificadas. La suite aislada del ZIP usa bytes sintéticos para
aislar seguridad/estructura, no demuestra procesamiento de audio.

## Evidencia de este corte

- Build Stack API/renderer: código 0; Haskell musical: 53 ejemplos, 0 fallos.
- Fixtures de créditos: 5 XML válidos, 8 comprobaciones semánticas y un rol
  inválido rechazado; código 0. Directorio de evidencia:
  `/var/folders/0s/0tg301f95s51dvsjf74ksxjm0000gn/T/tdf-ddex-credits.Tw9fGE`.
- Empaquetador: 11 escenarios, código 0; incluye reproducibilidad, ausencia
  de archivos privados no referenciados, UTF-16 rechazado, XSD alterado,
  traversal/entidades/symlinks, recursos faltantes y carrera sin sobrescritura.
  Un recurso llamado `manifest.json` también conserva su checksum: la revisión
  final corrigió una exclusión por nombre demasiado amplia.
- Node dirigido: 17/17, código 0; dos casos de limpieza usan dobles explícitos.
- Regresiones `--only=DDEX`: 2/2, código 0; recuperación inmutable y rechazo
  de jobs cuyo grafo dejó de ser válido. PostgreSQL/worker reales y transporte
  sustituido explícitamente; no se cuentan como prueba de compatibilidad S3.
- Regresión `--only='complete audio release'`: 1/1, código 0; incluye paso
  automático a listo para revisión exactamente una vez y gate de confirmación DDEX:
  un destinatario revocado durante generación impide confirmar. Renderer,
  empaquetador y transporte de este escenario son dobles explícitos, no se
  presentan como prueba de conformidad. La integración siguiente no los usa.
- Imagen final construida: `sha256:27b6f84424e13033484f348dac987d372071a0dbd3b8ace2b6355ff2000b3e80`.
  Renderer Linux: `019565a356023caa50a0b2798364643c50ee14a60efd9e4d1cec30fdb9e197cc`.
- Integración final sobre esa imagen: **8/8 Linux y 18/18 API/HTTPS/S3**,
  código 0. Incluye generación por renderer/worker reales, validación XSD,
  ZIP con recursos/checksums, PUT privado, denegación a otro artista y acceso
  anónimo, descarga autorizada, recuperación después de confirmar exportación
  y segunda descarga idéntica byte a byte. Compra/entitlement usa eventos
  canónicos sintéticos, no un cobro externo.
- Limpieza verificada por etiquetas: cero contenedores/redes del runner y
  cero contenedores del preflight; imagen y evidencia retenidas. Objetos,
  certificados y credenciales de prueba eliminados por el runner.
- Sintaxis shell/Node y `git diff --check`: código 0. No se repitió la suite
  completa del repositorio ni todos los 22 escenarios del worker en este corte.

Evidencia final: `/private/tmp/tdf-ddex-api-evidence-DxBEgs/`.

```text
66457c4cda51caba47ce7b72e1ad1aeec33bc348e243fdfd0a129afdcc9ff62f  package.zip
1ee7b3cd46143479a8193eee3415ef078c672197c6cb828fdd8fa543d2a68eb7  release.xml
8f715db5b86f06d419cad44b6c03068ee386870abce4416d0270e1c2732fc8a3  resources.tsv
0e18edd4b37551c11fba0a2ac0aa16413f9c037e468cf646d44a318a2954aef0  scripts/build-ddex-ern432-package.sh
01e137023dcc02ab57e7269ae533335be24331402643f760beebbbb887c5c114  scripts/test-music-ddex-package.mjs
52a28c60c2a6cfe4aced7ec041d3db5762f49e1422f0dee88f45a403a8cecc97  scripts/run-music-release-worker-once.sh
5be3f836a57f4ba7814139acae00884f973e87670d9faf86f975786e481eb4bf  tdf-hq/src/TDF/MusicRelease/DDEX/ERN432.hs
f3b48a2b2a639055669727855242b6b2a371a82a17fdde312108412e636db386  tdf-hq/app/MusicDdexRenderMain.hs
9aba0cc3021deea61d34f8649622bcd815c475bcd5cfff9254bef122d037a219  scripts/lib/music-api-s3-probe.mjs
```

La primera integración generó y descargó el paquete, pero falló en una
comprobación nueva: el test intentó interpretar dos columnas SQL como JSON.
Se corrigió a `json_build_object`, manteniendo la comprobación de privacidad.
Esa corrida no se cuenta como éxito. También hubo una invocación del script
de créditos sin permiso de ejecución; se ejecutó correctamente mediante `sh`.

## Despliegue y pendientes

No hay nueva migración ni backfill en este corte. Requiere las migraciones
musicales previas hasta `music_resource_graph_validation`, recompilar API y
renderer, reconstruir worker y montar XSD revisados. Pausar DDEX al sustituir
workers; no mezclar adaptadores. No regenerar paquetes históricos. Si se
revierte código, mantener generación DDEX pausada: el adaptador anterior
declara incorrectamente suscripción para una regla gratuita.

Pendientes: coreografía/nombres/manifiestos normativos y reglas completas del
perfil, contrato de receptor, DPID reales con evidencia, más configuraciones
comerciales, paquetes reales de update/takedown, transferencias de varios GiB,
proveedor/CDN y recuperación/rollback remotos. La entrega directa a DSP sigue
fuera de alcance. Pago externo y UI no se verificaron en este corte. No hubo
despliegue, activación de banderas, commits, PR ni interacción manual en navegador.
