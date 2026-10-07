# Verificación de la imagen Linux del worker

Fecha de comprobación: 2026-09-15 UTC (14 de septiembre en Ecuador).

## Procedimiento reproducible

Desde la raíz del repositorio:

```sh
docker build --progress=plain -f tdf-hq/Dockerfile.music-worker -t tdf-music-worker:local-verification .
npm run test:music-worker-image
```

El runner exige una imagen **ya construida localmente**. No descarga imágenes,
publica artefactos, despliega, lee `.env` ni usa credenciales de TDF. Resuelve la
etiqueta una vez a su ID SHA-256 y usa ese mismo ID en todos los escenarios.
Puede recibir otra referencia local como argumento después de `--`.

Solo monta el script de assertions en modo lectura; ejecuta los pipelines,
renderizador, supervisor y `tini` que contiene la imagen, y comprueba la sintaxis
del uploader (sin transferencias), sin montar el código fuente encima ni
reemplazar comandos. Los fixtures se generan con
FFmpeg dentro de un tmpfs efímero: tono PCM de tres segundos, portada azul y
bytes explícitamente corruptos. Nunca se procesa material de terceros.

Controles de ejecución: usuario `worker` UID 1000, raíz de solo lectura, red
deshabilitada, sin capabilities ni elevación de privilegios, 2 CPU, 1 GiB RAM,
128 procesos y tmpfs de 256 MiB. Estos límites son para fixtures pequeños; no
son dimensionamiento certificado para másteres de 8 GiB.

El runner verifica:

1. Dependencias Linux y arranque del binario Haskell enlazado.
2. Máster PCM intacto, cuatro calidades decodificables, preview de 1,75 s y retry
   que conserva el manifiesto exacto.
3. Portada intacta, procesamiento y thumbnail de 600 px.
4. SHA-256 de todos los recursos de ambos manifiestos.
5. Rechazo de entrada corrupta sin derivados publicados.
6. Worker sin configuración rechazado con código 2 a través de `tini`.
7. TERM enviado por Docker al supervisor real durante su espera de reintento:
   salida limpia sin OOM ni SIGKILL.
8. Los ocho scripts incluidos y los seis inputs del renderizador (tres fuentes,
   manifiesto, resolver y lockfile) coinciden por SHA-256 con el checkout;
   rechaza una imagen desactualizada aunque conserve la misma etiqueta. La
   huella de inputs se genera en la etapa de compilación; no es una attestation
   criptográfica contra imágenes maliciosas de terceros.

La limpieza resuelve cada nombre aleatorio y comprueba la etiqueta exclusiva de
la corrida antes de quitar sus contenedores. Se eliminan sus fixtures tmpfs y
se conserva la imagen local y las cachés de construcción.

## Evidencia ejecutada

La construcción final completa terminó con **código 0**, compilando/enlazando
el renderizador real (4/4 etapas, GHC 9.10.3). No fue solo `docker build --check`.
Después, `node scripts/test-music-worker-image.mjs` pasó **8/8 escenarios,
código 0**, con esta imagen **linux/amd64**:

```text
ID local: sha256:401d6874b59bd8337d70a187e09a6429e5dcfc7c5ea1a7cd3d3204a381d7a5cc
tdf-ddex-render SHA-256: 2e14c97ca1bcf00c190ca1811e1a459a81444dc77fa806d1d97ca680ae80eb58
```

Es un ID de imagen local; no se publicó en un registry ni se afirma un digest
de distribución remoto. Docker informó **634.074.690 bytes** de tamaño de imagen
local, no tamaño comprimido de descarga. El runner retiró sus cuatro contenedores y fixtures;
se conservaron imagen y cachés. Las comprobaciones usaron raíz read-only y
los límites indicados arriba, sin OOM ni necesidad de SIGKILL para el supervisor.
Una consulta posterior por la etiqueta `tdf.music-image-test` devolvió cero
contenedores (código 0).

Herramientas observadas dentro del artefacto: FFmpeg **5.1.9-0+deb12u1**, curl
**7.88.1** / OpenSSL **3.0.20**, PostgreSQL client **15.19**, jq **1.6**, tini
**0.19.0**. El máster sintético mantuvo SHA-256
`415ffe1a6a5d0b8da713e83e686f6ebd4cfaa48d948ff82d785d3915414e2e95` y la
portada `910b8c89fd6646844b68eeede962c977fd98ab650cf325eba8fc6bcc09fda90d`.
Se decodificaron los cinco archivos de audio y se comprobaron todos los
checksums de los manifiestos; las segundas ejecuciones conservaron sus bytes.

También pasaron juntos **13/13 tests, código 0**:

```sh
node --test scripts/__tests__/music-worker-build.test.mjs scripts/__tests__/music-worker-timing.test.mjs scripts/__tests__/music-test-database.test.mjs
```

`node --check`, `sh -n` del probe y `git diff --check` terminaron sin errores.
No se ejecutaron migraciones, despliegue, pagos externos, commit/PR ni revisión
manual de interfaz en esta continuación. Las suites completas PostgreSQL y
API/S3 no se repitieron aquí: sus últimas pasadas están documentadas por separado.

SHA-256 de los archivos verificados:

```text
1de769ca7636491fb5391fc2fe8b90ee19e20d19621b21d90a52ffa170366a83  tdf-hq/Dockerfile.music-worker
cd8794225ff4aa9550560653e371d570074d9f6dc261c1dfd138a4aa4fc37df8  tdf-hq/music-worker/stack.yaml
665eef4be54281d16a966188327ba4fe8586c92373b82f7f7cb58170a1b17553  tdf-hq/music-worker/stack.yaml.lock
5f4ad014847d5ae846642b61a29676f330ebc739dbe5102f3f50293490fdc904  tdf-hq/music-worker/tdf-music-worker.cabal
77621bf3782591ee259de08068c291ecefd88f939d5db4667b082093041707e3  scripts/test-music-worker-image.mjs
bb0863c945b3303ea7b5236fd3b6d36fc47fa413f2f3665dd0fc5817914563b5  scripts/lib/music-worker-image-smoke.sh
48ed0c6d1a7308dae7e8d1c6370d2ce14243bd7a1caf80f6b07b1bf5a6e62dcf  scripts/__tests__/music-worker-build.test.mjs
```

## Alcance pendiente

Esta prueba no sustituye el worker con PostgreSQL/S3 real, ni la conformidad
DDEX (solo comprueba que el renderizador arranca). La prueba TERM aquí cubre
espera de reintento; el fencing y la interrupción de transferencias activas
siguen cubiertos por las suites locales independientes. La continuación
posterior ejecutó los jobs de audio/arte/preview de esta imagen con PostgreSQL
y S3 HTTPS locales: ver [integración conectada](linux-worker-integration.md).
Falta ejecutar también la API dentro de Linux, probar varios GiB y certificar
proveedor/CDN, restauración, retención y pagos externos antes de producción.
Una imagen construida no equivale a un despliegue ni activa feature flags.
No se certificó ARM64 ni se ejecutó análisis de vulnerabilidades/SBOM del
artefacto. Este cambio no requiere migración: modifica construcción y pruebas,
no SQL ni el comportamiento del worker. Para rollback, conservar y promover
la imagen anterior aprobada; no borrar colas ni datos. No se ensayó aquí un
rollout/rollback remoto.

## Fuentes oficiales consultadas

Consultadas el 2026-09-15 UTC:

- [Docker: ejecución, aislamiento, recursos, usuarios y referencias de imagen](https://docs.docker.com/engine/containers/run/).
- [Stack: construcción de componentes y opciones de build](https://docs.haskellstack.org/en/stable/commands/build_command/).
- [Stack: lockfiles y planes de construcción reproducibles](https://docs.haskellstack.org/en/stable/topics/lock_files/).

Se mantiene Stack/GHC 9.10.3 y el contrato del componente `tdf-ddex-render` del
`.cabal` efectivo. El manifiesto reducido en `tdf-hq/music-worker/` referencia
los mismos `../app` y `../src`, sin copiar ni bifurcar código Haskell. Dos tests
comparan el componente completo (salvo rutas relativas) y el resolver con el
backend; cualquier cambio de dependencias/módulos exige mantenerlos alineados.
Las bases oficiales quedan fijadas por digest y el snapshot por el lockfile
generado por Stack, que coincide con el del backend. Un tercer test impide
retirar esas fijaciones o deshabilitar la comprobación del compilador. Revisar
y actualizar los digests ante parches de seguridad, reconstruir y repetir las
pruebas; fijar versiones no certifica ausencia de vulnerabilidades. Los paquetes
Debian instalados por APT siguen recibiendo la versión vigente del repositorio:
esto no promete reconstrucciones bit a bit. Promover siempre la imagen probada
por digest, nunca reconstruir silenciosamente entre staging y producción.

Stack se limita a dos trabajos para evitar saturar el host por paralelismo
automático. No se compila con GHC del host ni se introduce un renderizador stub.

La primera construcción, solicitando `tdf-hq:exe:tdf-ddex-render` desde el
paquete monolítico, empezó a compilar también dependencias ajenas al renderer
(`appar`, `auto-update`, `bsb-http-chunked`, `basement`, entre otras). Se detuvo
únicamente ese build local (salida 130, no una pasada verde) para aislar su
grafo de dependencias. La segunda construcción reutiliza las capas y la caché
válida; no es una medición de tiempo desde caché fría.
Esa segunda construcción completó 35 acciones y produjo la imagen intermedia
`sha256:9dc03cd1aa32a259698338a4cac9755b7e45128f99a8ba02b6fde276dee4bb9b`.
La tercera construcción incorporó digests, lockfile, locale UTF-8 y huellas:
solo recompiló el componente propio; la prueba 8/8 corresponde a esa tercera
imagen, no a la intermedia. Persisten advertencias de dependencias de terceros
y la advertencia esperada de rutas `../app`/`../src`: este manifiesto sirve para
construir dentro del checkout/imagen, no para publicar un paquete `sdist`
independiente en Hackage. No se ocultaron warnings ni se omitió el compilador.
