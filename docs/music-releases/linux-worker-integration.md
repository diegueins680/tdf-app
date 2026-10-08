# Worker Linux conectado a PostgreSQL y S3 locales

Fecha: 2026-09-15 UTC. Esta entrega amplía la prueba de la imagen, sin cambiar
el pipeline de producción ni crear recursos remotos.

## Brecha y decisión

La imagen Linux ya pasó los pipelines con red deshabilitada. El E2E HTTP ya
ejecutaba el worker en macOS contra PostgreSQL y MinIO. Faltaba demostrar que
los ejecutables incluidos en la imagen funcionan juntos con libpq, TLS, DNS
interno, almacenamiento real, reservas de trabajos y los handlers existentes.

Se reutiliza el mismo E2E, con una red Docker interna por corrida, PostgreSQL
desechable y MinIO por HTTPS. La API sigue siendo el ejecutable Stack local:
**no es todavía una prueba de toda la aplicación desplegada en Linux**. No se
montan fuentes sobre el worker ni se sustituyen psql/curl/FFmpeg. El preflight
comprueba la correspondencia de la imagen con el checkout antes de usarla.

## Ejecución

Requiere el backend compilado, la imagen descrita en
[verificación del artefacto](linux-worker-image.md) y las imágenes PostgreSQL y
MinIO ya presentes localmente. El comando no descarga ni construye imágenes,
no carga `.env` y no usa secretos del proveedor:

```sh
npm run test:music-linux-integration
```

PostgreSQL se fija a
`postgres@sha256:7958605b474b3d264a969cb3a123d6aa00ad1e1fe9da8a69984dabb704d93317`.
MinIO conserva el digest del [runner S3 existente](local-s3-integration.md).
La etiqueta local del worker se resuelve una vez a su ID inmutable, usado en
preflight y en todos los jobs.

Se generan credenciales y certificados efímeros. PostgreSQL exige SCRAM para
conexiones TCP y el cliente usa `verify-full`; cada worker comprueba también
`pg_stat_ssl`. El CA se monta solo lectura, sin la clave privada. S3 usa HTTPS
con verificación del mismo CA sintético y nombre `music-storage`. No se permite
`host.docker.internal`, red del host ni endpoints arbitrarios. Solo se publican
puertos aleatorios en `127.0.0.1` para la API y el cliente de prueba del host.
PostgreSQL y MinIO se conectan también a un segundo bridge dedicado para esas
publicaciones: Docker no materializó puertos en la primera corrida con red
exclusivamente interna. El worker permanece solo en la red interna. El bridge
de los servidores permite egreso de red; no se afirma aislamiento total de
Internet para esos dos fixtures. No se solicita tráfico remoto desde ellos.

El worker corre como el usuario de la imagen, raíz read-only, capabilities
eliminadas, sin elevación, 2 CPU, 1 GiB RAM y tmpfs de 512 MiB. PostgreSQL usa
su usuario no root y datos tmpfs; las migraciones existentes se aplican solo
en su base aleatoria. No hay migración nueva. La limpieza verifica etiquetas
exclusivas y quita únicamente los contenedores/red de esta corrida, incluso
si falla un job; intenta las demás limpiezas si alguna falla.

## Evidencia y límites

La tercera corrida completa terminó con **código 0**: **8/8 escenarios de
imagen**, conexión PostgreSQL TLS verificada y **18/18 escenarios HTTPS/S3 +
API**. Se ejecutaron los jobs reales de audio, portada y corrección de preview
en contenedores Linux separados, con la misma imagen. Las comprobaciones de
estado confirmaron audio y portada `succeeded`, ambos en el primer intento,
y después el E2E comprobó la nueva preview y la conservación de la anterior.

El E2E verificó máster de más de 16 MiB en dos partes, reanudación, confirmación
idempotente, promoción multipart del original, SHA-256, cuatro calidades,
privacidad del arte/audio, duración y exclusión de previews obsoletas, acceso
publicado por HTTPS con rangos exactos, descarga del máster intacto mediante
entitlement, corrección y retiro. Los 17 escenarios de transporte anteriores
siguen usando el firmador Haskell y uploader del host contra el mismo S3 real;
**no se afirma que todos los clientes o la API estén dentro de Linux**.

Artefacto usado, sin reconstrucción ni publicación:

```text
worker: sha256:401d6874b59bd8337d70a187e09a6429e5dcfc7c5ea1a7cd3d3204a381d7a5cc
PostgreSQL local: sha256:1edc8e87e53194e0cc8006c4e9df9b626c6c72cb43ad3385f0f34af7922065b1
MinIO local: sha256:9d668e47f1fc60ea49af4203deee87a657eb1aa0e2761fee2c7c2d1df282c880
```

También pasaron **20/20 tests dirigidos, código 0**, sin omisiones:

```sh
node --test scripts/__tests__/music-linux-integration.test.mjs scripts/__tests__/music-worker-build.test.mjs scripts/__tests__/music-worker-timing.test.mjs scripts/__tests__/music-test-database.test.mjs
```

Incluyen aislamiento, TLS y credenciales en la configuración, rechazo de rutas
libpq remotas antes de migrar, nombres exclusivos y dos pruebas unitarias de
limpieza con doble de comandos. Estas últimas no se presentan como ejecución
real de Docker. `node --check`, `sh -n` y `git diff --check` pasaron también.

Fallos previos conservados, no contados como verdes:

- Primera: Docker no publicó `5432/tcp` al usar exclusivamente un bridge
  `--internal`. Se agregó el segundo bridge descrito arriba.
- Segunda: PostgreSQL aceptó TLS `verify-full`, pero MinIO no respondió al
  health check (`ECONNRESET`, contenedor activo, sin OOM). Se añadió diagnóstico
  acotado de arranque sin credenciales. La tercera corrida arrancó sin ampliar
  el timeout ni desactivar TLS. No se determinó la causa de ese fallo de
  arranque intermitente; no se afirma que haya sido corregido.

Las tres corridas limpiaron sus recursos. Las consultas posteriores por etiqueta
devolvieron cero contenedores/redes de integración y cero contenedores del
preflight. No se modificaron buckets de TDF ni se crearon credenciales remotas.
No hubo despliegue, commit/PR ni comprobación manual de interfaz; las
verificaciones de estado se hicieron por CLI.

El modo anterior `node scripts/test-music-s3-local.mjs --with-api` también se
repitió completo por compatibilidad: **18/18, código 0**, con el worker del host,
sin omitir procesamiento, preview corregida, publicación, descarga ni retiro.
Se conservan ambos modos; no se eliminó la prueba anterior.
Tras esa repetición, las consultas por etiquetas Docker devolvieron cero
contenedores S3/imagen/integración y cero redes de integración; la consulta por
el nombre exacto de la base del modo host devolvió `0`. No se borraron imágenes,
cachés ni recursos de otras tareas.

Hashes de las fuentes ejecutadas:

```text
0141cde522954cf48f4c978ba683f151901ffe60a4bac0fc129d6b33ea4d4ac0  scripts/lib/music-linux-integration.mjs
1eecd5fbea8c990d57d158e25e45d7700f747b6c275605bc437088429b06edce  scripts/lib/music-api-s3-probe.mjs
a0de91aaa577b457426d0af9cbd32dd807e38898387f55ed3aceea1fb5ba3684  scripts/test-music-release-api-e2e.sh
cd3bea577587ba1486545b5d0c0309fa9d3cb3fdd72994dfe33a32ef8378b2f7  scripts/test-music-s3-local.mjs
10f53159dbd9d78eaf1a356c4fa026695d01e623d9a0e7618c8cd6e1dbab5015  scripts/__tests__/music-linux-integration.test.mjs
```

Se conservan los escenarios de multipart, hashes, derivados, portada, preview,
publicación, acceso anónimo, entitlements, corrección y retiro del E2E existente.
Los eventos de pago son fixtures del dominio canónico: no prueban cobros reales.
Esta prueba tampoco certifica CDN/proveedor remoto, varios GiB, retención,
restauración, conformidad DDEX ni rollout/rollback remoto.
El usuario inicial de PostgreSQL y el principal de MinIO administran únicamente
estos fixtures; no se ha probado aquí una política productiva de mínimos
privilegios de SQL/IAM. La aprobación de esos permisos sigue siendo una puerta
separada antes de conectar datos reales.

## Fuentes oficiales consultadas

Consultadas el 2026-09-15 UTC:

- [Docker: redes bridge definidas por el usuario y DNS](https://docs.docker.com/engine/network/drivers/bridge/).
- [Docker: publicación de puertos y binding loopback](https://docs.docker.com/engine/network/port-publishing/).
- [PostgreSQL 17: libpq, verify-full y PGSSLROOTCERT](https://www.postgresql.org/docs/17/libpq-ssl.html).
- [Imagen oficial PostgreSQL 17, variante Bookworm](https://github.com/docker-library/postgres/blob/master/17/bookworm/Dockerfile).
