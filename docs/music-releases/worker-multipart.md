# Transferencias multipart del worker

Implementación local del 2026-09-14 (Ecuador; 2026-09-15 UTC). No desplegada.

## Decisión y fuentes

La API ya carga por multipart, pero el worker promocionaba originales y subía
derivados/paquetes con PUT único. Se reemplazó ese punto común por
`scripts/music-s3-upload.pl`, usando Perl core, curl SigV4 y xmllint, ya disponibles
en la imagen. No se cambia de proveedor, SDK del backend ni esquema canónico.

Fuentes primarias consultadas el 2026-09-15 UTC:

- [AWS: límites multipart](https://docs.aws.amazon.com/AmazonS3/latest/userguide/qfacts.html):
  hasta 10.000 partes, de 5 MiB a 5 GiB, salvo la última.
- [AWS: completar multipart](https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html):
  completar exige números/ETags ordenados; un HTTP 200 puede contener un error.
- [AWS: UploadPart](https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html):
  SigV4 permite comprobar el payload mediante `x-amz-content-sha256`.
- [AWS: abortar cargas](https://docs.aws.amazon.com/AmazonS3/latest/userguide/abort-mpu.html):
  los segmentos incompletos necesitan aborto o políticas de ciclo de vida.
- [Cloudflare R2: cargas](https://developers.cloudflare.com/r2/objects/upload-objects/):
  PUT único hasta 5 GiB; multipart hasta 5 TiB; partes iguales salvo la última.
- [curl: opciones](https://curl.se/docs/manpage.html): firma SigV4, configuración por
  stdin, reintentos acotados, límites por petición y validación TLS.

Estos límites no certifican compatibilidad contractual ni pruebas en esos proveedores.

## Contrato operativo

| Configuración | Predeterminado | Rango admitido |
| --- | --- | --- |
| `MUSIC_WORKER_MULTIPART_THRESHOLD_BYTES` | 67.108.864 (64 MiB) | 5–256 MiB |
| `MUSIC_WORKER_MULTIPART_PART_BYTES` | 67.108.864 (64 MiB) | 5–256 MiB |
| `MUSIC_WORKER_TRANSFER_TIMEOUT_SECONDS` | 3.600 s | 1–86.400 s |

Por debajo del umbral usa PUT firmado; desde el umbral, partes secuenciales. Se
rechaza antes de crear una carga si harían falta más de 10.000 partes. A 64 MiB,
un máster de 8 GiB requiere 128 partes. Esto es cálculo de capacidad, no un ensayo
real de 8 GiB. El límite canónico de másteres no cambia; los paquetes pueden
contener varios másteres y deben dimensionarse por separado.

La lectura usa bloques de 1 MiB; se conserva un solo archivo temporal de parte,
de hasta el tamaño configurado. Se necesitan además los archivos de trabajo que
ya usa FFmpeg/DDEX: no se promete procesamiento multimedia sin disco ni memoria
constante para todo el pipeline. El manifiesto XML crece con el número de partes.

El helper calcula SHA-256 de origen, firma el payload de cada parte y calcula un
SHA-256 incremental de todos los bytes transferidos. Antes de completar compara
ambos hashes y detecta truncamiento/crecimiento. Un cambio bloquea la finalización.
El ETag se usa como evidencia opaca de partes, **no** como SHA-256 del objeto.
La prueba S3 vuelve a descargar objetos y compara bytes; producción no hace ese
GET adicional en cada transferencia.

PUT/UploadPart reintentan hasta tres veces dentro de los límites de curl. No se
repiten partes ya aceptadas en la misma ejecución. Crear/completar no se repite
a ciegas ante una respuesta perdida: el job queda fallido/reintentable y comienza
una carga nueva en su siguiente intento. Las claves canónicas/content-addressed
no cambian; las transacciones siguen protegidas por propietario/intento/lease.
Esto conserva bytes y referencias lógicas, pero no evita versiones físicas extra
si el proveedor tiene versioning habilitado.

La renovación del lease sigue en el worker padre. TERM/pérdida de lease detienen
el proceso de transferencia; el helper termina su curl y trata de abortar el ID
exacto conocido, con hasta 15 segundos para esa petición. Reservar al menos
30 segundos de gracia operativa para la terminación. No se publican metadatos
desde intentos vencidos. El aborto no necesita permiso para borrar otros objetos.

Solo se aceptan endpoints HTTPS de origen, sin credenciales/query/path. Se
codifican las claves y el upload ID; no se siguen redirecciones. TLS se verifica,
`.curlrc` se ignora y las credenciales llegan a curl por stdin, no por argumentos
ni archivos temporales. Las respuestas XML se limitan a 1 MiB, rechazan DTD/NUL y
exigen un resultado con un único campo esperado. Los errores no imprimen
respuestas completas del proveedor, URLs firmadas ni secretos.

## Retención, permisos y recuperación

- Añadir al principal limitado del worker permisos de creación, carga de partes,
  finalización y `AbortMultipartUpload` únicamente en sus buckets/prefijos.
- Configurar limpieza de multipart incompletos **en todos los buckets de destino**,
  no solo cuarentena. Elegir una ventana mayor que la duración máxima operativa
  de un job. Verificar esta política en staging antes de activar cargas grandes.
- SIGKILL, apagado, pérdida de la respuesta de creación o fallo de aborto pueden
  dejar partes en storage. No existe checkpoint durable del upload ID del worker;
  se requiere lifecycle y conciliación por proveedor. El navegador/API sí mantiene
  su contrato independiente de carga reanudable.
- Si se pierde la respuesta de finalización, el objeto podría existir. Mantener
  la conciliación de objetos sin fila y su ventana de seguridad; nunca borrarlo
  automáticamente por el mero fallo del job.
- Object Lock/bucket locks y reintentos sobre una clave existente siguen siendo
  una prueba pendiente del proveedor. No asumir que este cambio los resuelve.

## Verificación

Actualización posterior del 2026-09-15 UTC: se cerró la regresión editorial del
worker con **21/21** y el E2E API/S3 con **18/18**, ambos código 0 y sin ampliar
plazos. Se corrigió además el estado de salida TERM introducido por diagnóstico.
Consultar [la continuación y sus hashes finales](worker-regression-diagnostics.md).
Los resultados y hashes siguientes son históricos de la entrega inicial de multipart.

```sh
node --test scripts/__tests__/music-worker-upload.test.mjs
node scripts/test-music-s3-local.mjs
node scripts/test-music-release-worker-runtime.mjs --only=complete
docker build --check -f tdf-hq/Dockerfile.music-worker .
```

Las pruebas de fallos sin red sustituyen curl explícitamente. El harness S3 usa
MinIO local por TLS y un proxy TLS también local para responder 503 una vez,
alterar bytes, devolver un error XML con HTTP 200 y detener una parte. El proxy
no es una CDN; no recibe secretos externos ni medios reales. Los resultados
finales de esta continuación:

- `node --test scripts/__tests__/music-worker-upload.test.mjs`: **9/9, código 0**,
  repetido después de trasladar las credenciales a stdin; 196,8 s. Incluye
  mutación del origen, ETags duplicados, XML inseguro, error HTTP 200, fallo de
  parte, señal TERM y parámetros fuera de rango.
- `node scripts/test-music-s3-local.mjs`: **17/17, código 0** en la última pasada,
  con el código final. Bytes sintéticos: 5 MiB + 128 KiB; no varios GiB.
  La consulta posterior por etiqueta confirmó ausencia del contenedor temporal.
- La primera ejecución S3 no alcanzó salud del contenedor dentro del plazo.
  Se repitió con permiso explícito para Docker/HTTPS local: pasadas 12/12, 16/16
  y finalmente 17/17 conforme se añadieron aserciones. No contar la primera
  como verde ni atribuir su causa únicamente a la carga del host.
- `docker build --check -f tdf-hq/Dockerfile.music-worker .`: **código 0**, sin
  warnings, al repetir. La primera corrida falló con `frontend grpc server
  closed unexpectedly`. No se construyó ni ejecutó la imagen completa.
- Sintaxis Perl/Bash/Node y `git diff --check`: sin errores. Perl local emite un
  aviso de locale al heredar `C.UTF-8`; los harnesses usan `LANG=C, LC_ALL=C`.
- La primera regresión `--only=complete` del worker agotó el timeout existente
  de 120 s. Se añadieron diagnósticos acotados de stdout/stderr, sin ampliar el
  límite ni modificar las aserciones. La repetición **también agotó 120 s**:
  registró FFmpeg completo, promoción del máster y carga de derivados low/medium,
  pero no terminó el primer job. No hubo un error técnico explícito en stderr.
  El host registró load averages de 235–379 durante esta verificación; eso no
  demuestra por sí solo la causa. Esta regresión queda **sin pasada verde del
  código actual** y debe repetirse con recursos suficientes/investigar si persiste.
  Las pruebas S3 no sustituyen la aceptación editorial ni el E2E API completo.
  La consulta final a PostgreSQL confirmó ausencia de bases temporales
  `tdf_music_worker_%` y `tdf_music_s3_%`; `git diff --check` volvió a terminar
  con código 0 después de actualizar la documentación.

Huellas SHA-256 del código de transferencia verificado:

```text
7ae49c99d26df76b1e1d826d57e91a899556cfe8b1de07b5bac3ca5aa606ee9d  scripts/music-s3-upload.pl
2805e277d43e6a15ab29762584413fd0a6f19fc5615c643c0fc64ad37d6bc780  scripts/run-music-release-worker-once.sh
1aaf303b180f09f71e32f369b8671346161655ccb7946bf10610ae9890b588a4  scripts/test-music-s3-local.mjs
6a4f6a5483d4542be7fffa325a0a6c2fcffa2d15171d09213927e2b31517d958  scripts/lib/music-worker-s3-probe.mjs
85b62ef8c0264b420667a9c519c86cc73aaa926f585638fb97f9b5a70751d005  scripts/__tests__/music-worker-upload.test.mjs
```

## Despliegue y rollback

Este cambio no requiere migración SQL nueva. El worker continúa requiriendo las
migraciones musical base y de previews ya documentadas. Incluir el helper Perl
en la imagen y desplegar gradualmente con flags cerrados hasta la aceptación.

Para rollback: pausar nuevas cargas/jobs, terminar ordenadamente workers nuevos,
comprobar abortos y jobs recuperables, restaurar el artefacto de código anterior.
No volver a permitir objetos mayores que el límite de PUT del proveedor con el
worker antiguo; drenar/reprocesar con el nuevo o mantener esos jobs pausados.
Conservar activos, hashes y auditoría; no hay rollback destructivo de datos.

Pendientes: archivos de varios GiB y paquetes grandes, soak con TTL real,
contenedor Linux completo, IAM/lifecycle/retención del proveedor, CDN y despliegue
con rollback. No usar fixtures de 5 MiB como prueba de esos criterios.
