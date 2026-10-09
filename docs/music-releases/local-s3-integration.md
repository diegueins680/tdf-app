# Integración S3 local por HTTPS

Este test complementa el E2E HTTP de catálogo/editorial y la prueba de fallos del worker. Usa un servidor S3 real y el firmador de producción `TDF.MusicRelease.Storage.S3`, sin reemplazar el transporte. No certifica Tigris, R2, AWS ni una CDN.

## Ejecución

Requiere Docker activo, Node, curl con SigV4, OpenSSL y el entorno Stack del backend preparado. La instalación de la imagen es explícita:

```sh
docker pull quay.io/minio/minio:RELEASE.2025-04-22T22-12-26Z@sha256:a1ea29fa28355559ef137d71fc570e508a214ec84ff8083e39bc5428980b015e
npm run test:music-s3-local
```

El runner no descarga imágenes automáticamente. Usa credenciales aleatorias exclusivas de la ejecución, certificado TLS efímero con SAN para `127.0.0.1` y validación de certificado activa. Publica únicamente un puerto aleatorio de loopback; no expone la consola. Los objetos viven en un tmpfs limitado de un contenedor con nombre único, sin volúmenes de datos persistentes. Al finalizar, incluso si una aserción falla, elimina únicamente ese contenedor y sus archivos temporales. La imagen descargada se conserva para repetir la prueba.

Las operaciones usan conexiones HTTPS independientes; este test no mide keep-alive ni recuperación del pool de conexiones de Node. El contenedor tiene límites de memoria/CPU/procesos y no recibe capacidades Linux adicionales. Docker Hub rechazó la descarga durante la preparación; la imagen se obtuvo desde Quay y se fijó por digest, sin usar `latest`.

No carga `.env`, credenciales de Fly/AWS, proxies del entorno ni endpoints remotos. El puente Haskell acepta solamente el endpoint HTTPS local. No imprime URLs firmadas ni credenciales. El certificado no se instala en el almacén de confianza del sistema.

## Cobertura y límites

- Crear multipart con claves Unicode/espacios y firmador de TDF.
- Transferir una parte de 5 MiB, registrar ETag y repetirla con los mismos bytes.
- Terminar el proceso cliente de firma, recuperar ID/ETag del checkpoint y continuar la segunda parte sin reiniciar la carga.
- Rechazar evidencia de parte incorrecta; completar correctamente y comprobar SHA-256/longitud.
- Denegar GET/listado anónimos, cambio de objeto/método/caducidad y URL vencida.
- Recuperar rangos exactos con HTTP 206 y rechazar rangos fuera del objeto.
- Verificar cabeceras/preflight CORS para el origen local permitido y ausencia de autorización para otro origen.
- Abortar una carga y comprobar que no acepta más partes.
- Ejecutar el helper real del worker con PUT y multipart, comparar SHA-256/bytes,
  repetir la transferencia y comprobar ausencia de sesiones incompletas.
- Proxy TLS local: 503 transitorio con reintento de solo la segunda parte,
  corrupción rechazada por S3, error XML con HTTP 200 y cancelación con aborto.
  Contrato, evidencia y limitaciones en [multipart del worker](worker-multipart.md).

Los bytes son sintéticos; no representan un máster que haya pasado FFmpeg. El checkpoint prueba reanudación del protocolo y no la persistencia de sesiones de la API. El test básico ejecuta el cargador de storage, pero no arranca la API, el navegador, FFmpeg ni el scheduler. No cubre IAM de mínimo privilegio (la identidad local crea el bucket), renovación STS, retención, Object Lock, copias de seguridad, archivos de varios GiB, caché/revocación CDN, costes ni rendimiento regional. Tampoco interpreta CORS como sustituto de autorización.

## Modo integrado con API y worker

Actualización 2026-09-15 UTC: la repetición ampliada pasó **18/18, código 0**.
Ahora exige multipart real también en la promoción del máster por el worker,
además de la carga inicial por API. Incluye siete escenarios adicionales del
cargador respecto a la pasada histórica 11/11. El primer intento de esta
continuación falló en salud de MinIO; ver [diagnósticos y hashes](worker-regression-diagnostics.md).

El modo opcional conecta el E2E editorial existente al mismo servidor S3 local:

```sh
cd tdf-hq
stack build tdf-hq:exe:tdf-hq-exe --fast
cd ..
npm run test:music-api-s3-local
```

Además de las dependencias anteriores, requiere PostgreSQL en `127.0.0.1:5432`, permiso `CREATEDB` para el usuario local y los comandos del worker (Bash, FFmpeg/ffprobe, psql, jq, shasum, xmllint, zip/unzip). Descubre el ejecutable desde `stack path --local-install-root`; no usa una API desplegada ni un binario de otro checkout.

El runner crea cuatro buckets privados únicamente dentro del MinIO desechable, una base con nombre aleatorio y credenciales sintéticas. El E2E usa handlers HTTP reales para crear el release, cargar audio PCM de 24 bits de 62 segundos en varias partes, recuperar evidencia persistida/reintentar, cargar portada y confirmar cada carga dos veces. Después ejecuta el worker real, consulta sus activos sin fabricarlos y continúa con revisión, programación/publicación, biblioteca, permisos de descarga, corrección y retiro. Las URLs de portada/audio autorizadas se consumen por HTTPS; se cotejan bytes/SHA-256 y rangos. La descarga autorizada se compara con el máster original.

El backend de esta prueba se ejecuta con `+RTS -N2 -RTS` para no usar todas las capacidades del host durante seed/migraciones. Esto no cambia el ejecutable ni la configuración productiva. Las respuestas XML del transporte se interpretan con `xmllint --nonet`, rechazando DTD y campos ambiguos; no se decodifican entidades mediante sustituciones parciales.

En este modo **no** se insertan filas ficticias en `music_asset`, no se reemplaza curl y no se modifica `duration_ms` para simular procesamiento. La identidad verificada, términos, metadatos y evidencia de pago/reembolso continúan siendo fixtures explícitamente sintéticos: no prueba Datafast/PayPal ni derechos reales. No incluye navegador/player audible, DDEX, CDN, objetos de varios GiB ni infraestructura remota. Las otras limitaciones de proveedor/retención siguen vigentes.

El modo rápido anterior (`test-music-release-api-e2e.sh` sin endpoint S3 local) permanece disponible y conserva su inyección explícita de activos. No confundir sus resultados con los del modo integrado. El endpoint opcional se restringe a HTTPS en `127.0.0.1`; nunca apuntarlo a producción.

## Regresiones detectadas durante la integración

Las primeras dos ejecuciones integradas terminaron con error, aunque aprobaron los diez casos de protocolo. La primera expuso un error del lector XML del test: no decodificaba las comillas numéricas del ETag. La segunda completó cargas, procesamiento y acceso público con hashes/rangos, pero recibió 404 al descargar un máster comprado: el worker conserva originales inspeccionados en `valid`, mientras el handler exigía `ready`.

La corrección del backend admite `valid` exclusivamente para másteres inmutables fuera de cuarentena en las rutas privadas de descarga, conserva el acceso público separado y exige coincidencia de versión entre activo y permiso/oferta. La comprobación de contrato `node scripts/test-music-download-predicate.mjs` ejecuta el predicado SQL de ambos handlers contra PostgreSQL sin modificar datos: pasó los 14 casos. Un intento anterior agotó el timeout de conexión y no se contó como aprobado. Los tres tests del lector XML también pasaron. Esto no sustituye repetir el modo integrado con el backend recompilado.

El backend corregido se recompiló con `stack build tdf-hq:exe:tdf-hq-exe --fast`: 195 módulos, enlace e instalación local, código de salida 0. Conserva advertencias históricas de la aplicación y del enlazador; no hubo errores. No se aplicaron migraciones productivas ni se desplegó el ejecutable.

## Pasada final integrada — 2026-09-14

`node scripts/test-music-s3-local.mjs --with-api` terminó con **11/11 escenarios y código 0**, después de recompilar la corrección. Incluye los diez casos S3 y el recorrido API completo. Se verificaron máster en dos partes, portada, reanudación persistida, confirmación repetida, dos jobs reales exitosos, cuatro calidades, preview y arte privado. El acceso publicado y la descarga autorizada devolvieron los bytes esperados por HTTPS, con SHA-256 y rangos exactos. Otro usuario recibió 404 al intentar usar el entitlement; repetir la descarga conservó un solo registro. También pasaron la revocación por evidencia sintética de reembolso, corrección versionada, retiro y deduplicación del scheduler.

El runner confirmó su limpieza y las consultas posteriores comprobaron cero contenedores con etiqueta `tdf.test=music-s3` y ausencia de su base `tdf_music_s3_6f9d1fff1e1a4f29bd3201f8a67131df`. Se eliminaron únicamente los recursos sintéticos de esta prueba; la imagen se conservó. No es una prueba de CDN, navegador audible ni pago externo.

Hashes SHA-256 del código mantenido sin cambios durante esa pasada (no sustituyen un commit):

```text
15eade5ab15584082e774d118c0a5e78294a26034cfc6a3299c5bd88d51ffd52  scripts/test-music-s3-local.mjs
3883381a0f2c26bdc2823ca371d1421316a5e25d9548b4502210786448a6fb86  scripts/test-music-release-api-e2e.sh
b1d2037566b62557a860d0f54458348e4153589248d733a6a7491296bf8b245e  scripts/test-music-release-api-e2e.mjs
79736510188bf8fbef31bf68050e0c9ec15737d8ce47af42a45d82b0e4167c4d  scripts/lib/music-api-s3-probe.mjs
5f144e4e5da3a6e325a0a5413ca0f9c087b48ab0f03edce8955e2313b6941df5  scripts/lib/music-s3-test-xml.mjs
bca6f459131b70b0ac181a6b24b68fb7a853ee9c06121b604f2e3581f9c59bc4  scripts/test-music-download-predicate.mjs
31a47d965f551cfd742c583718cf5227de448d2a322a062c67c8ff5512e22c6f  tdf-hq/src/TDF/Server/MusicReleaseAssets.hs
```

## Referencias consultadas el 2026-09-14

- [UploadPart oficial de AWS](https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html): partes, ETag y operación de carga.
- [CompleteMultipartUpload oficial de AWS](https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html): lista ordenada de partes y posible error XML incluso con HTTP 200. Por ello la prueba comprueba el cuerpo y el checksum, no solo el status.
- [Release MinIO fijada](https://github.com/minio/minio/releases/tag/RELEASE.2025-04-22T22-12-26Z). El repositorio figura archivado en 2026: se usa aquí como herramienta de compatibilidad aislada con datos sintéticos, **no** como recomendación de infraestructura productiva ni versión actual mantenida.
