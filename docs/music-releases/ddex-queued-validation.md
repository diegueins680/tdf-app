# Revalidación de exportaciones DDEX encoladas

Fecha: 2026-09-16 UTC. ADR-014; continuación del gate editorial de recursos.

## Hallazgo y decisión

La API validaba las nuevas solicitudes, pero un job encolado antes de instalar
una regla nueva podía llegar al renderer sin repetirla. Además, el registro
de remitente/destinatario podía desactivarse o cambiar entre solicitud y proceso.

El worker ahora ejecuta `music_check_ddex_export` antes de generar archivos y
antes de marcar la exportación `valid`. Comprueba también coincidencia del
hash del snapshot, vigencia y rol de ambas partes y coincidencia de sus DPID
con los guardados al solicitar la exportación. No emite ni verifica códigos
oficiales por sí mismo.

Cada gate y su cambio de estado comparten una transacción y el guard existente
de versión/job/intento/lease. Las filas de registro se bloquean en orden por
UUID con `FOR SHARE`; no se mantiene una transacción abierta durante rendering
o transferencias. El gate final impide que una revocación confirmada durante
ese trabajo termine en `valid`. No sustituye la protección de escritores
canónicos ni promete atomicidad entre PostgreSQL y almacenamiento.

Fuente primaria consultada el 2026-09-16:
[aislamiento READ COMMITTED de PostgreSQL 16](https://www.postgresql.org/docs/16/transaction-iso.html#XACT-READ-COMMITTED).
Las consultas sucesivas pueden observar cambios confirmados; por ello no basta
con la comprobación inicial. La comprobación final y el UPDATE condicionado
utilizan la misma sentencia SQL y los bloqueos de su transacción.

Un paquete ya `valid` conserva la recuperación anterior: devuelve exactamente
sus referencias/hash y cierra el job sin generar ni subir otro. Este camino
no es una auditoría retroactiva ni una revocación de descargas de paquetes
históricos; esas políticas no cambian en esta entrega.

## Fallos, reintentos y privacidad

El job guarda `error_code=ddex_preconditions_failed`; la exportación conserva
`validation_report` con `valid=false`, fecha y `errors[{fieldPath,code,message}]`.
Incluye los códigos canónicos y cuatro adicionales:

- `snapshot_mismatch`: snapshot ausente o hash diferente al solicitado.
- `sender_registry_unavailable`: remitente inactivo, rol o DPID cambiado.
- `recipient_registry_unavailable`: destinatario inactivo, rol o DPID cambiado.
- `export_state_changed`: el estado ya no admite el cambio solicitado.

Los mensajes no incluyen claves de objetos, secretos, SQL ni datos del registro
de terceros. El resumen del job remite al reporte, sin copiar los campos
privados del renderer. Los fallos no editoriales conservan el manejo anterior.

Se conserva la política acotada del worker: `retry` con demora cuadrática y
`validation_failed` en la exportación; al agotar intentos, `dead_letter` y
`failed`. No se reinicia el contador ni se habilita una reparación automática
de metadatos aprobados. Corregir una versión aprobada requiere otra versión.
Si la causa es un registro operativo, el personal debe resolverla con evidencia;
un dead-letter exige ticket/auditoría, no un reintento ciego.

Si falla el gate inicial, no se invocan renderer, builder ni almacenamiento.
Si falla el final, pueden existir objetos privados y filas de recursos ya
subidos. No se enlazan como paquete válido ni se exponen por descarga DDEX;
conservarlos para reconciliación, sin borrar automáticamente objetos que
otro intento/exportación de la misma versión podría reutilizar.

## Despliegue y rollback

No hay nueva migración ni contrato HTTP. Requiere las migraciones ya
documentadas hasta `2026-09-16_music_resource_graph_validation.sql` para
incorporar las nuevas reglas de procedencia. El checker preexistente permite
funcionar con un esquema anterior, pero **no ofrece las reglas nuevas** sin
esa migración: verificar el esquema antes de reanudar DDEX.

Pausar supervisor/reclamación, esperar o reconciliar leases, aplicar las
migraciones en orden y desplegar la imagen reconstruida. Reanudar primero con
fixtures privados. No mezclar workers anteriores capaces de omitir el gate.
Rollback de código conserva datos/paquetes; mantener DDEX pausado si se vuelve
a una imagen anterior. No revertir el gate SQL como sustituto de resolver un
rechazo. No activar banderas ni usar identificadores reales sin las demás
puertas de licencia, verificación y configuración.

## Evidencia y límites

Resultados finales sobre las fuentes indicadas abajo:

- `node scripts/test-music-release-worker-runtime.mjs`: **22 escenarios**,
  código 0. Incluye transporte fallido, integridad, promoción/rollback,
  generación de audio/arte, los gates DDEX, renovación durante varios TTL,
  intentos vencidos, cancelación y propagación TERM/supervisor. El transporte
  usa el inyector explícito de fallos, no certifica S3 por sí solo.

- Pruebas runtime dirigidas `--only='queued DDEX'`, `--only='valid DDEX'`
  y `--only=complete`: todas código 0. Incluyen rechazo tras actualizar reglas,
  errores conservados hasta dead-letter, recuperación sin volver a generar,
  aislamiento entre versiones, cierre sintético exitoso y revocación durante
  rendering que impide el estado válido.
- Node dirigido: **16/16**, código 0, configuración TLS/imagen, compilación y
  propagación TERM/status/timing. Dos ensayos de limpieza usan dobles de
  comandos explícitos, no son ejecuciones Docker adicionales.
- Imagen Linux reconstruida, código 0:
  `sha256:863c30a2c6cf6c6b9d7f5026c30cc6e7b0abb73c26779c97e3a8a54bc2b1d66d`.
  El renderer no cambió y su compilación se reutilizó desde caché.
- Preflight Linux **8/8**, código 0: verifica hashes contra este checkout,
  audio/arte reales, usuario sin privilegios, entrada corrupta, configuración
  ausente y propagación TERM. No prueba por sí solo PostgreSQL ni S3.
- Sintaxis shell/Node y `git diff --check`: correctos.
- Integración final `npm run test:music-linux-integration`: **8/8 Linux y
  18/18 API/HTTPS/S3**, código 0. La API real crea un job antes de aplicar la
  migración del grafo; la imagen Linux actual lo reclama después y devuelve
  `ddex_preconditions_failed` con el campo cíclico, un intento reintentable,
  cero filas de recursos DDEX y sin modificar otra exportación encolada.
  No usa un renderer ni un transporte sustituidos para ese rechazo.
  También pasan máster original, cuatro calidades, preview, artwork, rangos,
  compra canónica sintética, descarga, correcciones/reintentos y retiro.
- XML del renderer real de esa integración validado localmente con
  `xmllint --nonet` contra el XSD oficial fijado, código 0. Ese XML no es
  la prueba de cierre con dobles ni un paquete completo generado por el worker.
  Artefactos: `/private/tmp/tdf-ddex-api-evidence-Blbayv/`.
- Limpieza Docker verificada por etiquetas: cero contenedores/redes de
  integración y cero contenedores del preflight. El runner retiró objetos,
  certificados y credenciales sintéticos; conserva imagen y XML/TSV.

SHA-256 del corte:

```text
114e2e1fb9da610bc1016886b8f050301d68c13218d84b63fc1ff51b5692e56f  scripts/run-music-release-worker-once.sh
94e1c0906a9a80b757cc677f9bd1cdab7a1bfc5ba6878cacfdb30e30fa8bd7af  scripts/test-music-release-worker-runtime.mjs
b12d4eaa208c4ea3167de24499e6c09275edd60bac7a2d451564730edc06a806  scripts/test-music-release-api-e2e.mjs
01535b16eef4eb7d449c95386e2b13e6fcf5426af359d281a1d934242187bd96  test/fixtures/music-worker/ddex-render.mjs
438d5f66b36297be5ba4bc77d275625ec8c6e228ddb18e22988060af609660a4  test/fixtures/music-worker/ddex-package.mjs
1968fdfd7542e27f1266f09f35d29ddbd2c55451c2cbfcc4ed64ac8d81aa92a6  /private/tmp/tdf-ddex-api-evidence-Blbayv/release.xml
5c45a8018fa1fa38d1ed4721dca75cb992ed2c0dda5a4c6225e73ed119cd2e84  /private/tmp/tdf-ddex-api-evidence-Blbayv/resources.tsv
```

La prueba del gate final utiliza PostgreSQL y worker reales, FFmpeg para
preparar audio/portada y **dobles explícitos** de renderer, builder y transporte.
Los archivos resultantes, el marcador XSD y los identificadores son sintéticos,
no un mensaje ERN conforme ni códigos oficiales. El objetivo es probar el
estado final, los hashes y la revocación, no validar el estándar.

Dos corridas iniciales del nuevo fixture fallaron antes de ejercer el guard:
un original con padre viola una restricción existente. Se corrigió el fixture
a un derivado cíclico, sin debilitar restricciones. La prueba de aislamiento
detectó además una regresión introducida al convertir un reporte vacío a JSON;
se corrigió con `NULLIF`/`COALESCE` y se repite en la suite final.

Sin despliegue, pagos externos, CDN remoto, navegador ni revisión manual de UI.
No hubo cambios al mapeo ERN, a la matriz de versiones o al player en ese corte.
Esa entrega no cerró el paquete DDEX completo, deals pendientes ni saneamiento
de contenido legado ya publicado. La continuación documenta el
[bundle offline real y la corrección del deal](ddex-offline-bundle.md), sin
confundirlos con conformidad de coreografía o aceptación por un destinatario.
