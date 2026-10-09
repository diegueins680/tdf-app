# Diagnóstico de la regresión editorial

Continuación del 2026-09-14 Ecuador / 2026-09-15 UTC. No hay despliegue ni
migración SQL nueva. Se mantienen los cambios previos y los flags cerrados.

## Evidencia y correcciones

La continuación anterior agotó dos veces el límite de 120 s durante el primer
job de audio. No se ha aumentado ese límite ni los TTL o timeouts de SQL.
Una medición local de tres arranques vacíos por ejecutable produjo:

| Proceso | Duraciones observadas |
| --- | --- |
| `true` | 51, 49, 31 ms |
| `perl` | 105, 91, 262 ms |
| `node` | 806, 1.924, 4.411 ms |
| `jq` | 699, 341, 121 ms |

La sustitución de transporte del harness arranca Node para cada GET/PUT/DELETE.
Esta medición demuestra sobrecarga variable del entorno, no la causa única de
los timeouts ni una regresión de rendimiento del servicio en producción.

El worker dispone ahora de `MUSIC_WORKER_DIAGNOSTICS=true`, apagado por defecto.
Emite líneas JSON `music_worker_timing` con etapa fija, inicio/fin, segundos y
código de salida para SQL y procesos hijos. No registra SQL, argumentos,
credenciales, URLs ni IDs de usuarios. Usa `SECONDS` de Bash con resolución de
un segundo: es diagnóstico operativo, no telemetría certificada ni reloj para
leases. Va a stderr original, separado del resumen persistido de errores.

La primera corrida instrumentada ni siquiera alcanzó el worker: `createdb`
agotó 120 s. La sesión de PostgreSQL siguió activa y terminó creando la base
después de morir el cliente. El booleano antiguo `created` seguía en false y
omitía el drop. Se verificó y eliminó únicamente la base sintética exacta
`tdf_music_worker_31866_18f905b891bd4bc8a71fea6eadfac1aa`, con salida 0.
No contenía datos de usuarios; se puede regenerar como fixture, no se conservó
una copia. Las plantillas `template0` y `template1` medían ambas 7.610.895 bytes.
`pg_isready -t 5` también llegó a devolver «no response».

`scripts/lib/music-test-database.mjs` corrige la carrera del harness:

1. Exige un nombre aleatorio con prefijo estricto de pruebas y comprueba que no
   exista antes de asumir su limpieza; jamás reutiliza una base existente.
2. Marca el nombre reservado antes de esperar a `createdb` y asigna a ese
   cliente un `PGAPPNAME` único.
3. Al salir, cancela solo la creación con ese identificador, usuario y nombre;
   espera a que desaparezca su sesión antes de intentar `dropdb --if-exists`.
4. Si no puede confirmar que terminó la creación, falla indicando el nombre
   para conciliación; no informa una limpieza ficticia ni cancela otras sesiones.

Referencias oficiales consultadas el 2026-09-15 UTC:
[variables de libpq](https://www.postgresql.org/docs/16/libpq-envars.html) y
[señales de PostgreSQL](https://www.postgresql.org/docs/16/functions-admin.html).
Enviar una señal no demuestra que haya terminado una operación: por eso la
limpieza comprueba la desaparición de la sesión antes del drop.

Se redujeron tres arranques de jq a uno por derivado de audio: diez procesos
menos por pista sin omitir hashes ni cambiar el JSON canónico. Una prueba del
helper real verifica preview, loudness, las cuatro tasas y los indicadores de
normalización/inmutabilidad.

Se adaptó además la expectativa de errores PUT del harness al contrato seguro
del cargador: exige el error público genérico y que no aparezca stderr del
proveedor. Las aserciones de estados, bytes, permisos e idempotencia permanecen.

## Verificación y alcance

- Diagnóstico del proceso hijo y señal TERM: **5/5**; conserva salidas 0/7/143, stdout y silencio
  por defecto, y no imprime el argumento secreto sintético.
- Limpieza de bases: **4/4** unitarias con comandos sustituidos explícitamente;
  incluye respuesta de creación perdida, sesión tardía, doble cleanup,
  base ya existente y rechazo de nombres amplios/inyección. No son una prueba
  de recuperación real ante caída de PostgreSQL.
- Metadatos de derivados: **1/1**, las cuatro tasas verificadas contra el helper
  real. El conjunto local final de diagnóstico/limpieza/metadatos pasó **10/10**, código 0.
- La corrida inicial instrumentada falló en `createdb`, no es un verde del
  worker. La base huérfana de esa corrida se eliminó y los procesos ajenos no
  se reiniciaron ni detuvieron.

El E2E real API/S3 ahora configura partes de 5 MiB en el worker para el máster
sintético de más de 16 MiB. Exige el recibo real `mode=multipart`, el número de
partes esperado y luego los hashes/estados de todos los assets. La API conserva
su política propia de multipart. Los pagos siguen siendo evidencia canónica
sintética; esto no autoriza ni prueba pagos reales o CDN.

Repetir:

```sh
node --test scripts/__tests__/music-test-database.test.mjs scripts/__tests__/music-worker-timing.test.mjs
MUSIC_WORKER_DIAGNOSTICS=true node scripts/test-music-release-worker-runtime.mjs --only=complete
node scripts/test-music-s3-local.mjs --with-api
```

La repetición instrumentada alcanzó el worker, pero volvió a agotar el límite:
123.326 ms observados incluido cierre del proceso. Registró GET de 12 s, pipeline
de audio de 31 s, cargas de 1–3 s y llamadas SQL de hasta 16–22 s, todas las etapas
que terminaron con código 0. Los tiempos SQL incluyen el cliente/proceso/conexión,
no son una medición aislada del motor; heartbeat y job pueden solaparse.
No sumar esas duraciones como si fueran estrictamente secuenciales. El corte
ocurrió antes de completar la regresión editorial. La inspección posterior no
mostró sesiones del creador/worker de esa prueba.

El primer E2E API/S3 de esta continuación falló al arrancar MinIO, sin alcanzar
API/worker. Su contenedor fue eliminado. El diagnóstico de salud ahora informa
último código HTTP/error y estado/exit/OOM del contenedor, sin volcar logs que
puedan contener credenciales. La repetición final pasó **18/18, código 0**:
17 escenarios de almacenamiento y el flujo completo API/PostgreSQL/FFmpeg.
La API cargó el máster en dos partes según su propia política; el worker lo
promocionó por multipart con la configuración de 5 MiB, exigida por el recibo
real. Pasaron preservación de SHA-256, cuatro calidades, preview configurable y
reprocesado, bloqueo del preview anterior, publicación territorial, biblioteca,
compra/entitlement sintéticos, descarga del original intacto, corrección y retiro.
El contenedor, objetos, certificado y credenciales temporales se eliminaron.

La carga del host bajó a 23,84 antes de repetir la suite completa del worker.
En esa corrida el escenario editorial de 120 s sí pasó, sin ampliar límites.
La corrida detectó una regresión introducida por los helpers de diagnóstico:
TERM devolvía 0 en vez de 143. Se reprodujo en la integración aislada; la prueba
mínima sin PostgreSQL no la reproducía. `finish_worker` compartía el nombre
`result` con variables locales de los helpers; se aisló su estado en una variable
local propia `worker_exit_status`. La integración TERM aislada pasó entonces:
143, hijo terminado y un único reintento.

Sobre ese código final, ambas suites se repitieron completas:

- **Worker: 21/21, código 0**, incluyendo previews/rollback, fallos, promoción,
  recuperaciones, editorial exactamente una vez, recuperación DDEX (estado
  sintético, no certificación de esquema), jobs hermanos, heartbeat de 65 s,
  intento vencido, transferencia obsoleta, cancelación, TERM y supervisor.
- **API/S3: 18/18, código 0**, nuevamente, con promoción multipart y todo el
  flujo integrado indicado arriba. No se amplió ningún timeout.

La consulta final confirmó ausencia de bases `tdf_music_worker_%`/`tdf_music_s3_%`
y de contenedores con la etiqueta `tdf.test=music-s3`, ambas con salida 0.
Esto cierra la regresión editorial local pendiente, no las puertas de producción.
Los fallos previos quedan conservados como evidencia histórica.

No usar las pasadas de código anterior como aceptación del worker actualizado.

Huellas SHA-256 del código congelado para estas verificaciones:

```text
467eeea4a16930dd59f41a02937462eb866ed86c3c41c77be7ec2090885466b2  scripts/run-music-release-worker-once.sh
8c55bbb8607ab1c0d229dc8485999cb8decba00b4580f1c2dec18fee62243b7a  scripts/lib/music-test-database.mjs
a4739f0d55893ffd53b36e2473eb13b73083b4e138cc954dadb424bc90c1a218  scripts/test-music-release-worker-runtime.mjs
8550fca34ccdb006ea60b116b65d3542b6c8ca1e55d944e8661c3f687502cd5f  scripts/lib/music-api-s3-probe.mjs
ed9db064cc7f635972af573614d2614e0a86913971ab79471ad087243b695127  scripts/test-music-s3-local.mjs
907a0871805a5fe6eb121d0917004b2bcca4ab2ee14090a9789ec55d1a425480  scripts/__tests__/music-worker-timing.test.mjs
```

Rollback: desactivar el diagnóstico no cambia los jobs. Revertir estos helpers
de pruebas no necesita rollback de datos, pero reintroduce la carrera descrita.
Para rollback de producción siguen aplicando las migraciones/flags y límites
del [runbook multipart](worker-multipart.md).
