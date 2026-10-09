# ADR-019: identidad y atomicidad de eventos de reproducción

Fecha: 2026-09-16. Implementación local; no desplegada. La evidencia de esta
entrega se registra por separado de las corridas verdes anteriores.

## Hallazgo y decisión

El puente de analítica reutilizaba sesiones por pista después de login/logout
o cambio de cuenta. El receptor deduplicaba UUID/secuencia sin comprobar que
pertenecieran a la misma identidad y solicitud. Además, evento e historial se
escribían en transacciones separadas: un fallo podía conservar sólo el evento,
impidiendo recuperar el historial al reintentar. Un evento antiguo podía
sobrescribir la posición más reciente.

Se mantiene el esquema canónico de eventos y el cálculo de elegibilidad
existente. No se añade otro sistema de métricas ni se altera el motor de audio.

- El cliente renueva sesiones/secuencias al cambiar entre visitante y sesión
  autenticada, de cuenta o de credencial. La comparación de credenciales vive
  sólo en memoria, nunca en metadatos, almacenamiento o logs.
- Si localStorage no está disponible, el identificador anónimo se mantiene en
  memoria durante la página. Borrar almacenamiento accesible genera otra
  identidad; no se recupera silenciosamente el identificador borrado.
- SQL exige exactamente una identidad (cuenta autenticada o hash anónimo),
  liga cada sesión a ella y cada secuencia a un evento. Usa advisory locks de
  transacción, primero por UUID de evento y después por UUID de sesión. Cada
  petición escribe un evento; todos los escritores deben pasar por la función
  protegida, con el aislamiento READ COMMITTED utilizado por la API.
- Un UUID repetido sólo devuelve `duplicate` si coincide la solicitud completa:
  identidad, sesión, secuencia, versión, grabación, tipo, posición, delta,
  calidad, territorio resuelto, fecha y metadatos JSON. Un cambio devuelve
  `conflict`, sin escribir. NULL en metadatos SQL se normaliza a `{}`.
- La API confirma evento y actualización de historial en una misma transacción.
  Sólo `inserted` modifica historial; un fallo revierte ambos. Un evento con
  fecha anterior no retrocede posición/versión, aunque un nuevo `play_start`
  sigue incrementando el contador de inicios. Este contador no es de regalías.

## Contrato HTTP y límites de identidad

`POST /music/playback-events` y `POST /music/me/playback-events` conservan sus
cuerpos y respuesta 200 vacía para inserción/replay. Un conflicto devuelve 409:

```json
{"code":"playback_identity_conflict","message":"La sesión, secuencia o evento pertenece a otra solicitud. Al cambiar de identidad usa una sesión nueva; un reintento debe conservar el evento original."}
```

No revela la identidad propietaria. Autenticación, disponibilidad y territorio
se comprueban antes; contenido no público sigue devolviendo 404. OpenAPI y tipos
web generados documentan ambas rutas. El generador raíz omite móvil si su
instalación está incompleta: eso no constituye generación o prueba de móvil.

Un retry debe conservar exactamente su evento, no asignarle otro UUID para
eludir un 409. Cambiar identidad requiere sesión nueva. El territorio se
resuelve en servidor: un replay desde otro territorio confiable puede entrar
en conflicto; no se confía en `territoryCode` del cuerpo para autorizar acceso.
El puente actual no implementa una cola persistente ni entrega garantizada.

El identificador anónimo es proporcionado por el visitante, **no una prueba de
identidad humana**. Conocerlo permite reutilizarlo; esto no da acceso a una
cuenta. No se corrigen aquí relojes futuros, deltas inventados, bots distribuidos
ni todos los abusos de ritmo. La protección contra eventos atrasados usa la
fecha declarada, no acredita tiempo real escuchado. La política de consentimiento,
retención y eliminación sigue pendiente de aprobación. No son métricas de
contabilidad certificada de regalías.

## Migración, legado y despliegue

`tdf-hq/sql/2026-09-16_music_playback_identity.sql` depende de la migración base
musical. En el despliegue completo se aplica después de `music_ddex_operations`.
No modifica eventos ni agregados existentes, no añade secretos ni cambia la
imagen del worker. Preserva la función previa como
`music_record_playback_event_unbound_v1` exclusivamente para rollback.
Esta función y el acceso directo a las tablas no deben exponerse a clientes;
los escritores de aplicación deben utilizar la función canónica protegida.

1. Respaldar/restaurar en staging; pausar ambos endpoints de ingesta en el edge
   y drenar peticiones activas. No usar las banderas generales como si fueran
   una bandera específica de analítica.
2. Aplicar SQL revisado y desplegar todas las réplicas API compatibles. La API
   anterior puede reconocer incorrectamente un conflicto como éxito: evitar
   escritores mezclados.
3. Desplegar UI con rotación de sesiones; clientes antiguos pueden recibir
   409 hasta recargar. Comprobar retry exacto, cambio de identidad y rollback
   de evento/historial antes de reabrir ingesta.
4. Consultar administrativamente `music_playback_session_sanitation`:
   session_id, event_count, first_received_at, last_received_at. Enumera sesiones
   legadas con múltiples identidades sin publicar identificadores de cuentas.
   No reatribuir eventos ni reconstruir métricas automáticamente. Mantener la
   evidencia y resolver el saneamiento mediante un procedimiento aprobado.

Las sesiones mezcladas antiguas rechazan eventos nuevos; un replay exacto de
un evento ya existente sigue siendo idempotente. El diagnóstico no es todavía
una cola con asignación/resolución en UI administrativa.

Rollback: pausar/drenar ingesta, volver a código compatible y ejecutar
`2026-09-16_music_playback_identity_rollback.sql`. Restaura la definición
anterior y retira la vista, sin borrar eventos/historial. Reintroduce el defecto
de identidad: **mantener ingesta pausada** hasta reparar/reaplicar. No ejecutar
el rollback global de tablas sobre datos poblados. No se ha probado este
procedimiento en producción ni contra proveedores remotos.

## Verificación de esta entrega

- Build API con Stack: código 0, binario enlazado e instalado localmente.
- Jest dirigido: 2 suites, 29 tests, código 0. Incluye login/cambio de cuenta/
  logout, localStorage bloqueado en lectura/escritura, borrado y nueva sesión
  tras completar; conserva las regresiones existentes del player.
- TypeScript: código 0. Generación OpenAPI web: código 0; móvil omitido por
  instalación incompleta.
- Build web final: código 0, 12460 módulos, 376939 bytes gzip iniciales y cinco
  preloads. Permanece warning de chunks mayores de 500 kB, sin ampliar límites.
- `node scripts/test-music-playback-identity.mjs` (también
  `npm run test:music-playback-identity`): **3 bloques correctos, código 0**
  contra PostgreSQL local real. Aplica dependencias/migración, comprueba identidad,
  replay, secuencia y legado; aplica dos veces, revierte vacío/poblado, compara
  hash exacto de función y reaplica conservando un evento inmutable. El fixture
  de identidad también está integrado en la suite musical completa.
  La base aleatoria propia fue eliminada. El helper de limpieza pasó 4/4 tests
  unitarios adicionales; estos tests usan dobles de comandos, no otra DB real.
  Cinco invocaciones adicionales rechazaron PGHOST remoto, PGHOSTADDR,
  PGSERVICE, PGSERVICEFILE y puerto no numérico antes de crear una base.
- La suite SQL completa Docker quedó bloqueada al crear el contenedor y terminó
  con EOF/código 125 durante el reinicio. **La repetición completa después de
  recuperar Docker terminó con código 0**: permisos, editorial, derechos,
  publicación/retiro, comercio sintético, analítica, correcciones concurrentes,
  grafo de recursos, DDEX y rollback. Incluye aplicación doble de la nueva
  migración, fixture de identidad, rollback exacto y reaplicación.
- Probe HTTP incorporado a la integración real: identidad/replay, carrera de
  dos propietarios, fallo inyectado en historial y evento atrasado. La primera
  corrida de esta entrega falló con `spawnSync docker ETIMEDOUT` al inspeccionar
  la imagen, antes de ejecutar pruebas. El fallback API/PostgreSQL local pasó
  el probe de analítica pero terminó después por elegir un nombre de base no
  admitido por el guard DDEX. La repetición con el nombre previsto terminó con
  código 1: el backend no alcanzó salud dentro del límite de 600 s, durante
  migraciones generales de arranque, antes del probe HTTP. No se amplió el
  límite ni se determinó la causa de esa lentitud. La corrida Linux/PostgreSQL
  Docker/MinIO sí arrancó la API y pasó nuevamente el probe de analítica,
  además de publicación, previews, descarga y enqueue DDEX. El cierre de la
  corrida completa sigue pendiente. No contar pruebas previas como actuales.
- Lint detectó un import de tipo con sintaxis no permitida en el test; corregido
  a `import type`. Repetición final: lint y 29/29 tests correctos, código 0.
- Sintaxis Node/shell y `git diff --check`: correctos después de corregir un
  separador de continuación faltante en la lista de migraciones del harness API.

Docker no se recuperó inicialmente con el reinicio CLI (timeout al cerrar), ni con cierre
normal/apertura de macOS. Se terminaron sólo los procesos Docker verificados,
con autorización de reinicio, incluido el backend que ignoró TERM; se reabrió
la aplicación. No se borraron imágenes, volúmenes ni datos. Después del arranque,
el socket respondió `OK` y el motor informó 29.8.0. Las consultas acotadas no
encontraron el contenedor de migración fallida ni contenedores Linux musicales.
La suite completa de migraciones pasó después. La repetición Linux/S3 usa
puerto aleatorio independiente: imagen 8/8 y controles S3 independientes
correctos; flujo integrado de API todavía en curso al registrar este avance.

Huellas SHA-256 del SQL probado:

```text
037ab01192fc5d34523d0e4dc25fe4bf6914f826a7f5b4d3addd9f17ea575100  2026-09-16_music_playback_identity.sql
525aff535ac80e3a71694c002df7a9faf533e2fb75f15615e669650ad601c289  2026-09-16_music_playback_identity_rollback.sql
ed91418a104c94c9a277d28a5dfa4da9df4b0d5a31b930663eefe8e70f43cb92  tdf-hq/src/TDF/Server/MusicRelease.hs
004329c88a0e171892b21295bf6464d3922bb379ab9c09239f1566ce6356b169  tdf-hq-ui/src/player/analytics.ts
ea44642f88074faa812887cdbd611d6720eb77fbb8985c6ec681726673c65420  tdf-hq-ui/src/player/analytics.test.ts
8e4cb3c3d1de1746ec692bfeb7da4cc20921ec77c492bfa26bff6c033c07ee43  scripts/lib/music-playback-identity-probe.mjs
a60a990dd03b9233777d241733d086d000b31fb0952b29a4addd84b3bad97f01  scripts/test-music-playback-identity.mjs
25803e5f8fc869220771b168a80530ed4aaa34f2aa023d6fd78c369d27f0f17d  tdf-hq/test/sql/music_playback_identity.sql
```

La prueba de carrera HTTP envía dos peticiones concurrentes; no incorpora una
barrera que demuestre solapamiento de locks. Los fixtures son sintéticos, sin
pagos externos. No se ha hecho UI manual, pruebas físicas, staging/CDN remoto,
despliegue, commit ni PR de esta entrega.

## Fuentes primarias

Consultadas el 2026-09-16:
[PostgreSQL: advisory locks de transacción](https://www.postgresql.org/docs/15/explicit-locking.html#ADVISORY-LOCKS)
y [MDN: localStorage y SecurityError](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage).
La política de rotar sesiones por credencial es una decisión de TDF, no una
exigencia del navegador.
