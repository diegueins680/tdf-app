# Validación editorial del grafo de recursos

Fecha: 2026-09-16 UTC. Complementa las correcciones multinivel y concurrentes.

## Hallazgo y ADR-013

La corrección ya rechazaba padres externos y ciclos, pero la validación
editorial anterior permitía aprobar ciertos grafos que después no podían
clonarse. Las claves foráneas prueban existencia, no pertenencia a una versión
ni compatibilidad entre el recurso derivado y su padre.

Se centraliza esa comprobación en `music_check_resource_graph(UUID)` y se
reutiliza en envío, aprobación, programación, publicación y nuevas solicitudes
DDEX. Se conserva el trigger SQL como barrera para escritores distintos de
la API. La API comprueba los errores bajo su bloqueo de versión antes de
crear snapshot o modificar estado y devuelve HTTP 422 accionable.

El recorrido parte de recursos sin padre y usa un conjunto finito de UUID
con `UNION`, no una recursión ilimitada por cada camino. Los nodos que no
alcanzan una raíz son inválidos. Fuente primaria revisada el 2026-09-16:
[consultas recursivas de PostgreSQL 16](https://www.postgresql.org/docs/16/queries-with.html#QUERIES-WITH-RECURSIVE).
Los recursos de salida DDEX se excluyen del grafo canónico.

## Errores y contrato

| Código | Condición |
|---|---|
| `resource_parent_outside_version` | Padre ajeno a la versión o salida DDEX excluida |
| `resource_graph_unrooted` | Ciclo o cadena sin raíz alcanzable |
| `resource_recording_outside_version` | Grabación del recurso ausente de las pistas |
| `audio_recording_required` | Recurso de audio sin grabación |
| `resource_parent_incompatible` | Roles o grabaciones incompatibles; original con padre |
| `rights_recording_outside_version` | Derechos asociados a grabación ajena |
| `rights_evidence_outside_version` | Evidencia ajena a la versión o a su ámbito de grabación |
| `credit_recording_outside_version` | Crédito asociado a grabación ajena |
| `availability_track_outside_version` | Regla asociada a pista ajena |
| `download_resource_outside_scope` | Descargable ajeno a la versión o grabación de la pista |

`POST /music/releases/{releaseId}/versions/{versionId}/validate` devuelve
`valid` y `errors[{fieldPath,code,message}]`. Las transiciones a
`ready_for_review`, `in_review`, `approved` y `scheduled` devuelven 422 con
los mismos errores si fallan los requisitos editoriales. No consumen la
clave idempotente ni crean eventos de transición; se puede reparar el borrador
y reintentar. Suspensión y retiro no quedan condicionados a reparar el grafo.
La publicación sigue protegida por el trigger de base de datos.

Los campos apuntan solo a UUID de entidades de la versión solicitada; los
mensajes no incluyen locators privados, SQL ni UUID externos. Los clientes
web/móvil fueron regenerados desde OpenAPI para estos dos endpoints. Studio
ya presenta los errores de validación; no se modificó ni verificó visualmente
su interfaz en esta entrega. La clasificación de otras transiciones de estado
inválidas y la cobertura OpenAPI del resto del dominio siguen siendo trabajo
separado.

## Migración, saneamiento y rollback

1. Pausar autoría, revisión, programación y reclamación de trabajos DDEX;
   conservar backup y completar el ensayo de restauración del ambiente.
2. Después de `party_details`, `correction_asset_graph` y
   `correction_concurrency`, aplicar
   `tdf-hq/sql/2026-09-16_music_resource_graph_validation.sql`.
   Es transaccional y repetible. Conserva las funciones anteriores para
   rollback; no modifica snapshots aprobados, objetos ni metadatos.
3. Desplegar la API recompilada. No se requiere nueva imagen del worker por
   este cambio. La actualización de flags del worker usa la nueva función
   SQL; solo actualiza estados editables. No reaplicar migraciones antiguas
   fuera de orden sobre estos wrappers.
4. Revisar, con acceso operativo restringido:

   ```sql
   SELECT release_id, release_version_id, state, field_path, error_code, message
   FROM music_resource_graph_sanitation_queue
   ORDER BY release_id, version_number, field_path, error_code;
   ```

   Es una **vista de diagnóstico**, no una cola persistente de tickets ni un
   panel de reparación. No infiere procedencia ni corrige datos automáticamente.
   Las versiones ya publicadas no se suspenden por aplicar esta migración:
   el operador debe evaluar los hallazgos y usar suspensión/retiro autorizado.
   No se invalidan automáticamente URLs firmadas ni caché de contenido legado.
5. Revisar exportaciones DDEX previamente encoladas antes de reanudar. El worker
   de esta entrega original no repetía el gate; la continuación
   [revalidación DDEX](ddex-queued-validation.md) añade la comprobación al
   generar y confirmar. Desplegar esa imagen nueva, no solo la migración,
   para proteger jobs anteriores; no mezclar workers antiguos y nuevos.
6. Registrar checksum y SHA de introducción en el manifiesto de producción
   cuando exista un commit real; no añadir un SHA ficticio.

Rollback: con los escritores pausados, aplicar
`2026-09-16_music_resource_graph_validation_rollback.sql` antes de revertir
las migraciones anteriores. Restaura las funciones previas y elimina la vista
y el nuevo checker, **sin borrar datos**. Retira la barrera temprana: mantener
autoría/revisión apagadas hasta reaplicar. No altera los guards de corrección
multinivel/concurrente que siguen instalados.

## Evidencia de esta entrega

- Backend recompilado tras el handler y otra vez tras actualizar OpenAPI:
  ambos builds Stack terminaron con código 0.
- Haskell dirigido: **51 ejemplos, 0 fallos**.
- Suite PostgreSQL completa: código 0. Diez clases de corrupción verifican
  errores de envío/exportación/vista, flags inválidos y rechazo SQL de envío.
  Un ciclo de dos nodos bloquea aprobación en revisión; reparar la procedencia
  en el fixture permite aprobar una cadena válida de cuatro niveles.
- Rollback restaura la definición anterior, elimina la vista y permite
  reaplicar. El hash de un snapshot aprobado permanece idéntico. Continúan
  pasando las regresiones previas de cinco generaciones y concurrencia.
- Generación de clientes web y móvil: código 0; TypeScript dirigido a ambos
  archivos generados: código 0. No equivale a probar todos los consumidores
  móviles: su instalación sigue incompleta.
- Typecheck web completo, repetido aisladamente: código 0.
- `npm run test:music-linux-integration`: **8/8 Linux + 18/18 API/HTTPS/S3**,
  código 0. Usa la imagen existente
  `sha256:7d5f8904a79d27e88f1d53ba12fe605ba21022e165f622156635f54e3d4f3839`;
  no se reconstruyó en esta entrega. La API se ejecuta en el host.
  Las nuevas aserciones HTTP prueban upgrade sobre snapshot aprobado intacto,
  bloqueo 422 de programación/exportación, diagnóstico legado, envío fallido
  repetido sin cambio de estado/audit, reparación y reenvío con la misma clave,
  revisión/aprobación y un único evento. Se conservan las pruebas previas de
  procesamiento real, rangos, original intacto, compra canónica sintética,
  descarga autorizada, correcciones concurrentes y retiro.
- El runner eliminó sus contenedores, redes, objetos sintéticos, certificados
  y credenciales temporales. Consultas Docker por etiquetas confirmaron cero
  contenedores/redes de integración y cero contenedores de preflight. Quedan
  la imagen y los artefactos XML/TSV de evidencia, no recursos remotos.
- `git diff --check`, sintaxis Node y shell: correctos.
- XML de esta integración validado localmente con `xmllint --nonet` contra
  `release-notification.xsd` oficial fijado: código 0. La exportación sigue
  encolada; este ensayo no genera ni certifica el paquete DDEX completo.

El fixture HTTP aplica intencionalmente esta migración **después** de crear
una versión aprobada con corrupción sintética, para ensayar la actualización
sin reescribir su snapshot. Ese orden excepcional es exclusivo del test, no
una recomendación de despliegue. Los recursos defectuosos son metadatos de
prueba y nunca se publican ni sirven. La reparación por SQL en el fixture
no representa una interfaz operativa de saneamiento terminada.

SHA-256 de fuentes verificadas:

```text
0bd6861a729bb7750be09b11efff3c8a4257b814a6208d900bfc7debfe25281e  sql/2026-09-16_music_resource_graph_validation.sql
0e68b6729957823cdcc98f37a859678a94843489a93b2091f44e65921effdaec  sql/2026-09-16_music_resource_graph_validation_rollback.sql
87506fe49731e3eacddf2814a6bec9515fec89c67811ce43568207703a746af9  test/sql/music_resource_graph_validation.sql
22a46b7dc9e189159fc64ff629afebde61d79f29b2f2df24b3d350aab3635f65  src/TDF/Server/MusicRelease.hs
0485379045851bfead43d4988f6dc561fb3fe04cdee78b160fd8495de941aef1  scripts/test-music-release-api-e2e.mjs
59481599c31c7adc67a96d9b3b398b839f02f7ed7891f44f1fe1ac8c2612fc32  tdf-hq/docs/openapi/api.yaml
81434f7327b51ba6d5b0efc389ce1e0ef76f080ca75b7ed49f3208328dd39151  tipos generados web/móvil idénticos
4635bd36119a4d786308a3b6ea1750e1aaee28f2a23af5ffb7f27e078f3df622  /private/tmp/tdf-ddex-api-evidence-LEfkl3/release.xml
9703cd258447e4b268c3411b240098a0f67cdeedbefd1c4e03c50115f1ccfcaf  /private/tmp/tdf-ddex-api-evidence-LEfkl3/resources.tsv
```

## Límites

Se valida estructura y pertenencia, no autenticidad jurídica ni integridad de
bytes por el mero hecho de tener un grafo válido. No certifica rendimiento
a gran escala ni toda concurrencia editorial. Sin navegador/manual UI,
despliegue, activación de banderas, commit/PR, pagos reales o CDN remoto.
No cierra el paquete DDEX completo ni sus reglas de deals pendientes, ni
las puertas remotas de staging, recuperación y operación.
