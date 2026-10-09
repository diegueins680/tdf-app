# ADR-018 — Exportación y job DDEX en una transacción

Fecha: 2026-09-16. Cambio local de API, sin despliegue.

## Auditoría y decisión

ADR-017 dejó explícita una ventana de fallo: la API confirmaba el export y luego
insertaba su job en otra transacción. Un error entre ambos dejaba una exportación
encolada sin procesador. La lectura de idempotencia anterior también era externa
a esas escrituras; dos peticiones podían competir y devolver errores ambiguos.
La búsqueda estaba limitada a la versión, aunque la clave es única por actor.

`createDdexExport` usa ahora una sola llamada transaccional a `runSqlPool`:

1. Lock transaccional por `music-ddex:<actor>:<key>`.
2. Lock de la versión y comprobación de pertenencia al release solicitado.
3. Resolución global de la clave del actor. Se comprueban versión, operación,
   remitente y destinatario, no solo parte de la solicitud.
4. Para solicitudes nuevas: validación editorial/operación, snapshot y créditos;
   registros DDEX bloqueados en orden de UUID, con roles/estado vigentes.
5. Inserción de export y job, comprobación del vínculo y respuesta; un solo commit.

No hay renderizado, S3 ni llamadas remotas dentro de la transacción. El orden
versión antes de job es compatible con el worker. No se modificaron su lease,
pipeline, imagen ni la combinación ERN 4.3.2 / Audio 2.3.1 / AVS 011.

Fuentes oficiales consultadas el 2026-09-16:
[locks transaccionales y de filas](https://www.postgresql.org/docs/15/explicit-locking.html)
y [aislamiento y visibilidad entre sentencias](https://www.postgresql.org/docs/15/transaction-iso.html).
Los locks se conservan hasta finalizar la transacción. La decisión TDF es
serializar primero la clave y luego la versión; las restricciones únicas
siguen siendo la última barrera. Una colisión de hash solo serializa de más:
la identidad se comprueba de nuevo mediante valores completos.

## Invariantes y errores

- Fallo SQL al crear job: ninguna de las dos filas se confirma.
- Error controlado después de escribir: `DdexAbort` sale de `runSqlPool` y
  provoca rollback antes de convertirse en HTTP. No retornar `Left` dentro
  de la transacción después de una escritura, pues eso confirmaría los cambios.
- Solicitudes concurrentes con igual actor/clave/cuerpo: mismo export y un job.
- Otra versión/operación/contraparte con la clave existente: 409 sin nueva fila.
- Otra clave para el mismo export natural: 409; no se reasigna ni consume la clave.
- Destinatarios/remitentes incompatibles o inactivos: falla cerrado; el conflicto
  de inserción devuelve 409 con instrucción de revisar exports y registro.
- Remitente y destinatario idénticos: 400. Release/versión inexistente: 404.
- Errores editoriales conservan 422 por campo y no consumen la clave.

Los cuerpos y formas de respuesta exitosos permanecen iguales. No cambia
ningún cliente generado ni se crea un endpoint alternativo.

## Recuperación de legado y operación

Un replay autorizado de un export `queued` recrea solamente el job ausente,
con el mismo UUID de export. El worker vuelve a aplicar sus gates actuales
antes de generar. Si ya existe un job, no se cambia estado, intentos, output,
fechas ni reservas. Un vínculo inconsistente produce 409 y exige revisión.
No se reinician jobs terminales ni se recrean jobs de exports valid/failed.
No hay backfill masivo ni reescritura de paquetes o snapshots.

Inventario autorizado de solo lectura:

```sql
SELECT e.id, e.release_version_id, e.generated_by, e.status
FROM music_ddex_export e
WHERE NOT EXISTS (
  SELECT 1 FROM music_processing_job j
  WHERE j.job_kind='generate_ddex' AND j.job_key=e.id::text
);
```

Para `queued`, recuperar mediante la solicitud original, mismo actor/clave/cuerpo
y permisos vigentes. Para otros estados o vínculos inconsistentes, investigar
historial y almacenamiento; no fabricar un job ni modificar el estado a ciegas.

## Despliegue y rollback

No hay SQL nuevo: se requieren las migraciones existentes hasta
`2026-09-16_music_ddex_operations.sql`. Pausar solicitudes DDEX, reemplazar todas
las instancias de API y ejecutar smokes antes de reabrir. No mezclar escritores
viejos/nuevos: los antiguos no respetan el lock por clave ni la transacción única.
La imagen worker v5 ya verificada permanece igual.

Rollback de código: pausar solicitudes, volver al ejecutable anterior, conservar
exports/jobs/snapshots/paquetes. No existe rollback de datos que ejecutar en
este corte. Mantener nuevas solicitudes apagadas si el rollback reintroduce
las dos transacciones; no presentar ese comportamiento como seguro.

## Evidencia de esta entrega

- Preflight: 15 OK, 3 advertencias, 0 errores; trabajo ajeno en main conservado.
- Build Stack API: código 0, solo recompila el módulo cambiado y enlaza.
- Node dirigido: 17/17, código 0.
- `npm run verify:formal`: código 0, modelo general del loop y auditoría sin
  errores/críticos; 351 warnings. La auditoría omite archivos no versionados,
  por lo que no constituye una prueba formal de este módulo nuevo.
- Prueba integrada final: **8/8 preflight Linux + 18/18 API/HTTPS/S3**, código 0.
  Pasan rollback SQL y controlado, barrera real con cuatro solicitudes,
  conflicto por clave natural/versión/cuerpo, reparación queued y preservación
  de intentos. La misma corrida genera alta/update/retiro con renderer, XSD,
  recursos, ZIP y S3 reales; descarga privada y recuperación idéntica comprobadas.
  Los fallos SQL/409 inyectados son resultados negativos esperados, no fallos de suite.
- Inspección final por CLI: manifiesto v3/adaptador v5, `deliveryPerformed:false`,
  hashes de paquetes y limpieza. Consultas por etiquetas sin contenedores/redes
  propios restantes; se retiraron objetos/certificados/credenciales sintéticos,
  conservando imagen y evidencia. No se verificó la UI manualmente.
- `git diff --check` y sintaxis Node: código 0.

Evidencia conservada: `/private/tmp/tdf-ddex-api-evidence-TzZq1M`.

```text
caa35d5ab52c36ed052b2739854d8cc2246c0b3c777c86041b727c577b403965  package.zip
2b76ce0ef76fbedac4d4cbe0f141b73d2dddb96fbc7257e746882aae6f3007bc  update-6ee7f44f-601b-42e6-a028-0d5c0b4a8701.zip
ab31d9e4469c6ae4d09b76c626d0ded685fec0608ee5ee18de4462fb0d3ec2df  takedown-a5d61be1-b339-41b7-ab2b-e24d9cc6bb63.zip
```

Fuentes finales SHA-256:

```text
803f46e01864c6b3afd56c46b106c9150251bfa7f2a54d980b3899c7dc5e5b70  tdf-hq/src/TDF/Server/MusicReleaseDDEX.hs
34cedc5cb92ce4e55fac1a3d7ea74ab2cbdeb8e14b69b4504ee3d6e7ca1a1de2  scripts/lib/music-ddex-enqueue-probe.mjs
5d6832805558f20c9e89902a4f8ff53af1cc2d16dba40fc373831075468e2c0f  scripts/test-music-release-api-e2e.mjs
```

Imagen conservada y verificada, sin reconstruir en este corte:
`sha256:c335ca0031529682a682efe9903af1b0d98c1ef5168edf2226bdc1860bb4669b`.

`music-ddex-enqueue-probe.mjs` usa un trigger temporal de fixture que falla la
inserción de job, y después otro que altera su vínculo para provocar un rechazo
controlado. Ambos deben dejar contadores intactos. Una conexión independiente
retiene la versión hasta observar cuatro peticiones simultáneas bloqueadas
(una por versión, tres por advisory lock). Se libera el bloqueo y se exige un
solo export/job. También prueba recuperación de un huérfano queued y conservación
de un intento previo. El trigger se elimina con `finally`; nunca se instala en
producción. No se matan conexiones ajenas: solo el nombre de sesión UUID del test.

Comando integrado:
`npm run test:music-linux-integration -- --ddex-schema-dir=/private/tmp/ddex-ern432-xsd/zip`.
No se envían datos a validadores externos.

No se verifican aquí UI manual, proveedor/CDN remoto, carga sostenida de muchos
administradores, caída física del servidor PostgreSQL ni aceptación DDEX por un
receptor. Las reglas completas de perfil/coreografía siguen pendientes. La prueba
de atomicidad de metadatos no convierte PostgreSQL y S3 en una transacción común.
No hubo deploy, commit/PR, activación persistente de flags ni acceso a servicios
de producción. La integración usa identidades, códigos y pagos sintéticos.
