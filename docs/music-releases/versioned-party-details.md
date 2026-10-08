# Nombres e identificadores por versión

Fecha: 2026-09-15. Continuación local, sin despliegue ni activación productiva.

## Auditoría y ADR-009

La pertenencia de partes ya estaba versionada, pero `upsertParty` ignoraba
ediciones de nombres existentes y agregaba identificadores al directorio global.
Además, el snapshot de aprobación no incluía partes y se leía fuera de la
transacción que aprobaba. Corregir directamente `music_party` habría afectado
otras versiones y no habría conservado qué datos recibió el revisor.

Se añaden a `music_release_version_party`:

- `party_details`: JSONB con displayName, legalName, partyKind e identificadores;
- `details_source`: `legacy_observed` o `user_provided`.

El UUID de identidad y el vínculo opcional con la cuenta TDF se conservan.
El API autorizado reemplaza los datos de **esa versión**, no los del directorio.
Los identificadores omitidos se quitan de la versión; los demás no se acumulan
globalmente. Cada identificador nuevo recibe procedencia `provided` y estado
`syntax_valid`, o `unvalidated` si es propietario. No se emiten códigos.
Si conserva el mismo tipo/valor de la versión anterior, conserva su evidencia
existente; modificar el valor no hereda una verificación de autoridad.
La API no admite que el cliente establezca los campos de verificación.

El GET compone la misma estructura de partes con `detailsSource` adicional;
Studio muestra un aviso para datos legados. Al guardar una parte vinculada a
TDF reutiliza solo `partyId`, evitando enviar a la vez `tdfPartyId` contra el
contrato existente. Nombre visible y legal se editan desde los controles ya
conectados. Cambiar un nombre no altera automáticamente el display artist del
release ni de sus pistas: son campos canónicos distintos.

La aprobación obtiene el grafo bajo el bloqueo de la versión y lo guarda en
esa misma transacción. El formato de snapshot pasa a **schemaVersion 2** e
incluye `parties`, ordenadas por UUID. Se mantiene la deduplicación por clave
de transición. Cada guardado de contenido agrega `parties_snapshot` a la
bitácora existente con actor y fecha; se puede reconstruir la evolución de las
partes a partir de esta entrega, no las ediciones antiguas que nunca se guardaron.
Las correcciones copian datos y procedencia; editar la copia no modifica el
snapshot ni el hash aprobado del origen.

Referencias primarias consultadas:
[JSONB PostgreSQL 16](https://www.postgresql.org/docs/16/datatype-json.html) y
[bloqueos de PostgreSQL 16](https://www.postgresql.org/docs/16/explicit-locking.html).
JSONB conserva valores, no bytes de representación JSON; el hash de aprobación
sigue calculándose con la serialización JSON de `hashValue` existente. No se
afirma conformidad con un estándar externo de canonicalización JSON.

## Migración y despliegue

1. Detener escritores de autoría/revisión y programación, tomar/restaurar un
   backup en staging y aplicar las migraciones de base, previews y pertenencias.
2. Aplicar `tdf-hq/sql/2026-09-15_music_party_details.sql`. Usa una transacción
   y bloqueos exclusivos sobre versiones/pertenencias para añadir columnas,
   copiar los datos legados y reinstalar la protección de aprobados sin una
   ventana de escritura concurrente. Es reejecutable, pero no paginada: medir
   volumen y duración de locks antes de aplicarla a un catálogo grande.
3. El backfill marca lo que **observa ahora**, no inventa una reconstrucción
   histórica ni reescribe `immutable_snapshot` o `snapshot_sha256` anteriores.
   Se exige reenviar los datos por la API de contenido autorizada antes de
   enviar a revisión; Studio pide revisarlos. El autoguardado constituye ese
   reenvío, no una verificación por autoridad externa. La aprobación humana
   y las declaraciones legales siguen siendo necesarias.
4. Desplegar API y UI compatibles antes de reactivar la cohorte. No mantener
   escritores viejos junto a los nuevos: no conocen estas columnas. La
   migración no activa banderas ni cambia variables, proveedor o imagen worker.
5. Registrar el checksum y SHA de introducción reales en el manifiesto de
   migraciones cuando exista el commit. No se agregó un SHA ficticio.

Rollback preferido: mantener datos/esquema y autoría apagada, volver a código
compatible o corregir hacia delante. El rollback SQL
`2026-09-15_music_party_details_rollback.sql` solo elimina columnas si son datos
legados reconstruibles, no hay datos `user_provided` ni aprobaciones v2.
Rechaza perder evidencia nueva. No borrar partes para eludirlo. Ejecutar en
orden inverso de dependencias si también se retiran migraciones anteriores.

## Frontera DDEX

Continuación posterior: [créditos versionados en ERN](ddex-versioned-credits.md)
incorpora partes/créditos del snapshot v2 y validación local XSD. El texto
de este apartado conserva el alcance histórico del corte de persistencia.

Los snapshots v1 no contienen evidencia histórica de partes; nuevos paquetes
se bloquean con `party_snapshot_missing` hasta crear/revisar/aprobar una
corrección. No se reescriben paquetes ya generados y su descarga exacta sigue
disponible. El retiro interno no depende de exportar DDEX; esta puerta también
afecta paquetes nuevos de retiro de versiones legadas y exige el saneamiento
correspondiente antes de usar entrega externa.

Esta entrega **no amplía el renderer ERN**: actualmente representa artista y
sello desde los display fields de release/pista. Incorporar créditos múltiples
e identificadores de colaboradores del snapshot v2 mediante el adaptador es
un corte posterior. No presentar nombres editables ni el gate de exportación
como prueba de representación completa de créditos en DDEX. No hubo una nueva
validación XSD de paquete ni se cambiaron las versiones ERN/perfil/AVS.

## Evidencia de esta continuación

- Preflight: 15 OK, 3 advertencias, 0 errores (árbol sucio, auth GitHub inválida
  para polling y loop dirigido a main). No hubo cambio de rama/pull/loop.
- Build API `stack build tdf-hq:exe:tdf-hq-exe --fast`: código 0; recompiló
  los dos módulos modificados, enlazó e instaló. Avisos previos del linker
  sobre `-U` y `-lm`; no se regeneró el manifiesto Cabal.
- Migración: primer intento denegado por sandbox del socket Docker (126);
  repetición autorizada código 0. Segunda pasada, después de ampliar casos
  de evidencia de identificadores y cambios del directorio, también código 0.
  Incluye reaplicación, backfill sin mutar snapshots, copia a correcciones,
  bloqueo de edición aprobada, snapshots inválidos, gate legado DDEX y rollback
  tanto seguro como bloqueado por evidencia. Solo PostgreSQL desechable.
- `stack exec -- runhaskell -isrc -itest test/MusicReleaseSpecMain.hs`:
  35 ejemplos, 0 fallos, código 0; no se amplió la suite Haskell en este corte.
- Jest dirigido a Studio: **9/9**, 2 suites, código 0, 8,578 s; incluye nueva
  prueba de reconstrucción del borrador con identidad vinculada y datos locales.
- ESLint dirigido: código 0. Build UI: TypeScript/Vite código 0, 12460 módulos,
  30,72 s de Vite, 376939 bytes gzip iniciales y 5 preloads. Persiste aviso de
  chunks >500 kB; no se amplió el presupuesto.
- Primera integración API/S3: código 1 por consulta **del test** a `created_at`
  de la bitácora, que usa `occurred_at`. Pasaron las aserciones previas de nombres
  e identificadores, pero no se alcanzaron navegador/aprobación/corrección.
  Se corrigió la consulta, conservando las aserciones. El harness limpió sus
  recursos locales. Repetición completa: **15/15 navegador, 18/18 API/HTTPS/S3
  y 8/8 preflight Linux**, código 0. No se ampliaron timeouts ni se añadieron
  reintentos para este corte.

Informe final `/private/tmp/tdf-music-browser-results-tzAe6v/results.json`,
SHA-256 `4c7abb31deb797f384e378b39cf0edad846c2dde8869c4c01c81242487070805`.
Inicio UTC `2026-09-15T19:41:02.511Z`, duración navegador 255025,619 ms;
15 esperados, 0 fallos/skips/flaky/reintentos. Los cinco casos Studio crean
una parte externa, editan nombre visible/legal manteniendo su UUID y leen los
datos tras recargar. Después del navegador pasaron compra sintética,
descarga del original intacto, reembolso/revocación, corrección y retiro.
La corrección cambia nombre, quita nombre legal e identificadores y exige
igualdad exacta de partes/snapshot/hash del origen aprobado. Repetir la clave
de aprobación tampoco cambia el hash.

Revisión de red: casos de biblioteca sin HTTP >=400; visitante solo el 404
esperado del máster. Studio conserva los 403 de catálogos administrativos de
RadioWidget (no del selector público del editor), más el 400 esperado de
autoridad incompleta y el 404 público del borrador. No se afirma red sin errores.

Limpieza: el harness retiró contenedores/redes, objetos sintéticos, certificados
y credenciales temporales propios; las consultas Docker posteriores devolvieron
cero recursos por etiquetas/nombre de ambos harnesses y `lsof` no encontró
listener en 4187. Se conservó el informe local. Sintaxis de scripts y
`git diff --check` terminaron con código 0.

Huellas de fuente (SHA-256):

```text
music_party_details.sql          c1d252c91a23539350015a9c63b28c7b79e7c8f7735c540392303ee20012d0bc
music_party_details_rollback.sql c07dfd35912163cfc3efa7688e0fd0956b677cd8c9ff66e244bf65ad04c60bff
MusicReleaseCatalog.hs           3771062ff9562ae91973e61c41cc25c6c8afac50430933670441d314e095088c
MusicRelease.hs                  c7ddade934a1de238937070cccc10aa3276f4ab9e6dc2b3763a62e0e0c2c9f68
MusicReleaseStudioPage.tsx        3f09b1e6cefe7125024fd6933e5ec46c54d41f78c8df9cb5768b3febdb0443f0
music-player-integration.spec.mjs 2ffa2744931cc356a8df4d5e15705e3e23bfc6531ed0c03cc9b8717b37259b6c
test-music-release-api-e2e.mjs    26829bcf8aa143105532cb98145b39cc0716cf4c18fd90d43a45db6e4609f9ac
```

No hubo UI manual, inspección visual de capturas, dispositivos físicos,
pruebas remotas de CDN/proveedor/pagos, despliegue, commit ni PR. Quedan
pendientes estrés concurrente de aprobación, diffs completos de todo el grafo,
renderer DDEX completo, consultas 403 de radio y recorrido editorial completo
con carga/procesamiento/revisión desde la UI.
