# Colaboradores independientes de créditos y splits

Fecha: 2026-09-15. Corte aditivo local; no despliegue.

## Hallazgo y ADR-008

El PUT de contenido creaba `music_party`, pero el GET y la respuesta del PUT
solo recuperaban partes referenciadas por créditos o derechos. El editor
sustituía su estado con esa respuesta y perdía al colaborador sin referencias.
Además, otro editor del equipo no podía reutilizar esa parte externa: el
control de acceso solo reconocía al creador o un crédito de la versión.

`music_release_version_party(release_version_id, music_party_id)` representa
ahora la pertenencia explícita, con PK compuesta y claves foráneas. No asigna
un crédito, una cuenta TDF ni un porcentaje ficticio. La transacción existente
reemplaza la pertenencia junto al contenido, bajo el mismo control optimista.
El GET mantiene la unión con créditos/splits para consumidores legados.
Los permisos de artista/equipo no cambian; una parte privada ajena no se vuelve
accesible por conocer su UUID. Las partes ya asociadas sí están disponibles
para los editores autorizados de esa versión.

La relación reutiliza la protección SQL de contenido aprobado. Un trigger en
la inserción de una corrección copia las pertenencias dentro de la misma
transacción y comprueba que el origen sea del mismo release. No cambia el
clonador de grabaciones, assets, derechos, términos o identificadores.
Las partes sin créditos no se convierten en créditos ni elementos DDEX.

Referencias primarias consultadas en esta fecha:
[triggers PostgreSQL 16](https://www.postgresql.org/docs/16/sql-createtrigger.html)
y [INSERT / conflictos](https://www.postgresql.org/docs/16/sql-insert.html).

## Migración y recuperación

1. Pausar autoría y programación; tomar y comprobar backup. Aplicar base,
   previews y `tdf-hq/sql/2026-09-15_music_version_parties.sql`, en ese orden.
2. La migración recupera exclusivamente vínculos probados por créditos/splits,
   incluso de publicados, antes de instalar la protección de la nueva tabla.
   Es transaccional y reejecutable: un fallo revierte el lote completo; una
   reaplicación no duplica vínculos ni resucita colaboradores eliminados sin
   referencias. No es un backfill paginado: medir volumen/locks en staging
   antes de aplicarlo a un catálogo grande; no se probó a escala productiva.
3. No se asignan automáticamente partes huérfanas anteriores. Conservarlas
   para saneamiento con evidencia; ni fecha ni creador demuestran a qué
   release pertenecían. Tampoco se borran filas globales al quitar un vínculo.
4. Desplegar la API compilada con esta tabla disponible. No hay cambio de DTO,
   dependencias, worker, almacenamiento, variables o frontend en este corte.
5. Rollback preferido: pausar autoría, revertir API y mantener tabla/datos.
   La UI anterior no garantiza mostrar partes sin referencias: mantener la
   autoría apagada hasta recuperar la versión compatible.
6. `2026-09-15_music_version_parties_rollback.sql` elimina solo si **todos**
   los vínculos son reconstruibles desde créditos/splits; bloquea el rollback
   si perdería un colaborador. No usar CASCADE ni borrar partes para eludirlo.
   Reaplicar la migración restaura los vínculos reconstruibles.

No se registró un SHA ficticio en el manifiesto productivo de migraciones.
Ese registro debe hacerse con el commit real antes del despliegue.

## Verificación

- `stack build tdf-hq:exe:tdf-hq-exe --fast`: código 0; recompilación de
  MusicReleaseCatalog, enlace e instalación. Persisten avisos del linker
  sobre `-U` y `-lm` duplicado; no se regeneró el manifiesto Cabal.
- `./scripts/test-music-release-platform-migration.sh`: código 0. Además del
  flujo previo, verifica backfill sobre publicado, reaplicación, ausencia de
  asignación de huérfanos, rechazo de INSERT/UPDATE/DELETE publicados, copia
  a dos generaciones de corrección, eliminación solo del vínculo del borrador,
  rollback seguro/reaplicación y rechazo de rollback con colaborador nuevo.
  PostgreSQL desechable; se eliminó la base/contenedor propios del harness.
- `sh -n` de ambos harnesses, `node --check` del E2E API y navegador,
  `git diff --check`: código 0.
- `stack exec -- runhaskell -isrc -itest test/MusicReleaseSpecMain.hs`:
  **35 ejemplos, 0 fallos**, código 0; incluye el nuevo contrato de parte
  sin crédito/split y los tests previos de dominio, ERN y firma S3. No equivale
  a validación XSD ni conformidad DDEX de un paquete nuevo.
- Integración ampliada: repetición final **15/15 navegador, 18/18 API/HTTPS/S3
  y 8/8 preflight Linux**, código 0. Incluye compra/descarga de bytes originales,
  reembolso/revocación, copia del colaborador a corrección y retiro idempotente.
  El primer intento de integración no salió del
  preflight por sandbox del socket Docker (código 1); se repitió con acceso
  autorizado. No fue un fallo del motor ni se reinició Docker.

La primera matriz conectada quedó en **12/15**, código 1: tiempos agotados en
Firefox tras recargar playlist (02) y Studio (03), y WebKit al entrar en
biblioteca (02). Informe `/private/tmp/tdf-music-browser-results-2tLDXx/results.json`,
SHA-256 `86a6be921c458b0e4a39d01e77f045206ad5697d5388d4b55641ef9042832f19`;
inicio UTC `2026-09-15T17:28:54.620Z`, duración 452034,051 ms, sin skips/retries.
Los snapshots de Firefox contienen la playlist y el colaborador; WebKit sigue
en la página de release. Eso no prueba disponibilidad dentro de los 8 s y no
establece una causa de runtime. La red no presenta errores HTTP en los casos
02; Studio conserva los 403 de radio y el rechazo 400 esperado. Hubo solicitudes
externas bloqueadas por la allowlist y cancelaciones locales al navegar.
No se alcanzaron compra/reembolso/corrección/retiro posteriores al navegador.
El harness limpió sus recursos y conservó el informe.

Se separaron explícitamente las fases de carga: biblioteca espera su heading
tras cambiar URL; tras reload se espera el GET real de playlists o de versión
y se valida su contenido antes de comprobar el DOM. La playlist se localiza
por su nombre, no por un texto compartido entre tarjetas. Estas esperas usan
el presupuesto existente de 90 s del caso; las aserciones de UI conservan 8 s.
Es un cambio de límites efectivos por fase, no una mejora probada de latencia.
No hay reintentos, clics repetidos, respuestas sustituidas ni límites globales
ampliados.

Informe final `/private/tmp/tdf-music-browser-results-twyFTf/results.json`,
SHA-256 `d5bb7a299cdcc746351760f0d37c62a4759ee57e53b03eb167b5725586bc4a46`.
Inicio UTC `2026-09-15T17:43:33.631Z`, duración navegador 296345,699 ms;
15 esperados, 0 fallos/skips/flaky/reintentos. Los cinco casos Studio pasaron
con el colaborador sin cuenta/crédito/split retenido después de recargar.
Los cinco casos de biblioteca no registraron HTTP >=400; visitante solo el
404 esperado del máster. Studio conserva los 403 de radio documentados,
el rechazo 400 de autoridad y el 404 público del borrador, ambos esperados.
No afirmar que toda la red esté libre de errores.

El harness retiró contenedores/redes, objetos sintéticos, certificados y
credenciales temporales propios. Consultas Docker posteriores por etiquetas
de ambos harnesses y nombre del test de migración devolvieron cero recursos;
`lsof` no encontró listener en 4187. Se conservaron los informes locales.
No hubo interacción manual de navegador ni inspección visual de capturas:
se revisaron código, resultados, red y snapshots textuales de fallos.

El E2E API añade parte sin cuenta/crédito/split, guardado por otro editor,
lectura posterior, unicidad del UUID, rechazo del usuario ajeno y del UUID
privado de otro creador, y conservación en una corrección. El caso de navegador
crea al colaborador desde Studio, guarda por la API real y comprueba el nombre
tras recargar, sin modificar créditos o splits ni escribir por fuera de la UI.

Huellas de las fuentes verificadas (SHA-256):

```text
MusicReleaseCatalog.hs                 bfaa21b3563222bdc7920a05217acc82404fc6922f02e7f7c802de3b4ec14e8c
2026-09-15_music_version_parties.sql    d0f28c9c19efb7e54dae79679bd8249ef2c31037b2de1b67e1297e91ca36b366
music_version_parties_rollback.sql     410cef92a69cbec4f601b825323ab0d614273e6f1e26d2d3697d5cdcdd214102
music-player-integration.spec.mjs      cb67062db7bfde428d48b1df3f8683adb582cfe39b628e2e3fb4675ac9f55046
test-music-release-api-e2e.mjs          17927aa664321f1bfe3de2afb6ad73e7980077a689ce71c3b65df51cb3c76b5f
```

La primera matriz 12/15 usó el test anterior SHA-256
`c7f3aed18f42100484c523d7da7f04496aa56a606af448654429e198b0ec4db3`;
el hash de la tabla corresponde a las esperas explícitas de la repetición.

## Límites que no resuelve

Estado histórico de este corte: la continuación de
[datos por versión](versioned-party-details.md) aborda nombres/identificadores
y bitácora de partes. Su evidencia y migración son independientes de esta.

- Los nombres/legalName/kind de partes existentes todavía no se editan por
  `upsertParty`. Los identificadores de partes tampoco tienen aún snapshots
  por versión. No mutar la parte global para arreglarlo: afectaría otros grafos.
- El audit existente registra actor, fecha y cantidades de contenido reemplazado;
  no ofrece todavía un diff completo recuperable de cada edición del borrador.
- No hay pruebas nuevas de DDEX, proveedor/CDN remoto, pagos reales, UI manual,
  lector de pantalla humano, dispositivos físicos o migración de producción.
- El navegador usa el servidor Vite del harness, no un despliegue productivo;
  estas esperas verifican funcionalidad, no objetivos de latencia de navegación.
- Los 403 observados al abrir Studio proceden de `RadioWidget`, que consulta
  `/catalog/genres/items` y `/catalog/countries/items` administrativos. Studio
  usa correctamente el lector público `/catalogs/genres/items`. Corregir la
  radio sigue pendiente; no atribuir esos 403 al selector del editor.
- Carga/procesamiento/revisión completa desde Studio y las demás puertas de
  producción conservan el estado de las entregas anteriores.
