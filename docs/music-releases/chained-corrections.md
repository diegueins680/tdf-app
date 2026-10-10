# Correcciones encadenadas: recursos multinivel

Continuación posterior: [concurrencia y errores accionables](correction-concurrency.md)
añade otra migración después de esta. Las limitaciones de errores 500 y
asignación entre fuentes señaladas al final describen el corte anterior.

Fecha: 2026-09-16 UTC (continuación del 15 de septiembre en Ecuador).

## Defecto y decisión

La integración ERN encontró un error 23514 al corregir una corrección:
`music_create_release_correction` copiaba raíces primero y después ordenaba
todos los demás recursos por fecha y UUID. Una preview puede depender de un
stream, que depende del máster. Las copias comparten `NOW()`; ordenar UUID no
garantiza que el stream exista antes que su preview. El mapa devolvía NULL y
`music_asset_check1` rechazaba la inserción.

ADR-011: copiar por profundidad explícita desde raíces, conservando las
restricciones y la transacción existentes. Se usa un CTE recursivo y un
`ORDER BY depth,id`, no el orden implícito de evaluación. Referencia primaria
consultada el 2026-09-16: [PostgreSQL 16, consultas recursivas y orden de
recorrido](https://www.postgresql.org/docs/16/queries-with.html#QUERIES-WITH-SEARCH).

Cada recurso tiene como máximo un padre. Partir únicamente de raíces NULL
impide alcanzar un ciclo: ningún nodo del ciclo puede tener también un padre
en el árbol conectado. La comparación del número copiado contra el número
de recursos fuente detecta ciclos, padres externos y padres DDEX excluidos.
Todos abortan con 23514, sin dejar una versión o recursos parciales. También
se rechaza un recording_id no perteneciente a las pistas fuente, en vez de
convertirlo silenciosamente en NULL.

No se copian bytes, no se renormaliza audio, no se toca almacenamiento ni se
regeneran paquetes DDEX. Se conservan locators privados, hashes, estado técnico
y procedencia; los nuevos UUID enlazan exclusivamente el grafo corregido.
Las partes versionadas siguen copiándose mediante el trigger existente.
Los términos de publicación deben aceptarse de nuevo.

## Migración y recuperación

1. Mantener autoría/revisión y creación de correcciones pausadas; tomar backup
   y verificar restauración en el entorno objetivo.
2. Aplicar las cuatro migraciones musicales previas y después
   `tdf-hq/sql/2026-09-16_music_correction_asset_graph.sql`.
   Es transaccional e idempotente; sustituye solo la función, sin backfill ni
   modificación de datos o migraciones ya existentes.
3. Probar el flujo antes de reabrir autoría. No cambia el contrato HTTP ni
   requiere recompilar API/worker por este cambio SQL.
4. Si hay que revertir, mantener correcciones deshabilitadas y ejecutar
   `2026-09-16_music_correction_asset_graph_rollback.sql`. Restaura exactamente
   el cuerpo anterior; no elimina versiones, assets, auditoría ni objetos.
   **Reintroduce la limitación multinivel**, por lo que no es seguro reabrir
   correcciones hasta reaplicar la reparación.
5. Tras disponer de un commit real, registrar la migración y checksum en el
   manifiesto de producción siguiendo el procedimiento existente. No se
   inventa un SHA de introducción ni se despliega desde el árbol sucio.

## Verificación

Resultados finales del 2026-09-16 UTC, todos con código 0:

- `./scripts/test-music-release-platform-migration.sh`: incorpora fixture
  transaccional `tdf-hq/test/sql/music_correction_asset_graph.sql`.
  Reproduce el defecto anterior con UUID adversos y fechas iguales; prueba
  cinco generaciones con cuatro niveles, evidencia de partes, hashes,
  referencias locales y términos no heredados. Rechaza ciclos, padres
  externos/excluidos y grabaciones externas sin registros parciales.
  Aplica dos veces, revierte en base poblada, coteja definición original,
  reproduce de nuevo el defecto y reaplica; también rollback vacío.
  **Pasó completo**: el fallo antiguo se reprodujo dos veces y la regresión
  reparada pasó dos veces, sin desactivar restricciones ni triggers.
- `npm run test:music-linux-integration`: usa la migración nueva y ahora
  crea por HTTP una corrección de la corrección aprobada, repite su clave
  idempotente, comprueba grafo local/bytes/evidencia y continúa hasta revisión,
  rechazo DDEX por rol no soportado y retirada. **8/8 preflight Linux y
  18/18 API/HTTPS/S3**, incluyendo las nuevas aserciones dentro del escenario
  integrado existente (no se cuentan como escenarios adicionales).
- Sintaxis shell/Node, `git diff --check` y comparación byte a byte del cuerpo
  de rollback con la función de la migración original: correctos.
- XML producido por el renderer real en esta pasada: validó localmente con
  `xmllint --nonet --noout --schema
  /private/tmp/ddex-ern432-xsd/zip/release-notification.xsd
  /private/tmp/tdf-ddex-api-evidence-4bDMZ6/release.xml`.
  Se reutilizó el XSD oficial fijado en la matriz DDEX. No se envió a un
  validador externo; el export sigue queued, no es un paquete completo.

No hubo recompilación de Haskell ni de la imagen: no cambiaron sus fuentes.
El preflight verificó los inputs contra el checkout y usó la imagen local
`sha256:77460f8581e16d4904a0f93159b19254c0f2ee7bef65e66d170169f6164ed9e2`,
con renderer Linux
`e02d6fc848ac89e65eab5ec01dc4c53b789c47fba3eef72483b547201012643a`.
Se ejecutaron el pipeline FFmpeg, multipart, ranges, compra sintética,
entitlement, descarga de bytes originales, reembolso y retiro existentes.

SHA-256 del corte verificado:

```text
86e1a2804c306f02965e9a0ccd8116ffa9a659bd5ee9a5d206d1cab29af5293c  2026-09-16_music_correction_asset_graph.sql
9807a820331425a0383be20729d5b02114647aea44fbf4235c7796ffdedbc839  2026-09-16_music_correction_asset_graph_rollback.sql
38b7f28265b110df62c8dcfac76a88ce2c978b2a280b464c7229c08b0ae5175e  test/sql/music_correction_asset_graph.sql
8b1b182f8b4845462dd4a1c54b492d44e5e698046ded51e3af772d1db7f9db85  scripts/test-music-release-api-e2e.mjs
c1d18984e55809a45e262feb22e284988198f09d4c82675c931d765416a7bc72  scripts/test-music-release-platform-migration.sh
9e4b7fd4b099ff14419931143e7725f6a6bbe89a045fe30faa00e5cd1a1af133  /private/tmp/tdf-ddex-api-evidence-4bDMZ6/release.xml
2066aa0bc670f40df8493e602f515a7c4077eff6333bf9ec86b98c040159d4a5  /private/tmp/tdf-ddex-api-evidence-4bDMZ6/resources.tsv
```

Limpieza comprobada con consultas Docker: cero contenedores de estas pruebas
y cero redes etiquetadas de integración. Se conservaron la imagen y XML/TSV;
el harness retiró objetos sintéticos, certificados y credenciales temporales.
Se revisaron código, restricciones y resultados; no hubo navegador ni revisión
manual de UI. Sin commit, PR, despliegue o cambios en proveedores remotos.

## Límites

No se ha desplegado ni aplicado en producción. No constituye prueba de
pagos remotos, CDN, UI, paquete DDEX completo o rollback operativo remoto.
No repara automáticamente grafos legados corruptos: los bloquea.
No cubre concurrencia entre correcciones de fuentes distintas ni cambia la
política de números de versión; requiere una regresión independiente.
Los errores de integridad inesperados aún necesitan presentación accionable
en la API, en lugar de un error interno genérico. Siguen pendientes los
deals comerciales y las demás puertas de producción documentadas.
