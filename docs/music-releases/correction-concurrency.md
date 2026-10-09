# Correcciones concurrentes y errores accionables

Fecha: 2026-09-16 UTC. Complementa la [reparación multinivel](chained-corrections.md).

## Hallazgos y ADR-012

El bloqueo de la versión fuente no coordina dos fuentes distintas del mismo
release. Ambas pueden calcular el mismo MAX(version_number)+1 y una falla
por clave única. La consulta de idempotencia también devolvía una corrección
previa aunque la misma clave se enviara con otro sourceVersionId.

Se añade un bloqueo asesor transaccional por release dentro de la función SQL,
antes del bloqueo de fuente y del cálculo del número. Protege también llamadas
SQL directas. La API mantiene su bloqueo por clave; no se añade un bloqueo
de fila de release que invierta el orden versión→release de publicación.
La garantía presupone READ COMMITTED, la configuración probada, y no cubre
inserts manuales que eludan la función. Otros aislamientos requieren ensayo.

Referencias primarias revisadas el 2026-09-16:
[bloqueos asesores PostgreSQL 16](https://www.postgresql.org/docs/16/explicit-locking.html#ADVISORY-LOCKS)
y [READ COMMITTED](https://www.postgresql.org/docs/16/transaction-iso.html#XACT-READ-COMMITTED).
El bloqueo termina con la transacción; una colisión del hash solo serializaría
dos releases no relacionados, nunca omitiría la exclusión.

La clave queda vinculada al sourceVersionId del evento de auditoría.
Misma clave/fuente devuelve la corrección existente; otra fuente devuelve 409
sin nueva versión. Evidencia antigua sin sourceVersionId falla cerrada.

## Errores y privacidad

Se captura SqlError fuera de runSqlPool, después de revertir la transacción.
Solo mensajes/estados conocidos se traducen a errores del cliente:

| Condición | HTTP / código | Campo |
|---|---|---|
| Ciclo, padre externo o padre DDEX excluido | 422 / correction_resource_graph_invalid | assets |
| Grabación fuera de la versión | 422 / correction_recording_reference_invalid | assets.recordingId |
| Fuente que dejó de admitir correcciones | 409 / correction_source_unavailable | sourceVersionId |
| Serialización/deadlock revertido | 409 / correction_retry_required | sourceVersionId |
| Clave reutilizada con otra fuente | 409, explicación textual | — |

El JSON contiene message y errors[{code,fieldPath,message}]. Nunca expone SQL,
locators privados ni identificadores del fallo. Grafos inválidos requieren
soporte: no se inventa ni elimina procedencia. Errores SQL desconocidos no
se ocultan bajo 422. OpenAPI y los clientes generados incluyen este endpoint.
La cobertura OpenAPI del resto del dominio musical sigue pendiente.

## Migración y recuperación

Con autoría/revisión pausadas y backup probado, aplicar después de asset_graph:
`tdf-hq/sql/2026-09-16_music_correction_concurrency.sql`.
Es transaccional y repetible, sin backfill ni cambios de objetos o datos.
Desplegar la API recompilada para errores seguros y vinculación de claves.
El dominio Haskell compartido exige reconstruir el worker para su gate de
hashes, aunque no cambian transformaciones de audio ni mapeo DDEX.

Rollback: pausar correcciones y aplicar
`2026-09-16_music_correction_concurrency_rollback.sql` antes de revertir
asset_graph. Restaura exactamente la función previa, conserva la reparación
multinivel y los datos, pero **reintroduce la carrera**. Mantener correcciones
apagadas hasta reaplicar. Registrar checksum y SHA de introducción en el
manifiesto de producción solo cuando exista un commit real.

## Evidencia

- Haskell dirigido: **51 ejemplos, 0 fallos**, con cinco casos nuevos de
  clasificación/no filtración.
- Backend y renderer: ambos `stack build ... --fast`, código 0.
- Imagen Linux construida, código 0:
  `sha256:7d5f8904a79d27e88f1d53ba12fe605ba21022e165f622156635f54e3d4f3839`.
  Renderer Linux:
  `6f3032a55f3f950d061fa1ce28bc2287edf8cd2299535d28e9384abdfe548493`.
- Suite de migración completa, código 0. El runner
  `scripts/test-music-correction-concurrency.mjs` observa con pg_blocking_pids
  dos sesiones: transactionid→23505 antes, advisory→dos versiones distintas
  y consecutivas después. Repite ambas tras rollback/reaplicación, coteja
  definición previa y conserva la regresión multinivel y rechazos atómicos.
- Generación web correcta. El wrapper raíz omitió móvil por instalación
  incompleta; `npm --prefix tdf-mobile run generate:api` usó el generador
  existente en raíz y terminó correctamente, sin instalar dependencias.
- TypeScript dirigido a ambos archivos generados: código 0.
- La sincronización del cliente móvil incluye deriva anterior del contrato
  canónico (diff amplio), no solo este endpoint. No se editaron manualmente
  tipos ni se tocaron sus cambios de pantalla ajenos. La instalación móvil
  incompleta impide afirmar typecheck de todos sus consumidores o ejecución.
- Typecheck completo web (`npm run typecheck --workspace=tdf-hq-ui`): código 0.
- Integración final `npm run test:music-linux-integration`: **8/8 Linux y
  18/18 API/HTTPS/S3**, código 0. Nuevas aserciones dentro del escenario API
  existente: cuatro creaciones paralelas entre dos fuentes y artista/equipo,
  números consecutivos; cuatro reintentos paralelos devuelven un solo UUID
  y un evento de auditoría; otra fuente/misma clave obtiene 409 sin versión.
  Dos intentos sobre un grafo cíclico aprobado de prueba obtienen 422 seguro,
  conservan los conteos de versiones/recursos/grabaciones/créditos/partes/audit
  y un usuario ajeno obtiene 403. Pasaron además compra sintética, reembolso,
  descarga de originales, DDEX por snapshot y retirada existentes.
- El XML de esa ejecución valida contra el XSD oficial fijado, con xmllint
  local y `--nonet`; export sigue queued, no equivale a paquete DDEX completo.
- Sintaxis shell/Node, comparación exacta del rollback y `git diff --check`:
  correctos. Consultas finales Docker: cero contenedores/redes de pruebas;
  se conservaron imagen y XML/TSV, no credenciales/certificados/objetos temporales.

SHA-256 del corte verificado:

```text
cd5076e4bd10bc38d19a212499b2076e3cf2c6b8724a049a1d919767ea2823ae  sql/2026-09-16_music_correction_concurrency.sql
d12328bd05497c5d77fc2e6cf83c4e7af4f0a6ae82ac6cdaae7c909f36a1a277  sql/2026-09-16_music_correction_concurrency_rollback.sql
bee841ec6ea00e6cd1cb585013ce02a914743cd9b99e57d575b0e7462350c706  src/TDF/Server/MusicRelease.hs
0f22bc111131e3a52d3d68bd6d9614c4b1005dd51dae59ac9fea1b92f0b7a193  src/TDF/MusicRelease/Domain.hs
4eaa1e0ab054775eda5de94a7ad415d4268c69b0ee31b71fbabcc2efe85d977d  scripts/test-music-correction-concurrency.mjs
3cc59e0482f33ffe5230825891d77bbc833ff00933b93a916f53b76cbcbf196d  scripts/test-music-release-api-e2e.mjs
9f68681ab7b08747e544efbac2709ad12730766b375880ddc4b8fd09494bc543  tdf-hq/docs/openapi/api.yaml
25c2e721a702ee1faf48927e26c6d13ed863394e788d83311614b4b6cc4ae2ce  tipos generados, web y móvil idénticos
5b233915619ab712efaf7df3e1fdf14c9f1bfdb32782deeb23e6ada1a5ac6439  /private/tmp/tdf-ddex-api-evidence-7b54eM/release.xml
4ad0ae3576429c6172059930b9f223e299852532a88fa041bcd6d8d4b2c9ace5  /private/tmp/tdf-ddex-api-evidence-7b54eM/resources.tsv
```

Se revisaron código, consultas y resultados; no hubo navegador ni revisión
manual de UI. La instalación móvil incompleta no bloqueó generar/verificar
sus tipos, pero sí limita la afirmación de compatibilidad de sus consumidores.

## Límites

Sin despliegue, activación de banderas, commit/PR, pagos reales ni CDN remoto.
La carrera SQL controlada no certifica carga ni toda concurrencia editorial.
El fixture HTTP de corrupción se inserta explícitamente en un borrador de
prueba y nunca se publica ni sirve. La validación editorial de procedencia
y la interfaz de saneamiento requieren trabajo adicional.
No cierra el paquete DDEX completo, sus deals ni las puertas remotas.
