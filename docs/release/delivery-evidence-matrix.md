# Matriz de evidencia de entrega

Trazabilidad: `SYS-VERIFY-002`. Implementa un inventario de evidencia para las tres
prioridades de la revisión CTO: identidad de entrega, dinero/capacidad y
recuperación/autorización. No sustituye los requisitos de dominio, la admisión de
merge, el informe formal ni los procedimientos de producción.

## Uso

```sh
npm run audit:delivery
npm run audit:delivery -- --mobile /ruta/a/un/checkout/movil
npm run audit:delivery -- --evidence /ruta/externa/evidence.json --require-complete
npm run test:delivery
```

El comando solo lee archivos/Git y escribe JSON en stdout. No accede a red,
proveedores, base de datos ni tiendas. Guardar stdout fuera del checkout evita
que el reporte altere la huella de sus propios inputs.

Salida 0: se generó el inventario; **no significa entrega aprobada**.
Salida 1: entrada inválida o fallo de ejecución.
Salida 2 con `--require-complete`: evidencia faltante/inconsistente, fuente sucia
u otra condición pendiente. `releaseAuthorized` es siempre `false`.

El SHA base y el digest de fuentes identifican también cambios locales sin commit.
Para considerar consistente la matriz se requiere un candidato limpio y un checkout
móvil limpio exactamente en el gitlink del padre. Un `main` móvil más nuevo debe
integrarse y verificarse por el proceso ordinario; esta herramienta no mueve el pin.

## Niveles y criterios

| Prioridad | Evidencia requerida |
| --- | --- |
| Identidad de entrega | Contrato API, backend/configuración/migraciones, browser, binarios firmados en iOS/Android físicos y escritorio instalado |
| Dinero y capacidad | Esquema completo, carreras observadas y controles negativos para pagos, reservas y capacidad de tickets/cursos/merch |
| Recuperación y garantías | Denegación entre actores, restauración conjunta e informe formal que conserve la decisión de release |

Los criterios y comandos existentes están en `scripts/lib/delivery-matrix.mjs`.
No se deben ejecutar todos a ciegas: los runners de PostgreSQL necesitan una base
**desechable**, y el runner HTTP también un backend compilado. Las pruebas de
restauración necesitan un conjunto aislado autorizado. No apuntar estos runners a
producción. La matriz no invoca ninguno automáticamente.

`npm run generate:api` seguido de `node scripts/check-generated-api.mjs` es la
secuencia existente de CI; el segundo comando aislado comprueba bytes/pin, no
regenera ni demuestra compatibilidad semántica. Las pruebas del servidor real son
una obligación distinta. Un modelo de pagos no reemplaza las carreras de SQL/HTTP.

## Recibos

Archivo JSON externo: `{ "schemaVersion": 1, "receipts": { "CHECK_ID": RECEIPT } }`.
Cada recibo tiene:

- `schemaVersion: 1`, `checkId`, `level` exactamente como la fila y `result: "passed"`.
- `rootRevision`, `sourceDigest` de esta matriz; `mobileRevision` igual al pin para
  las filas que usan móvil. No copiar identidades sobre ejecuciones antiguas.
- `completedAt` ISO con zona horaria, `provenance`, `scope`, `limitations` (array).
- `counts: {passed: N, failed: 0, skipped: 0}` con N positivo y casos realmente ejecutados.
- `artifacts: [{reference: "referencia revisable", sha256: "digest SHA-256 real"}]`.

Campos específicos:

- `device-test`: `physicalDevice: true`; pruebas nativas: `binarySha256` y
  `signatureVerified: true`. Una compilación o simulación no cumple.
- `database-test`/`http-test`: `completeSchema: true`, `migrationManifestSha256`.
  Concurrencia: `concurrentScheduleObserved` y `negativeControlsPassed` verdaderos.
- Backend: `observedRevision`, `imageSha256`, `migrationManifestSha256`,
  `runtimeConfigSha256` (configuración no secreta); observación dentro de 24 horas.
- Recuperación: `isolated`, `recoverySetId`, `restored` con `database`, `publicAssets`,
  `privateUploads`, `secrets`; `contentHashesVerified` y `privateAccessVerified`.
  Solo registrar indicadores y digests: nunca secretos ni archivos privados.
- Formal: `releaseDecision` copiada del reporte canónico (`BLOCKED` o `READY`).
  Una ejecución correcta con decisión `BLOCKED` mantiene la matriz incompleta.

Estos recibos son afirmaciones atribuidas a evidencia revisada: el inventario
**no autentica sus autores, descarga artefactos ni recalcula sus digests**. Una
matriz `RECEIPTS_CONSISTENT` solo indica consistencia estructural y de identidad,
no que esas afirmaciones hayan sido verificadas independientemente. La revisión
humana/CI debe consultar logs y artefactos originales. No conectar este resultado
como único gate de despliegue. No convertir conteos de modelos en conteos de
pruebas físicas o garantías de producción.

## Cobertura de fuentes

La huella compartida ahora incluye `tdf-desktop`, `services`, `functions`,
`streaming`, `tidal-agent`, `e2e` y `test`, además de las raíces anteriores. Una modificación en
esos ejecutables invalida la evidencia asociada. La revisión móvil sigue fijada
por el gitlink y debe inspeccionarse en su checkout. Esta huella no constituye un
grafo completo del compilador ni prueba correspondencia con una imagen desplegada.

## Pendientes de esta entrega

La incorporación de criterios no ejecuta por sí misma carreras PostgreSQL,
pruebas físicas, firma, publicación ni restauración. Esas filas deben permanecer
faltantes hasta tener ejecuciones identificables. Los tests de este inventario
usan recibos sintéticos únicamente para verificar negativas y nunca se presentan
como resultados del producto.
