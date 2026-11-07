# SYS-MONEY-003: correspondencia y obligaciones pendientes

Estado: **PARTIAL**. La implementación completa y su refinamiento siguen
**UNPROVEN**. Este trabajo aporta un contraejemplo real, una corrección aditiva y
pruebas reproducibles; no promueve un resultado acotado a demostración universal.

## Revisiones y trabajo existente

- Main observado: `a71e3b9ae5a6ac6e80588aa120a70829071b3ce4`.
- Especificación de #581: `580a74560405112ae817748f4d9e796a8ba2a002`.
- Implementación de #551: `9d7551353a7c278cd0937eb57d04a94241318d42`.
- Introducción de la corrección aditiva:
  `493d40252f48643a14ce47c5328a13844f3b837f`.

Las dos PR seguían abiertas al inspeccionarlas. No se atribuye su contenido a
main. #551 ya amplía SYS-MONEY-003 y contiene pruebas de publicación, aritmética,
inmutabilidad, migración y reversión concurrente. Este seguimiento parte de esa
rama para conservar su trabajo; #581 mantiene otro registro y deberá reconciliar
el requisito por su ID al integrar. No deben desaparecer las nuevas obligaciones
por escoger una de las dos versiones del JSON.

El inventario de #581 tiene 123 requisitos completos, todos PARTIAL, 246
obligaciones completas de implementación/refinamiento sin demostrar y 103
requisitos críticos. Ninguno de esos contadores se reduce por estas pruebas.

[critical-triage.json](evidence/2026-10-16-ledger-refinement/critical-triage.json)
enumera los 103 críticos y sus cruces con los archivos modificados de las PR
abiertas, fijando los SHA. Se calculó cada conjunto con
`git diff --name-only <main-observado>...<head-PR>` y se intersectó con los campos
`implementation` y `tests` del registro de #581. Trece requisitos no tienen
ninguna coincidencia. Una coincidencia es una ayuda de búsqueda: puede ser un
verificador compartido, una modificación incidental o una rama antecesora, y
**no acredita una corrección ni su integración**. Tampoco la ausencia de
coincidencias demuestra que falte implementación.

## Contraejemplo en PostgreSQL

El disparador de entradas de #551 usa `SELECT ... FOR NO KEY UPDATE` sobre la
fila padre. Conserva la exclusión durante la escritura, pero no crea una nueva
versión de esa fila que invalide una instantánea anterior.

1. Existe un borrador con entradas USD +1 y -1.
2. A inicia REPEATABLE READ y lee la tabla, fijando su instantánea.
3. B, en READ COMMITTED, inserta USD +1 y conserva su transacción abierta.
4. A intenta publicar y queda bloqueada por B. La prueba observa la relación
   real con `pg_blocking_pids`; no deduce el bloqueo por una pausa.
5. B confirma. A adquiere la fila padre, pero valida las entradas de su
   instantánea anterior: +1 y -1.
6. A confirma `posted`. Una tercera sesión observa saldo USD **1**.

La regresión contra #551 falla con `(0, '', 'posted|1')`: hubo éxito SQL y una
violación real del saldo, no un fallo de infraestructura ni un modelo supuesto.
[Registro original](evidence/2026-10-16-ledger-refinement/lock-only-failure.log).

La misma clase de defecto existe si la escritura ya confirmó antes del intento
de publicar, siempre después de fijar la instantánea. Los controles reproducen
ambos órdenes para INSERT, UPDATE y DELETE.

La migración `2026-10-16_ledger_entry_write_fence.sql` reemplaza ese bloqueo por
`UPDATE commerce_ledger_transaction SET created_at = created_at ... RETURNING
status`. El valor no cambia; se crea una versión MVCC y se conserva la exclusión.
La publicación en READ COMMITTED valida entradas actualizadas; una publicación
con instantánea anterior en REPEATABLE READ o SERIALIZABLE aborta con 40001.
El disparador de publicación escucha `status/reversal_of`, por lo que este
UPDATE no lo invoca recursivamente. Cada modificación de entrada añade una
escritura física/WAL: el coste y la contención requieren evaluación bajo carga.

La migración bloquea escritores durante la comprobación y rechaza historia
publicada vacía o desequilibrada antes de sustituir el disparador. No repara
importes. Conserva intactos los 208 registros anteriores del manifiesto y añade
el 209 con su introducción inmutable. La protección se conserva al revertir la
aplicación; reinstalar el disparador anterior reabre el contraejemplo.

El fundamento de la diferencia entre bloqueo y versión MVCC está descrito en la
[documentación oficial de PostgreSQL 17](https://www.postgresql.org/docs/17/applevel-consistency.html).
Esto respalda el análisis; la evidencia de la corrección son las ejecuciones.

## Evidencia y límites

| Nivel | Evidencia observada | Alcance pendiente |
| --- | --- | --- |
| Modelo acotado | `LedgerPosting.cfg` termina sin error; cinco eliminaciones de guardas producen sus contraejemplos nombrados | Abstrae publicación atómica. No representa sesiones ni instantáneas PostgreSQL |
| Integración PostgreSQL | 24 escenarios: 3 aislamientos × 6 órdenes de entrada/publicación, publicación primero y rollback del escritor | Conjunto finito de intercalados; no prueba de todos los historiales |
| Controles negativos | 6 sustituciones por bloqueo sin escritura: INSERT/UPDATE/DELETE, escritor bloqueante o ya confirmado, todos producen `posted|1` | El control restaura y compara la definición original; no cambia SQL histórico |
| Migración aditiva | Historia vacía y desequilibrada rechazada con 23514, sin cambiar datos ni guarda; dos reaplicaciones limpias aceptadas | Datos desplegados y ensayo operativo real no consultados |
| Aritmética SQL | Los 6 vectores existentes pasan: saldo correcto, incorrecto, monedas cruzadas, vuelta modular, compensación extrema y vacío | No demuestra semántica compilada de todos los escritores |
| Regresiones previas | 5 historias inválidas rechazadas, repetición limpia y 2 carreras de reversión pasan en el script existente | La suite Hspec compilada completa no se ejecutó localmente |

El nuevo ejecutor produce **33 registros**: 24 escenarios, 6 controles y 3
comprobaciones de migración. Emite revisión, hashes de fuentes y función
instalada, versión del servidor y número de migraciones; rechaza un manifiesto
incompleto. El registro marca honestamente el árbol de trabajo modificado.
Los hashes fijan los bytes ejecutados; el SHA padre no certifica esas ediciones.

Los registros están en
[evidence/2026-10-16-ledger-refinement](evidence/2026-10-16-ledger-refinement/).
La ejecución local usó PostgreSQL 16.15 nativo, UTF8 y JIT desactivado. El
contenedor solo permite UID 0: el servidor usó el adaptador de identidad/`stat`
retenido en esa carpeta. Los binarios PostgreSQL no se modificaron y los
clientes/verificador no usaron el adaptador. Este entorno adaptado no sustituye
la ejecución hospedada ordinaria con PostgreSQL 17. Stack/GHC 9.10.3 no estaba
disponible: no se declara una pasada del backend compilado ni del gate global.

## Reproducción

En el fixture PostgreSQL completamente migrado que ya crea el lane de identidad:

```sh
export TDF_LEDGER_POSTING_DATABASE_URL="$TDF_IDENTITY_HTTP_DATABASE_URL"
python3 scripts/__tests__/ledger-posting-snapshot.py --negative-controls
python3 scripts/__tests__/ledger-posting-upgrade.py
psql "$TDF_LEDGER_POSTING_DATABASE_URL" -X -v ON_ERROR_STOP=1 \
  -f tdf-hq/test/integration/ledger_posting_balance.sql
```

`scripts/test-ledger-posting.sh` incorpora la nueva ejecución tras Hspec y las
pruebas anteriores. La URL pasa por el validador canónico antes de cualquier SQL;
solo se admite el fixture local/CI `_test`, sin sobrescrituras libpq. Los controles
cambian temporalmente la función global del fixture y necesitan propietario de
tablas: ejecutar de forma exclusiva en la base sintética del lane. Nunca usar
una base compartida con trabajo real. La limpieza elimina solo filas con un
prefijo aleatorio y restaura las guardas en una transacción.

Para recrear la base se conservan los fixtures existentes
`production-schema-20260814.sql` y `catalog-production-source-fixture.sql`, y el
SQL de `node scripts/render-production-migration-batch.mjs`; el manifiesto
completo se aplica y repite. No se reemplazan por tablas simplificadas.

## Obligaciones de refinamiento que siguen abiertas

| Obligación | Estado y siguiente evidencia necesaria |
| --- | --- |
| SUM por moneda con precisión exacta | Vectores SQL y revisión de `numeric`; falta correspondencia mecanizada para tamaños arbitrarios, recursos y semántica del motor |
| Entradas y publicación comparten un punto de serialización | Reparados los contraejemplos nombrados; falta refinamiento MVCC de todos los intercalados, múltiples escritores y transacciones multiasiento |
| Aborto, rollback y reintento | Rollback del escritor cubierto; falta validar manejo real de 40001/40P01 y retry de toda la transacción en cada llamador, sin repetir efectos externos |
| Escritores reales | Contrastar captura, reembolso y liquidación de depósitos con el esquema completo, incluidos triggers laterales y fallos entre pasos. El helper Haskell sin llamador no los demuestra |
| Reversiones | Los controles existentes cubren estabilidad de la referencia. El modelo no prueba compensación contable exacta del reversor ni decide reglas nuevas para reutilizarlo |
| Autoridad efectiva | Verificar privilegios del rol desplegado; DDL, TRUNCATE, desactivación de triggers y escrituras privilegiadas quedan fuera de la propiedad de DML |
| Entrega | Integrar los SHA revisados por el proceso normal, repetir todos los gates, ejecutar preflight sobre datos desplegados y comprobar funciones/manifiesto efectivos |
| Prueba completa | Una simulación/refinamiento mecanizado de SQL, transacciones y llamadas hacia la especificación; ninguna de las filas anteriores lo sustituye |

## Orden de las siguientes obligaciones por riesgo

| Prioridad | Requisitos / PR existentes | Corrección disponible o vacío | Siguiente comprobación concreta |
| --- | --- | --- | --- |
| P0 | SYS-MONEY-003 / #551 y este seguimiento | Correcciones de publicación e inmutabilidad disponibles; nueva anomalía de instantánea reparada aquí | PostgreSQL 17 ordinario, escritores compilados sobre esquema completo, retry y revisión independiente antes de integración |
| P0 | PAY-RETRY-001, PAY-CHECKOUT-002 / #551, #554, #555, #569 | Captura incierta, snapshot y términos tienen correcciones consolidadas. #551 documenta incompatibilidades restantes en fixtures de otros proveedores | Intento con resultado ambiguo frente a cambio de rail; captura/reembolso y rollback HTTP sobre esquema completo, sin activar proveedores deshabilitados |
| P0 | PAY-MUSIC-DOWNLOAD-001 / #589, #590, #591 | Hay correcciones de estado, replay y cantidades en ramas abiertas. El registro #581 aún declara ausencia de reconciliación verificada tras cargo ambiguo | Exigir vínculo pedido/importe/moneda/clave y rechazo de retry incierto; confirmar recibo durable de incertidumbre y consulta del resultado antes de nueva ejecución |
| P0 | SYS-RELEASE-001, DEPLOY-JOURNAL-001, DEPLOY-TARGET-001 / #495, #574 | Hay bibliotecas de journal, migraciones y verificaciones; el coordinador productivo completo sigue declarado sin implementar | Demostrar corte/reinicio en cada frontera intent/efecto/recibo, custodia de claves y recuperación independiente antes de habilitar ejecución |
| P1 | PRIV-RAG-001 / #580; OPS-INBOUND-CAPTURE-001 / #546, #584 | Hay filtros de recuperación pública y política de captura; falta correspondencia completa de canales, consentimiento y retención | Cambiar visibilidad/consentimiento tras cachear, denegar disclosure y captura sin política en cada canal HTTP real |
| P1 | AUTH-PUBLIC-001 / #565; ID-SESSION-* / #581 | Barridos de denegación y contratos seleccionados existentes; no prueba completa de autorización y sesiones | Casos por endpoint con rol/propietario/consentimiento cambiados durante la espera y sesión revocada; resultado denegado sin efectos |
| P1 | MEDIA-MUSIC-ASSET-001 / #548, #550 | Correcciones de recuperación tras dead letter disponibles | Reupload concurrente, checksum incorrecto y finalización tardía del worker antiguo sobre una versión nueva |
| P2 | SYS-MONEY-001/002 / #581, #593 | Contratos aritméticos y precondiciones seleccionados, con alcance explícitamente limitado | Composición mecánica de precondiciones, despacho y persistencia; comprobar además si el adaptador participa en un camino real antes de atribuirle cobertura de proveedores |

Estas prioridades dirigen la próxima verificación y no autorizan activación de
proveedores, despliegues ni operaciones financieras de prueba. La admisión de
integración mantiene revisión y gates existentes. No se emitió aprobación,
merge, migración productiva ni declaración de demostración completa.
