# Devoluciones por cancelación del Domo

Implementación dependiente de la PR #691, base `593b1f286b75e31338d28a03f94be05a08b3777c`.
No autoriza activar proveedores, aplicar migraciones en producción ni devolver dinero real.
Conserva AUTHORITY-093, PAY-DOMO-CANCEL-001 y sus importes/versiones aprobados.

## Circuito operativo

1. El cliente cancela usando el token privado de su cotización, o un administrador
   cancela por el cliente/TDF. Se guardan política, anticipo/saldo cobrado, retención
   e importe debido. Se libera la fecha. La deuda de devolución permanece abierta.
2. Si canceló el cliente, `POST /admin/domo/quotes/{quoteId}/cancel` crea la solicitud
   por el importe ya registrado. Este paso no liquida la deuda ni la conciliación.
3. Otra persona con administración estricta y acceso al módulo Admin llama
   `POST /admin/domo/quotes/{quoteId}/refund/approve`. No contacta al proveedor.
4. `POST .../refund/execute` consume una única autorización persistente. El
   solicitante no puede ejecutarla. Un reintento devuelve el estado guardado.
5. Se verifica el resultado efectivo y se contabiliza la devolución. Solo entonces
   se cierra la excepción `domo_cancellation_refund_due`, en la misma transacción
   que el asiento balanceado y la nota de crédito canónica.

Todos los endpoints administrativos usan bearer auth. Se revalidan sesión,
roles y permisos actuales bajo bloqueo antes de escribir; no se mantienen
bloqueos de base de datos mientras se espera HTTP. `GET .../refund` consulta
el estado. La página privada de la cotización restaura los importes y el estado
persistido al recargar. Aprobación/procesamiento nunca se presentan como devolución.

## Proveedores disponibles en esta implementación

| Proveedor del cobro original | Ejecución de esta ruta | Confirmación |
| --- | --- | --- |
| PayPal | Adaptador compartido, un POST por devolución; requiere captura original exacta | GET autenticado del refund conocido, con captura, entorno, comercio, moneda e importe exactos y estado COMPLETED |
| Datafast | Intervención manual; esta ruta no tiene un adaptador automático habilitado | Comprobante inmutable y revisión por otra persona |
| Otros proveedores ya registrados en un checkout elegible | Intervención manual; no se activa automáticamente ningún adaptador | Mismo circuito de evidencia independiente |

Esta tabla describe los adaptadores cualificados del repositorio, no una
limitación comercial de las APIs externas. No reutiliza las aprobaciones de
servicios, entradas o mercancía: sus contratos vinculan otros tipos de pedido.
Se reutilizan RefundStore, su reserva de fondos y barrera de ejecución,
el transporte PayPal acotado y RefundReconciliation con autorización actual.
El asiento del Domo revierte `revenue.domo_event_quote`, no ingresos del estudio.

PayPal requiere `checkout.paypal.refunds` habilitado explícitamente incluso en
sandbox, `PAYPAL_REFUND_RECONCILIATION_ENABLED=true`, credenciales del entorno y
comercio originales, la bandera de consulta y las capacidades cualificadas de
`RefundReconciliation.queryReady`. Obtener OAuth ocurre antes de consumir el
permiso de POST. La respuesta del POST, incluso COMPLETED, conserva estado
processing hasta una lectura autenticada mediante `POST .../refund/reconcile`.

Un timeout, rechazo, estado pendiente, identidad desconocida o evidencia distinta
mantiene los fondos reservados y la excepción abierta. No hay otro POST automático
ni liberación de reserva por transcurso del tiempo. Si no se conoce el refund ID,
un operador investiga el registro del proveedor usando el ID canónico como
PayPal-Request-Id. Puede registrar el ID encontrado mediante
`POST .../refund/provider-reference` con `{"providerRefundId":"ID-ENCONTRADO"}`;
esto solo vincula un candidato inmutable para consulta. Después debe usar
`reconcile`, que valida la evidencia completa. No se inventa evidencia ni se
vuelve a enviar dinero.

## Comprobantes manuales

La bandera `domo.refund_manual` nace apagada en ambos entornos. Su activación
requiere un equipo que ejecute la devolución externa y revise el comprobante.
`execute` registra el traspaso manual; no hace transferencias ni marca dinero devuelto.

Después de devolver el dinero externamente, una persona distinta del solicitante
registra `POST .../refund/manual-evidence` con:

```json
{
  "externalReference": "REFERENCIA-DEL-PAGO-DEVUELTO",
  "evidenceReference": "secure-receipt:IDENTIFICADOR-INTERNO",
  "amountMinor": 100352,
  "currency": "USD",
  "occurredAt": "2026-11-02T17:00:00Z"
}
```

Debe coincidir exactamente con la solicitud aprobada. Se conserva la referencia
segura del comprobante, no datos bancarios o credenciales. Otra persona distinta
del registrador y del solicitante verifica el movimiento real y llama
`POST .../refund/confirm-manual`. Un comprobante externo no puede reutilizarse
para otra devolución del mismo proveedor, comercio y entorno. Los reintentos y
confirmaciones concurrentes no duplican el asiento ni la nota de crédito.
Este circuito exige una verificación humana real: el backend no puede comprobar
por sí solo un documento de un proveedor sin adaptador de consulta.

## Migración y recuperación

La migración es aditiva. La vista de obligaciones pendientes ahora incluye
solicitudes/aprobaciones/procesamientos/errores hasta el éxito efectivo. Reabre
solo excepciones de cancelación cerradas prematuramente por el código propuesto;
no cambia importes ni estados financieros. Su rollback rechaza borrar historial.
La política de cancelación aprobada y sus migraciones originales no se reescriben.

La admisión de capturas y la cancelación comparten los bloqueos de cotización y
checkout. Una captura persistida en created ya bloquea la rama de cancelación sin
cobro antes de que responda HTTP. Un resultado de captura desconocido exige
conciliación, no un nuevo intento con otra clave.

## Validación

- `npm run typecheck:ui`
- `npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/__tests__/DomoQuoteCheckoutPage.test.tsx`
- `python3 formal/payments/domo_refund_lifecycle.py` y la misma orden con `-O`.
- `stack test`, con la versión fijada en `tdf-hq/stack.yaml`.
- `sh scripts/test-provider-retry-runtime.sh` en una base PostgreSQL nueva y aislada.
  Incluye las pruebas del Domo, los otros dominios y el esquema completo revisado.
- `node scripts/inspect-compiled-api.mjs --binary ... --output ...` compara las
  rutas compiladas con la declaración y el OpenAPI actualizado.

Las pruebas de proveedor inyectan conexiones HTTP con respuestas sintéticas;
no utilizan cuentas reales, ni siquiera para consultas. El modelo explícito es
acotado y tiene controles que detectan reejecución y cierre prematuro. No demuestra
refinamiento de todo el sistema; las pruebas de runtime cubren Haskell y SQL.

## Otras observaciones de #691

Este cambio atiende la aprobación/ejecución ausente, el cierre anticipado,
la carrera captura/cancelación, la restauración del resumen y el reporte de fallo
oculto por `errexit`, y hace determinista el encabezado no hexadecimal de
la prueba Meta. El generador CORS ya exigía una letra en la base inspeccionada;
no se modificó. La PR base tiene además observaciones sobre validación SQL
del depósito, tip idempotente y propiedad de cuentas sociales; no se consideran
resueltas por la entrega de reembolsos. La integración de toda #691 con main
requiere reconciliar esos cambios y sus conflictos por separado.
