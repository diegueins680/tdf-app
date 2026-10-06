# PATCH CULTURE — estado para activar ventas

Actualizado el 6 de octubre de 2026. **Activación autorizada; ventas de producción todavía deshabilitadas.**

## Términos confirmados

El organizador aprobó veinte plazas a USD 20 finales, con taller, jam, showcase y
una pinta; acceso para mayores de 18 años. El reparto aprobado es USD 15 para TDF
y USD 5 para Andes por entrada. Los costos del procesador los absorbe TDF.
No se requiere repetir la aprobación comercial de estos términos.

Emisor: TDF Records, RUC 1793215092001. Se configura IVA 0% para el paquete completo
por confirmación expresa del organizador. El certificado RUC consultado fue
emitido el 21 de mayo de 2024; acredita la identidad registrada en esa fecha,
no una determinación tributaria independiente del paquete. La tarifa confirmada
no se aplica retroactivamente a los escenarios sandbox anteriores.

El [borrador verificado](draft-verification.json) conserva evento 141, venue 22 y
tier 2. La URL canónica prevista es `https://www.tdfrecords.net/eventos/141`;
no se presenta como una página de venta ya publicada. La
[propuesta aprobada](propuesta-condiciones.md) conserva los plazos y condiciones.
La configuración pendiente mantiene cuatro entradas por orden, reserva de diez
minutos, cero cortesías y ningún código promocional activo.

| Asistentes | GMV USD | Andes USD | TDF antes del procesador USD |
|---:|---:|---:|---:|
| 8 | 160 | 40 | 120 |
| 14 | 280 | 70 | 210 |
| 20 | 400 | 100 | 300 |

Son escenarios aritméticos, no una previsión de demanda. El neto de TDF es la
última columna menos las comisiones reales del proveedor; no se inventa una
liquidación ni se presenta una tarifa de sandbox como comisión de producción.

## Flujo verificado en el sandbox oficial

La [evidencia de compra y reembolso desde TDF](ticket-refund-api-sandbox-2026-10-06.json)
identifica la fuente nativa `a058dc0bb80dd04e3341fdbcd0df066b6732701c` y la posterior
comprobación de presentación/finanzas `8aa554edf845be7d32a9acfe4301b6b7d903932f`.

Se completaron dos compras desde web móvil por USD 40 y USD 20, con captura oficial
PayPal, tres tickets y QR decodificables. Ocho escaneos concurrentes de una entrada
produjeron una admisión y siete rechazos por reutilización. El flujo canónico de
TDF aprobó un reembolso parcial de USD 20 y otro total de USD 20, con asignación por
ticket, asiento balanceado y nota interna de crédito. Los tickets devueltos no
pudieron ingresar y la entrada conservada sí admitió una vez. La devolución
restante de USD 20 fue limpieza externa de fondos de prueba después del check-in,
no una asignación canónica adicional de TDF. Los USD 60 de esas compras quedaron
devueltos en sandbox.

Cinco callbacks auténticos (dos capturas y tres devoluciones) tuvieron validación
oficial de firma y reenvíos idempotentes. Dos confirmaciones fueron aceptadas por
el SMTP aislado. Esto no acredita entrega a una bandeja externa, facturación
electrónica autorizada por SRI, publicación móvil nativa ni una compra live.
Los receptores/túneles temporales se cerraron y el webhook temporal se eliminó.
Las credenciales permanecen fuera del repositorio.

## Correcciones posteriores y alcance de las pruebas

La integración reutiliza órdenes, proveedores, ledger, inventario y perfiles.
Incluye asignación de reembolsos por ticket, consultas autenticadas de recuperación,
filtro de credenciales transferidas, estados de checkout y analítica sin QR ni
identificadores privados de órdenes. La evidencia firmada de un reembolso anterior
a su captura sobrevive a ocho intentos agotados y bloquea un check-in posterior.
Las órdenes canónicas rechazan cambios masivos legacy que podrían perder esa
asignación; las entradas conservadas tras una devolución parcial siguen siendo
transferibles según la política comprada.

La ejecución PostgreSQL de `1341645f9bd3d4d4e6846896827c4f42920ec2ad` pasó
**304 pruebas financieras/de proveedor en una base sintética nueva**. Incluye:

- Obtener OAuth antes de consumir el permiso de una sola solicitud de reembolso;
  un fallo previo permite reintentar, pero una respuesta POST incierta no permite
  enviar otra devolución.
- Seleccionar entradas completas aun cuando el reparto estable difiere por un
  centavo, incluyendo devoluciones de 4171 y 8343 centavos sobre 12515.
- Conservar el identificador aceptado por PayPal ante importe/moneda inesperados;
  una consulta incorrecta mantiene la reserva, un reintento inmediato recibe 429,
  y una consulta posterior exacta completa una sola nota de crédito sin otro POST.

Estas nuevas pruebas del handler usan el ejecutor HTTP real con conexiones en
memoria. No son nuevas transacciones del sandbox oficial ni prueban TLS externo.
El código del proveedor cambió después de la compra oficial anterior: las fuentes
y alcances se mantienen separados. Los checks y revisión de la integración final
siguen siendo obligatorios antes de fusionar.

## Producción y pasos pendientes

La inspección de solo lectura del 6 de octubre a las 05:35 UTC encontró el backend
`645f56fcc44f81609fbfd0e03d683b40376ce77a` y 159 migraciones. PayPal live autentica y
su webhook HTTPS canónico contiene los eventos de captura, refund y reversal;
eso confirma configuración, no una compra live ni activación del proveedor.

El manifiesto candidato de 184 migraciones fue aplicado dos veces sobre una copia
real aislada de PostgreSQL 17: 652 tablas y 159 entradas históricas preservadas.
La comprobación del esquema pasó y el contenedor aislado se retiró. No se escribió
la base de producción ni se desplegó ese backend durante el ensayo.

Para activar restan la fusión protegida, el despliegue compatible con respaldo y
recuperación verificados, la configuración del proveedor y de la política aprobada
del evento, y la comprobación del checkout público. La habilitación compartida de
PayPal debe preservar las demás rutas desactivadas y evitar reabrir la política
antigua del evento 121; no debe alterar sus órdenes ni su recuperación de pagos.
Debe verificarse también la entrega externa de las comunicaciones transaccionales.

Los archivos de pruebas anteriores se conservan como evidencia histórica:
[primera compra](official-sandbox-purchase-2026-10-05.json),
[repetición con IVA 0](official-sandbox-iva0-2026-10-05.json) y
[asignación de reembolsos](ticket-refund-allocation-local-2026-10-06.json).
Las decisiones del proveedor siguen su documentación de
[autenticación](https://developer.paypal.com/api/rest/authentication/),
[reembolsos](https://developer.paypal.com/api/payments/v2) y
[eventos](https://developer.paypal.com/api/rest/webhooks/event-names/).
