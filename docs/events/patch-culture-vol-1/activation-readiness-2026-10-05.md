# PATCH CULTURE — verificación para activar ventas

Actualizado el 5 de octubre de 2026. **Activación autorizada, ventas todavía deshabilitadas.**

Diego indicó «Continúa y activa cobros», seguido de «Figure it out and continue».
La autorización comprende los términos ya aprobados: veinte plazas, USD 20
finales, una pinta y acceso a jam/showcase. No corresponde solicitar otra
aprobación comercial para esos mismos términos. Autorización y funcionamiento
verificado se registran por separado.

## Compra oficial de prueba completada; activación aún pendiente

El 5 de octubre se completó la compra desde web 390×844: selección de dos entradas,
checkout USD40, aprobación y captura real en **PayPal sandbox**, dos tickets, QR
decodificados y check-in HTTP atómico (ocho intentos: un 200 y siete 409). Los
reintentos de captura y webhook conservaron dos tickets, un asiento balanceado y
una confirmación aceptada por SMTP local. El pago de prueba fue reembolsado por
USD40; el callback firmado bloqueó la entrada restante. La evidencia sin datos
personales ni credenciales está en [el reporte de ejecución](official-sandbox-purchase-2026-10-05.json).

Se corrigieron cuatro fallos observados: CORS de consulta de la orden, ausencia de
binding en la respuesta POST de PayPal, asiento de comisión cero y montaje del
botón dentro del diálogo. El reembolso externo **no** completa todavía el flujo
financiero de TDF (aprobación, asignación por ticket, asiento y nota de crédito).
El estado de la revisión administrativa tampoco puede rehabilitar un ticket con
reembolso verificado. El organizador confirmó IVA 0 % para el paquete; sigue pendiente la integración
protegida y el despliegue, la entrega externa de correo y la validación nativa.
No se activaron ventas de producción ni se solicita de nuevo la autorización comercial.

## Repetición con IVA 0 % confirmado

La [segunda compra oficial](official-sandbox-iva0-2026-10-05.json), con backend
`2590f7f7`, capturó USD40, emitió dos tickets y produjo un asiento balanceado
sin líneas de impuesto ni comisión cero. Los dos QR se decodificaron correctamente;
ocho escaneos concurrentes admitieron una vez, y tres reintentos de captura
conservaron las dos entradas. Cotizaciones reales adicionales dieron USD20/60/80
para una/tres/cuatro entradas, siempre IVA0 y sin emitir tickets impagos.

PayPal completó la devolución USD40. Su callback tuvo firma oficial SUCCESS,
respondió 200 también en dos reenvíos y bloqueó la entrada no utilizada con 409.
En esta repetición no se observó el callback de captura: su evidencia proviene
del endpoint de captura y la consulta autenticada de PayPal. La prueba anterior
sí verificó ambos callbacks. Se recibió una confirmación en SMTP local; sigue
sin acreditarse entrega externa ni el ciclo financiero del reembolso en TDF.

## Continuación: sandbox oficial disponible

Después de que Diego inició sesión en PayPal Developer, se verificó la aplicación
sandbox existente de Ecuador: OAuth HTTP 200 y registro de webhook real HTTP 201
el 5 de octubre a las 21:21 UTC. También está disponible el comprador Personal de
pruebas. Este acceso resuelve el bloqueo anterior de credenciales sandbox.
Las credenciales permanecen fuera del repositorio en archivos privados; no se
reutilizan credenciales de producción en el entorno aislado.

El webhook temporal expone exclusivamente su ruta de recepción; el resto responde
404. Reenvía cuerpo y firmas al backend aislado para la verificación oficial.
La base local aplica el manifiesto canónico de 181 migraciones, conservando sus
identidades y checksums. La configuración de habilitación del proveedor en esta
base es una **precondición sintética**, no prueba de una transacción verificada.
La compra y captura posteriores se acreditan en el reporte anterior; el reembolso financiero en TDF y la entrega externa siguen pendientes.
El receptor SMTP local prueba procesamiento de la cola, no llegada a una bandeja.

La revisión del backend combinado identifica dos puntos que deben comprobarse y
corregirse antes de habilitar ventas: la aprobación legacy de reembolsos de tickets
solo llama a Stripe; el webhook PayPal de refund/reversal registra una excepción
de conciliación sin revocar por sí mismo la entrada. Se usará el sandbox real para
verificar la transición, siguiendo la [API oficial de reembolsos](https://developer.paypal.com/api/payments/v2)
y los [eventos oficiales](https://developer.paypal.com/api/rest/webhooks/event-names/).
No se declara completo el flujo por obtener autenticación o por registrar el webhook.

## Inspección de producción

| Comprobación | Resultado observado | Límite de la evidencia |
|---|---|---|
| Configuración del servidor canónico | PayPal live, webhook, clave de cifrado y SMTP configurados | Las claves permanecieron en el servidor; presencia no equivale a una compra |
| PayPal, 17:07:56 UTC | OAuth HTTP 200; consulta del webhook configurado HTTP 200 | Consulta oficial autenticada, sin crear orden, cobro, reembolso ni modificar el webhook |
| Webhook PayPal | HTTPS en `api.tdfrecords.net/services/storefront/paypal/webhook`; suscripciones `PAYMENT.CAPTURE.COMPLETED`, `PAYMENT.CAPTURE.REFUNDED`, `PAYMENT.CAPTURE.REVERSED` | Registro correcto; aún no se verificó recepción y procesamiento de una transacción del evento |
| Cuentas de la plataforma en PostgreSQL | Datafast, PayPal, PayPhone y PlaceToPay deshabilitados; metadatos `contract_status=unverified`, `credential_status=absent` | La marca de credenciales PayPal está desactualizada respecto a la configuración comprobada; no cambiarla a «operativo» solo por obtener OAuth |
| Capacidades públicas, EC/USD 2000/entrada | Los seis métodos responden HTTP 200 con `routes=[]` | No hay ruta pública habilitada para vender esta entrada |
| Otros proveedores en el entorno del servidor | Sin credenciales Datafast, PlaceToPay o PayPhone | No se afirma que no existan en otro gestor o cuenta inaccesible |
| Stripe existente | API de cuenta HTTP 200; país US; nombre comercial coincide con TDF; `charges_enabled=false`, `payouts_enabled=false`, `details_submitted=false`, tarjetas pendientes | No utilizar como sustituto funcional; no se verificó que la entidad estadounidense sea el emisor ecuatoriano ni se modificó su alta |
| SMTP existente | TLS con certificado validado; autenticación 235 y NOOP 250 | Cero mensajes enviados; no prueba llegada a bandeja. El worker de confirmación sigue apagado |
| API y esquema | Backend `645f56fcc44f81609fbfd0e03d683b40376ce77a`, 159 migraciones | Las correcciones nuevas de tickets aún no están acreditadas en este backend |

La inspección administrativa reutilizable se ejecutó nuevamente en
[Actions 37345400601](https://github.com/diegueins680/tdf-app/actions/runs/37345400601)
y terminó correctamente: evento 141 en planificación, privado y con compra
deshabilitada. El paso de preparación conservó el borrador e inventario inactivo.
La suite adicional RACI de PR485 también terminó correctamente en
[Actions 37340733399](https://github.com/diegueins680/tdf-app/actions/runs/37340733399).

## Emisor y tratamiento fiscal

Se localizó y leyó `RUC.pdf` en el Drive conectado del organizador. El certificado
fue emitido el **21 de mayo de 2024**: TDF RECORDS S.A.S., RUC 1793215092001,
régimen general, obligado a llevar contabilidad. Incluye las actividades
R900001, R900003 y R900004. La fecha del archivo en Drive no actualiza la fecha
del certificado. No se copian al repositorio domicilio, datos del representante
ni código de verificación.

Esto confirma documentalmente el emisor indicado por Diego y actividades
culturales registradas a esa fecha. **No determina por sí solo el impuesto del
paquete taller + espectáculo + cerveza**, ni acredita una consulta actual al
registro, inscripción RUAC o autorización educativa. No se encontró evidencia
adicional de estos puntos en las búsquedas acotadas de Drive y correo conectado.

La [guía oficial del SRI](https://www.sri.gob.ec/web/intersri/servicios-artisticos-y-culturales)
vincula la tarifa cultural cero al servicio efectivamente prestado y a su
actividad registrada; la sección de espectáculos añade condiciones sobre
promotor/espacio cultural y aforo. No convertir el reparto interno USD15/USD5
en bases tributarias sin justificarlo. Diego confirmó posteriormente: «Por ser un evento artístico, educativo, y cultural, el IVA es 0%.»
Se registra **IVA 0 % por confirmación del organizador**, para el paquete completo
de USD20, con emisor TDF Records y RUC 1793215092001. Es una configuración
expresa basada en esa respuesta, no una conclusión fiscal independiente ni una
exención aplicada por defecto. El escenario sandbox anterior del 15 % conserva
su historial y no se modifica retroactivamente.

## Cálculo del total: discrepancia reproducida

Se ejecutó la función real `TDF.Commerce.EventTickets.calculateTicketPrice`
con `stack exec -- runghc`, usando el toolchain canónico. El 15 % empleado aquí
es **un escenario de prueba**, no una determinación fiscal del evento.

| Cantidad | Precio de tier USD20 + impuesto adicional 15 % | Intento de base USD17,39 + 15 % | Total aprobado |
|---|---:|---:|---:|
| 1 | 23,00 | 20,00 | 20,00 |
| 2 | 46,00 | 40,00 | 40,00 |
| 3 | 69,00 | 60,00 | 60,00 |
| 4 | 92,00 | 79,99 | 80,00 |

Cambiar el tier a USD17,39 no resuelve el precio final para todas las cantidades
y además altera el precio mostrado en selección. La continuación implementa un modo explícito `tax_included` de política y
snapshot de orden, con total autoritativo y etiqueta web de impuesto incluido.
La tarifa fiscal del evento ya fue confirmada por el organizador en 0 %.
Las pruebas con esa configuración y el despliegue siguen siendo pasos separados.
No se aplicó ninguno de esos dos atajos en producción.

## Trabajo necesario para activar

1. Desplegar el backend y las migraciones revisadas por la vía canónica Hetzner,
   con comprobación del esquema, versión y recuperación.
2. Aplicar la configuración explícita IVA 0 % confirmada por el organizador
   y comprobar totales USD20/40/60/80 sin modificar órdenes históricas.
3. Ejecutar una compra de proveedor y reembolso en un entorno oficial utilizable,
   incluyendo callback/webhook, emisión, QR, check-in y entrega de confirmación.
   La compra, emisión, QR y check-in sandbox ya pasaron. El 6 de octubre también
   pasaron reembolso parcial y total desde TDF, ledger, nota interna de crédito,
   bloqueo del ticket devuelto y reenvíos de webhooks firmados. Ver
   [evidencia y versiones exactas](ticket-refund-api-sandbox-2026-10-06.json).
   La confirmación llegó al SMTP aislado; la entrega externa sigue pendiente.
4. Activar la ruta PayPal y la política del evento solamente con la evidencia
   anterior, luego publicar y comprobar el checkout canónico. La autorización
   del organizador ya está registrada; no constituye una prueba de estos pasos.

Fuentes técnicas consultadas:
[autenticación PayPal](https://developer.paypal.com/api/rest/authentication/) y
[consulta de webhooks](https://developer.paypal.com/api/webhooks/v1).
Los recibos operativos completos y los scripts de inspección sin secretos se
conservan fuera del repositorio público. La inspección inicial no envió emails, cobros ni payouts. La cualificación posterior
realizó compras y reembolsos exclusivamente en PayPal sandbox y aceptó dos
confirmaciones en SMTP local; no habilitó cobros reales ni envió correo externo.
