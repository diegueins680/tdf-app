# PATCH CULTURE — verificación para activar ventas

Actualizado el 5 de octubre de 2026. **Activación autorizada, ventas todavía deshabilitadas.**

Diego indicó «Continúa y activa cobros», seguido de «Figure it out and continue».
La autorización comprende los términos ya aprobados: veinte plazas, USD 20
finales, una pinta y acceso a jam/showcase. No corresponde solicitar otra
aprobación comercial para esos mismos términos. Autorización y funcionamiento
verificado se registran por separado.

## Hallazgos nuevos

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
en bases tributarias sin justificarlo. El tratamiento específico permanece
sin confirmar; tampoco se configura una exención por defecto.

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
y además altera el precio mostrado en selección. Antes de una política gravada
se necesita soporte coherente de precio con impuesto incluido, conservado en
el snapshot de orden y compatible con descuentos, reembolsos, ledger y UI.
No se aplicó ninguno de esos dos atajos en producción.

## Trabajo necesario para activar

1. Desplegar el backend y las migraciones revisadas por la vía canónica Hetzner,
   con comprobación del esquema, versión y recuperación.
2. Completar la clasificación fiscal del producto y, si corresponde impuesto,
   resolver el total incluido con pruebas para una a cuatro entradas.
3. Ejecutar una compra de proveedor y reembolso en un entorno oficial utilizable,
   incluyendo callback/webhook, emisión, QR, check-in y entrega de confirmación.
   Se encontró configuración live utilizable para autenticación; no se encontró
   un conjunto sandbox utilizable en las configuraciones inspeccionadas.
4. Activar la ruta PayPal y la política del evento solamente con la evidencia
   anterior, luego publicar y comprobar el checkout canónico. La autorización
   del organizador ya está registrada; no constituye una prueba de estos pasos.

Fuentes técnicas consultadas:
[autenticación PayPal](https://developer.paypal.com/api/rest/authentication/) y
[consulta de webhooks](https://developer.paypal.com/api/webhooks/v1).
Los recibos operativos completos y los scripts de inspección sin secretos se
conservan fuera del repositorio público. No se enviaron emails, cobros ni payouts.
