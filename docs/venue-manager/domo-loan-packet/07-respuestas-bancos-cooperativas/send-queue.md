# Cola de envio a bancos y cooperativas

Fecha: 2026-04-19

## Estado de entrega

- 2026-04-18: El primer contacto por WhatsApp publico de Banco Pichincha fue entregado por OpenClaw a `+593962992999`.
- Metadatos de entrega confirmados: `messageId=3EB0BC2D72A97C56DDFE0D`, `runId=711e732a-820a-4fac-825b-114ea23320c9`, `toJid=593962992999@s.whatsapp.net`.
- Pichincha respondio con un asistente automatico que indica que no puede entender texto especifico ni imagenes adicionales, y luego pidio elegir un tipo de identificacion para autenticar.
- OpenClaw respondio automaticamente dos veces antes de quitar el numero del banco de la lista permitida de WhatsApp. El bot de Pichincha rechazo el texto libre y repitio el menu de identificacion, por lo que ese chat no debe usarse para enviar el expediente de credito.
- Siguiente accion practica: hacer seguimiento por telefono PYME o agencia de Pichincha y enviar `email-pichincha.eml` con `../00-resumen/domo-public-lender-packet.zip` solo por un canal seguro confirmado por el prestamista. No enviar cedula, RUC, declaraciones, escrituras, estados bancarios ni otros documentos sensibles por el chat del bot.
- Produbanco queda listo para gestion manual por telefono/correo porque la pagina publica PYME lista soporte telefonico, pero no un canal seguro de carga documental.
- 2026-04-19: El ZIP publico seguro para prestamistas fue reconstruido con carta de presentacion, indice de entrega, asignacion corregida de uso de fondos por USD 100.000, pro forma de 36 meses, cronograma de uso de fondos y tabla comparativa de prestamistas.

## Prioridad inicial de envio

1. Banco Pichincha
2. Produbanco

Puntos oficiales de partida:

- Banco Pichincha PYME credits: https://www.pichincha.com/detalle-catalogo/pymes-creditos
- Banco Pichincha productivo empresas: https://www.pichincha.com/portal/principal/empresas/creditos/productivo
- Banco Pichincha banca telefonica: `(02) 2999 999`
- Produbanco activos fijos PYME: https://www.produbanco.com.ec/pymes/activos-fijos/
- Produbanco capital de trabajo PYME: https://www.produbanco.com.ec/pymes/capital-de-trabajo/
- Telefono de Produbanco listado en su pagina PYME: 02 400 9000, opcion 5.

Guion para telefono o agencia:

- `phone-intake-script.md`

Adjuntar el paquete publico seguro:

- `../00-resumen/domo-submission-cover-letter.pdf`
- `../00-resumen/domo-bank-submission-index.pdf`
- `../00-resumen/domo-lender-one-pager.pdf`
- `../00-resumen/domo-del-pululahua-financial-plan.pdf`
- `../00-resumen/domo-36-month-pro-forma.pdf`
- `../04-proformas-uso-de-fondos/domo-use-of-funds-schedule.pdf`
- `../06-evidencia-comercial/domo-public-quote-page-evidence.pdf`

O adjuntar el ZIP:

- `../00-resumen/domo-public-lender-packet.zip`

## Antes de enviar

- Agregar nombre, telefono y correo reales del remitente en el cliente de correo.
- Confirmar si el destinatario es un ejecutivo bancario, correo de agencia, formulario web o canal empresarial de WhatsApp.
- No enviar RUC, cedula, estados bancarios, declaraciones, escrituras ni documentos de propiedad hasta que el prestamista confirme un canal seguro de recepcion.
- Registrar fecha de envio y contacto en `lender-outreach-tracker.csv`.

## Orden de envio

| Orden | Institucion | Borrador | Estado |
| ---: | --- | --- | --- |
| 1 | Banco Pichincha | `email-pichincha.eml` | Bucle de bot de WhatsApp; falta canal seguro |
| 2 | Produbanco | `email-produbanco.eml` | Listo para enviar |
| 3 | Banco Guayaquil | `lender-outreach-drafts.md` | Borrador listo |
| 4 | Banco Internacional | `lender-outreach-drafts.md` | Borrador listo |
| 5 | Jardin Azuayo | `lender-outreach-drafts.md` | Borrador listo |
| 6 | JEP | `lender-outreach-drafts.md` | Borrador listo |
| 7 | Cooprogreso | `lender-outreach-drafts.md` | Borrador listo |
| 8 | Cooperativa 29 de Octubre | `lender-outreach-drafts.md` | Borrador listo |
