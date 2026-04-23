# Revision de respuestas WhatsApp y seguimiento

Fecha de revision: 2026-04-22 19:42:40 -0500  
Canal revisado: browser adjunto de OpenClaw, WhatsApp Business Web.  
Regla operativa: no enviar seguimientos fuera de horario de oficina. Los mensajes quedaron programados para el 2026-04-23 09:00 America/Guayaquil y finalmente se ejecutaron manualmente dentro de horario el 2026-04-23.

Job programado originalmente: `at` job 3, `Thu Apr 23 09:00:00 2026`.
Script programado: `/tmp/domo-vendor-whatsapp-followup-2026-04-23.sh`.  
Log esperado: `vendor-whatsapp-followup-log-2026-04-23.md`.

Nota 2026-04-22 20:24: el job 2 fue reemplazado por el job 3 para incluir a Eventos VVS como proveedor sustituto de Crystal Eventos en mobiliario/menaje.

Actualizacion 2026-04-23 10:55: el `at` job 3 quedo atascado en cola y no corrio a las 09:00. A las 10:38 se ejecuto manualmente el script; el CLI `openclaw message send` fallo para los 8 intentos (timeouts de gateway y errores de validacion de target WhatsApp). Luego se recupero manualmente por el browser adjunto de WhatsApp Web. Quedaron enviados por browser: Vivero (10:52), Conjardin (10:53), Palcar (10:54), Adoquines Quito (10:54), Crystal Eventos (10:54), Kukayo (10:54) y PEBEL (10:54). Eventos VVS quedo sin resultado visible en WhatsApp Web.

## Resultado por proveedor

| Proveedor | Estado observado | Evidencia en browser | Accion |
| --- | --- | --- | --- |
| Vivero Camila | Seguimiento enviado | Browser 2026-04-23 10:52 confirma el nuevo mensaje saliente; antes ya habia respondido con `decoraciondejardines_camila@hotmail.com` | Esperar respuesta/proforma. |
| Conjardin / Instituto Ecuatoriano de Jardineria y Paisajismo | Seguimiento enviado | Browser 2026-04-23 10:53 confirma el nuevo mensaje saliente | Esperar respuesta y confirmar correo. |
| Palcar Constructora | Seguimiento enviado | Browser 2026-04-23 10:54 confirma el nuevo mensaje saliente | Esperar respuesta/visita/proforma. |
| Adoquines Quito | Seguimiento enviado | Browser 2026-04-23 10:54 confirma el nuevo mensaje saliente | Esperar respuesta/correo/proforma. |
| Crystal Eventos | Cierre enviado | Browser 2026-04-23 10:54 confirma el acuse; previamente habian indicado que no estan realizando alquileres | Cerrar esta via y usar sustituto. |
| Eventos VVS | Sin resultado en WhatsApp Web | El fallback manual no encontro chat ni resultado visible para `593991231388` | Llamar o buscar canal alterno; si no responde, pasar al siguiente sustituto. |
| Kukayo Catering & Eventos | Seguimiento enviado | Browser 2026-04-23 10:54 confirma el nuevo mensaje saliente | Esperar respuesta/correo/proforma. |
| PEBEL Consultores | Seguimiento enviado | Browser 2026-04-23 10:54 confirma el nuevo mensaje saliente | Esperar respuesta/proforma. |

## Mensajes preparados para 2026-04-23 09:00

### Vivero Camila

Buenos dias, gracias por responder. Tomo nota del correo decoraciondejardines_camila@hotmail.com. Para el expediente de financiamiento de Domo del Pululahua, nos ayudan por favor con una proforma formal en USD, PDF, firmada o validada por correo, para paisajismo, drenaje, senderos y acabados exteriores? Si necesitan visita tecnica o medidas adicionales, me indican por este medio. Diego Saa / 0984755301.

### Conjardin

Buenos dias, gracias por responder. Para enviarlo tambien y dejar trazabilidad para el credito, nos confirman por favor si el correo correcto es conjardines2022@gmail.com? Necesitamos proforma formal en USD, PDF, firmada o validada por correo, para paisajismo, drenaje, senderos y acabados exteriores de Domo del Pululahua. Diego Saa / 0984755301.

### Palcar Constructora

Buenos dias, retomo la solicitud de Domo del Pululahua. Necesitamos una proforma formal en USD, PDF, firmada o validada por correo, para mejoramiento de camino, acceso, drenaje, compactacion, parqueo y seguridad de llegada. Nos pueden confirmar si pueden cotizar esta semana y si requieren visita tecnica? Diego Saa / 0984755301.

### Adoquines Quito

Buenos dias, retomo la solicitud de Domo del Pululahua. Necesitamos una proforma formal en USD, PDF, firmada o validada por correo, para adoquines, bordillos, canaletas, cunetas y topes de parqueo/acceso. Nos pueden confirmar si pueden cotizar esta semana y el correo formal para enviar el alcance? Diego Saa / 0984755301.

### Crystal Eventos

Buenos dias, gracias por confirmar. Dejo sin efecto la solicitud de proforma para esta ola, ya que nos indicaron que al momento no realizan alquileres. Si retoman alquiler de mobiliario o menaje en las proximas semanas, nos pueden avisar por este medio. Diego Saa / 0984755301.

### Eventos VVS

Buenos dias, soy Diego Saa, propietario de Domo del Pululahua. Estamos preparando el expediente para financiamiento productivo y necesitamos una proforma formal en USD, PDF, firmada o validada por correo, para mobiliario/menaje operativo, carpas o coberturas, tarima, mesas/sillas y elementos para eventos del proyecto en Pululahua. Nos pueden confirmar el correo o canal formal para enviar el alcance y cotizar esta semana? Contacto: diego@tdfrecords.net / 0984755301.

### Kukayo Catering & Eventos

Buenos dias, retomo la solicitud de Domo del Pululahua. Necesitamos una proforma formal en USD, PDF, firmada o validada por correo, para kit inicial de catering y barra, estaciones de servicio, menaje y utensilios. Nos pueden confirmar si pueden cotizar esta semana y el correo formal para enviar el alcance? Diego Saa / 0984755301.

### PEBEL Consultores

Buenos dias, gracias por el correo alvaroperalt@gmail.com. Para el expediente de financiamiento de Domo del Pululahua, nos ayudan por favor con una proforma formal en USD, PDF, firmada o validada por correo, para apoyo en permisos, seguridad, gestion legal/operativa y requisitos aplicables al venue? Diego Saa / 0984755301.

## Pendiente operacional

- Registrar nuevas respuestas en este mismo directorio y actualizar el tracker en cuanto lleguen.
- Llamar a Eventos VVS o pedir un canal alterno hoy; si no responde, usar el siguiente sustituto de mobiliario/menaje.
- Revisar por separado la falla de `openclaw message send` con WhatsApp, porque hoy el fallback CLI fallo y hubo que recuperar por browser.
