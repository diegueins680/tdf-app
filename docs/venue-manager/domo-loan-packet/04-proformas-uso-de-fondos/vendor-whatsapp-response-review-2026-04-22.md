# Revision de respuestas WhatsApp y seguimiento

Fecha de revision: 2026-04-22 19:42:40 -0500  
Canal revisado: browser adjunto de OpenClaw, WhatsApp Business Web.  
Regla operativa: no enviar seguimientos fuera de horario de oficina. Los mensajes quedaron programados para el 2026-04-23 09:00 America/Guayaquil.

Job programado: `at` job 3, `Thu Apr 23 09:00:00 2026`.
Script programado: `/tmp/domo-vendor-whatsapp-followup-2026-04-23.sh`.  
Log esperado: `vendor-whatsapp-followup-log-2026-04-23.md`.

Nota 2026-04-22 20:24: el job 2 fue reemplazado por el job 3 para incluir a Eventos VVS como proveedor sustituto de Crystal Eventos en mobiliario/menaje.

## Resultado por proveedor

| Proveedor | Estado observado | Evidencia en browser | Accion |
| --- | --- | --- | --- |
| Vivero Camila | Respondio | 2026-04-20 09:22: "Buenos dias", "Envio correo solicitado", "decoraciondejardines_camila@hotmail.com" | Programar seguimiento para confirmar proforma formal en USD, PDF y firmada o validada por correo. |
| Conjardin / Instituto Ecuatoriano de Jardineria y Paisajismo | Respondio, pero falta canal claro | 2026-04-20 09:10: "Perdon enviarme tambien" | Programar seguimiento para confirmar si `conjardines2022@gmail.com` es el correo correcto y pedir proforma formal. |
| Palcar Constructora | Sin respuesta visible | Solo aparece el mensaje saliente del 2026-04-20 09:03 | Programar recordatorio por WhatsApp para visita/proforma de camino, drenaje, acceso y parqueo. |
| Adoquines Quito | Sin respuesta visible | Solo aparece el mensaje saliente del 2026-04-20 09:04 | Programar recordatorio por WhatsApp para adoquines, bordillos, canaletas, cunetas y topes. |
| Crystal Eventos | Respondio negativo | Auto-respuesta y luego "Mil didculpas al momento no estamos realizando alquileres" | Cerrar para esta ola y reemplazar con otro proveedor de mobiliario/menaje. Programar acuse breve. |
| Eventos VVS | Sustituto programado | No contactado aun; fuente publica indica alquiler para eventos en Quito, Latacunga y Ecuador | Enviar solicitud inicial por WhatsApp para proforma de mobiliario, menaje, carpas/coberturas y tarima. |
| Kukayo Catering & Eventos | Sin respuesta visible | Solo aparece el mensaje saliente del 2026-04-20 09:06 | Programar recordatorio por WhatsApp para kit inicial de catering/barra y menaje. |
| PEBEL Consultores | Respondio | Mensaje con correo `alvaroperalt@gmail.com` | Programar seguimiento para alcance de permisos, seguridad y gestion legal con proforma formal. |

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

- Verificar el resultado del `at` job 3 despues de las 09:00.
- Registrar IDs de mensaje y cualquier respuesta nueva en este mismo directorio.
- Si Eventos VVS no responde, usar Amazonas/otro proveedor local de mobiliario y menaje como siguiente sustituto.
