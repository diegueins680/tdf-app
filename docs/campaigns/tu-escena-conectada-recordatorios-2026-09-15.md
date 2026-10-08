# Tu Escena Conectada — recordatorios del 15 de septiembre de 2026

Estado: **completado el 14 de septiembre entre 19:17 y 19:24** tras la orden de continuar con el nuevo límite de 20. Los cinco recordatorios están confirmados. No repetir este bloque el 15.

Diego eligió expresamente recordar a los invitados anteriores, no invitar artistas nuevos. El bloque se programó inicialmente para el 15 a las 09:00 tras alcanzar el límite anterior de diez mensajes. Después Diego elevó el máximo diario a 20 y pidió continuar; los mismos cinco destinatarios recibieron el recordatorio el 14 por la noche.

| Instagram | Nombre | CRM | Canal confirmado al preparar | Estado |
| --- | --- | --- | --- | --- |
| comounmadrigal | Madrigal | 235 | Instagram | Enviado el 14 a las 19:17; una burbuja y compositor vacío |
| da_pawn | Da Pawn | 233 | Instagram | Enviado el 14 a las 19:18; una burbuja y compositor vacío |
| phia.dj.music | PHIA dj | 234 | Instagram | Enviado el 14 a las 19:24; una burbuja y compositor vacío |
| yosoysheen | Sheen | 236 | Instagram | Enviado el 14 a las 19:20; una burbuja y compositor vacío |
| miguelgallardokeys | Miguel Gallardo | 238 | Instagram | Enviado el 14 a las 19:21; una burbuja y compositor vacío |

La lectura autenticada del 14 de septiembre confirmó una ficha exacta por handle, `hasUserAccount=false` y ausencia de correo, teléfono o WhatsApp en los cinco registros. Esta evidencia debe renovarse antes del envío, incluyendo búsqueda de identidades separadas, respuestas, bajas y recordatorios anteriores.

Cada recordatorio llevará el enlace individual original, oferta de ayuda y opción de baja. No prometerá aprobación automática, porque el despliegue de ese cambio no está verificado. Los detalles de ejecución están en `tu-escena-conectada-recordatorios-2026-09-15-prompt.md`.

Límites: hasta cinco destinatarios, una vez por canal verificado, máximo **20 mensajes de campaña en total por día de America/Guayaquil**, contando invitaciones y recordatorios de todos los canales. No sustituir destinatarios excluidos ni repetir entregas ambiguas. Los cinco destinatarios recordados el 14 quedan fuera de este bloque.

## Programación

- Tarea única OpenClaw: `efae45f5-4e2a-4d03-acd1-7d2246340621` (`tdf-artist-reminders-2026-09-15`).
- Hora originalmente programada: `2026-09-15T14:00:00.000Z`, equivalente a las 09:00 de Ecuador. Tras completar los envíos el 14, `cron disable` confirmó **enabled=false** y estado sin próxima ejecución. La tarea quedó conservada, no eliminada.
- Ejecución: `/bin/sh scripts/run-artist-reminders-2026-09-15.sh`, con el ejecutor local ya utilizado por la campaña. Se usa una tarea de comando porque el ejecutor de IA predeterminado de OpenClaw presenta errores de saldo del proveedor. La sesión local de Codex confirmó acceso mediante ChatGPT.
- El lanzador conserva el sandbox `workspace-write` y la revisión automática de permisos del flujo existente; no desactiva aprobaciones ni el sandbox. La sintaxis POSIX pasó y ejecutarlo el 14 devolvió «No reminders sent», confirmando que no inicia antes de la fecha autorizada.
- El trabajo original tenía un límite de 90 minutos y salida final prevista en `tu-escena-conectada-recordatorios-2026-09-15-result.md`; no llegó a ejecutarse. El lanzador se retiró y ahora solo informa que el bloque está completado, sin iniciar agentes ni enviar mensajes. Su sintaxis y ejecución inocua fueron verificadas. El prompt también marca el bloque completado, de modo que una invocación antigua no debe repetirlo.

## Resultado de ejecución

Cinco recordatorios enviados y confirmados dentro de la ventana de Ecuador. Cada hilo mostró una sola aparición del mensaje nuevo, compositor vacío y ausencia de indicador de envío pendiente o error. Se revalidaron los cinco contactos por handle y nombre, sin duplicados exactos, cuentas vinculadas ni canales adicionales registrados. No se crearon usuarios ni contactos.

Los cinco hilos contenían la invitación original del 11 de septiembre y ningún recordatorio posterior ni baja. Sheen había reaccionado con 👍 a su invitación. Los textos nuevos ofrecen el enlace individual y ayuda para continuar, sin prometer aprobación automática.

PHIA no tenía botón Message en el perfil. La búsqueda normal del selector de destinatarios abrió su hilo existente `/direct/t/18053540891499783/`, donde se verificaron el handle, la invitación original y el compositor antes de enviar. No se siguió la cuenta ni se pagó por mensajería. Madrigal ofreció solicitud normal y prioritaria; se usó solo la normal.

La recuperación del navegador se logró activando la pestaña de Instagram mediante el endpoint local de Chrome y usando comandos directos a esa página. No fue necesario reiniciar Chrome ni alterar perfiles. El conector general Playwright seguía fallando, pero las lecturas y las acciones sobre la página activa respondieron.

### Intento de adelanto — 14 de septiembre, desde 17:38

Diego pidió continuar después de elevar el límite diario a 20. Se intentó adelantar el bloque de cinco recordatorios dentro de la ventana vigente. No se enviaron mensajes: la conexión de lectura a CRM e Instagram agotó sus plazos antes de poder actualizar cuentas, respuestas o bajas.

El endpoint local de listado de pestañas respondió y confirmó las pestañas de TDF e Instagram. Sin embargo, tanto Playwright sobre CDP como la evaluación directa de la página fallaron por timeout, también en una comprobación fuera del sandbox. No se reinició ni cerró el navegador, no se introdujeron credenciales y no se ejecutó ningún control de envío. La programación del 15 permanece como respaldo y debe renovar las comprobaciones antes de enviar.
