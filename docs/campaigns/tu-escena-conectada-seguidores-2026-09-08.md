# Tu Escena Conectada — seguidores 2026-09-08

Estado del corte: preflight de las 09:00 detenido sin envíos por sesión concurrente; bloque **reanudado con autorización expresa** tras ampliar la ventana a 09:00–20:00. Cierre final: **1 DM confirmado y 1 contacto CRM verificado** el 8 de septiembre de 2026 (`America/Guayaquil`).

## Resultado

- Hora de inicio del preflight: 09:00; hora de detención y documentación: 09:04.
- La hora local estaba dentro de la ventana vigente de 09:00–18:00.
- Los informes y la memoria del día no registraban DMs nuevos del 8 de septiembre; el total diario previo era 0 de 10.
- DMs nuevos confirmados durante el preflight inicial: **0 de 5**.
- Contactos CRM creados, completados o verificados durante el preflight inicial: **0**.
- Conversiones atribuidas: **0**.

## Incidente y punto exacto de detención

Durante el preflight, antes de revisar seguidores, seleccionar destinatarios, abrir hilos o consultar el CRM, se comprobó que el navegador OpenClaw autorizado estaba activo con el perfil persistente `openclaw` y una pestaña de Instagram en `https://www.instagram.com/tdf.records.label/`.

La inspección read-only de procesos y del endpoint local de depuración detectó simultáneamente otro Chrome persistente con el perfil `tdf-instagram`, iniciado el 7 de septiembre, cuya pestaña activa era `Instagram • Messages` en `https://www.instagram.com/direct/inbox/`. Esto constituye una sesión concurrente según la regla de seguridad del bloque.

Se detuvo toda la ejecución en ese punto. No se cerró, reemplazó, liberó ni tomó control de ningún perfil persistente; no se abrió una sesión adicional; no se revisaron ni contactaron seguidores; no se escribió ningún texto; no se pulsó enviar; y no se creó, editó ni eliminó ninguna ficha del CRM.

## Reanudación autorizada — 19:00 a 19:49

- A las 19:00 Diego amplió expresamente la ventana permanente de la campaña a 09:00–20:00 y pidió continuar.
- Se verificó que el Chrome conflictivo del puerto 9333 era huérfano y había sido cerrado limpiamente sin borrar ni modificar el perfil. El navegador OpenClaw conservaba una sola sesión autenticada como `tdf.records.label`.
- La lista de seguidores devolvió a `gabbydiazart` como seguidora vigente. Su perfil público seguía activo y la identificaba como artista, primera mujer DJ de techno y hard techno en Ecuador. El hilo exacto contenía menciones antiguas de historias y no contenía una invitación de esta campaña, objeción ni solicitud de baja.
- Una consulta transaccional `READ ONLY` nueva, realizada antes de abrir el envío, no encontró coincidencias por Instagram ni por el nombre público `Gabby Díaz` en el CRM y tampoco encontró una cuenta de usuario asociada. La transacción posterior al envío volvió a comprobar la ausencia antes de insertar.
- A las 19:43 se envió una sola invitación combinada a `gabbydiazart`. Se confirmó una única burbuja completa en el hilo, el `utm_content=gabbydiazart` exacto y el compositor vacío; después el mensaje apareció como visto y, en la comprobación técnica posterior al cierre, el inbox mostró una reacción `🙌` de Gabby. No se envió seguimiento ni se atribuyó conversión por esa reacción. Instagram no mostró CAPTCHA, restricción, solicitud pagada ni advertencia de automatización.
- Solo después del envío confirmado se creó `Gabby Díaz` como `Persona`, `partyId 213`, Instagram exacto `gabbydiazart`. La relectura devolvió una única ficha y `has_user=false`.
- El transporte local de OpenClaw acumuló latencias y timeouts durante las lecturas. A las 19:49 se cerró conservadoramente el envío del día: el margen restante no permitía completar perfil, hilo, CRM, envío y verificación de otro destinatario sin acercarse indebidamente al cierre de las 20:00. No se forzó la interfaz ni se envió un segundo DM.
- A las 19:52 Diego volvió a ordenar continuar. Se reabrió únicamente el perfil propio mediante navegación de solo lectura para intentar validar a `biieege`, pero el transporte CDP agotó el tiempo antes de abrir de forma determinista la lista de seguidores. A las 19:55 se detuvo el intento sin abrir el perfil o hilo del candidato, sin escribir texto, sin pulsar enviar, sin modificar relaciones de seguimiento y sin tocar el CRM. El total del día permaneció en 1 DM y 1 contacto.
- A las 19:57 Diego volvió a ordenar continuar. Quedaban menos de tres minutos y no se inició otro DM. Al cerrar la ventana se añadió y validó un helper local de CDP para la ejecución del 9 de septiembre y se endureció su prompt contra referencias visuales obsoletas; estas acciones posteriores fueron únicamente técnicas y no tocaron Instagram ni CRM.

## Recuperación técnica posterior al cierre

- Después de las 20:00 no se inició, escribió ni envió ningún DM y no se consultó ni modificó el CRM.
- Una comprobación posterior encontró el proceso de Chrome administrado bloqueado: el target seguía anunciado, pero el puerto CDP no completaba comandos. Se cerró el navegador limpiamente mediante `Browser.close`; el perfil, las cookies y los archivos persistentes quedaron intactos.
- `openclaw browser doctor` confirmó que el gateway, el plugin y la configuración del perfil estaban sanos. No existían archivos `Singleton*` ni otro proceso escuchando en 18800. Dos intentos de arranque del supervisor agotaron su ventana sin que Chrome publicara CDP.
- El mismo Chrome 152 se inició directamente con los argumentos administrados y el puerto 18800 respondió. El directorio contiene dos subperfiles: `Default` (`openclaw`) no está autenticado, mientras `Profile 1` (`Diego`) conserva la sesión de `tdf.records.label`. Se abrió únicamente el inbox del subperfil correcto, se cerró la pestaña de login creada durante el diagnóstico y `Local State.profile.last_used` quedó en `Profile 1`.
- La comprobación final devolvió `running: true`; `openclaw browser tabs` mostró `Instagram • Messages`, la instantánea mostró `tdf.records.label` y el helper local confirmó el inbox autenticado. El helper ahora prioriza el target `/direct/` y falla cerrado si no puede demostrar la identidad `tdf.records.label`.
- El job único `70df8ef3-e7ae-436c-9462-62bed8d2f773` se revalidó habilitado para 2026-09-09 09:00 `America/Guayaquil`, con borrado posterior y máximo restante de cuatro DMs. Su prompt documenta el subperfil autenticado y prohíbe usar o completar el formulario de login.
- A las 22:35 una nueva comprobación mostró que el puerto 18800 y el inbox autenticado seguían respondiendo aunque el estado del gateway devolvió transitoriamente `running: false`; `lsof`, `/json/version` y el helper confirmaron que Chrome 152 continuaba escuchando. Para no depender de ese indicador bajo carga, el launcher del 9 de septiembre ahora ejecuta un preflight directo: demuestra la identidad con el helper y, solo si falla, abre `Profile 1`, espera hasta un plazo total real de 45 segundos y entrega el control al ejecutor. La sonda del preflight tiene un timeout propio de cuatro segundos para que un CDP parcialmente bloqueado no multiplique ese plazo; los diagnósticos ordinarios del helper conservan treinta segundos. Si no logra demostrar `tdf.records.label`, el prompt obliga a detenerse sin enviar. La sintaxis POSIX, la ruta de Node, la sonda real y `git diff --check` pasaron.

Mensaje confirmado:

> Hola, Gabby 👋 Vimos tu trabajo pionero en el techno y hard techno ecuatoriano. Queremos invitarte a dos fechas de TDF: Listening Party de Labii + Llama Este Pez, jue. 10 sep., 20:00 en TDF Records – Studio Legends (Quito); y Entre Panas en el Domo con Llama Este Pez, sáb. 12 sep., 15:00–18:00 en Domo del Pululahua (entrada $5, reservas 0984755301). También nos ayudaría mucho que pruebes TDF creando tu perfil público y reuniendo tu música; si encuentras errores o tienes sugerencias, cuéntanos sin compartir contraseñas ni códigos: https://tdf-app.pages.dev/login?signup=1&intent=artist&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=gabbydiazart. Es un solo contacto; si no te interesa, no volvemos a escribir. — TDF Records

## Destinatarios del preflight inicial

Ninguno. No hubo selección ni precontacto debido a la condición de parada.

## Total diario al corte

- DMs nuevos confirmados el 8 de septiembre: **1 de 10**.
- Contactos CRM verificados derivados de envíos del día: **1** (`partyId 213`).
- Conversiones confirmadas: **0**.
- Del bloque conservador de cinco quedan **cuatro** invitaciones. La ejecución única del 9 de septiembre debe excluir `gabbydiazart`, revalidar cada destinatario restante y operar solo dentro de 09:00–20:00.
