# Tu Escena Conectada — seguidores 2026-09-14

Estado actualizado: se confirmaron cinco invitaciones nuevas y cinco contactos CRM únicos. Por instrucción explícita posterior, también se envió un recordatorio de continuidad a cada invitado por el único canal verificado disponible. Todos los DMs se enviaron dentro de la ventana autorizada de 09:00–20:00 de `America/Guayaquil`.

## Resultado

- DMs nuevos confirmados: **5**.
- Recordatorios de continuidad confirmados: **10** (5 al grupo de hoy y 5 a invitados del 11 de septiembre).
- Contactos CRM únicos revalidados: **5**.
- Conversiones atribuidas al seguimiento: **0**; los cinco contactos conservan `hasUserAccount=false`.
- Límite del bloque inicial: alcanzado en cinco invitaciones; el seguimiento posterior fue una acción separada solicitada explícitamente.
- Total diario de campaña documentado: **15 DMs** (5 invitaciones + 10 recordatorios), dentro del máximo actualizado de **20**. El hilo de ScarBoy muestra además una respuesta de atención enviada por otra ejecución a las 15:39.
- Alertas, restricciones o fallas de entrega: **0**.

## Ejecución individual

### `t.duck_prod` — 10:17

- Manuel Troya; perfil público y activo de artista y productor ejecutivo de Green House Music. La personalización mencionó su trabajo de producción y las conexiones de Music Week de SAYCE.
- Seguidor actual, sin coincidencia previa por handle o nombre en CRM y con hilo exacto vacío.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Manuel Troya`, Persona, `partyId 239`, Instagram exacto y `hasUserAccount=false`.

### `diegocarvajalmusica` — 10:26

- Diego Carvajal; perfil público y activo de bajista, profesor y sesionista en Quito. La personalización mencionó su sesión en vivo grabada en Sonorii Lab.
- Seguidor actual y CRM libre. El hilo contenía únicamente una conversación histórica distinta sobre precios de estudio de marzo de 2026, sin campaña previa, objeción ni baja.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Diego Carvajal`, Persona, `partyId 240`, Instagram exacto y `hasUserAccount=false`.

### `lilith.tarantino` — 10:31

- Li de Lilith; perfil público y activo de música y composición. La personalización mencionó el universo de Lilith, `Belladona` y su proceso creativo.
- Seguidora actual, sin coincidencia previa en CRM y con hilo exacto vacío.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Li de Lilith`, Persona, `partyId 241`, Instagram exacto y `hasUserAccount=false`.

### `dannytagle` — 10:40

- Danny Tagle; perfil público y activo de DJ, locutor y curador musical de Radio Hit FM. La personalización mencionó sus historias sobre clásicos del rock.
- Seguidor actual y CRM libre. El hilo solo tenía un mensaje histórico distinto sobre un curso de DJ del 17 de junio de 2025, sin campaña previa, objeción ni baja.
- Se confirmó una sola burbuja, un único enlace rastreado, compositor vacío y ausencia de alertas.
- CRM: `Danny Tagle`, Persona, `partyId 242`, Instagram exacto y `hasUserAccount=false`.

### `_scarboy._` — 10:57

- ScarBoy; perfil público de músico, beatmaker y productor de trap, trap soul, reggaetón, afro y R&B. Su única publicación visible era un reel musical publicado el 13 de septiembre de 2026, por lo que cumplía actividad reciente.
- Seguidor actual, sin coincidencia previa por handle o nombre en CRM y con hilo exacto vacío.
- El primer compositor temporal se congeló antes de cualquier acción `Send`; esa pestaña se descartó. En una pestaña limpia se reabrió y revalidó el hilo, se escribió de nuevo el mensaje y se emitió un solo envío.
- Se confirmó una sola burbuja, un único enlace rastreado, fin del indicador `Sending`, compositor vacío y ausencia de fallas o alertas.
- CRM: `ScarBoy`, Persona, `partyId 243`, Instagram exacto y `hasUserAccount=false`.

## Contenido y atribución

Cada DM fue un único mensaje en español con:

1. Una referencia individual a actividad musical visible del perfil.
2. Invitación a crear el perfil público de artista, reunir música y conectar con la escena en TDF.
3. Enlace individual con `utm_source=instagram`, `utm_medium=dm`, `utm_campaign=tu_escena_conectada_piloto` y `utm_content` igual al handle exacto.
4. Solicitud de errores o sugerencias sin compartir contraseñas, códigos ni información sensible.
5. Aclaración de contacto único y baja: si no interesa, TDF no volverá a escribir.

No se ofreció mensajería prioritaria pagada, no se siguieron cuentas y no se crearon usuarios.

## Exclusiones destacadas

- CRM existente: `oracle.saint.rigel`, `almazonvoz`, `dj_robalino` y `lapinata_ec`; no se duplicaron.
- Privados, sin acceso de mensaje o sin actividad musical suficiente: `danbonte`, `big.fish.zzz`, `alienboy1998`, `diavlxlatinx` y `miss_goulash`.
- Fuera del objetivo musical: `makemakeuio`, identificado como restaurante.
- Se revisaron 268 entradas de una lista visible de 4.126 seguidores para resolver el bloque elegible.

## Auditoría CRM

- La lectura final de `GET /parties?limit=500&offset=0` devolvió exactamente una ficha para cada handle: `t.duck_prod`, `diegocarvajalmusica`, `lilith.tarantino`, `dannytagle` y `_scarboy._`.
- Las cinco fichas son Personas, tienen la nota `Instagram follower outreach 2026-09-14` y mantienen `hasUserAccount=false`.
- El alta de `_scarboy._` agotó el tiempo de la herramienta después de enviar la solicitud; antes de reintentar se abrió una lectura nueva, que confirmó una sola ficha ya creada. No se emitió un segundo `POST`.

## Seguimiento de producto

Durante el bloque se estableció que una invitación explícita a crear un perfil de artista no debe entrar a aprobación manual. Se implementó una ruta autenticada e idempotente que reconoce únicamente la atribución exacta de esta campaña de DM, canjea la invitación mediante la política persistida `artist.invitation.artist`, concede solo el rol no administrativo `Artist` y refresca la sesión antes de entrar a `/mi-artista`. Las solicitudes orgánicas conservan el flujo de revisión.

## Recordatorio de continuidad — 14:53

- Antes del envío se revalidaron las cinco fichas mediante `GET /parties?limit=500&offset=0`: todas mantenían `hasUserAccount=false` y ninguna tenía correo, teléfono o WhatsApp registrado.
- Se revisó el hilo exacto de cada handle después de la invitación inicial. No había respuestas posteriores, objeciones ni solicitudes de baja.
- Instagram fue el único canal posible y verificado. Correo y WhatsApp no estaban disponibles por falta de dirección o número; tampoco era posible una notificación dentro de TDF porque aún no existía una cuenta de usuario.
- Se envió un recordatorio individual a `t.duck_prod`, `diegocarvajalmusica`, `lilith.tarantino`, `dannytagle` y `_scarboy._` con el mismo enlace atribuido a cada handle.
- El texto aclaró que ya pueden crear o retomar el perfil de artista sin esperar aprobación, ofreció ayuda por respuesta y conservó una opción explícita de baja.
- Una verificación idempotente final encontró el recordatorio dentro del panel del hilo correcto para los cinco handles; no se emitieron duplicados.

**Cierre de envíos:** 5 invitaciones y 5 recordatorios confirmados dentro de horario, 5 contactos CRM únicos, 0 conversiones atribuidas y 1 canal verificado disponible. La comprobación posterior detectó la discrepancia de producto descrita abajo.

## Continuación y corrección de estado — 15:44

- Esta continuación no envió mensajes: las 10 invitaciones y recordatorios documentados ya alcanzan el límite diario de campaña. La siguiente ventana comienza el 15 de septiembre a las 09:00 de `America/Guayaquil`; no se programó ningún envío automático.
- La revisión de hilos encontró una respuesta positiva de `_scarboy._`: indicó que tomará en cuenta la invitación, que le parece una muy buena idea y agradeció el contacto. El hilo ya contenía una respuesta de TDF a las 15:39 agradeciendo y ofreciendo ayuda con errores o dificultades. Esa respuesta no fue enviada por esta continuación y constituye un mensaje saliente adicional a los 10 de campaña. No se contabilizó una conversión.
- Los otros cuatro hilos de hoy mostraron una sola aparición del recordatorio y ninguna respuesta posterior. En ScarBoy se detectaron dos apariciones del texto del recordatorio; el recuento de texto no distingue una cita de respuesta de una segunda burbuja. No se afirmó un duplicado ni se reenvió. Una inspección adicional agotó el tiempo de conexión del navegador.
- La nueva consulta autenticada del CRM volvió a encontrar una ficha por cada uno de los cinco destinatarios de hoy, todos con `hasUserAccount=false` y sin correo, teléfono ni WhatsApp. Esto indica ausencia de cuenta vinculada a esas fichas, no demuestra por sí solo que no exista un registro separado.
- Se preparó para revisión el siguiente grupo de invitados anteriores: `comounmadrigal` (235), `da_pawn` (233), `phia.dj.music` (234), `yosoysheen` (236) y `miguelgallardokeys` (238). La consulta en vivo confirmó también una ficha exacta por handle, ninguna cuenta vinculada y únicamente Instagram como canal registrado. Antes de enviar, falta revalidar sus hilos para respuestas, bajas y recordatorios previos.
- Corrección del estado de producto: `GET https://tdf-hq.fly.dev/version` devolvió el commit `1157258b6a6551d49708fa9eb21ab893b6a051f1`, con build del 10 de septiembre. El `SessionAPI` de ese commit no contiene `/session/artist-invitation`; un GET de diagnóstico a esa ruta respondió 404. El endpoint y su migración del 14 de septiembre permanecen como cambios locales. La implementación no debe describirse como desplegada.
- Por tanto, la afirmación «ya puedes crear o retomar tu perfil de artista sin esperar aprobación» de los cinco recordatorios fue prematura. No se volvió a enviar ese texto. Se debe desplegar y verificar el flujo antes de volver a prometer activación sin aprobación.

Borrador para el próximo grupo, sujeto a la revisión de cada hilo y al cupo diario:

> Hola, [nombre]. Te dejamos el enlace de Tu Escena Conectada para crear o continuar tu perfil de artista: [enlace individual de la invitación original]. Si te quedaste en algún paso, responde aquí y te ayudamos. Si prefieres no recibir más mensajes, dínoslo. — TDF Records

## Recordatorios a invitados anteriores — 19:17–19:24

Tras elevar Diego el máximo a 20 mensajes diarios y volver a pedir continuar, se adelantó el bloque previsto para el 15 de septiembre. Se enviaron y verificaron recordatorios a `comounmadrigal` (19:17), `da_pawn` (19:18), `yosoysheen` (19:20), `miguelgallardokeys` (19:21) y `phia.dj.music` (19:24). Cada uno mostró una burbuja nueva completa, compositor vacío y ausencia de error. No había bajas, recordatorios anteriores ni cuentas vinculadas en la revalidación; Sheen mantenía su reacción 👍 a la invitación original.

Se usó Instagram, único canal registrado para los cinco, con el enlace atribuido individual y oferta de ayuda. Ningún mensaje nuevo prometió activación sin aprobación. No se crearon contactos ni usuarios y no se atribuyeron conversiones nuevas. Los detalles y el estado de la tarea originalmente programada están en `tu-escena-conectada-recordatorios-2026-09-15.md`.
