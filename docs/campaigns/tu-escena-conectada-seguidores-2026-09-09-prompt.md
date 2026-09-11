# Ejecución autónoma: seguidores de Instagram — 2026-09-09

Trabaja en `/Users/diegosaa/GitHub/tdf-app`. Antes de actuar, cumple `AGENTS.md`: lee completos `SOUL.md`, `USER.md`, `memory/2026-09-09.md` si existe, `memory/2026-09-08.md` y `MEMORY.md`. Lee también completos:

- `docs/campaigns/tu-escena-conectada-seguidores-2026-09-08-prompt.md`
- `docs/campaigns/tu-escena-conectada-seguidores-2026-09-08.md`
- los cuatro informes históricos que exige el prompt del 8 de septiembre.

El launcher ejecuta primero un preflight local de identidad. Si su salida indica que el target autenticado no está disponible, no intentes enviar: limita la ejecución a diagnóstico seguro y documentación. Una salida positiva no sustituye ninguna de las revalidaciones por destinatario que siguen.

El bloque programado del 8 de septiembre se detuvo antes de cualquier selección, revisión de hilo, consulta CRM o envío al detectar otro Chrome persistente. El usuario volvió a indicar **Continue** después de ese corte y a las 19:00 amplió expresamente la ventana operativa permanente hasta las 20:00. Una revisión posterior confirmó que ese Chrome era huérfano, sin clientes CDP conectados, y se cerró limpiamente mediante `Browser.close`; no se borró ni modificó su perfil. Si el bloque se ejecuta el 9 de septiembre, solo puede operar dentro de 09:00–20:00 de `America/Guayaquil` y debe descontar cualquier contacto confirmado en la ejecución reanudada del 8 de septiembre.

## Alcance autorizado

Aplica íntegramente las reglas de contenido, selección, seguridad, CRM y auditoría del prompt del 8 de septiembre, con estas sustituciones obligatorias:

- Fecha de ejecución: 9 de septiembre de 2026.
- Máximo restante del bloque original: cuatro DMs nuevos confirmados, porque `gabbydiazart` consumió uno de los cinco cupos el 8 de septiembre; máximo diario absoluto: diez.
- Informe del bloque: `docs/campaigns/tu-escena-conectada-seguidores-2026-09-09.md`.
- Memoria: añade un resumen fiel a `memory/2026-09-09.md` mediante `apply_patch`, sin sobrescribir contenido existente.

Cada destinatario recibe un único DM breve y natural en español que combine:

1. Listening Party de Labii y Llama Este Pez — jueves 10 de septiembre, 20:00, TDF Records - Studio Legends, Quito.
2. Entre Panas en el Domo con Llama Este Pez — sábado 12 de septiembre, 15:00–18:00, Domo del Pululahua, entrada USD 5, reservas 0984755301.
3. Invitación a probar TDF: crear su perfil público de artista, reunir su música y compartir errores o sugerencias; nunca pedir contraseñas, códigos ni información sensible.
4. Enlace individual exacto, sustituyendo `<handle>` por el usuario normalizado: `https://tdf-app.pages.dev/login?signup=1&intent=artist&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=<handle>`.
5. Aclaración de que es un solo contacto y que, si no interesa, no se volverá a escribir.

No dividas el texto en varios mensajes ni excedas el límite visible del compositor.

## Cola preparada, con revalidación obligatoria

Evalúa en este orden, sin asumir elegibilidad:

1. `biieege`
2. `domingo.caballero.techno`
3. `percuto`
4. `dj_robalino`
5. `almaconvoz` — suplente
6. `sophiesilvermusic` — suplente
7. `fusionimperium` — suplente

Inmediatamente antes de cada envío confirma de nuevo que la cuenta:

- sigue a `@tdf.records.label`;
- muestra actividad pública reciente, relación musical clara y señales razonables de ser adulta;
- dispone de compositor normal gratuito;
- no tiene en el hilo invitación previa de esta campaña, objeción, baja ni entrega ambigua;
- no coincide exactamente por Instagram o nombre con un usuario del sistema ni con una identidad CRM duplicada o ambigua.

La auditoría preparatoria no encontró coincidencias exactas para la cola, pero no sustituye la consulta en vivo. Excluye `amwordhouse`, `klausrevolucion`, `nigredo.hex`, `iraizoviedocanta` y `calavery.inc` porque ya tienen contacto exacto en CRM. Excluye también `cachorroblackec` y `calandriasincendiandoelespejo` por pertinencia musical insuficiente, además de todas las cuentas contactadas o descartadas en los informes históricos. No crees usuarios.

Excluye expresamente `gabbydiazart`: recibió el DM combinado a las 19:43 del 8 de septiembre, apareció una sola vez y quedó registrada como `Gabby Díaz`, `partyId 213`, sin cuenta de usuario. No abras un nuevo contacto ni envíes seguimiento por silencio.

## Controles de envío

- Verifica antes de cada envío la hora local y el total diario. Si está fuera de 09:00–20:00, no envíes nada; no inicies un DM después de las 20:00.
- Usa solo la sesión autenticada de Instagram en el navegador OpenClaw. No cierres, sustituyas ni borres perfiles persistentes.
- El directorio administrado contiene dos subperfiles de Chrome. La sesión autenticada de `tdf.records.label` está en `Profile 1` (nombre visible `Diego`); el subperfil `Default` (`openclaw`) muestra el formulario de login y nunca debe usarse para la campaña. El preflight del 8 de septiembre dejó abierto el inbox autenticado y `Local State.profile.last_used` en `Profile 1`.
- Usa la sintaxis instalada sin opciones inexistentes: `openclaw browser status`, `openclaw browser tabs` y `openclaw browser snapshot --efficient`. No añadas `--json`, `--browser-profile` ni `--timeout` a subcomandos que no las admiten.
- Antes de revisar seguidores, confirma que `openclaw browser tabs` muestra una pestaña `Instagram • Messages` y que la instantánea contiene `tdf.records.label`. Si aparece el selector de perfiles o solo una pestaña de login, no ingreses credenciales: abre el subperfil correcto con `'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' --remote-debugging-port=18800 --user-data-dir=/Users/diegosaa/.openclaw/browser/openclaw/user-data '--profile-directory=Profile 1' --no-first-run --no-default-browser-check --disable-sync --disable-background-networking --disable-component-update --disable-features=Translate,MediaRouter --disable-session-crashed-bubble --hide-crash-restore-bubble --password-store=basic --no-proxy-server https://www.instagram.com/direct/inbox/`, vuelve a comprobar la identidad y cierra únicamente cualquier pestaña de login creada por el diagnóstico. Si la identidad sigue sin confirmarse, detén el bloque.
- Si el gateway de OpenClaw vuelve a tardar más de 20 segundos o devuelve timeout, puedes usar `node scripts/instagram-openclaw-cdp.mjs` para consultar el único target de Instagram del puerto local 18800. El helper no selecciona destinatarios ni envía por sí solo. Sigue siendo obligatorio inspeccionar visualmente perfil, hilo y compositor y confirmar la burbuja posterior.
- Antes de cada interacción vuelve a localizar el elemento en el DOM o toma una instantánea nueva. Nunca reutilices una referencia `eN` después de navegar, abrir o cerrar un diálogo. No pulses `Follow`, `Following`, `Remove` ni controles de relación; la campaña solo autoriza abrir perfiles/hilos y enviar el DM aprobado.
- Un segundo Chrome **realmente activo en Instagram** es condición de parada. Procesos dormidos de servidores Playwright/MCP, por sí solos, no constituyen otra sesión. El Chrome huérfano del puerto 9333 ya fue cerrado; si reaparece o surge cualquier duda, detén el bloque.
- Si aparece una solicitud normal gratuita y otra prioritaria pagada, usa únicamente la normal gratuita. No pagues ni actives publicidad.
- Envía una sola vez por persona. Confirma una única burbuja completa en el hilo y el compositor vacío. Si el estado es ambiguo, no reintentes.
- Detén todo el bloque ante CAPTCHA, verificación, restricción, advertencia de automatización, concurrencia, error persistente o comportamiento inesperado. No intentes evadirlo.

## CRM y evidencia

Solo después de un envío visiblemente confirmado crea o completa un contacto único con el nombre público e Instagram exacto. Usa `Empresa` para bandas, sellos o proyectos, y `Persona` para individuos. Revalida una sola ficha exacta, el Instagram correcto y ausencia de cuenta de usuario. Ante ambigüedad, no edites ni elimines nada y detén el bloque.

No atribuyas conversiones sin evidencia de un registro nuevo posterior al DM. Documenta en el informe hora, destinatarios confirmados, resultado individual, CRM, incidentes y total diario. Termina indicando DMs confirmados, contactos CRM verificados y cualquier bloqueo; no afirmes éxito sin evidencia visible.
