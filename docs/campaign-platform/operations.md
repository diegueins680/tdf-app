# Campañas musicales desde TDF

Implementación autorizada por Diego el 29 de octubre de 2026. El panel independiente está en https://api.tdfrecords.net/campaign-platform/ y utiliza la sesión habitual de TDF. La ruta web `/social/campanas` incorpora el panel después de la revisión e integración normal del frontend; su existencia en esta rama no implica despliegue de esa ruta.

## Acceso y revisión

Cualquier usuario autenticado puede crear campañas, sin rol de artista. Cada campaña pertenece a su usuario; los roles de equipo son viewer, editor, reviewer y manager. Ser administrador no otorga lectura general de campañas privadas. Las cuentas conectadas pertenecen a su usuario; los canales TDF aparecen como destinos compartidos, pero cada versión exacta requiere aprobación de un administrador de TDF. El servidor mantiene un registro de identidades oficiales: al conectar para TDF filtra las demás páginas que el proveedor pudiera devolver por autorizaciones anteriores. Una conexión personal tampoco importa esas identidades oficiales. Los mensajes privados tienen aprobación separada y envío asistido. Editar una pieza invalida sus aprobaciones y la aprobación de campaña.

Los originales se conservan como objetos independientes. Las cargas llegan por fragmentos reanudables de 4 MiB y se calculan SHA256 al terminar. Las conexiones sociales permanecen cifradas en el servidor; no se incluyen en exportaciones, copias de campaña ni registros HTTP. Desconectar una cuenta invalida futuras ejecuciones. Eliminar un original exige confirmación expresa y no se permite si sigue referenciado.

## Flujo

1. Crear campaña y conectar una cuenta propia administrada por el usuario.
2. Subir el máster o materiales terminados, verificar destino específico y derechos.
3. Proponer el plan de 14 días o introducir intervalos manualmente. Se generan seis clips, ocho Stories y seis láminas a partir de fotogramas reales. Los picos RMS son una propuesta de selección; requieren revisión musical, visual y de las entradas y salidas. No se generan letras ni atribuciones no confirmadas.
4. Revisar archivos, copias, cuentas, fechas y ruta hasta la sesión. El material que menciona TDF utiliza el logo oficial.
5. Aprobar la versión; un administrador revisa adicionalmente cada pieza para TDF. Ejecutar activa la cola. Pausar conserva la cola; cancelar impide nuevos trabajos. La acción que ya haya cruzado un límite externo requiere reconciliación y no puede deshacerse mediante una pausa.
6. Descargar y completar los pasos nativos que la plataforma no expone. Los avisos distinguen bloqueo, procesamiento y resultado incierto.

## Capacidades reales

| Plataforma | Desde el servidor | Paso pendiente o asistido |
| --- | --- | --- |
| YouTube | OAuth, renovación, carga privada reanudable, espera de HD, publicación tras revisión de Studio; comentarios aprobados; estadísticas y Analytics autorizados | Derechos y restricciones completos, reproducción, miniatura y sincronía se revisan en Studio sobre el video exacto antes de confirmar. Comunidad, colaboraciones, video relacionado y pantallas finales se completan en la interfaz nativa. |
| Instagram | Reels, imágenes y carruseles; Stories elegibles sin sticker | Stories con enlace y mensajes se completan en Instagram. La elegibilidad y permisos dependen de la cuenta y aplicación Meta. |
| Facebook | Conectores de texto/enlace, foto, varias fotos, video y Reels, con espera de procesamiento y control de publicación | Falta conectar una página con los permisos efectivos y validar la integración real. Las pruebas de protocolo no son publicaciones reales. Stories y mensajes siguen asistidos. |
| TikTok | Preparación, descarga y calendario | No hay claves de aplicación ni auditoría de publicación configuradas. La publicación es asistida hasta completar la integración y aprobación. |

El consentimiento de YouTube para el piloto TDF ya terminó y se verificó el canal `UCx9Jpaw_XDrMtIdzWYlU51g`. El aviso de aplicación no verificada sigue siendo un bloqueo para un lanzamiento general sin fricción: debe completarse la verificación de Google y los permisos/revisión de Meta y TikTok según los casos de uso. No se sustituyen estas revisiones por automatización de navegador.

## Ejecución y recuperación

El API y el ejecutor son servicios systemd independientes en el servidor de TDF. SQLite WAL conserva versiones, aprobaciones, trabajos, métricas y auditoría. La toma de trabajos usa transacción inmediata. El ejecutor mantiene su latido durante procesamientos largos. Tras interrupción, una publicación cuyo resultado puede ser externo queda `unknown` y nunca se reenvía automáticamente. Los trabajos internos y consultas admiten recuperación limitada; no se promete exactamente una vez frente a un proveedor externo.

Los plazos tienen zona horaria IANA. Un trabajo vencido se bloquea en lugar de enviar una publicación atrasada sin revisar. Las URLs de entrega a los proveedores están firmadas y vencen. FFmpeg corre con CPU y RAM limitadas, sin modificar el original. El servidor rechaza reservas si no queda capacidad física, aunque el plan comercial tenga cuota.

Un monitor independiente comprueba el latido y crea avisos persistentes. Avisos dentro de TDF están activos; correo requiere elección del usuario y SMTP configurado. No se enviaron correos de prueba a terceros. Push, WhatsApp y otros canales todavía requieren integración y consentimiento; no se anuncian como activos.

La copia nocturna conserva un snapshot SQLite consistente y un manifiesto local. Además, el Drive de TDF está conectado para respaldos privados cifrados, previa comprobación del espacio ya contratado. El servicio independiente respalda configuración, SQLite y todos los archivos terminados; carga el manifiesto cifrado al final. Los fragmentos de cargas todavía incompletas quedan excluidos explícitamente. Los objetos ya verificados se reutilizan, y una carga interrumpida continúa desde el rango confirmado por Drive. Tres interrupciones bloquean la solicitud para revisión. No se compró almacenamiento ni se activaron cobros.

Solo el administrador custodio configurado administra este respaldo. La clave privada se descarga una vez y permanece fuera del servidor. Su custodia se comprueba con una firma local, sin subir la clave. El servidor conserva la clave pública y cifra cada objeto antes de transferirlo. La carpeta debe permanecer privada, sin permisos compartidos; no publique su enlace como material de campaña. El respaldo funciona con la Mac apagada; la clave local solo es necesaria para recuperar datos.

La clave de recuperación de esta instalación está en `~/Downloads/tdf-campaign-recovery-c09086095a2984fc.pem`, con permisos 0600. Diego debe conservar otra copia privada fuera de esta Mac, bajo su propia custodia. No incluirla en Git, correos, el servidor ni en el mismo Drive del respaldo. Sin esa clave no puede descifrarse el respaldo. No se cambió ni se rotó la clave después de confirmar su custodia.

Antes de una futura congelación coordinada de escritores de producción, detener **API, worker, ambos servicios de respaldo, servicio monitor y los tres timers (backup, backup-request y monitor)** de campañas, además de las otras superficies que determine el plan de recuperación. Esta entrega no amplía ni debilita el inventario fijado de admisión de producción; se requiere una revisión explícita antes del siguiente procedimiento coordinado.

## Piloto Roj(o)

La sesión existente https://youtu.be/uRNyIMLiqU8 se publicó el 28 de octubre a las 19:34 de Quito; se grabó el 24 de febrero de 2025. La importación conserva original y derivados, créditos, calendario, aprobaciones y publicaciones ya existentes. No repite sesión completa, Short, Comunidad, comentario fijado ni invitaciones ya entregadas. La conexión Instagram corresponde a `@tdf.records.label`; YouTube a TDF Records y al identificador arriba indicado.

La campaña se activó en el servidor el 29 de octubre. Las Stories iniciales ya vencidas permanecen bloqueadas para revisión. El próximo Reel está previsto para el 29 de octubre a las 18:30, Quito. Los puntos de medición se conservan a 24/72/168/336 horas desde la publicación real; los resultados todavía no disponibles no se convierten en ceros ni en conversiones atribuidas.

## Capacidad propuesta

| Plan | USD/mes | Espacio | Campañas activas | Minutos exportados/mes |
| --- | ---: | ---: | ---: | ---: |
| Free | 0 | 5 GB | 1 | 15 |
| Artist | 9 | 25 GB | 3 | 60 |
| Pro | 19 | 100 GB | 10 | 180 |
| Team | 49 | 500 GB | 25 | 600 |

Precios y capacidades son propuestas; no hay facturación ni upgrade automático. El piloto tiene una excepción interna de 25 GB porque su máster supera 5 GB. El host actual tiene capacidad limitada: los tiers mayores requieren provisionamiento y validación de costos antes de venderlos. No se promete espacio físico inexistente.

## Verificación y pendientes al 29 de octubre

Se verificó el identificador y consentimiento del canal de YouTube y se habilitó YouTube Analytics API en el proyecto existente de TDF. Las consultas de tiempo de visualización, suscriptores, retención y fuentes responden sin errores, pero la sesión todavía no tiene filas de Analytics disponibles. La lectura pública inicial observada fue de 94 reproducciones, 13 me gusta y 8 comentarios; incluye posibles reproducciones de verificación. No representa un objetivo ni atribución de resultados a la campaña.

Las impresiones de miniatura y CTR requieren lectura/exportación de Studio o integración de Reporting API. No se sustituyen por impresiones de anuncios. Referencias: [informes de canal](https://developers.google.com/youtube/analytics/channel_reports), [métricas](https://developers.google.com/youtube/analytics/metrics).

La prueba de continuidad ejecutó dos trabajos internos, una sola vez cada uno, después de finalizar la conexión SSH y reiniciar el ejecutor. No se apagó físicamente la Mac. La copia SQLite se restauró en un directorio aislado, con integridad y claves foráneas correctas y recuentos de tablas equivalentes; esto no prueba recuperación de objetos ni claves fuera del servidor. La reimportación del piloto devolvió `already_imported`.

La prueba real de FFmpeg exportó seis clips, ocho Stories y seis láminas con original conservado y cero publicaciones. La selección por energía necesita revisión de musicalidad y encuadres. Estos archivos de prueba no sustituyeron los materiales de Roj(o) ya revisados. Las guías de estrategia, calendario, copies, kit del artista y metadatos del paquete original están disponibles como materiales privados de la campaña.

Primera calificación (conservada con su identidad de fuente): 35 pruebas del servicio, 3.670 pruebas web en 318 suites, 653 pruebas Mobile en 98 suites, build/lint web, validación Mobile y calidad general del repositorio completados. La calidad general conserva 11 omisiones explícitas de plataforma. El [recibo de alcance y fuentes](../../reports/campaign-platform-2026-10-29.json) distingue ejecuciones, fuente desplegada y limitaciones. Google Auth muestra En producción y 1 de 100 usuarios para permisos adicionales sin verificar; no se cambió ese estado.


## Recuperación y alcance de la ampliación

La ampliación tiene 61 pruebas locales y en el servidor. Su recibo separado [respaldo y conectores](../../reports/campaign-platform-drive-2026-10-29.json) conserva la versión desplegada, la interrupción controlada y el resultado real de recuperación; no reemplaza las evidencias originales de web/Mobile ni atribuye sus ejecuciones a la nueva fuente Python.

Para verificar una copia con el host disponible, ejecutar `verify_drive_backup.py` con `--key-file`, `--ssh-host`, `--ssh-key`, `--manifest` y `--receipt`. El comando descarga únicamente objetos cifrados por SSH, descifra y verifica hashes localmente, restaura SQLite en un directorio aislado y comprueba integridad, claves foráneas, cobertura de archivos y recuperación del almacén de credenciales. El máster se autentica por streaming sin guardar otra copia. La herramienta nunca inicia servicios ni publica desde las credenciales restauradas.

Si se pierde el host, descargar los objetos cifrados directamente desde la carpeta privada con la cuenta de Drive custodio. El manifiesto identificado en el recibo se descifra primero con `backup_crypto.decrypt_stream`; contiene los identificadores, tamaños y SHA256 de configuración, SQLite y materiales. Restaurar en un entorno aislado, sin red de publicación, con todos los servicios/timers detenidos. Verificar todos los objetos antes de reconstruir sus rutas y conciliar trabajos externos. La base capturada puede preceder al registro de finalización del propio respaldo: reconstruir el índice de objetos desde el manifiesto antes de habilitar nuevas copias, para evitar duplicados. La reconstrucción completa del host, PostgreSQL principal, activación de colas y failover no están automatizados ni calificados por esta entrega. No hay borrado automático ni política de retención destructiva.


Para completar Facebook, las direcciones de retorno de Facebook e Instagram del panel ya quedaron añadidas y verificadas en la aplicación Meta existente, conservando su URI anterior. `pages_manage_posts` quedó agregado al caso de uso de Pages y figura listo para pruebas; esto no equivale a aprobación para usuarios externos. Falta confirmar cuál de las dos páginas de TDF utilizar y completar su consentimiento nativo sin modificar las conexiones de otros proyectos. No se realizaron publicaciones de prueba. El registro de identidad y los permisos efectivamente devueltos se verifican antes de admitir publicaciones.


La recuperación real del 29 de octubre pasó: la primera copia verificó por descifrado los 22 materiales audiovisuales, incluido el máster completo de 9.672.941.728 bytes, y recuperó SQLite, configuración y tres registros del almacén cifrado de credenciales. La copia incremental de las 12:23 de Quito añadió las 16 guías privadas: sus 41 objetos referenciados cubren 38 archivos de campaña, SQLite, configuración y manifiesto. Los 22 objetos sin cambios utilizan el recibo previo completo fijado por SHA256; los otros 19 se descifraron y comprobaron en la segunda prueba. En ambas bases aisladas, integridad, claves foráneas y cobertura de archivos fueron correctas. La clave privada permaneció en la Mac. No se iniciaron servicios restaurados ni se publicaron pruebas externas. Los límites sobre reconstrucción del host y PostgreSQL principal permanecen vigentes.


## Integración de la rama de código

El 29 de octubre se reconciliaron las fuentes de campañas con la versión protegida `6197b87326a437350dca239ee64902e78da6964c`, mediante una fusión normal que conserva los dos padres. Los tres conflictos correspondían a archivos generados y se resolvieron regenerándolos; los 156 requisitos de la versión entrante, el requisito de campañas, el código de reservas, los archivos históricos y el ejecutor desplegado quedaron preservados. La nueva calificación local pasó 3.670 pruebas web en 318 suites, build/lint, 61 pruebas del servicio, ocho pruebas de catálogo y calidad de repositorio con sus 11 omisiones de plataforma. El [recibo de integración](../../reports/campaign-platform-main-integration-2026-10-29.json) identifica la fuente ejecutada y los límites; el menú general aún requiere la revisión e integración protegidas. No se volvió a desplegar ni reiniciar el servicio operativo por esta fusión.

La lectura de campaña de las 13:12 de Quito mostró 147 reproducciones, 17 me gusta y 9 comentarios; Analytics continuaba sin filas de tiempo de visualización y retención. Es una observación previa a las 24 horas, no reemplaza el punto de evaluación programado ni demuestra atribución causal. Las dos colaboraciones nativas seguían pendientes de aceptación; se comprobaron mediante lectura de Studio, sin guardar cambios ni reenviar invitaciones.


## TikTok: conector preparado y bloqueos externos

La ampliación quedó desplegada en el panel el 29 de octubre, con las 80 pruebas del servicio aprobadas antes de activar la versión. TikTok permanece deshabilitado: no hay credenciales, aprobación de auditoría, prefijo verificado ni cuenta conectada. API, ejecutor y los tres temporizadores continúan activos; CL01 conserva su aprobación y su horario. La interfaz real cargó correctamente, con logo oficial y sin desbordamiento en 320 y 390 píxeles.

El conector incorpora video y publicaciones de fotos, renovación de credenciales con bloqueo entre procesos, revisión real de archivos y seguimiento del identificador nativo. El formulario consulta al creador, muestra su identidad y duración permitida, exige elegir privacidad sin valor por defecto y pide consentimiento expreso para cada versión y fecha. Se pueden conservar borradores sin autorizar envíos. El seguimiento distingue procesamiento, publicación restringida y confirmación pública del proveedor; esta última aún necesita comprobación independiente de reproducción. Nunca reinicia una carga cuyo resultado fue incierto.

Para habilitarlo, el administrador debe disponer de una aplicación TikTok aprobada para el servicio público multiusuario, configurar `TIKTOK_CLIENT_KEY` y `TIKTOK_CLIENT_SECRET` por el canal privado del servidor, registrar el retorno `https://api.tdfrecords.net/campaign-platform/oauth/callback/tiktok`, obtener `video.publish` y `user.info.basic` para identificar la cuenta, y verificar en el portal el prefijo `https://api.tdfrecords.net/campaign-platform/media/`. Solo después de comprobar la aprobación real y la propiedad se registran `TIKTOK_CLIENT_AUDITED: true` y `TIKTOK_VERIFIED_MEDIA_PREFIX` con ese prefijo exacto. Son condiciones externas, no interruptores para eludir una revisión. Completar luego el OAuth nativo desde el panel y una calificación real autorizada, sin publicaciones de prueba automáticas.

Los archivos del servidor se transfieren por `PULL_FROM_URL`; no se cambia a una carga local para sortear la verificación. Las URLs firmadas duran dos horas. Fotos: JPEG/WebP, hasta 35 y selección explícita de portada; no se añade música ni una marca de agua de la aplicación. El logo oficial de TDF se conserva en las piezas aprobadas que mencionan TDF. Las reglas de TikTok sobre marcas promocionales añadidas requieren revisar la elegibilidad de esos diseños; no se quitará el logo ni se declarará que el contenido está aprobado por TikTok.

La calificación del código comprende 80 pruebas Python, cuatro pruebas del formulario real y dos controles que eliminan guardas y fallan como se esperaba. No existen aún una aplicación TikTok conectada, una publicación real ni aprobación externa comprobada. El [recibo específico](../../reports/campaign-platform-tiktok-2026-10-29.json) conserva las fuentes y el alcance; las pruebas web/Mobile y la recuperación Drive anteriores conservan sus identidades originales.

Referencias oficiales comprobadas el 29 de octubre de 2026: [Direct Post de video](https://developers.tiktok.com/docs/en/content-posting-api-reference-direct-post), [fotos](https://developers.tiktok.com/docs/en/content-posting-api-reference-photo-post), [información del creador](https://developers.tiktok.com/docs/en/content-posting-api-reference-query-creator-info), [estado](https://developers.tiktok.com/docs/en/content-posting-api-reference-get-video-status), [transferencia](https://developers.tiktok.com/docs/en/content-posting-api-media-transfer-guide) y [requisitos de experiencia y auditoría](https://developers.tiktok.com/docs/en/content-sharing-guidelines).


## Integración posterior con la corrección de artistas

La rama se reconcilió normalmente con `cd091a3c81d5ab70e175d339860fa1921ca1f0c3`. Conserva los 157 requisitos entrantes, el requisito de campañas y 131 archivos de la corrección de artistas sin alterar sus bytes. Los 44 archivos del servicio desplegado y el pin Mobile permanecen idénticos; esta integración no provocó otro despliegue ni cambios del calendario. Los conflictos afectaron el registro de requisitos y cuatro índices generados. La auditoría estricta detectó un nuevo dato simulado de privacidad en la prueba del formulario de TikTok: se revisó únicamente ese candidato, conservando los 1.352 objetos de decisión y toda la conciliación histórica anterior.

La fuente combinada pasó 3.680 pruebas web en 320 suites, build/lint, 80 pruebas del servicio, cuatro del formulario real, ocho de catálogo y calidad general con sus 11 omisiones explícitas. Se verificaron 862 archivos de la fuente ejecutada contra los blobs del commit. El [recibo de esta integración](../../reports/campaign-platform-tiktok-main-integration-2026-10-29.json) identifica el commit, el fallo inicial del catálogo, su corrección y los límites. Las aprobaciones y los checks hospedados del nuevo commit siguen siendo condiciones separadas; no se integró la rama protegida ni se publicó el menú general.


## Primer Reel automático y medición de 24 horas

CL01 se publicó desde el ejecutor el 29 de octubre a las 18:30:55.358835 de Quito, en un único intento: https://www.instagram.com/reel/DfGMOaVDhJ9/ . Se comprobó en la página publicada una reproducción de 24 segundos a 720 × 1280, el logo oficial y la coincidencia completa del copy, URL de YouTube y llamada al enlace de la bio. La comprobación utilizó la sesión nativa autenticada de TDF; no prueba acceso anónimo ni sincronía por escucha. Las posibles reproducciones de control no se atribuyen a público orgánico. El recibo privado está en los materiales de la campaña como `CL01-publicacion-verificada-2026-10-29.json`. El video no se volvió a publicar.

El punto de 24 horas se ejecutó una sola vez a las 19:34:15 de Quito y registró 206 reproducciones, 20 me gusta y 10 comentarios. Se corrigió después un error de calendario: YouTube Analytics agrupa los días en horario del Pacífico, por lo que la fecha UTC del lanzamiento excluía su primer día local del rango consultado. La nueva función incluye el 28 de octubre y maneja los cambios estacionales de `America/Los_Angeles`. La captura original permanece intacta; una lectura correctiva posterior registró 208 reproducciones, 20 me gusta y 10 comentarios, con rango 28–29 de octubre del Pacífico. Analytics seguía sin filas: tiempo de visualización, retención y suscriptores siguen pendientes, no cero. No se atribuye la variación al Reel ni se afirma cobertura completa de 24 horas para los informes de días calendario.

La aplicación TikTok `TDF Records Campaigns` fue creada en borrador bajo la organización `TDF Records`, con la cuenta de TDF como propietaria. El logo de 1024 × 1024 conserva sin modificar el SVG oficial. Los datos básicos se prepararon en el formulario; la verificación de las URLs y una demostración auténtica de sandbox son requisitos pendientes antes de guardar/configurar completamente y solicitar revisión. No se solicitaron permisos de listado de videos que el conector no usa, ni se presentó una demostración simulada como real. No se obtuvieron credenciales ni se habilitó la publicación de TikTok todavía.

Referencias: [días de YouTube Analytics](https://developers.google.com/youtube/analytics/dimensions#time-periods), [registro de aplicaciones TikTok](https://developers.tiktok.com/docs/en/getting-started-create-an-app), [sandbox](https://developers.tiktok.com/docs/en/add-a-sandbox).


### Sandbox aislado de TikTok — continuación del 29 de octubre

El dominio `tdfrecords.net` quedó verificado en producción y en el sandbox nativo. La configuración de `TDF Campaigns Integration` está guardada, y la cuenta de prueba `@tdf.records.label` fue añadida por el propietario. El sitio y callback del sandbox utilizan ahora `/campaign-platform-sandbox/`, con inicio de sesión canónico de TDF y acceso reservado al administrador designado. Las credenciales de SDK se obtuvieron únicamente de sus campos nativos de la aplicación y se transfirieron por SSH al archivo privado de configuración del sandbox, sin publicarlas ni guardarlas en el equipo local.

El servicio de prueba tiene sus propios usuario de sistema, base de datos, archivos y claves; no altera el servicio ni las colas de producción. `TIKTOK_MODE=sandbox` exige la ruta y directorio separados, `TIKTOK_CLIENT_AUDITED=false`, usuario administrador y nombre de creador autorizados. Los consentimientos y las credenciales nuevas se vinculan al entorno, cliente y URL del servicio. El modo no permite otras plataformas ni una privacidad distinta de `SELF_ONLY`. La interfaz la ofrece sin valor predeterminado y conserva los controles de revisión y aprobación de TDF. El servicio no cambia automáticamente la privacidad de ningún perfil; una modificación nativa requiere autorización expresa del propietario.

La autorización real del servidor confirmó el nombre exacto del creador y la opción privada disponible. La muestra de validación es un fragmento horizontal de diez segundos del máster autorizado (02:22–02:32), sin gráficos ni marcas de agua añadidos. No reemplaza las seis piezas promocionales aprobadas ni modifica el original. Un envío privado, su procesamiento y su resultado se registran por separado; hasta confirmar el resultado del proveedor no debe marcarse publicado ni presentarse como lanzamiento público.

La extensión pasó 92 pruebas Python locales y del servidor y cinco pruebas del formulario real de JavaScript. Sus respuestas simuladas no constituyen aprobación de TikTok. Para futuros bloqueos coordinados de escritores, detener también `tdf-campaign-sandbox-api.service` y `tdf-campaign-sandbox-worker.service`; no se modificó la política fijada de admisión del host. El sandbox no tiene copia de la cola de publicaciones ni credenciales de recuperación de producción. La integración protegida del sitio principal mantiene sus revisiones y orden habituales.


El intento privado real se admitió una sola vez para la versión aprobada, con `SELF_ONLY`, comentarios/Duet/Stitch desactivados, consentimiento de vista previa, originalidad y música y aprobación de TDF. TikTok respondió HTTP 403 sin identificador de publicación. El estado quedó `unknown` / «Resultado por verificar», conservando el intento y sin reenvío automático. No se afirma que exista una publicación privada ni pública. El detalle del error no se conservó; la documentación exige cuentas privadas para clientes no auditados, pero no se atribuye ese 403 a una causa específica sin evidencia. Se solicitó una cuenta privada de prueba administrada por TDF; no se cambió la privacidad del perfil oficial.

La grabación auténtica de la pestaña muestra el flujo y el resultado pendiente (201,53 s, 2,66 MB, H.264, sin audio del escritorio ni credenciales). Es evidencia del intento, no una demostración exitosa ni un archivo enviado a revisión. No se cargó esa grabación para aprobar artificialmente el formulario de producción. El recibo de contenido y límites está en `reports/campaign-platform-tiktok-sandbox-2026-10-29.json`.


### Cuenta personal privada para validación — 30 de octubre

El propietario eligió otra cuenta de prueba y autorizó expresamente activar su privacidad; el estado se comprobó tras recargar TikTok. La identidad personal no se publica en este informe. `TIKTOK_SANDBOX_SCOPE=own` mantiene esa conexión como cuenta de su propietario, en lugar de registrarla como canal compartido de TDF. El backend exige que coincida con el nombre de usuario y ámbito configurados; los cambios invalidan contextos, consentimientos y credenciales anteriores del sandbox. El canal oficial de TDF no admite el ámbito personal. La configuración de producción y la privacidad del canal público no se modificaron.

Se revocó únicamente la antigua conexión del sandbox, y se mantuvieron pausados su campaña y el intento HTTP 403. La nueva cuenta utiliza una campaña y consentimiento distintos. El despliegue está versionado en `services/campaign-platform/deploy_sandbox.py`: exige detener solo los dos servicios aislados antes de cambiar su versión, vuelve a comprobar el servicio completo y reinicia esos dos procesos. La ampliación pasó 94 pruebas Python locales y del servidor y seis del formulario. El primer fallo de la prueba nueva correspondía a una respuesta OAuth simulada sin vencimiento ni renovación; se corrigió la respuesta de prueba antes de calificar la versión, sin relajar las reglas del producto.


La segunda prueba se ejecutó una sola vez desde el servidor y terminó en `published_restricted` con `SELF_ONLY`; una consulta posterior confirmó el resultado. La reproducción del propietario avanzó en HD (1280 × 720, 10,07 s), sin error del reproductor. No se afirma acceso público o anónimo ni una revisión auditiva de sincronía. La grabación auténtica para revisión dura 97,27 s; las esperas se recortaron y se indican en pantalla. No captura audio del escritorio ni campos de credenciales.

El 30 de octubre a las 00:53 de Quito se envió la aplicación con esa demostración mediante el portal nativo. TikTok confirmó la recepción y el estado «In review» persistió después de recargar. Eso no constituye una auditoría aprobada. Producción conserva su configuración y no tiene habilitada la publicación pública de TikTok. El cliente solicita solo `user.info.basic` y `video.publish`; el portal añade `video.upload` con el producto, pero ese flujo de borradores no se implementa ni se presenta como probado. El recibo de la validación es `reports/campaign-platform-private-tiktok-2026-10-30.json`.
