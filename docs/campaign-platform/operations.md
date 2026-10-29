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
