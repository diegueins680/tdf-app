# TDF Records: recuerdos y activación posterior al evento

El diagnóstico inicial y sus posiciones de migración se conservan como historial.
El [informe actualizado de integración y autorización](event-memory-authorization-repair-2026-10-25.md)
registra la confirmación de fin del evento, la composición con main y los resultados posteriores.

Informe de ejecución del 25 de octubre de 2026. **Entrega parcial implementada;
no desplegada en producción ni habilitada para comunicaciones reales.** Las pruebas locales,
los resultados de CI y la verificación de producción se distinguen abajo.

## 1. Diagnóstico y capacidades reutilizadas

La auditoría utilizó el checkout protegido y una sesión de Chrome existente.
La consulta de producción verificó el enlace canónico Hetzner/contenedor/volumen y
utilizó el rol de inventario en una transacción `READ ONLY`, terminada con
`ROLLBACK`. La evidencia original, sin publicar contactos ni credenciales, permanece
en `.private/event-memories-20261025` del workspace original.

Última comprobación de base de datos: **2026-10-25 10:10:26 UTC**.

| Evento piloto 141 | Observado |
| --- | --- |
| Nombre / organizador público | PATCH CULTURE Vol. 1 / Diego Saa |
| Horario programado en Ecuador | 24 de octubre, 14:00–21:00 |
| Fin programado UTC | 25 de octubre, 02:00Z |
| Estado real | `on_sale`; no confirmado como finalizado |
| Artistas vinculados | 4 |
| RSVP aceptados | 3; expresan intención, no asistencia acreditada |
| Miembros de logística | 1 |
| Cuentas Party distintas relacionadas | 7; 5 activas |
| Señales profesionales / RSVP | 5 / 3; una cuenta en ambos grupos |
| Alias archivados en esta cohorte | 0 |
| Entradas / admisiones auditadas / momentos | 0 / 0 / 0 |
| Pedidos | 1 pendiente y 3 cancelados en la consulta inicial |
| Migración de invitaciones instalada | No |

Artistas: Emanuele Pilo-Pais, Kevin Montenegro, Llama Este Pez y Oracle Saint Rigel.
El evento, entradas, momentos, RSVP propio y resumen respondieron 200 en Chrome
con autenticación. El compositor estaba visible. Un 200 del shell público, por sí
solo, no se contó como verificación funcional. La API continúa en
`4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`; `/health` indica DB saludable.

La arquitectura existente es React/Vite, Expo y Haskell/Servant/Persistent sobre
PostgreSQL. Se reutilizaron `EventMoment`, almacenamiento duradero de assets,
Party, artistas, RSVP, admisión auditada, logística, sesión vigente, bloqueos,
reacciones, conversaciones, menciones, controles de moderación de conversaciones,
bandeja interna y previsualizaciones públicas de eventos. No se creó otra red
social, otra autoridad de contenidos ni un sistema paralelo de mensajería.

El módulo CRM de campañas/WhatsApp y las suscripciones push de Operations no
constituyen consentimiento para esta función. No se invocó ningún proveedor de
correo, WhatsApp o push.

## 2. Cambios implementados

- Backend: publicación y recuperación de imagen, video, audio y texto; lectura
  extendida explícita y feed histórico compatible para clientes antiguos.
- Carga mediante endpoints del evento, MIME/extensión/firma/tamaño, comprobación
  de sesión y acceso actual, archivos duraderos y límites de escritura existentes.
  Raster: 10 MiB; audio/video: 25 MiB. Se admite MOV/QuickTime.
- Edición de descripción con versión esperada y conflicto 409. Edición y borrado
  solamente por el autor actual, con alcance de evento y protección contra IDOR.
  Se conservan los controles y triggers de las interacciones canónicas.
- Lecturas autenticadas de momentos con `private, no-store`. Se rechaza la carga
  de archivos públicos en eventos privados, suprimidos o inexistentes.
- Web: compositor de texto/audio/imagen/video, límite uniforme de 280 caracteres,
  errores recuperables, reutilización de una carga exitosa si falla la publicación
  y bloqueo de publicación si cambia la cuenta durante la carga.
- El evento terminado prioriza recuerdos en el orden real del DOM. Oculta compras
  vencidas, mantiene información histórica y enlaza artistas mediante su Party
  canónico, además del descubrimiento de eventos futuros. Seguir es voluntario.
- Los enlaces de invitación y CTA conservan evento, intención y ancla al pasar por
  autenticación o registro. La compartición usa el enlace canónico del evento.
- Móvil: elimina nulos opcionales rechazados por el codec, usa carga autenticada
  propia del evento y rechaza el cambio de cuenta entre carga y publicación.
  Una carga fallida conserva el editor; ya no se presenta un borrador local como
  publicación remota. Esta fase móvil compone imágenes y videos.
- Invitaciones: preferencia explícita, campaña deshabilitada por defecto,
  gestión de campañas limitada al organizador actual, resúmenes agregados y
  bandeja interna reutilizada. Texto diferenciado para profesionales, asistencia
  auditada e intención RSVP.
- Contratos OpenAPI, clientes generados web/móvil, navegación compartida,
  migración/reversión, requisitos formales, fixtures SQL, pruebas HTTP y navegador
  y pasos obligatorios del CI actualizados.

## 3. Arquitectura e invariantes

`EventMoment` sigue siendo la única publicación. Las nuevas tablas únicamente
registran configuración de campaña, preferencia y entrega. La migración es la
entrada **214** del manifiesto, introducida en `18690e78a`; los 213 objetos
anteriores, sus SHAs de introducción y bytes SQL históricos se preservan.

El despachador requiere estado de ciclo de vida `completed`, fin confirmado al
menos una hora antes y dentro de los últimos 30 días, acceso y cuenta actuales,
campaña habilitada y preferencia interna explícita. Una compra aislada no acredita
presencia. Las identidades se deduplican por Party; nunca se transfieren
consentimientos por nombres, correo parecido ni alias archivados.

La clave única `(evento, Party, paso)`, los bloqueos de transacción y la escritura
atómica de notificación/recibo evitan duplicados. Se admite una invitación y como
máximo un recordatorio, separados por siete días, con límite de frecuencia entre
campañas. Publicar suprime el recordatorio incluso si después se borra el momento.
La baja vuelve a comprobarse y oculta las notificaciones históricas correspondientes.
El UUID de atribución no otorga permisos.

Se respeta la pausa global existente. El trabajador limita entregas a 20 por tick,
en una transacción independiente del otro trabajador de notificaciones. Un lote
fallido revierte y se reintenta; todavía falta aislar una fila problemática para
que no bloquee otros destinatarios.

La reversión anterior a entregas conserva contenidos/tablas/preferencias, desactiva
campañas, restaura autoridades registradas y deshabilita la capacidad añadida.
Rechaza ejecutarse después de una entrega o de un cambio posterior de autoridad.

Contrato detallado: [event-memories.md](../formal/system/event-memories.md).
El binario describe 913 métodos y 2 montajes Raw: frente a la base protegida se
añaden exactamente 8 métodos autenticados, cambia la lectura extendida/no-store
de momentos y no se elimina ninguno. Persisten 396 métodos sin documentación
OpenAPI, 25 operaciones documentadas sin ruta tipada y 8 diferencias de estado
heredadas; este snapshot no declara conformidad universal.
El modelo acotado no demuestra refinamiento SQL ni orden universal de operaciones
concurrentes sobre todas las autoridades del sistema.

## 4. Commits, PR e integración

Trabajo aislado en `/private/tmp/tdf-event-memories-20261025`, rama
`feat/event-memories-20261025`; se preservaron los cambios personales del workspace
original. Base protegida integrada: `089dce6ba` (PR #603 de comprobantes).

- `18690e78a`: backend, web, invitaciones, contratos y verificación inicial.
- `468de7907`: cargas móviles propias del evento y procedencia de migración.
- `aa92ce034`: inventarios y trazabilidad.
- `9b6512125`: integración normal de main protegido, sin descartar comprobantes.
- `8facd40f2`: evidencia final, accesibilidad y sucesores revisados tras integrar main.
- [PR principal #608](https://github.com/diegueins680/tdf-app/pull/608), borrador publicado. CI del PR iniciado; sus resultados deben comprobarse en el último SHA, separados de la validación local.
- [PR móvil #168](https://github.com/diegueins680/TDF-mobile/pull/168), borrador
  apilado sobre #163; pin `6b8f4fe6d3efc290587163a5d63b8c4ed401fcd9`.

La entrega todavía no está fusionada. `main` exige 20 contextos, base actualizada, una aprobación independiente vigente
y administración sujeta a protecciones. No se activó auto-merge ni se alteraron
protecciones, aprobaciones, FIFO o políticas de despliegue.

## 5. Pruebas ejecutadas

| Verificación | Resultado real |
| --- | --- |
| Web, suite completa tras integrar main | 3.356 pruebas / 298 suites satisfactorias |
| Web, tipos / lint / build final | Satisfactorios; 13 pruebas de la página repetidas después de retirar autoFocus; JS inicial gzip 334.211 bytes / 5 preloads |
| Móvil, Node 20.19.4, instalación limpia del lock | 631 pruebas / 98 suites; tipos, lint y release:check satisfactorios |
| Navegador Chrome escritorio y Pixel 7 | 2 recorridos satisfactorios; API sintética |
| Accesibilidad del compositor | Sin infracciones serious/critical WCAG A/AA en esos recorridos; no auditoría universal |
| PostgreSQL 16 real, esquema completo y migración 214 | Replay, reversión/reaplicación, políticas y concurrencia satisfactorios |
| Modelo de invitaciones | 160 estados acotados; detectó 3 controles negativos |
| API real inicial | 38 peticiones / 7 grupos satisfactorios; binario y fuente anteriores a integración final |
| API real final con MOV | 41 peticiones / 8 grupos satisfactorios; PNG, WAV, MP4 y MOV; reinicio real y limpieza comprobados |
| Stack canónico GHC 9.10.3 / lts-24.42 | Compilación integrada satisfactoria; Hspec: 4.037 ejemplos, cero fallos y 20 pendientes, no contados como aprobados |
| Inventario y trazabilidad, pruebas de generadores | 6 + 10 pruebas satisfactorias |
| Conformidad de clientes generados | 13 pruebas satisfactorias; navegación compartida comprobada |
| Auditoría de listas, conservación y sucesores | 7 pruebas y auditoría estricta JSON/CSV satisfactorias; ninguna lista sin revisar ni decisión activa obsoleta |
| Rutas públicas / máquinas de estado | Admisión comprobada con 14 controles negativos de rutas y 55 controles negativos de validadores |
| Calidad completa del repositorio | Satisfactoria; snapshot compilado actualizado y comprobado |

El recorrido de navegador conserva el destino desde registro, retiene el texto
ante 503, publica mediante teclado, encuentra el contenido al recargar y comprueba
que no haya desbordamiento horizontal. La persistencia del fixture del navegador
no se presenta como persistencia de base de datos.

Recibo HTTP final: [http-results.json](event-memories-evidence-20261025/http-results.json).
Fuente base `9b6512125` con árbol de trabajo modificado: el recibo registra las
huellas estables de los archivos relevantes y del ejecutable
`0c9ddc2f0402b07a49af61e8843cb6050199eeb07d249c9c5db7e5efe361655a`.
No se atribuye este recibo a un checkout limpio de un commit posterior.

La prueba HTTP real usa dos identidades sintéticas y una DB desechable propia:
archivos recuperados byte a byte, tipos extendidos/feed legado, sesión/permisos,
IDOR, edición optimista, reacciones/comentarios, preferencia/campaña, recibo interno,
supresión del recordatorio, borrado y persistencia tras reiniciar la API. No contacta
proveedores externos. Los recibos conservan fuente, huellas del código/binario y
estado de limpieza del árbol; no son una atestación del compilador.

Hubo fallos de preparación conservados en sus logs: alias SSH incorrecto, transporte
stdin de la consulta de solo lectura, módulos locales faltantes, primera compilación
interrumpida, tipos Haskell ambiguos, configuración inválida de fixtures y conexión
PostgreSQL, ausencia local de jq, ejecución inicial de Hspec desde un directorio incorrecto, autoFocus rechazado por accesibilidad y snapshots/sucesores pendientes tras integrar
main. No se contaron como pruebas aprobadas. Los tests SQLite no eluden el testigo
de sesión: su rechazo se comprueba allí y el caso positivo se ejecuta en PostgreSQL/HTTP.

## 6. Evidencia web y móvil

- Capturas del compositor sintético: [escritorio](event-memories-evidence-20261025/desktop-composer.png) y [teléfono](event-memories-evidence-20261025/phone-composer.png). No muestran una publicación productiva.
- Capturas y resultados completos de navegador en
  `/private/tmp/tdf-event-memories-browser-artifacts` y
  `/private/tmp/tdf-event-memories-browser-results.json`; logs locales con prefijo
  `/private/tmp/tdf-event-memories-`.
- Metro arrancó localmente con API offline. Esto no demuestra funcionamiento en
  dispositivo, permisos de galería/micrófono, recuperación en segundo plano ni iOS.
- [Mobile Validate 40183425216](https://github.com/diegueins680/TDF-mobile/actions/runs/40183425216): satisfactorio en el pin móvil indicado.
- [Android test build 40183423677](https://github.com/diegueins680/TDF-mobile/actions/runs/40183423677): satisfactorio en el mismo pin móvil; API local de prueba, OTA deshabilitado, sin publicación en tiendas.

## 7. Despliegue

**No desplegado.** Producción conserva la versión indicada en el diagnóstico y no
contiene esta migración. Se necesita revisión independiente, CI obligatorio e
integración protegida antes del procedimiento de despliegue. Después corresponde
ensayo de migración/canario, verificación de la versión y flujos sintéticos con
almacenamiento real, antes de habilitar campañas o distribuir el cliente móvil.
No se afirma una compilación de tienda, publicación nativa ni prueba productiva
de la funcionalidad nueva.

## 8. Invitaciones reales del evento 141

| Concepto | Estado |
| --- | --- |
| Cuentas relacionadas / activas | 7 / 5, observadas por auditoría |
| Exclusión por cuenta inactiva en esta cohorte | 2, cálculo de auditoría; no contador de campaña desplegada |
| Cuentas activas retenidas | 5; evento sin confirmar como finalizado y sin preferencia nueva explícita |
| Destinatarios habilitados para enviar por esta tarea | 0 |
| Enviadas / entregadas / fallidas | 0 / 0 / 0; cero intentos reales |
| Recordatorios reales | 0 |

Los contadores productivos de campaña no existen todavía: no se deducen entregas
ni exclusiones de una simulación. Un organizador debe confirmar la finalización
real; no se modificó automáticamente el ciclo de vida. Solo se podrán habilitar
envíos después del despliegue, la comprobación de elegibilidad y el consentimiento.
No se inventaron asistentes ni contactos y no se envió correo, push o WhatsApp.

## 9. Métricas disponibles y límites

Se implementaron agregados por evento de candidatos, consentimiento, invitaciones
internas comprometidas, recordatorios, aperturas atribuidas, publicación posterior
a la invitación, momentos y autores únicos. Los ve únicamente el organizador actual.
No se exponen listas de contactos ni contenido en analíticas. «Entregada» significa
inserción comprometida en la bandeja; no lectura humana ni entrega por correo.

Registro atribuido, tiempos/pasos de primera publicación, conexiones causadas,
visitas recurrentes, cohortes D7/D30 y participación cruzada aún no están medidos de
forma suficiente. No hay evidencia para atribuir mejoras de conversión o retención.

## 10. Riesgos y siguientes prioridades

1. **P0:** revisión del límite de archivos públicos y moderación de publicaciones
   fuente, además de la existente para conversaciones. No se ofrece una falsa
   opción de contenido «solo participantes». Falta entrega autenticada/revocable de
   archivos y borrado del asset público; borrar la publicación no elimina copias.
2. **P0:** cierre del CI y validación nativa con identidades/contenido sintéticos;
   confirmación del estado real del piloto por su organizador. La firma reconocida
   no sustituye validación completa de contenedores, duración, transcodificación,
   eliminación de metadatos ni revisión humana.
3. **P1:** publicación con clave duradera para recuperar respuestas perdidas sin
   duplicar; galerías y medios combinados; audio/texto nativos. Las publicaciones
   múltiples móviles siguen siendo momentos individuales.
4. **P1:** consentimiento real y prueba humana controlada después de revisión y
   despliegue; correo/push solamente con autorización propia del canal. WhatsApp
   queda como integración futura, sin dependencia nueva.
5. **P2/P3:** aislamiento de fallos por destinatario, paginación/ensayos de carga,
   descubrimiento más amplio y previsualizaciones públicas por publicación, siempre
   sometidas a visibilidad; medición atribuida y cohortes D7/D30.

Los requisitos nuevos conservan clasificación formal **PARTIAL**. Esta entrega no
satisface todavía toda la lista de aceptación del encargo, ni se presenta como
integrada, desplegada o verificada en producción.

## Continuación y conciliación de main — 25/oct, después de 19:40 UTC

Diego confirmó que el evento 141 se celebró y ya finalizó. Esto resuelve la duda
sobre lo ocurrido; no acredita presencia de los RSVP ni consentimiento de campaña.
La lectura productiva de las 19:03 UTC conservaba on_sale, sin migración de
recuerdos ni acceso público habilitado para completed. La corrección de estado
espera ese soporte desplegado para no ocultar la página. Envíos reales: cero.

Se fusionó normalmente main protegido cfe75b187 después de la integración de #607.
Los 138 requisitos de ese main se conservaron íntegros junto con nuestros dos.
Los conflictos se limitaron a los índices y requisitos generados; la traceabilidad
se regeneró con su herramienta específica. El backend, SQL, contratos y gitlink
móvil de esta entrega permanecen byte a byte iguales al head previamente aprobado
dc9fb129. Se repiten pruebas de composición web y controles de repositorio; CI y
revisión independiente deben vincularse al nuevo head, sin reutilizar la aprobación
anterior. La moderación fuente avanza en una rama sucesora separada, con su propia
evidencia PostgreSQL/HTTP/navegador; no se presenta como parte de este PR ni desplegada.

## Continuación del 25/oct, 21:20 UTC — CI y nuevo main

La aprobación independiente de 7b16d44bb llegó a las 21:20:22 UTC y se conserva
como evidencia de ese head. CI encontró un fallo real del runner de navegador:
516 casos pasaron, 22 quedaron omitidos por configuración y el único fallo fue
la navegación de registro en WebKit desktop. La prueba abría una segunda URL
antes de que terminara la redirección al login; ocurrió también en el sucesor
#615. No se cuenta ninguno de esos runs como satisfactorio.

El recorrido ahora espera la pantalla utilizable y pulsa «Crear cuenta general»,
como una persona usuaria. Conserva las aserciones de destino original, registro,
fallo/reintento, publicación, recarga, ancho de pantalla y accesibilidad, y añade
la comprobación del contexto tras abrir el registro. No hay skips o retries nuevos.
Firefox local y 34 pruebas de página/API/preferencias pasan; la matriz Chrome local
y el nuevo WebKit Linux quedan registrados por sus ejecuciones independientes.

Se fusionó normalmente main 000cf8ad3 (PR financiero #609) en ef512e3f2, conservando
íntegros sus 139 requisitos y añadiendo los dos de recuerdos. El helper financiero
de expiración procede de main; las migraciones, métodos API y pin Mobile de esta
entrega no cambian. Los índices se regeneran después del cambio del test y sus
comprobaciones deben pasar. La aprobación del head anterior no autoriza este nuevo
head. La cola de integración informaba espera por #610: no se altera ese orden.

El defecto de cierre de sesión fallido reproducido por el flujo de activación
también impide una aceptación con personas reales hasta su corrección y
verificación; esa reparación está en trabajo por su propio flujo. Esta entrega
no modifica SessionContext ni presenta la sesión sintética como aceptación humana.
No hubo despliegue, cambio de estado de 141 ni comunicaciones productivas.

La fixture ahora representa también el contrato real de GET /session: 200 con
null para una visita anónima. Se conserva 401 como caso separado de sesión
caducada; ambos recorren registro, recuperación del error y publicación persistida.
Los 28 recorridos Chrome y uno Firefox anteriores corresponden a la variante
401; las dos variantes se ejecutan de nuevo y WebKit queda a cargo de CI Linux.

La nueva matriz pasó sin reintentos: ocho casos en cuatro tamaños Chrome y
dos casos Firefox, para ambas respuestas de sesión. El [recibo compacto](event-memories-evidence-20261025/browser-continuation.json)
conserva el hash exacto del test modificado y los resultados sintéticos. No se
atribuye esa ejecución al commit limpio posterior ni se extrapola a WebKit.


## Integración de menús y control de sesión — 25/oct, 22:15 UTC

El PR #610 se integró como main b732efd65. El merge normal local 107680b7e conserva
íntegros sus 140 requisitos y añade los dos de recuerdos. Se combinaron las
clasificaciones de catálogo sin resolver diferencias por descarte: 1.331 decisiones
activas y auditoría estricta JSON/CSV satisfactoria. La composición pasó 58 pruebas
en cinco suites, tipos y 32 recorridos compilados Chrome en cuatro tamaños.

El CI de baa1f8cc5 terminó la matriz de personas satisfactoriamente, incluyendo
WebKit/Linux con ambas sesiones. La UI de ese head pasó 3.377 pruebas/301 suites;
la UI del sucesor 7a836b214 pasó 3.409/303. Son evidencias de esos heads, no del
merge posterior ni de cambios sin publicar.

Una comprobación adicional del sucesor reprodujo tres envíos indebidos de la
interfaz candidata cuando las credenciales cambiaban antes de un nuevo render:
un borrador, una preferencia de invitaciones y una decisión de moderación.
No se observaron datos reales expuestos ni se desplegó esta funcionalidad.
La corrección vive en #615, reutilizando la propiedad de sesión del editor de
perfiles y capturando la propiedad de cada intento; esta entrega debe esperar
esa corrección, su CI y revisión. La reparación independiente del logout fallido
sigue siendo otro requisito para una prueba con personas reales.

Estos cambios no autorizan una campaña. Continúan pendientes revisión del head
actual, CI obligatorio, FIFO y despliegue verificado; los envíos reales siguen en cero.


## Reconciliación con main protegido — 26/oct, 02:42 UTC

Se incorpora mediante merge normal main917806ff9c8d455a01bfb8a29c80d6ff353e0e0b,
que integra #551 y #611. Sus140 requisitos se conservan como objetos exactos,
incluidos los cinco financieros actualizados. El registro tiene221 entradas:
las220 de main son el prefijo exacto, con sus SQL/SHAs intactos, seguido de la
migración original de invitaciones18690e78. «214» en los informes anteriores es
su posición histórica; su identificador y archivo son la autoridad estable.

Se conservan las revisiones de los registros214 y220 mediante la cadena previa
y referencias al archivo/objeto Git original con sus hashes comprobados. La
auditoría estricta JSON/CSV tiene1332 candidatos sin decisiones pendientes ni
obsoletas; las siete regresiones de historial y los índices pasan. Hay142
obligaciones con las dos de recuerdos. Tipos y47 pruebas web en cuatro suites
pasan. El primer intento incluyó por error un archivo de test que solo existe
en el sucesor615; se conserva como fallo de preparación y se repiten los cuatro
archivos reales, sin crear una prueba ficticia.

La composición del backend financiero nuevo debe compilarse y verificarse con
el sucesor615 antes de cualquier despliegue. Los binarios, HTTP y aprobaciones
de la base anterior no se atribuyen a esta composición. Se mantiene borrador y
auto-merge desactivado hasta incluir y revisar las correcciones P0 de615. No hay
escrituras de producción ni invitaciones reales.
