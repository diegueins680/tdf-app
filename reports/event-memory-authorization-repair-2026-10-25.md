# Recuerdos: reparación de autorización y publicación móvil

**Implementado y probado localmente; nuevo CI y revisión independiente pendientes.
Sin integración a main, despliegue productivo ni invitaciones reales.** Continúa el [diagnóstico y
los diez entregables originales](event-memories-2026-10-25.md) y el
[informe de moderación y sesión web](event-memory-source-moderation-2026-10-25.md).

La revisión automática de #615 detectó una autorización permisiva cuando el
organizador era NULL. La regresión sobre PostgreSQL real reprodujo una respuesta
`state=hidden` para un lector común. Todo ocurrió en una base sintética y una
transacción que se revierte; no acredita un incidente productivo.

Se añadió la migración 216, introducida por `c6ecae133a93b6bab93c84cee4d72290f9e28982`.
Conserva íntegros los 215 registros y SQL anteriores. Normaliza los permisos a
booleanos y exige autorización explícitamente verdadera antes de revisión,
decisión y replay. Conserva las mismas funciones, OID, publicaciones, reportes,
auditoría y ledger. Verifica cuerpos y atributos exactos del predecesor o de la
reparación y rechaza sucesores desconocidos. Reaplicar o revertir 215 se niega
después de la reparación; la recuperación debe conservarla o usar otro arreglo
revisado hacia adelante. No se elimina información para facilitar una reversión.

Una segunda revisión detectó que la decisión no retenía el bloqueo de la
asignación del moderador. La nueva migración **217**, introducida por
`baab857f085b74f333c9f5d29e31306072fbc496`, reutiliza
`interaction_lock_permissions` antes de calcular capacidades. Sus locks FOR SHARE
se mantienen hasta el commit; no crea otro sistema de permisos. Conserva el OID,
los 216 registros y todos los bytes SQL anteriores. La reaplicación admite solo
las definiciones exactas aprobadas y rechaza el sucesor desconocido o el downgrade
a través de 216. No hay borrados ni transferencia de consentimiento.

El [control negativo y los ocho casos](event-memory-source-evidence-20261025/permission-lock-verification.json)
reproducen dos transacciones confirmadas con la asignación ya revocada y el
recuerdo retirado. La reparación observa pg_blocking_pids en ambos órdenes para
retirar, restaurar, ocultar y resolver reportes. La revocación primera deniega
sin cambiar fuente, versión, ledger ni auditoría; una decisión primera mantiene
la revocación esperando y un replay posterior queda denegado. La protección del
último rol administrador de emergencia rechazó un fixture adicional de
desactivación global y se mantuvo intacta; ese intento no cuenta como caso
positivo ni como vulnerabilidad. No se afirma linealizabilidad de todo el
catálogo de seguridad ni E2E del proceso administrativo de aprobación de roles.

Las pruebas verifican rechazo sin efectos para ocultar, restaurar, retirar y
resolver reportes, capacidades falsas, ausencia de filas/motivos de revisión,
reportes permitidos al lector, moderación legítima y revocación del replay de un
antiguo organizador. También se comprueban reversión histórica antes de 216,
reaplicación canónica, sucesor desconocido y conservación de autoridades.

En móvil se corrigieron dos defectos adicionales. Una respuesta sin ID ya no se
transforma en una publicación remota con identificador inventado. Las respuestas
HTML, malformadas o de otro evento/fuente se rechazan; se mantienen compatibles
las respuestas heredadas válidas sin campos opcionales. La selección y envío de
archivos conservan la sesión que inició la acción. Los resultados tardíos no
restauran el borrador anterior sobre uno nuevo; A→B→A no revive el intento. Se
reutilizan el propietario de Party, la versión de sesión y AuthQueryBoundary.
El cambio de evento retira su editor mediante una clave del contenido de ruta.

Cambios revisables: raíz [#615](https://github.com/diegueins680/tdf-app/pull/615),
base [#608](https://github.com/diegueins680/tdf-app/pull/608); móvil
[#170](https://github.com/diegueins680/TDF-mobile/pull/170), commit
`7000df1da934aa467e443ad267c0a8caa1e9cb72`, apilado sobre #169 y #168.
Los contratos existentes y sus mapeos se actualizan; no hay API ni dependencia nueva.

Resultados independientes, sin sumar ejecuciones que se solapan:

- PostgreSQL: esquema completo, permisos, NULL, replay, controles de sucesión,
  reversión histórica y reaplicación de la reparación pasan.
- [API/DB con 217](event-memory-source-evidence-20261025/http-permission-lock-results.json):
  **120 solicitudes, 17 grupos**, cero llamadas a proveedores. Publicación y
  recuperación de texto/PNG/WAV/MP4/MOV, permisos, concurrencia, reinicio y rechazo
  de reversión con evidencia. La reparación conserva exactamente las seis
  autoridades registradas antes del intento de reversión. Los ocho casos reales
  de revocación y las regresiones de invitaciones también pasan por separado.
- [Navegador con 217](event-memory-source-evidence-20261025/browser-permission-lock-results.json):
  escritorio y Pixel 7 sobre API y DB sintéticas; publicación, denuncia, cierre,
  ocultación, recarga y restauración. No es un teléfono físico ni producción.
- Móvil: **653 pruebas/98 suites**, release:check, tipos y lint pasan en Node
  20.19.4. Expo arranca sin red, responde /status y compila el bundle Android.
  Mobile Validate 40248248167 pasó en el commit exacto 7000df1.
  Antes de corregirlo, 17 aserciones detectaban confirmaciones inválidas y una
  regresión del botón real detectaba un envío tras cambiar credenciales.
- Composición web aislada de #615 `e9db49c342` y reparación de logout #616
  `9e96e07f45`: **3.475 pruebas/308 suites**, tipos y lint pasan. Precede a la
  migración 216 y al pin móvil nuevo; no es CI ni una integración protegida.
- CI histórico de #615 `e9db49c342` pasó: 3.440 pruebas web/305 suites y 524
  recorridos, 22 skips existentes; backend, contratos y migraciones pasaron.
  No autoriza este sucesor: la revisión encontró después el defecto de NULL.
  El CI completo de `6c5f6ebe4` también pasó, pero su revisión encontró la carrera
  de revocación; tampoco sustituye el CI ni la aprobación del sucesor con 217.

Los [recibos y hashes](event-memory-source-evidence-20261025/authorization-repair-verification.json)
conservan revisiones y árboles modificados reales. Los 24 hashes de ejecución de
API/navegador de 217 coinciden con la fuente actual; el ejecutable conserva su identidad
anterior, sin fingir una compilación limpia del nuevo commit. El primer recorrido
HTTP de esta reparación falló en una aserción antigua que esperaba el digest de
215: se sustituyó por igualdad exacta de las seis autoridades instaladas antes y
después de la reversión rechazada. El segundo recorrido pasó. La primera prueba
móvil de comentario válido esperaba un campo que no existe en el modelo mapeado;
ahora comprueba ID/cuerpo persistidos y el endpoint del padre solicitado.

El evento 141 fue celebrado y terminó, según confirmó Diego. Última consulta DB
registrada, 25/oct 21:36 UTC: on_sale, siete cuentas relacionadas/cinco activas,
sin esquema de campaña ni preferencias recogidas. Última comprobación pública,
23:26 UTC: página y API HTTP 200, DB ok y backend anterior `4dac175d`. Corregir el
estado solo después de desplegar y verificar la lectura pública de completed.
RSVP no acredita presencia. Intentadas/enviadas/entregadas/fallidas: **0/0/0/0**;
elegibles consentidos y exclusiones definitivas no están acreditados.

Siguen pendientes revisión independiente y CI del nuevo head, integración FIFO,
reparación de logout integrada, recuperación/canary, despliegue y aceptación real.
También medios privados/revocables, galerías mixtas, texto/audio nativo,
idempotencia general, previsualizaciones específicas y medición D7/D30. No se
atribuye adquisición, conversión ni retención a estas pruebas sintéticas.


## Composición con el main financiero y canary regional — 26/oct03:02UTC

Se integra mediante merge normal la base534c5631ac79e927c887c5c730e252fe4dad7cc5,
que contiene el main protegido917806ff9c8d455a01bfb8a29c80d6ff353e0e0b (#551 y
#611). Se conservan íntegros sus140 requisitos y todos los objetos/SQL de las221
migraciones de base. Las tres sucesoras originales de moderación se añaden sin
cambiar archivos ni introducedBy:224 entradas y144 requisitos en total. Los
números215/216/217 anteriores son posiciones históricas de esta rama; los
identificadores estables son event_memory_source_moderation,
event_memory_source_authorization y event_memory_source_permission_locks. En la
composición ocupan222/223/224. Ambas historias de revisión217/221 se conservan
con referencias a sus objetos Git y hashes, incluida la financiera220.

La composición pasa77 pruebas web/7suites y tipos, siete regresiones de historial,
auditorías estrictas JSON/CSV con1334 candidatos, índices y quality:repo. El
PostgreSQL completo224 pasa permisos/NULL/revocación en ocho intercalaciones,
rollback histórico, sucesores y reintentos. Invitaciones, deduplicación, despacho
concurrente y supresión también pasan. Estas ejecuciones preceden al commit de
merge y conservan ddb923798 con árbol modificado; no se les cambia la identidad.

La compilación Stack canónica del backend financiero combinado pasó con
GHC 9.10.3/lts-24.42. El ejecutable nuevo declara el commit
`810d646208215bcc7c99d870b40f90e17f5beb00`, buildTime
`2026-10-26T03:23:26Z`, SHA-256
`4d71101f0c02a2735fafacffe5560fd8a6972a04c2a450b1495588a3e054f487`.
Es un binario Darwin local, no una imagen Linux desplegada.

El [recibo HTTP nuevo](event-memory-source-evidence-20261025/http-financial-main-917-results.json)
pasa **120 solicitudes/17 grupos**, y sus 24 hashes coinciden con las fuentes de
esta composición. El [navegador nuevo](event-memory-source-evidence-20261025/browser-financial-main-917-results.json)
pasa publicación, denuncia, cierre, ocultación con teclado, recarga y restauración
en escritorio y Pixel 7 emulado, sobre la misma API/DB real sintética. Se
conserva `sourceWorktreeDirty=true`: había dos enlaces de dependencias propios,
con los archivos versionados limpios. Se detuvieron API/Vite y se eliminó solo
la base desechable del runner. Cero llamadas a proveedores externos.

También pasan los diez casos Hspec de integridad contable contra PostgreSQL y
los cuatro grupos musicales no vacíos: dominio 25, contenido 10, DDEX 22 y
almacenamiento 4. Los controles SQL financieros pasan admisión histórica,
reversión concurrente, 33 escenarios finitos con controles negativos, namespace
con dos contraejemplos esperados y arranque con diez mutaciones rechazadas y dos
configuraciones compatibles; se restauran las autoridades exactamente. No se
convierten estos escenarios finitos en una prueba de refinamiento completo.

La inspección compilada encuentra 913 métodos y dos montajes raw; conserva
396 operaciones sin documentación, 25 documentadas sin ruta tipada y ocho
diferencias de estado, sin autoridades competidoras. El snapshot declarado
coincide; no se afirma conformidad global de la API.

El [checkpoint de verificación](event-memory-source-evidence-20261025/financial-main-917-verification.json)
distingue cada identidad y alcance. `quality:ui` completo pasó en Node 24.13.1:
lint, tipos, **3.440 pruebas/305 suites**, compilación de producción y presupuesto
inicial de JavaScript: 334.257 bytes gzip, cinco preloads. No es una medición de
latencia de usuarios reales. `quality:repo` también pasó. Su primer intento
falló porque `jq` no estaba en el PATH del subprocess y el fixture del importador
no pudo ejecutarse; se conserva ese fallo. La repetición completa con el binario
real jq 1.8.1 disponible pasó, sin cambiar pruebas ni aserciones.
La suite general Stack terminó con código 0: **4.041 ejemplos, cero fallos y 21
pendientes** de runners de integración específicos. Esos 21 no se presentan
como aprobados. El grupo contable se ejecutó aparte contra la base sintética
propia: diez ejemplos, cero fallos. Las demás integraciones conservan sus
prerrequisitos y gates dedicados. El runner de reintentos del proveedor pasó
**308 ejemplos, cero fallos**, junto con el rechazo de rollback del historial
usado. La [admisión por caducidad](event-memory-source-evidence-20261025/financial-main-917-expiry-results.json)
pasó en la implementación real y detectó tres mutantes independientes de reloj;
las fuentes permanecieron intactas. Es un fixture PostgreSQL sintético, sin
HTTP a proveedores ni datos productivos.
El CI y las aprobaciones históricos no se trasladan al nuevo head de #615.
#608 recibió una aprobación nueva para su commit 534c5631, pero permanece
retenido hasta incluir la reparación P0 revisada. No hay integración,
despliegue ni comunicación productiva de estos cambios.


## Estado del piloto y límites de entrega — 26/oct, actualización

Diego confirmó que PATCH CULTURE Vol. 1 se celebró y terminó. La consulta
productiva de solo lectura de **26/oct 03:22:22 UTC** conserva `on_sale`, siete
cuentas relacionadas y cinco activas. Hay cinco señales profesionales y tres
RSVP aceptados, con una cuenta en ambos grupos; no son ocho personas ni prueban
asistencia presencial. No hay alias archivados en esta cohorte. El esquema y las
preferencias de campaña de esta entrega todavía no están instalados. No se ha
calculado un total definitivo de elegibles consentidos ni de excluidos.

Intentadas/enviadas/entregadas/fallidas reales: **0/0/0/0**. Ningún resultado
sintético se presenta como una invitación real. Cambiar `on_sale` requiere
primero desplegar y comprobar que el evento finalizado sigue siendo legible.
La confirmación del fin no sustituye consentimiento ni habilita proveedores.

| Capacidad | Implementación y prueba disponible | Producción |
| --- | --- | --- |
| Texto, audio, foto, video, edición/borrado, reacciones y comentarios web | API/DB y navegador sintéticos | Pendiente |
| Moderación de fuente, permisos actuales, NULL y revocación concurrente | SQL, HTTP y controles negativos | Pendiente |
| Navegación y registro con contexto del evento | Web local; reparación independiente de logout #616 aún necesaria | Pendiente |
| Foto/video móvil, confirmación remota y retiro de sesión | 653 pruebas, CI exacto y bundle Android | Sin aceptación física ni publicación móvil |
| Invitación interna y un recordatorio, preferencias, deduplicación, métricas agregadas | SQL/HTTP sintéticos; deshabilitados por defecto | Sin esquema ni envíos |
| Galerías mixtas, medios privados/revocables y eliminación física | Pendiente | No disponible por esta entrega |
| Audio/texto móvil, previsualización específica de publicación y D7/D30 | Pendiente | Sin evidencia de mejora |

La arquitectura reutiliza las autoridades existentes y conserva información
histórica. La ocultación de la publicación controla lecturas e interacciones de
TDF; **no revoca una URL de archivo público ya conocida**. No se admite carga de
medios públicos para un evento privado. La visibilidad restringida y la
revocación de medios requieren trabajo y validación propios antes de anunciarse.
La recuperación todavía requiere ensayo con la imagen Linux final y el
estado real de flags. Conservar tablas y funciones no acredita por sí solo un
downgrade seguro del backend o del lector web. No se ha autorizado volver a un
lector anterior como recuperación de esta función: debe conservarse la política
de moderación, incluso con interacciones pausadas, o aplicarse una corrección
revisada hacia adelante. El rechazo de rollback SQL probado no se presenta como
prueba de rollback de la aplicación.

Correo, push y WhatsApp no se activaron. Los agregados observados no permiten
atribuir adquisición, conversión ni retención.


La inspección móvil adicional mantiene el alcance de #170: tipos, codec local,
feed remoto, carga y tarjeta admiten imagen/video. Texto/audio requieren una
ampliación coordinada de esas capas y sus controles de sesión; cambiar solo el
selector dejaría publicaciones que el propio cliente no recupera o representa
correctamente. No se modificó el commit móvil al evaluar esta brecha. Para
seleccionar audio se evaluó el [selector de documentos de Expo](https://docs.expo.dev/versions/latest/sdk/document-picker/),
pero su documentación actual y la ruta histórica v54 redirigen a la última
versión. La app usa SDK 54: antes de instalar debe verificarse la versión
compatible del SDK y la selección/carga en dispositivos. Esta evaluación no se
presenta como soporte nativo implementado ni como impedimento de autorización.
También sigue pendiente etiquetado estructurado en la publicación; las menciones
reutilizadas actualmente pertenecen a los comentarios canónicos.


A las 03:48:01 UTC se integró externamente #614 y `main` avanzó a
`e938c72ba4b87010214f6838f06bfac45a0f45c0`. Esta composición de #615 continúa
sobre su base #608 `534c5631`, que contiene `917806f`. Los recibos conservan esa
base y no califican el directorio nuevo de #614. La entrega raíz final requiere
reconciliarse con el main entonces vigente, preservar su manifiesto e historial,
y repetir las comprobaciones afectadas y la revisión del candidato exacto.
No se modifica la rama de #614 ni se interpreta la aprobación de #608 como
aprobación de una combinación futura.


La verificación final de inventario y trazabilidad pasó. La primera llamada de
trazabilidad se detuvo por ausencia de PyYAML en el Python por defecto; se usó
el entorno existente con PyYAML 6.0.3, sin modificar dependencias ni aserciones.
El recibo conserva ambos resultados y los hashes de los logs locales completos.
Las ejecuciones generales se realizaron antes del commit final de documentación:
las fuentes de ejecución mantienen sus hashes y el binario conserva su identidad
810d646 real. No se les atribuye otra revisión compilada.


## Preparación aislada con directorio y logout — 26/oct 05:18 UTC

Se prepara un merge normal de la entrega de recuerdos `31c16bffe` con el main
protegido `1c9c833cc1ba62b47822fca3c20e14e86cf28daa`, que ya contiene #614 y
la reparación de logout #616. Las ramas publicadas de #615/#608 conservan sus
heads durante la preparación. Esta sección no convierte los recibos anteriores
sobre 917 en pruebas de la nueva composición.

El código de aplicación se integró sin conflictos. Se resolvieron nueve
conflictos de manifiestos e índices manteniendo íntegros los 142 requisitos del
main, sus 221 entradas ordenadas y todos sus SQL. Se añaden los cuatro registros
originales de recuerdos sin modificar SQL ni SHAs de introducción: 225 migraciones
y 146 requisitos. El registro técnico `cff15f30aa13965e5729` conserva la
clasificación/disposición de seguridad de main, el grafo 224 de recuerdos y el
registro 221 de directorio, con sus objetos originales, hashes y referencias.

Pasan ocho regresiones de linaje reforzadas: se mantienen las comprobaciones
213/214/220/221; se agregan el prefijo 221 exacto, las cuatro entradas originales
y los hashes completos de ambos reviews. Las aprobaciones antiguas 221/224 se
rechazan para el registro 225. No se cambia el hash histórico de archivado, que
usa el orden de JSON.stringify; mi primer helper aplicó erróneamente el formato
canónico ordenado y se detuvo. El ajuste preserva el hash original. El primer
checkout móvil con --reference no aceptó el worktree enlazado; el checkout SSH
normal verificó después el commit 7000df1. La primera exploración del catálogo
se canceló por checkout incompleto y otra se sustituyó al cambiar la prueba:
no cuentan como ejecuciones aprobadas.

Las auditorías estrictas JSON/CSV frescas pasan con 1.336 candidatos. La web
combinada pasa lint, tipos, **3.475 pruebas/308 suites**, build de producción y
presupuesto inicial de JavaScript: 335.236 bytes gzip, cinco preloads. Estas
pruebas preceden al commit de preparación y conservan 31c16bffe con árbol
modificado; no se les atribuye una revisión compilada nueva.

La comprobación global `quality:repo` también pasó, usando el jq y el entorno
PyYAML existentes desde el inicio de esta ejecución.

El backend cambió con Directory.hs de #614. El ejecutable 810d646 anterior no
califica ese cambio: faltan compilación canónica y pruebas de runtime de la
composición, además de los gates y revisión independientes de la entrega raíz.
El CI principal de #615 pasó en 31c16bffe; el workflow RACI seguía compilando al
último chequeo. No se ha fusionado #615 ni promovido esta preparación a #608,
y no hay despliegue ni envíos productivos de recuerdos.


## Verificación de la preparación 262295679 — 26/oct 05:52 UTC

La compilación canónica y Hspec pasan en el checkout limpio
`262295679df492dde850333779db431132092993`: **4.041 ejemplos, cero fallos,
21 pendientes** de integración externos, que no se cuentan como aprobados.
Los cuatro grupos musicales no vacíos también pasan. El binario nuevo conserva
la identidad real 262295679, buildTime `2026-10-26T05:36:20Z`, SHA-256
`f01ef34b75f1f1c1da4a41cdbb541de9b620fd4c1f4b24aba64025e7c1b6e17d`.

La [API/DB con navegador](event-memory-source-evidence-20261025/http-protected-main-1c9-results.json)
pasa **120 solicitudes/17 grupos** sobre 225 migraciones, escritorio y Pixel 7
emulado, con 24 hashes coincidentes. Ese runner conserva dirty=true por sus dos
enlaces de dependencias; el checkout que compiló el binario estaba limpio.
Una [ejecución HTTP adicional limpia](event-memory-source-evidence-20261025/http-clean-protected-main-1c9-results.json)
pasa 120 solicitudes/16 grupos con sourceWorktreeDirty=false e identidad
compilada 262295679. Los servidores de ambos runners y sus bases se retiraron.
También pasan sobre HEAD limpio los ocho casos SQL de revocación, NULL,
sucesores/reversión e invitaciones con el esquema completo 225.

El runner existente del directorio detectó un límite del fixture inicial: su
base con locale C no pliega la mayúscula acentuada en LÚNA y falló una aserción
real de búsqueda antes de llegar al HTTP. Ese fallo se conserva. La repetición
sin modificar pruebas ni SQL, en una base nueva en_US.UTF-8, pasa consultas,
facetas, 4.000 documentos sintéticos, replay, rollback/reaplicación con filas y
ledger intactos, y **107 solicitudes HTTP** con el binario recién compilado.
Los tiempos observados son descriptivos; no se atribuye mejora de rendimiento.
La consulta productiva de solo lectura del 26/oct 05:50:35 UTC confirmó locale
en_US.utf8, encoding UTF8 y el plegado correcto de LÚNA a lúna. Esto verifica
ese dato ambiental; no demuestra despliegue de recuerdos ni modifica producción.

El [recibo completo de preparación](event-memory-source-evidence-20261025/protected-main-1c9-verification.json)
conserva revisiones, hashes, intentos fallidos y límites. La API compilada mantiene
913 métodos/dos raw mounts y su snapshot coincide; las brechas globales siguen
abiertas. El CI principal y RACI de #615 pasan en 31c16bffe, conservando esa
identidad. La preparación 262 no tiene todavía CI protegido ni revisión de la
entrega raíz. #615 sigue pendiente de revisión independiente; no se fusionó ni
promovió esta preparación a #608. No hay despliegue ni invitaciones productivas.


Capturas sintéticas de esta ejecución: [escritorio](event-memory-source-evidence-20261025/desktop-protected-main-1c9.png)
y [Pixel 7 emulado](event-memory-source-evidence-20261025/phone-protected-main-1c9.png).
El main avanzó después a `3af768dacd95c1223975c62da3ce8088bed89b47` al integrar
#618, que conserva el diario durable de despliegue. El runtime aquí probado
sigue siendo 262295679; sus recibos no acreditan los controles operativos nuevos.
La preparación debe incorporar esa sucesión y verificar sus checks afectados
antes de cualquier entrega protegida.


## Sucesión operativa incorporada — 26/oct 06:05 UTC

La preparación incorpora mediante merge normal el main protegido `3af768d`
(#618), preservando sus 142 requisitos exactamente y sus controles de diario
sin reiniciar historia. Solo chocaron inventario y trazabilidad, regenerados
desde las autoridades combinadas. `quality:repo` pasa con esos controles.
Se comprobó con Git que todos los inputs de backend, frontend, móvil y
package/lock permanecen idénticos al runtime 262295679 probado. No se cambia
la identidad de su ejecutable ni se atribuye una compilación de otro commit.
El [recibo de sucesión](event-memory-source-evidence-20261025/release-journal-main-preparation.json)
registra ese límite y el hash del log completo.

El main observado después avanzó a ed9eb9a16 mediante la recuperación del
club de fans #613. La propuesta de bienvenida #619 seguía abierta. Esta preparación conserva su base congelada 3af768d; antes de promover
#608 deben reconciliarse el main entonces vigente y sus controles, ejecutar los
checks afectados, cumplir FIFO y obtener revisión/CI protegidos del root exacto.
No se actualizó el head publicado de #615 ni el de #608 para forzar una fusión.
El primero conserva CI principal/RACI satisfactorios y revisión independiente
pendiente; el segundo permanece en borrador. Los intentos/envíos/entregas/fallos
reales del evento 141 continúan en 0/0/0/0. No hay despliegue productivo de recuerdos.


## Comprobación pública y bloqueo de entrega — 26/oct 06:10 UTC

La [consulta pública final](event-memory-source-evidence-20261025/public-readonly-final-20261026.json)
respondió 200 para la URL canónica del evento 141, salud (DB ok) y versión.
El backend publicado sigue en `4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`,
anterior a la funcionalidad de esta entrega. El shell 200 no se cuenta como
prueba del compositor nuevo. Python no pudo validar su cadena local de CA;
se conserva ese intento fallido y se utilizó curl del sistema con validación
TLS normal, sin desactivar certificados. No se escribió en producción.

Los tres worktrees propios de base, fuente y preparación quedaron limpios tras
retirar solo sus enlaces temporales; se conservaron los directorios destino y
los logs. API/Vite/Metro y el PostgreSQL sintético están detenidos; las bases
sintéticas se retiraron. El checkout original divergente y las ramas de otros
operadores se conservaron.

El bloqueo inmediato es revisión independiente de #615 (31c16bffe, CI verde)
y de la cadena móvil #168/#169/#170. La entrega raíz #608 sigue en borrador:
debe incorporar el sucesor revisado, el main entonces vigente (incluida la
recuperación del club de fans posterior a esta preparación), CI y aprobación nuevos del root,
imagen/recuperación/canary autorizados y comprobación productiva. Después puede
corregirse el estado del evento confirmado y comprobar preferencias/eligibilidad
reales antes de cualquier invitación. Consentidos elegibles y exclusiones siguen
sin un total acreditado; intentadas/enviadas/entregadas/fallidas: **0/0/0/0**.
También permanecen las limitaciones funcionales enumeradas: medios privados y
revocables, galerías mixtas, audio/texto nativo, etiquetado estructurado de la
publicación, idempotencia general, previews específicos y medición D7/D30.
Esto es avance implementado y probado, con preparación publicada; no una entrega
integrada, desplegada o verificada funcionalmente en producción.


## Actualización de integración — 26/oct

Se corrige la atribución anterior de ed9eb9a16: el registro de GitHub confirma
que corresponde a #613, recuperación del club de fans. #619 sigue abierta;
no se cuenta como entregada. La preparación incorpora ahora ese main mediante
merge normal, preservando todos sus requisitos y añadiendo los cuatro originales
de recuerdos. El backend, SQL y contratos conservan sus bytes; se comprueba de
nuevo la composición web y la recuperación del contexto del artista.


La reconciliación ed9 preserva exactamente los **143 requisitos** protegidos y
los cuatro requisitos originales de recuerdos (**147** combinados). Backend,
SQL, contratos, pin móvil y dependencias siguen byte-idénticos al runtime 262.
Pasan `quality:repo`, catálogo estricto JSON/CSV (**1336** entradas), regeneración
formal, lint/tipos web, **3496 pruebas / 309 suites** y build web
(**335236 bytes gzip**, cinco preloads) con Node **22.14.0**. Las pruebas del
runtime 262 conservan su identidad; no se presenta una compilación nueva.
El cambio móvil de texto/lectura de audio tiene validación independiente en su
rama, apilada sobre #170; esta preparación todavía conserva el pin móvil anterior.
