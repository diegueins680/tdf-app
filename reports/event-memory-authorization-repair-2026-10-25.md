# Recuerdos: reparación de autorización y publicación móvil

**Implementado y probado localmente; nuevo CI y revisión independiente pendientes.
Sin integración, despliegue ni invitaciones reales.** Continúa el [diagnóstico y
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

La compilación Stack canónica del backend financiero combinado está en curso.
Los binarios, HTTP y CI anteriores NO califican esa composición. Antes de
proponer integración quedan Hspec/grupos necesarios, binario nuevo, API/DB y
navegador sobre224, CI y revisión del nuevo head. #608 y #615 permanecen retenidos
y no existe despliegue ni comunicación productiva.
