# Backlog de valor para usuarios — 25 de octubre de 2026

Responsable: motor de finalización de valor TDF. Documento vivo: actualizar estado,
SHA, evidencia, bloqueo y siguiente acción después de cada validación, revisión,
merge o despliegue. Una implementación local, un PR aprobado y un resultado
disponible para usuarios son hitos separados.

## Base y alcance de la observación

- `main` protegido actual: `cfe75b18786c18d18fb2fc806f843a95e029aeb0` (#607, merge normal del25/oct19:40:53UTC). La observación inicial partió de `a1dcb77fafbc66cd786b7f4249a3c8027bb64990`.
- Inspección productiva de solo lectura del 25/oct, 08:13:07 UTC:
  `/private/tmp/tdf-user-value-runtime-20261025.json`. Versión pública del backend:
  `4dac175d42616b5b2f4a3e8faebb83692cd2e6b0`; 213 migraciones registradas.
  La inspección distingue versión pública de identidad del contenedor y no
  concede autorización de despliegue.
- Se contrastaron PR abiertos y fusionados, incidencias, CI de `main`, código,
  `specs.yaml`, `formal/system/requirements.json`, sus contratos y los recorridos
  de `docs/persona-testing/journey-specifications.md`. No se interpreta un PR
  abierto como ausencia de funcionalidad: #594, #595, #598 y los recorridos de
  Comunidad/chat #576/#578 ya están integrados en `main`.
- La copia principal del operador contiene trabajo ajeno y un `main` local
  divergente. La reparación nueva se desarrolla en una copia aislada de la base
  protegida. No se modifica esa copia principal ni se adopta otro despliegue.
- Esta revisión no representa una auditoría exhaustiva de cada ruta, una prueba
  de corrección total ni una validación de cada dispositivo físico.

## Modelo de prioridad

Primero se aplica una categoría obligatoria: **S0** para defectos verificados de
seguridad, privacidad, integridad financiera o pérdida de datos; **S1** para los
demás recorridos. S0 siempre precede a S1. Un trabajo S0 bloqueado conserva esa
prioridad y su dependencia explícita; se selecciona el siguiente trabajo seguro
y ejecutable sin omitir revisiones ni abrir una implementación paralela.

Dentro de cada categoría, cada dimensión puntúa de 0 a 5:

`P = (15A + 25G + 15V + 15J + 15E + 5C + 5R + 5T) / 5`, de 0 a 100.

| Dimensión | Criterio de puntuación |
| --- | --- |
| A, alcance | Tipos y cantidad de usuarios afectados. Sin conteo observado, es una hipótesis ordinal, no una cifra de usuarios. |
| G, gravedad | 0: cosmético; 5: pérdida de datos, incumplimiento de autoridad o impedimento grave. |
| V, resultado | Efecto esperado en activación, retención, interacción o monetización. Es hipótesis hasta medirlo. |
| J, recorrido | Cercanía a una intención y resultado persistido completos; 5 cierra un bloqueo directo. |
| E, evidencia | 0: conjetura; 3: código y requisito; 5: reproducción y control negativo con estado autoritativo. |
| C, coste | 5: cambio pequeño en componente existente; 0: implementación amplia o dependencia externa. |
| R, entrega segura | 5: pocas dependencias, coste operativo y riesgo de regresión acotados; 0: restricciones relevantes sin resolver. |
| T, tiempo al usuario | 5: entrega próxima bajo gates existentes; 0: plazo desconocido o bloqueo externo. |

Los pesos son una decisión de producto documentada. A/G/V/J/C/R/T son estimaciones
ordinales revisables. Ninguna puntuación equivale a ROI ni a mejora medida de
conversión. Las puntuaciones siguientes usan la evidencia disponible antes de la
reparación nueva, excepto la evidencia del borrador propio, actualizada de 3 a 5
tras controles negativos y ejecución descartable (85→91).

| Orden | Trabajo existente o nuevo | Categoría | A/G/V/J/E/C/R/T | P | Estado y acción |
| --- | --- | --- | --- | --- | --- |
| 1, integrado | Evitar guardar borradores ajenos/sin lectura propia y revocar callbacks al cambiar credenciales | S0 | 4/5/4/5/5/4/4/4 | 91 | MERGED: #607 integrado como `cfe75b18`,20checks obligatorios y revisión vigente verificados. Epoch síncrono, lectura propia y caché calificados localmente; verificación desplegada en curso, sin afirmar disponibilidad todavía. |
| 2 | Preservar términos aceptados, ledger y capturas inciertas | S0 | 3/5/5/5/5/3/2/2 | 86 | BLOCKED. Reutilizar #551, sin promover #555/#569 por separado ni activar proveedores. |
| 3 | Subir portada con sesión HttpOnly válida | S1 | 3/4/4/5/5/5/4/3 | 83 | COMPLETE para el transporte web cookie-only: #605 integrado como `9a927fa3` y publicado, con fixture de navegador verificado. Google real no certificado por esta prueba. |
| Entregado | Reflejar foto guardada en Comunidad | S1 | 4/3/4/5/5/5/4/3 | 81 | COMPLETE en el alcance de invalidación web: #604 integrado como `512d590e` y assets publicados verificados. Browser real con API sintética; no afirma upload ni escritura productiva real. |
| 4 | Abrir comprobantes privados desde gestión de entradas | S1 | 3/4/4/5/5/3/2/3 | 79 | BLOCKED para release. #603 integrado como `089dce6b`, backend productivo4dac; falta configuración privada durable y admisión de nuevo diario, y el assurance exacto sigue BLOCKED. |
| 5, seleccionado | Alertas guardadas respetan filtros no geográficos | S1 | 2/3/3/4/5/2/3/2 | 64 | PARTIAL:10 discrepancias reproducidas sobre27 casos iniciales; reparación no geográfica compartida pasa68SQL,16UTC,7planes y106HTTP. Última calificación107HTTP preparada; falta revisión/CI/merge/release. Radio sigue BLOCKED por centro/privacidad y fallback legacy fuera del índice no cubierto. |
| 6 | Correo/push de chat controlado por destinatario | S1 | 3/3/4/4/3/1/1/1 | 60 | BLOCKED para entrega; UNIMPLEMENTED el alcance externo #577, pendiente consentimiento, adaptadores y dispositivos. |
| 7 | Habilitación explícita del club de un artista nuevo | S1 | 2/2/4/4/5/1/2/1 | 59 | UNIMPLEMENTED en #606; falta ese camino de contenido. El follow funciona y existen enlaces públicos alternativos. Nunca provisionar por GET ni crear miembros/contactos sin consentimiento. |
| 8 | Ciclo completo de recuperación, expiración y vuelta al intento | S1 | 4/4/3/3/2/1/1/0 | 58 | PARTIAL, sin defecto adicional reproducido. Conservar ST-001 y el transporte existente. |

La selección conserva la cola FIFO de integración: #604/#605/#607 ya están integrados; la reparación #534 aún no se publica. No tocar drafts financieros ni #608 para alterar la admisión. El controlador `scripts/merge-admission.py` admite un único PR no
draft de menor número. Protección observada: 20 contextos, checks estrictos,
aprobación independiente, descarte de revisiones obsoletas, administradores
sujetos a protección y resolución de conversaciones. Nunca convertir esta
priorización en una excepción a esos controles.

## Hallazgo seleccionado: borradores de perfiles sin base propia

**Actor e intención.** Un fan o artista abre su perfil para cambiar un dato
conservando el resto de su información. En un dispositivo compartido, cambia de
cuenta sin garantizar un desmontaje completo de la interfaz.

**Fuente y fallo observado en código.**
`tdf-hq-ui/src/features/fans/useFanProfile.ts` empieza con campos vacíos y permite
`Fans.updateProfile(profileDraft)` sin comprobar que finalizó la lectura.
`tdf-hq-ui/src/pages/FanHubPage.tsx` hace lo mismo para el artista, condicionado
por géneros, pero sin exigir una lectura propia válida. Además, el efecto de
cambio de cuenta solo sustituye `apuArtistId`, conservando otros campos del
borrador anterior. Las consultas están separadas por usuario; el estado local
del formulario necesita la misma separación.

**Riesgo y resultado esperado.** Guardar durante una lectura pendiente/fallida
puede sustituir campos persistidos por blancos; al cambiar de cuenta, puede
copiar datos anteriores al nuevo perfil. Se pretende evitar esas escrituras y
permitir recuperación explícita. No se ha demostrado un bypass de autorización
del servidor ni se afirma que hayan ocurrido pérdidas en cuentas reales.

**Criterios de aceptación.**

1. Con GET pendiente o fallido, no se emite PUT de perfil con borrador vacío ni
   con un borrador perteneciente a otra cuenta. El formulario informa del estado
   y permite recuperar la lectura.
2. Una respuesta exitosa se asocia con su actor antes de habilitar edición y
   guardado. Una ausencia confirmada permite crear un perfil sin confundirla con
   un fallo de transporte.
3. Cambiar de actor descarta la autoridad del borrador previo inmediatamente;
   respuestas o callbacks tardíos no hidratan ni guardan ese borrador para el
   actor nuevo. Cerrar sesión impide nuevas escrituras desde el formulario.
4. Un GET recuperado habilita el recorrido normal: editar, guardar y volver a
   leer el resultado persistido. Los cambios de otro usuario nunca se reutilizan.
5. La reparación comparte la guardia de propiedad/base entre fan y artista;
   conserva campos, validación de géneros, autorización del servidor y la
   compatibilidad de las reparaciones de foto ya preparadas.
6. Las pruebas reproducen las escrituras inseguras sobre el código original y
   pasan con el arreglo. Los resultados exactos, controles negativos y límites
   de browser/API/PostgreSQL se registran después de ejecutarse, sin promover
   pruebas de UI a evidencia de persistencia real.

**Implementación/validación/entrega.** En curso; consultar el informe JSON
acompañante. El responsable de la reparación completa SHA, comandos, resultados,
PR, merge y despliegue. No se reclaman pruebas ni beneficio observado antes de
registrar sus recibos.

## Estado de los siete recorridos de la misión

| Recorrido | Clasificación | Evidencia, disponibilidad y límites |
| --- | --- | --- |
| Registro, autenticación y recuperación | PARTIAL | Login/sesión y deadlines existen; #594 está en `main`. ST-001 mantiene pendientes ciclo de reset/expiración/inbox y retorno al intento en la plataforma objetivo. La prueba de login no completa recuperación. |
| Perfil de artista, publicación y compartir | DEFECTIVE | Hallazgo de borrador seleccionado; subida cookie-only #605 y refresco #604 tienen solución existente pendiente. No confundir un fallback de imagen o una foto reparada individualmente con corrección general. |
| Descubrimiento, seguimiento e interacción fan | PARTIAL | #576 ya integrado; la búsqueda canónica y el seguimiento persistido tienen evidencia previa aislada. Inconsistencia de foto/caché sigue en #604. Primer valor de onboarding, favorito y follow social son métricas distintas. |
| Comunidad, notificaciones y mensajería | PARTIAL | #578 ya integra avisos internos y respuestas. Correo/push de #577 no está completado. `social_v2_runtime.enabled=false` en la observación: no certificar V2 disponible ni activar la bandera. |
| Música, vídeo, eventos y contenidos | PARTIAL / BLOCKED por subcapacidad | Descubrimiento y contenido existente no implican compra/distribución disponible. Las cinco banderas `music_releases.*` están desactivadas; distribución/merch comercial también. #514 conserva condiciones antes de activar releases. |
| Reservas, clases, entradas y compras | PARTIAL / BLOCKED por subcapacidad | Entradas y marketplace tienen banderas activas; clases, service bookings y Domo comercial están cerrados. #603 completa revisión de comprobantes; #551/#596 contienen dependencias financieras. No hacer cobros reales para verificar. |
| Promoción y desarrollo de audiencia bajo control del usuario | PARTIAL | Hay perfiles, enlaces, campañas y seguidores; la automatización o una campaña preparada no demuestra consentimiento, llegada, retención o promoción autocontrolada de extremo a extremo. #599 sigue draft. |

Clasificaciones acotadas adicionales:

- **COMPLETE:** exige implementación, verificación y disponibilidad en el entorno
  previsto. Ningún recorrido entero recibe una promoción nueva a COMPLETE en
  esta revisión de alcance limitado. La incorporación a `main` de #594, #595 y
  #598 sí es un hecho verificable por Git; no autoriza a reimplementarlos ni
  certifica por sí sola todos sus recorridos productivos.
- **UNIMPLEMENTED:** entrega offline de correo/push de chat del alcance #577,
  separada de avisos internos existentes.
- **OBSOLETE:** repetir desde cero las solicitudes antiguas de búsqueda
  compartida #576, avisos/respuestas #578, deadline #594 o recuperación de carrito
  #595. El trabajo ha sido sustituido por implementación integrada; cualquier
  nuevo cambio exige evidencia de un defecto restante.
- **BLOCKED:** activar funciones financieras/música y distribución cerradas sin
  sus gates, y declarar una prueba matemática de implementación completa donde
  la trazabilidad conserva obligaciones UNPROVEN. Un modelo acotado no cambia
  esa clasificación.

## Correspondencia con requisitos y prompts anteriores

| Fuente de intención | Comprobación de implementación y decisión |
| --- | --- |
| Misión actual §§1–4 y 7: reparar perfiles y maximizar recorrido completo | Hallazgo nuevo de conservación de datos; reutilizar #604/#605. Medir estados reales, no contar PRs. |
| ST-004, completar perfil y salir con seguridad | Exige propietario, persistencia, errores recuperables y estados loading/empty/stale distintos; fundamenta la reparación de borrador y prueba de cambio de cuenta. |
| ST-001, registro desde intención protegida y recuperación | Login/deadline integrado no satisface inbox/reset/expiración completo; mantener brecha de verificación. |
| `docs/engineering/user-value-priorities-2026-10-16.md` | Sus tareas deadline/carrito están integradas; sus observaciones de backend antiguo son históricas y se sustituyen por la inspección del 25/oct. |
| `docs/music-directory/functional-spec.md` y `docs/social/audit.md` | Reutilizar identidad canónica, consultas, consentimiento y espacios existentes. Cada anotación histórica se contrasta con el código actual; no reabrir fallos por el texto antiguo. |
| `specs.yaml` y contratos de `formal/system/requirements.json` | Conservar invariantes, fuentes, inventario y trazabilidad al cambiar implementación. Aseguramiento por alcance, sin afirmar corrección total por tests o modelos. |
| #564/#569/#555/#551 y #596 | La existencia de soluciones en ramas no completa `main`; conservar consolidación, migraciones históricas, modelos y restricciones de release. |
| #128/#130 y piloto de Quito | El experimento de onboarding está desactivado. Su meta humana es hipótesis y exige consentimiento; no iniciar outreach ni mensajes reales para esta prueba. |

## Medición y actualización de resultados

La cohorte histórica del 12–18/oct registra **13 altas y 3 primeros valores
(23,08%)**; muestra pequeña observada el 24/oct. Los tres fueron
`artist_followed` de onboarding; no equivalen automáticamente a relaciones nuevas
deduplicadas, VAR ni retención. No atribuir ese valor a la reparación y no
proyectar mejora porcentual. No se dispone aquí de conteo de usuarios afectados
por guardados prematuros ni frecuencia de cambio de cuenta.

La evidencia mínima del incremento es: cero PUT antes de base propia válida,
cero copia entre actores, guardado permitido después de recuperación y lectura
posterior igual al resultado persistido. Las métricas de producto futuras son
intentos/éxitos de edición, abandono recuperado y tiempo hasta perfil publicado,
con identidad/consentimiento, denominador y ventana explícitos. Recoger solo
metadatos agregados; no añadir texto de perfiles o credenciales a telemetría.

Antes de cerrar cada fila, anotar: problema original; beneficio esperado y
observado; SHA y archivos; comandos/recibos y control negativo; PR/revisión/merge;
versión efectivamente desplegada; flags y dependencias restantes. Cuando se
actualice `main`, repetir la comparación de contenido antes de reabrir trabajo.

## Cinco siguientes después del incremento seleccionado

1. Resolver los bloqueos de la consolidación financiera #551, conservando sus revisiones, modelos y límites de release.
2. Preparar la entrega segura del backend #603: almacenamiento privado durable, admisión revisada del siguiente release y disposición de assurance exacta. Reutilizar los drafts #495/#600 donde corresponda; nunca reiniciar el diario completado.
3. Integrar la reparación no geográfica compartida de #534 por revisión/CI y release autorizado; conservar dedup, privacidad y cero canales externos. Radio continúa pendiente de definición compatible con privacidad.
4. Resolver los prerrequisitos de entrega de chat #577; no prometer email/push antes de evidencia real.
5. Habilitación explícita e idempotente del club #606, con permisos probados y sin crear miembros o notificaciones implícitos.

Las correcciones web #604/#605 ya están integradas y publicadas. Las puntuaciones siguen siendo hipótesis ordinales; no hay uplift observado ni un conteo validado de usuarios afectados.

## Evidencia del arreglo seleccionado

- Nueve regresiones fallaron sobre la base protegida antes de la reparación; trece
  pruebas finales pasan e incluyen cambios de cuenta, respuestas tardías y slug.
- Dos cuentas sintéticas en navegador real de escritorio y emulación Pixel7:
  GET pendiente/503 causa cero PUT, datos persistidos intactos, reintento recupera
  los valores y cuatro guardados explícitos cambian solo la ciudad. Recarga y
  conservación de biografía, foto, enlaces y géneros pasan; cero errores de página.
- PostgreSQL descartable: manifiesto completo de 213 migraciones, repetición y
  dos ciclos rollback/reapply; base eliminada y procesos/puertos cerrados.
- UI servida por Vite desde bytes verificados del candidato. Backend E407
  reutilizado con árbol de fuentes equivalente; no es una nueva compilación
  identificada por el candidato. Cambio de cuenta en el mismo montaje cubierto
  separadamente por Jest. Build productivo pasa (334201 bytes gzip, 5 preloads).
- Lint, build, calidad general del repositorio y catálogo estricto 1326 pasan; suite web completa: 3350 pruebas / 297 suites PASS, salida 0 (507,685s). Estados finales y recibos se
  mantienen en `reports/user-value-completion-2026-10-25.json`.
- Beneficio observado: conservación de perfiles y recuperación en entorno
  descartable. No se afirma disponibilidad productiva ni mejora de conversión.

## Investigación del siguiente trabajo ejecutable: alertas guardadas (#534)

El trigger vigente se redefine en
`tdf-hq/sql/2026-10-07_directory_artist_derived_listings.sql:940`; sus claves
no cubren fechas, servicio, modalidad remota, disponibilidad ni tipo de perfil,
aunque la consulta pública sí usa esos predicados. Mobile persiste el sentinel
`entityType: "all"`, que el trigger interpreta como un tipo literal y excluye
coincidencias. También divergen substring y aliases en búsqueda textual. Son
contraejemplos derivados de fuente, todavía no una reproducción SQL/HTTP.

La reparación no geográfica es ejecutable mediante matcher compartido y pruebas
reales de paridad. Completar radio está BLOCKED: la especificación del directorio
prohíbe almacenar coordenadas exactas y Mobile solo guarda `radiusKm`; falta
definir centroide aproximado o rechazo explícito del filtro no soportado. No
introducir casts que permitan a consultas históricas malformadas interrumpir
actualizaciones del documento de búsqueda. Preservar visibilidad, vencimientos,
deduplicación y las excepciones de filtros exclusivos de personas.

Chat tiene mayor puntuación ordinal (60 frente a 59), pero su entrega requiere
consentimiento/adaptadores; alertas aparece como siguiente investigación ejecutable
por esa elegibilidad, sin alterar el orden numérico ni prometer cierre completo
de #534. La corrección del PR #586 ya fusionado no cubre este matcher.

Observación pública25/oct 08:25 UTC: web `a1dcb77` (asset
`index-B4Qx1_LC.js`, SHA256
`e1f64cc56405d415385f006340d50016cd28af14e2af5c6c922838a09becac80`),
API `4dac175d`, health y DB `ok`. La reparación de perfiles todavía no está
integrada ni desplegada. Recibo de solo lectura en el informe JSON.

Cierre local: 3350 pruebas / 297 suites web PASS, lint/tipos/build PASS,
quality:repo PASS y catálogo estricto 1326PASS. Los 136 requisitos anteriores se
conservan campo por campo y se añade `COMMUNITY-PROFILE-001`; se regenera
inventario y trazabilidad sin afirmar prueba matemática. Revisión local
independiente sin hallazgos materiales. La revisión de GitHub del commit definitivo,
los gates hospedados, la admisión FIFO, el merge y el despliegue siguen pendientes.

Entrega en revisión: [PR #607](https://github.com/diegueins680/tdf-app/pull/607),
rama `fix/artist-profile-load-safety-20261025`. El incremento mantiene estado
PARTIAL hasta completar revisión independiente, CI, admisión FIFO, merge y
verificación desplegada. Auto-merge desactivado; no cambia las reglas ni la cola.

Actualización de integración: #603 se fusionó como `089dce6b` durante la
publicación de #607. Se incorpora mediante merge normal, conservando exactamente
los 136 requisitos del nuevo main y el nuevo requisito de perfiles. El requisito
`EVT-TICKET-MANUAL-001` incorpora los campos de comprobantes aprobados en #603.
Los conflictos de merge afectan solo inventario y trazabilidad generados. Las
3350 pruebas web se ejecutaron antes de esa integración; se renuevan las
comprobaciones dirigidas y los gates hospedados. Los bytes de los cuatro módulos
UI de perfiles se conservan; el binario E407 del ensayo no es una compilación
del nuevo candidato con #603. Se inicializa el gitlink Mobile exacto heredado
del main protegido, sin introducir un pin propio.

Controles posteriores al refresh: 101 pruebas en 8 suites de perfiles, onboarding
y comprobantes PASS; 13 comprobaciones de clientes API PASS; build/tipos PASS
(334202 bytes gzip, 5 preloads), inventario/trazabilidad y admisión de superficies
PASS. Estos recibos actualizan la integración sin atribuir al nuevo head la
ejecución histórica de 3350 pruebas. CI hospedado y revisión siguen pendientes.

## Continuación — 18:00 UTC — fuente y evidencia actuales

#604 fue integrado por otro operador como `512d590e`d590e; nuestra precondición rechazó
el intento duplicado antes de modificar GitHub. El árbol coincide con el head
`314ae256c` aprobado a las 16:27 UTC. La web canónica `512d590e` pasó actualización de tarjeta en
Chromium escritorio/Pixel7 con API/sesión sintéticas y cero llamadas productivas
reenviadas; backend saludable en `4dac175d`. Recibo: `/private/tmp/tdf-user-value-604-deployed-20261025.json`.

La composición local607+604 conserva las tres invalidaciones públicas antes del guard
de propietario. 64 pruebas / 4 suites y navegador/API/PostgreSQL descartables pasan;
un control sobre `7f0e7d951` falla por caché pública obsoleta.

La revisión encontró además una ventana anterior al render: tras iniciar sesión como B, un
callback de A admitía un PUT real del transporte con bearer de B y campos de A. Se reprodujo
con proveedores/cliente/cabecera reales y Fetch interceptado, nunca en producción.
Cuatro controles fallan en e715; el hook ahora captura y verifica el epoch de
autenticación síncronamente, incluyendo setters/ack. Los4 casos de transporte y
68 regresiones / 5 suites pasan después del arreglo. La revisión de las 16:27 UTC sobre `7f0e7d951` es
histórica y no se presenta como aprobación de este código nuevo. Las peticiones
ya enviadas y los conflictos de edición entre dispositivos siguen fuera del
contrato. El detalle de recibos vigentes está en el informe JSON.

#534 pasa de sospecha de fuente a27casos PostgreSQL:6 falsos positivos y4 falsos
negativos en consultas válidas;3 JSON históricos malformados y controles de
privacidad/expiry/dedup. No hay evidencia HTTP todavía. El nuevo alcance comparte
helpers tipados entre búsqueda y trigger; evita inventar centros geográficos o
guardar coordenadas precisas, y no reescribe JSON/hashes históricos.

## Cierre de la composición sobre main `9a927fa3`

El operador existente integró #605 como `9a927fa3` a las 18:37 UTC. Se incorpora
mediante merge normal sin conflicto de runtime: se conservan exactamente los 137
requisitos protegidos y el requisito propio actualizado (138 total). Los cinco
archivos UI fueron estables durante la calificación final.

La suite actual pasa **3369 pruebas / 300 suites** (salida 0), build/tipos
(334208 bytes gzip,5 preloads), lint, 13 comprobaciones de clientes API, inventario/trazabilidad,
admisión y catálogo estricto. Navegador Chromium escritorio/Pixel7 con sesión
HttpOnly real y API/PostgreSQL descartables verifica: omisión de credenciales401,
subida 200 con almacenamiento sintético, guardado persistido, tarjeta de Comunidad
actualizada, recarga y eliminación de foto, cero PUT por lecturas pendientes/503
y rechazo 400 de URL privada sin cambio persistido. Los cuatro controles de sesión reales
se mantienen separados: revocación anterior al render y reautenticación del
mismo Party, Fetch interceptado sin salida a servidor.

La web pública #605 ya publica `9a927fa3`; su fixture desplegado confirma cookie sin
bearer y tarjeta actualizada sin reenviar ninguna llamada API productiva.
#607 aún necesita CI/revisión del commit definitivo y no se declara desplegado.
Los recibos y los límites exactos están en el informe JSON.

La evaluación de release #603 encontró bloqueos concretos: ausencia productiva
de `TICKET_PAYMENT_EVIDENCE_DIR` (el código nuevo devuelve 503), 25 registros del
diario anterior completados sin admisión revisada de otro nonce y assurance
exacto de `089dce6b`/`9a927fa3` `BLOCKED` sin riesgos aceptados. Hay imagen `089dce6b` probada y cero
migraciones pendientes, pero no equivalen a permiso ni preparación de release.
No se cambió configuración, storage, contenedores, flags ni diario productivo.


## Estado vigente después de integrar #607

#607 está **MERGED** como `cfe75b18786c18d18fb2fc806f843a95e029aeb0`,
a las19:40:53UTC:20checks obligatorios, revisión independiente vigente del head
`35de` y cero conversaciones pendientes. El merge normal conserva exactamente el
árbol aprobado. Recibo: `/private/tmp/tdf-user-value-607-merge-20261025.json`.
La verificación desplegada está en curso; los estados históricos anteriores
no se presentan como disponibilidad actual ni se afirma uplift de conversión.

El siguiente incremento ejecutable es #534. La fuente conserva los138 requisitos
de ese main campo por campo y añade `DIR-SAVED-SEARCH-001` (139 total). La
migración214 es aditiva; todos los213 objetos y bytes históricos permanecen
idénticos. La búsqueda pública y las alertas comparten predicados tipados e
inlinables; filtros no admitidos fallan de forma cerrada. Las búsquedas nuevas
rechazan coordenadas exactas y radio con alertas activas; quitar el radio permite
guardar sin inventar un centro. JSON/hash/ledger históricos no se reescriben.

La calificación previa pasa68 controlesSQL,16UTC,7comparaciones públicas/planes,
106peticionesHTTP, full214/replay y rollback/reapply exacto; el recibo final añade
medición de provenance nativo (107HTTP). Hspec construido con Stack/GHC9.10.3 pasa
4035 ejemplos,0fallos,20pending mediante ejecución directa. El comando Stack test
que además empezó recompilar innecesariamente el exe fue detenido con salida143;
no se presenta comoPASS. Compilación original del exe:salida0, sin relabel de
commit por variables de entorno. El alcance #534 sigue PARTIAL: radio sin centro
especificado, cambios exclusivos de fallback legacy sin trigger, revisión/CI,
merge y despliegue todavía pendientes. No hay proveedor, cobro ni escritura real
productiva en esta reparación. Contrato: `docs/directory/saved-search-parity.md`.
