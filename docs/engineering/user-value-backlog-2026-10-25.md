# Backlog de valor para usuarios — 25 de octubre de 2026

Responsable: motor de finalización de valor TDF. Documento vivo: actualizar estado,
SHA, evidencia, bloqueo y siguiente acción después de cada validación, revisión,
merge o despliegue. Una implementación local, un PR aprobado y un resultado
disponible para usuarios son hitos separados.

## Base y alcance de la observación

- `main` protegido actual: `1c9c833cc1ba62b47822fca3c20e14e86cf28daa` (#616, después de #551/#611/#614). La observación inicial partió de `a1dcb77fafbc66cd786b7f4249a3c8027bb64990`.
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
| 1 | Salida privada y recuperación sin restaurar una cuenta anterior | S0 | 4/5/4/5/5/3/2/3 | 87 | #616 SOURCE MERGED como1c9: aprobación vigente42b9 y20controles, ensayo221 completo; verificación de web publicada pendiente. Native171c944 tiene validate verde y espera revisión; no cambiar el pin Mobile por inferencia. |
| 2 | Conservar identidad, origen y auditoría de asientos contabilizados | S0 | 3/5/5/5/5/3/2/2 | 86 | DEFECTIVE en la base financiera verificada; candidato existente621185 aprobado sobre su propia fuente, pero requiere reconciliar actualmain, prefijo221+guard222, ambas historias del catálogo y CI/revisión del head final. No duplicar la implementación. |
| 3 | Entregar backend financiero, comprobantes privados y correcciones ya integradas | S0 / gates S0 | 3/5/5/5/5/3/2/2 | 86 | BLOCKED para release.551/609/603/614 están integrados; imagen e938/221 está cualificada estáticamente,4039pruebas y107HTTP enCI. Guard621, configuración privada, diario/coordinador, recuperación/canary y disposición formal siguen pendientes. |
| 4, ejecutable seleccionado | Admitir siguiente release conservando diario y exclusión global | S0, dependencia | 3/5/4/4/5/2/2/2 | 79 | PARTIAL.618 conserva código/modelos revisadosd682 y ahora compone main real1c9,142requisitos y221migraciones. Publicar una vez después de QA del árbol final; obtener CI/revisión vigentes. Ninguna invocación productiva autorizada. |
| 5 | Entregar alertas y cerrar alcance restante534 | S1 | 2/3/3/4/5/2/3/2 | 64 | PARTIAL.614 no geográfico ya integrado comoe938, backend aún4dac en última observación. Radio requiere contrato explícito de centro aproximado/privacidad; fallbacklegacy continúa pendiente. |
| 6 | Correo/push de chat controlado por destinatario | S1 | 3/3/4/4/3/1/1/1 | 60 | BLOCKED para entrega externa577 por consentimiento, adaptadores y dispositivos; avisos internos ya existen. |
| 7 | Habilitación explícita del club de un artista nuevo | S1 | 2/2/4/4/5/1/2/1 | 59 | UNIMPLEMENTED en alcance606; conservar613draft y no provisionar por inferencia. |
| 8 | Recuperación, expiración y vuelta al intento | S1 | 4/4/3/3/2/1/1/0 | 58 | PARTIAL ST-001; sin defecto adicional reproducido. |
| Entregado | Borradores propios y callbacks de perfil607 | S0 | 4/5/4/5/5/4/4/4 | 91 | COMPLETE en su alcance web acotado y publicado: cfe75b18,3369pruebas yfixtures; no reabrir sin evidencia. |
| Entregado | Transporte cookie-only de portada605 | S1 | 3/4/4/5/5/5/4/3 | 83 | COMPLETE en su alcance web publicado; Google real no certificado por la fixture. |
| Entregado | Invalidación genérica de foto604 | S1 | 4/3/4/5/5/5/4/3 | 81 | COMPLETE en alcance caché web publicado; sin afirmar escritura productiva de una prueba interceptada. |

La cola FIFO actual es #618→#619→#620→#621, después del merge protegido de #616 como `1c9c833c` a04:41:10UTC. #616 conservó aprobación independiente42b9 de04:25:42UTC,20controles y árbol exacto6849. El ranking S0 no autoriza alterar FIFO, drafts, controles ni revisiones. Las menciones posteriores a estados previos son evidencia histórica; esta tabla y el cierre posterior616 gobiernan la prioridad actual.

## Hallazgo seleccionado: borradores de perfiles sin base propia

**Actor e intención.** Un fan o artista abre su perfil para cambiar un dato
conservando el resto de su información. En un dispositivo compartido, cambia de
cuenta sin garantizar un desmontaje completo de la interfaz.

**Fuente y fallo observado en la base original.**
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

**Implementación/validación/entrega.** #607 integrado y web publicada `cfe75b18`, verificada con fixtures sintéticos; no implica persistencia de cuentas reales. Consultar el informe JSON acompañante. El responsable de la reparación completa SHA, comandos, resultados,
PR, merge y despliegue. No se reclaman pruebas ni beneficio observado antes de
registrar sus recibos.

## Estado de los siete recorridos de la misión

| Recorrido | Clasificación | Evidencia, disponibilidad y límites |
| --- | --- | --- |
| Registro, autenticación y recuperación | PARTIAL | Login/sesión y deadlines existen; #594 está en `main`. ST-001 mantiene pendientes ciclo de reset/expiración/inbox y retorno al intento en la plataforma objetivo. La prueba de login no completa recuperación. |
| Perfil de artista, publicación y compartir | PARTIAL | Guardia web de borrador #607 integrada/publicada y verificada con fixtures sintéticos; subida cookie-only #605 y refresco #604 publicados y verificados en su alcance. No confundir un fallback de imagen o una foto reparada individualmente con corrección general. |
| Descubrimiento, seguimiento e interacción fan | PARTIAL | #576 ya integrado; la búsqueda canónica y el seguimiento persistido tienen evidencia previa aislada. Foto/caché #604 publicada y verificada; el recorrido completo conserva sus propios límites. Primer valor de onboarding, favorito y follow social son métricas distintas. |
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

## Cinco acciones siguientes por prioridad actual

1. Privacidad de logout #616, S0, hipótesis87: preservar intención de salida después de POST fallido y evitar restauración de A. El propietario mantiene su READY9e96 y composición; reutilizar su trabajo y esperar gates vigentes.
2. Entrega financiera #551/#609, S0, hipótesis86: código ya integrado; resolver los bloqueos de backend y assurance antes de liberar. No rehacer la consolidación ni activar proveedores.
3. Control de release #618, dependencia S0, hipótesis79: admisión explícita del nuevo diario y condiciones de release; respetar propietario, revisión y fuente efectivamente publicada. No equivale a permiso de ejecución.
4. Alertas no geográficas #614/#534, S1, hipótesis64: publicar la composición final revisada, completar CI exacta y merge protegido; desplegar solo con los gates de backend. Radio y fallback siguen PARTIAL. Es primero en FIFO, aunque la prioridad S0 de otros trabajos sea mayor.
5. Chat externo consentido #577, S1, hipótesis60: consentimiento, adaptadores y dispositivos antes de prometer correo/push.

La activación explícita del club #606 y la recuperación ST-001 quedan después de estas cinco acciones. Los vectores de planificación de616/618 y todos los scores son hipótesis ordinales del modelo, no conteos de usuarios, ROI ni uplift. Las correcciones web604/605/607 tienen evidencia publicada acotada; Completar un subrecorrido no certifica el recorrido entero.

## Evidencia histórica inicial del arreglo #607 (antes de composición final)

Este bloque conserva el ensayo inicial pre603; los números3350/297,334201gzip y1326 son históricos. La composición final3369/300 y334208gzip se describe después; la disponibilidad publicada actual se registra en el cierre607.

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

## Investigación inicial y reproducción confirmada: alertas guardadas (#534)

El trigger vigente se redefine en
`tdf-hq/sql/2026-10-07_directory_artist_derived_listings.sql:940`; sus claves
no cubren fechas, servicio, modalidad remota, disponibilidad ni tipo de perfil,
aunque la consulta pública sí usa esos predicados. Mobile persiste el sentinel
`entityType: "all"`, que el trigger interpreta como un tipo literal y excluye
coincidencias. También divergen substring y aliases en búsqueda textual. Las hipótesis iniciales se contrastaron después:10 discrepancias válidas sobre27 casos PostgreSQL; la reparación actual pasa68 controlesSQL y107HTTP reales. El recibo v12 y sus límites se describen al final.

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

Entrega histórica en revisión, anterior al merge19:40: [PR #607](https://github.com/diegueins680/tdf-app/pull/607),
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


## Estado histórico después de integrar #607 (basecfe, anterior a551/611)

#607 está **MERGED** como `cfe75b18786c18d18fb2fc806f843a95e029aeb0`,
a las19:40:53UTC:20checks obligatorios, revisión independiente vigente del head
`35de` y cero conversaciones pendientes. El merge normal conserva exactamente el
árbol aprobado. Recibo: `/private/tmp/tdf-user-value-607-merge-20261025.json`.
La web publicada se verificó a las19:49:43UTC con assets cfe y fixtures sintéticos de escritorio/Pixel7; los estados históricos anteriores no representan disponibilidad actual ni se afirma uplift de conversión.

El siguiente incremento ejecutable es #534. La fuente conserva los138 requisitos
de ese main campo por campo y añade `DIR-SAVED-SEARCH-001` (139 total). La
migración214 es aditiva; todos los213 objetos y bytes históricos permanecen
idénticos. La búsqueda pública y las alertas comparten predicados tipados e
inlinables; filtros no admitidos fallan de forma cerrada. Las búsquedas nuevas
rechazan coordenadas exactas y radio con alertas activas; quitar el radio permite
guardar sin inventar un centro. JSON/hash/ledger históricos no se reescriben.

La calificación previa pasa68 controlesSQL,16UTC,7comparaciones públicas/planes,
107peticionesHTTP con medición de provenance nativo, full214/replay y rollback/reapply exacto. El recibo final v12 confirma263 fuentes backend y todos los bytes candidatos estables antes/después, base eliminada, cluster detenido y puerto cerrado. Hspec construido con Stack/GHC9.10.3 pasa
4035 ejemplos,0fallos,20pending mediante ejecución directa. El comando Stack test
que además empezó recompilar innecesariamente el exe fue detenido con salida143;
no se presenta comoPASS. Compilación original del exe:salida0, sin relabel de
commit por variables de entorno. El alcance #534 sigue PARTIAL: radio sin centro
especificado, cambios exclusivos de fallback legacy sin trigger, revisión/CI,
merge y despliegue todavía pendientes. No hay proveedor, cobro ni escritura real
productiva en esta reparación. Contrato: `docs/directory/saved-search-parity.md`.


Recibo histórico no geográfico sobre full214: `/private/tmp/tdf-alert-parity-repro-20261025/v12-frozen-full214/results.json`
(PASS sobre `8b2f981fa952b6d281a22b4c3eb187f3828df870`). SQL SHA256
`673f7bb790c4e791a0c8caeb5eacbf20873331b65470c000a8ed27fff68cf8e4`;
Directory SHA256 `d900bc237cf4331aa9ca03b67641bdefa832f52b6f801b1ce1c753ea97848494`.
El binario conserva commit compilado9a y SHA256
`fc8c2819d29760efc7108e5b7c801fc58efc37ff7847bb37fb00d577b19e7d83`;
se contrastaron todas las fuentes backend con esa base salvo Directory, cuyo hash
fue calificado en la compilación original. No se relabela el binario ni se
presenta como compilación del nuevo commit de docs/UI. CI exige compilar el head
exacto. Después del recibo solo cambian documentos/trazas y se elimina un espacio final del fixture SQL sin alterar sus tokens. Backend, migraciones forward/rollback y runners conservan exactamente sus bytes calificados. Beneficio observado: las10 divergencias iniciales desaparecen
en casos sintéticos y se confirma guardado recuperable/alerta interna persistida;
no hay beneficio productivo ni conversión atribuible antes del release autorizado.


Verificación publicada #607: `/private/tmp/tdf-user-value-607-deployed-20261025.json`,
a las19:49:43UTC. Asset `index-dzlEpVI6.js`, SHA256
`287eac22278297be85cf7e3ee8ddecc03ffd85558afd5dd7b749a4c6c9001997`.
Chromium escritorio/Pixel7 verifica503/ceroPUT, reintento, guardado conservador,
cookie HttpOnly/XHR y tarjeta actualizada. Tras confirmar el cambio de sesión en React,
invocar callback viejo de A produce0PUT y B puede guardar su
propio perfil. Esa prueba publicada ocurre después de confirmar el cambio en React; la ventana
anterior al render se cubre por los4 controles locales separados. No se reenvió
ninguna API/escritura productiva y hubo0errores de página. El backend público
sigue `4dac175d`, health/DB ok. Este resultado completa el alcance web acotado607;
no certifica credenciales/cuentas reales, persistencia productiva ni dispositivo
físico. La reparación backend #534 continúa sin publicar/desplegar.


## Composición actual después de #551 y #611

#551 se integró como `5114fe95ac8e328b58162a8dcae5c9015a47ce09` y #611 como
`917806ff9c8d455a01bfb8a29c80d6ff353e0e0b`. La composición conserva exactamente
los220 objetos/SQL de main, sus siete introducciones financieras y los140 requisitos
protegidos completos; agrega alerta221 con el ancestro original `eb2f71d6` y DIR
(141 requisitos). Los snapshots revisados213/214/220 siguen inmutables; el sucesor221
`8229b8202552298c955e` no autoriza ejecutar ni liberar nada. Los tres fingerprints
retirados siguen rechazados por el gate real. No cambia código financiero aprobado.

La nueva compilación Stack/GHC9.10.3 terminó con salida0 en una única invocación
`stack --no-terminal test --force-dirty tdf-hq --copy-bins`:4039 ejemplos,0 fallos,
21 pendientes. El binario identifica exactamente `fe179de45d9b0e085bb7cd2f00a9704991fa1652`,
SHA256 `cfe718ef2465e872e9db2ab3892b60f5df81642e5c959d6ad07a6ff20bf8e76a`. Sobre ese mismo código pasan full221/replay,
rollback exacto/reapply,68 casos SQL,16 UTC,7 planes y107 HTTP reales. Se verifican
JSON/hash/ledger preservados, privacidad/aislamiento y cero proveedores externos.
Base eliminada, clúster detenido y puerto55512 cerrado. Recibo:
`/private/tmp/tdf-alert-parity-post551-20261026/results.json`.

Después se incorporó611 por merge normal. Sus cambios son dos helpers Python e
índices generados: se contrastaron los263 inputs backend, el árbol SQL completo,
manifiesto, renderer y fixtures/runners; permanecen byte a byte iguales a la
calificaciónfe179. Esa equivalencia no relabela el resultado como ejecución del
nuevo commit de documentos/ops. CI hospedada compilará y probará el head exacto
que finalmente se publique. Recibo de equivalencia:
`/private/tmp/tdf-alert-parity-post551-20261026/post611-equivalence.json`.

#614 sigue sin merge/despliegue y #534 sigue PARTIAL para geografía sin centro y
cambios exclusivos del fallback legacy. El beneficio observado es sintético:
las10 discrepancias iniciales desaparecen y el guardado recuperable llega a una
alerta interna persistida; no hay uplift ni beneficio productivo atribuible todavía.
La imagen5114 tiene identidad estática verificada; no prueba startup, recuperación
ni despliegue. Las observaciones anteriores de storage/diario requieren reinspección
y el assurance exacto conserva280 obligaciones UNPROVEN. No se modificaron storage,
diario, contenedores, flags, proveedores ni datos productivos.

#616 tiene evidencia local separada64/9,178 casos nativos y3411 pruebas UI/303 suites
sobre las identidades indicadas por su propietario. Sus pruebas reales API/PG se
ligan a f9, con adapters/auth byte equivalentes a9e; no se relabelan como ejecución
exacta de otra composición. Recibos:
`/private/tmp/tdf-616-p3-joint-review-20261025-evidence/receipt.json` y
`/private/tmp/tdf-616-f9-independent-20261025-evidence/receipt.json`.

## Incremento de siguiente admisión del diario

**Problema:** el diario productivo observado19:03 conserva25registros completos;
`initialize` rechaza un nuevo nonce incluso después de completar las doce fases.
El contrato reconoce esta admisión futura como obligación separada. #495 y #600
no implementan sucesión terminal y no se encontró responsable activo registrado
para este incremento en la revisión de GitHub. No se ha borrado ni modificado ese
diario, instalado un coordinador ni cambiado el backend productivo.

**Aceptación:** conservar todos los bytes de los25registros v1 y el inode del
mismo lock; publicar el plan sucesor al final de la cadena global solo con
terminal completo, hash de punta exacto y nonce nunca usado; impedir sucesión
sobre pendiente/temporal/corrupción; envenenar el handle tras IO incierto; conservar
posibles escrituras históricas y rechazar competencia, replays y handles heredados.
El límite128releases falla cerrado y no autoriza rotación ni borrado. Cada efecto
nuevo se vincula exclusivamente a su plan/nonce y sigue requiriendo intención
durable y observación correspondiente.

**Verificación local:**22pruebas de filesystem/procesos pasan normal y Python-O,
incluyendo tres releases completos, dos procesos competidores, muerte durante
publicación y los tres puntos de fsync. Modelo adicional TLC:2configuraciones
positivas,8mutaciones de guardias y1testigo de tres releases completos; se mantiene
intacto el modelo anterior. Verificación canónica completa y revisión independiente
se registran al terminar. Estos resultados no prueban refinamiento de Python,
durabilidad del hardware, efectos externos, liveness ni corrección total.

**Valor esperado:** permitir preparar entregas siguientes sin abandonar exclusión
e historial. Beneficio observado limitado a datos y procesos descartables; sin
entrega productiva, tiempo al usuario ni conversión medidos. Resta PR enfocado,
CI/revisión/admisión y coordinación protegida. La configuración privada de
comprobantes y las restricciones formales/de release siguen independientes.


## Reconciliación local después del merge protegido de #614

La base real es `e938c72`, con221migraciones y141requisitos. Se conserva cada objeto canónico de main salvo la extensión revisada de DEPLOY-JOURNAL-001 de d682; código, pruebas y modelos del diario no cambian. Las ejecuciones normal/optimizada en589 y los recibos d682 conservan sus identidades: esta composición no se presenta como una ejecución nueva. Inventario, trazabilidad y admisión de superficies se verifican de nuevo. La rama permanece local hasta main final de616; después requiere CI y revisión de su nuevo head.

## Cierre protegido de616 y composición final de618

616 se integró normalmente como1c9c833c, con padres e938+42b9 y árbol exacto6849ae7eda702460b893febb40b371c9505114f0. La composición618 conserva los142objetos salvo DEPLOY-JOURNAL-001, cuya extensión revisadad682 se mantiene exacta; las221migraciones, backend, cliente web y contratos permanecen exactos del main protegido. Solo se regeneran índices para esa unión y se actualiza este backlog. El remote618anterior d682 y sus aprobaciones/ejecuciones no se presentan como revisión del nuevo commit.

La imagen e938 está ligada al índiceOCI1721e6b1a130a685aba1793a2144d9c0a060ba0771bf2d57157e6402e0b4bbd2, manifiestoamd64e5a0548af8ef458822ab9f7d20df42001118f59cfd9ab6819e68a5279bd2cf43, ejecutableCI641c964c2a33dfaa55ce53cd47508a40e28949f69c3e2faf2e980b3258d3650b ySQL221byteexacto. CI ejecutó4039ejemplos/0fallos/21pendientes y107HTTP. Es verificación estática y del CI existente, sin ejecutar la imagen ni declarar un canary/release; no incluye guard621. Su reporte exacto conserva141requisitos y282obligaciones completasUNPROVEN, releaseBLOCKED yningún riesgo aceptado.

El defecto621 permite a un rol SQL ordinario cambiar transaction_type/origen/auditoría después de contabilizar. La evidencia existente y sus232huellas de entrada conservan identidades e57c/185; no es un incidente productivo ni prueba de doble cobro. Sobre e938, el arreglo se añade después del prefijo221; ambos catálogos de221elementos tienen diferentes fingerprints y deben conservar sus historias antes de revisar el sucesor222. No reconstruir la implementación de su responsable ni permitir release financiero con ese bloqueo abierto.

Próximos cinco resultados: salida privada web/nativa; identidad financiera621; backend financiero/comprobantes/alertas; diario/control618; alcance restante534. Todas las puntuaciones de impacto siguen siendo hipótesis ordinales, sin usuarios afectados ni mejora de conversión medida.
