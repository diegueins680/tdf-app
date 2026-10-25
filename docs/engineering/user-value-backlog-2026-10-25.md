# Backlog de valor para usuarios — 25 de octubre de 2026

Responsable: motor de finalización de valor TDF. Documento vivo: actualizar estado,
SHA, evidencia, bloqueo y siguiente acción después de cada validación, revisión,
merge o despliegue. Una implementación local, un PR aprobado y un resultado
disponible para usuarios son hitos separados.

## Base y alcance de la observación

- `main` protegido observado: `a1dcb77fafbc66cd786b7f4249a3c8027bb64990`.
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
| 1 | Preservar términos aceptados, ledger y capturas inciertas | S0 | 3/5/5/5/5/3/2/2 | 86 | BLOCKED. Reutilizar consolidación #551; no promover #555/#569 individualmente. Requiere reconciliación, gates y resolver restricciones formales/operativas. |
| 2, seleccionado | Evitar guardar borradores sin lectura propia y aislar cambios de cuenta | S0 | 4/5/4/5/5/4/4/4 | 91 | DEFECTIVE en la base; reparación local implementada, 13 regresiones y navegador/API/PostgreSQL descartables pasan. Pendiente CI/revisión/merge/despliegue. Comparte causa entre perfiles fan y artista, sin cambiar API ni SQL. |
| 3 | Subir portada con sesión HttpOnly válida | S1 | 3/4/4/5/5/5/4/3 | 83 | PARTIAL. Reutilizar #605; reconciliar con `main`, renovar CI/revisión y respetar admisión. No duplicar uploader. |
| 4 | Reflejar foto guardada en Comunidad | S1 | 4/3/4/5/5/5/4/3 | 81 | PARTIAL. Reutilizar #604; conservar invalidación compartida y verificar persistencia e imagen cargada. |
| 5 | Abrir comprobantes privados desde gestión de entradas | S1 | 3/4/4/5/5/3/2/3 | 79 | PARTIAL. #603 está en la cola y otra tarea mantiene su integración; no tocar su rama. CI y despliegue de almacenamiento privado siguen siendo hitos separados. |
| 6 | Correo/push de chat controlado por destinatario | S1 | 3/3/4/4/3/1/1/1 | 60 | BLOCKED para entrega; UNIMPLEMENTED en el alcance descrito por #577; depende de consentimiento, adaptadores y observación de entrega. Avisos internos y respuestas ya existen. |
| 7 | Alertas guardadas respetan todos los filtros | S1 | 2/3/3/4/3/3/3/2 | 59 | DEFECTIVE confirmado por comparación de fuentes actuales y contraejemplos booleanos; pendiente de prueba SQL/HTTP. No confundir con #586 ya fusionado sobre aliases de géneros. |
| 8 | Ciclo completo de recuperación, expiración y vuelta al intento | S1 | 4/4/3/3/2/1/1/0 | 58 | PARTIAL: prioridad de investigación, sin defecto nuevo reproducido. ST-001 exige inbox local y estado del servidor; no inventar otro login. |

La selección no cambia la cola FIFO de integración: actualmente #603 precede a
#604 y #605. El controlador `scripts/merge-admission.py` admite un único PR no
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

1. Resolver la entrega protegida de la consolidación financiera #551, respetando
   sus bloqueos; no duplicar #555/#569 ni activar proveedores.
2. Reconciliar y entregar #605; validar cookie-only y persistencia con proveedor
   falso, sin Drive productivo ni credenciales reales.
3. Integrar #604 y verificar en escritorio/móvil que guardar, quitar y recargar
   una foto actualiza perfil y Comunidad desde la misma fuente persistida.
4. Completar #603 en su tarea propietaria y verificar almacenamiento/recuperación
   privada y acceso autorizado antes de desplegar el backend.
5. Reproducir #534 contra todos los filtros guardados, reutilizar la búsqueda
   canónica y corregir el primer filtro discrepante con regresión de selección
   y notificación local deduplicada.

Ninguno de estos pasos autoriza cobros, mensajes, publicación social, migraciones
productivas o eludir revisión. Un gate externo conserva el trabajo como bloqueado
y permite avanzar al siguiente candidato seguro.

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
