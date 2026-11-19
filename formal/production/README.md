# Producciones musicales

La ruta web autenticada es `/producciones`. El productor puede entrar desde Proyectos del label, el menú o el buscador de funciones. El formato, las canciones, la fecha y la plantilla al crear un proyecto determinan las primeras tareas. La duración se expresa en días naturales. Una etapa de cero días representa un hito. Cada fecha de una reserva corresponde a la hora del estudio en America/Guayaquil.

## Integración y estado

Las notas operativas de `LabelProjectNote` y los pipelines de órdenes siguen sus flujos actuales. `production_song.label_track_id` permite vincular una pista existente del propietario desde la plantilla o por API. Las canciones nuevas se mantienen privadas. `Session` representa una sesión operativa del estudio, `SessionDeliverable` admite enlaces y una marca de aprobación y `RecordingSession` pertenece al catálogo musical. Para agenda la autoridad es `Booking` con sus recursos e intervalo. Una tarea se vincula a una sola reserva y a cero, una o varias canciones. Las tareas sin canciones se muestran a nivel de proyecto.

Los integrantes son identidades Party con cuenta TDF. La invitación concede acceso al usuario existente en cuanto la confirma el productor. Cada tarea tiene uno de los cinco estados del flujo. El motivo de bloqueo se guarda aparte. En revisión actúa el aprobador. Al solicitar cambios vuelve a actuar el responsable. La vista por canción identifica la versión aprobada y las revisiones anteriores conservan comentarios y decisiones.

`/drive/upload` con destino TDF y visibilidad privada utiliza la cuota, el límite de 50 MiB por archivo y el volumen de archivos gestionados del servicio. El propietario debe tener capacidad disponible. Los revisores abren una ruta autenticada que comprueba proyecto, membresía y versión antes de devolver los bytes. El navegador reproduce un objeto temporal ligado a la sesión. El cliente ve solo los audios compartidos. Las aprobaciones se aceptan exclusivamente del aprobador designado. Un cliente autorizado para esa versión también puede solicitar cambios.

## Propiedades y evidencia reproducible

| Regla | Modelo | Punto de aplicación | Prueba |
|---|---|---|---|
| Dependencias acíclicas | Dos usuarios preparan aristas y las confirman contra el grafo actual | `production_command` bloquea el proyecto y recorre los antecesores antes de insertar | Dos conexiones intentan A→B y B→A. También se reintenta con revisión actual |
| Aprobación exacta | `CurrentApproval`, versiones 3 y 4 | Rol, aprobador, número vigente, estado y bloqueo se validan en la misma transacción | La decisión de v3 recibe 409 tras publicar v4. Un ingeniero recibe 403 al intentar aprobar |
| Decisión única y repetición | `UniqueDecision` y repetición de una decisión existente | Índice parcial por versión e identificador único por actor. Se compara el contenido antes del control de vigencia | Dos decisiones incompatibles solapadas, repetición tras nueva versión y recuento del historial |
| Propuesta y fechas | `PlanSafe` con propuesta, edición y confirmación | Una propuesta conserva la revisión, la causa y las fechas. Solo un productor puede confirmar. Se comprueban las reservas antes de aplicar un cambio | La propuesta no mueve fechas. Un escritor cambia el plan mientras otro confirma. Una reserva en su horario anterior impide la confirmación |
| Alcance de lectura | `Private` diferencia proyecto y versión compartida | `production_visible_version`, proyección `production_workspace` y lector autenticado de audio en `TDF.Server.Production` | Cliente con entrega, cliente sin entrega, proyecto ajeno y referencia al archivo exacto |

Desde la raíz del repositorio:

```sh
curl -fL https://github.com/tlaplus/tlaplus/releases/download/v1.7.2/tla2tools.jar -o /tmp/tdf-tla2tools-1.7.2.jar
TLA2TOOLS_JAR=/tmp/tdf-tla2tools-1.7.2.jar scripts/verify-production-workspace.sh
python3 scripts/test-production-workspace.py
(cd tdf-hq-ui && npm test -- --runInBand src/api/productions.test.ts src/pages/LabelProjectsPage.test.tsx)
```

El verificador valida el SHA-256 del JAR mediante `shasum` y permite seleccionar Java con `JAVA_BIN=/ruta/java`. TLC exploró por separado 1.087 estados distintos del grafo de tres tareas, 20 estados de revisión, 20 del escenario de decisiones, 16 del plan con dos ediciones o confirmaciones como máximo y 6 del permiso de lectura. El escenario de lectura usa dos proyectos y dos versiones. Cada configuración insegura produjo el contraejemplo previsto. La exploración comprueba seguridad en estos límites, sin hipótesis de equidad. Se abstraen la red, los bytes, el sistema de archivos, los relojes reales y el mecanismo de bloqueo de PostgreSQL.

La prueba de persistencia crea un contenedor PostgreSQL 17 sin red y utiliza conexiones independientes que se solapan dentro de la transacción. Comprueba el EP, una única tarea de batería para tres canciones, el aprobador, dos identificadores de archivo, v3/v4, tres carreras, retención del archivo, planificación inversa y privacidad del segundo proyecto. La prueba de calendario de la interfaz separa extremos de sesión contiguos, horarios que se solapan y tareas que comparten un vencimiento. Los criterios de reproducción en navegador y operación en producción se registran por separado en la PR.

## Evidencia del recorrido en navegador

Con el backend compilado, las dependencias de la raíz instaladas, Docker, Chromium y el paquete Python Playwright, ejecutar desde la raíz:

```sh
python3 scripts/test-production-workspace-browser.py --backend /ruta/absoluta/tdf-hq-exe
```

La prueba crea un PostgreSQL desechable a partir del esquema de producción y aplica las 259 migraciones. Arranca el backend real y la aplicación web, crea cuatro sesiones autenticadas y abre el EP desde la navegación web. La artista usa una ventana de 390 píxeles. La ingeniera comparte dos WAV de un segundo. Chromium decodificó y reprodujo v3 (330 Hz, SHA-256 `ea23a0b81765da59b1526c576ddd3bf1e16e8f185eed6f31754b32fdc58246f7`), v4 (550 Hz, `334e25294fa6db746bae4e044ebd12fb60b7247aabb086f6c9a5697f0d2fedf1`) y de nuevo la v3 histórica. Se verificaron las próximas acciones desde ambas cuentas, el rechazo de una decisión obsoleta, el historial, la reserva compartida, la modificación de la reserva antes del ajuste, la carga entre dos proyectos y la denegación de acceso desde una cuenta ajena. El productor invitó al equipo y vinculó la batería a los tres temas desde la interfaz. La ingeniera la completó una vez y el tablero actualizó las tres tarjetas. En cada revisión, artista e ingeniera comprobaron el responsable en el tablero y la presencia o ausencia de la mezcla en Mis tareas. Se usó la API autenticada para preparar las dependencias y crear y modificar las reservas. Un borrador de tarea abierto durante una edición concurrente conserva el texto escrito, bloquea el guardado y ofrece cargar los valores actuales. El navegador verificó que se recupera la fecha comprometida por la otra sesión. Se ejecutó sobre la restauración local.

## Migración y recuperación

El manifiesto de migraciones añade `2026-11-18_production_workspace`. La migración conserva los registros existentes. Para revertir la interfaz, usar una imagen web anterior. Para el servicio, usar el procedimiento de recuperación de Hetzner y conservar las tablas y los archivos históricos. El disparador de retención sigue protegiendo un audio ya vinculado al volver a una imagen anterior. La comprobación del manifiesto y del esquema debe ejecutarse sobre una restauración aislada antes de liberar el backend.

## Revalidación del 19 de noviembre

La continuación integra la principal protegida `6ccd1e55af264fd5a6a83b82ec732bf8a5f66239` en la PR #784. Conserva las 259 migraciones y los contratos de ambas ramas. Las 17 pruebas del catálogo comprueban también las retiradas históricas de la principal y su nueva clasificación técnica, sin reactivar las listas retiradas.

Se repitieron las cinco exploraciones TLC 1.7.2 (1.087/20/20/16/6 estados distintos) y sus cinco controles negativos, y el ensayo PostgreSQL 17 del EP, archivos exactos, permisos y tres pares de escritores solapados. El modelo sigue siendo acotado; las pruebas de persistencia constituyen evidencia de la implementación, no una prueba general de refinamiento. Los clientes generados coinciden con la revisión móvil fijada. Los workflows ejecutan ahora explícitamente los modelos, las carreras PostgreSQL y el EP de navegador; estos nuevos pasos necesitan su propia ejecución hospedada.

La inspección de solo lectura de Hetzner fue rechazada en `container-api: compose-files`, sin crear recibo. La principal ya contiene el ejecutor protegido de liberación, pero su existencia no satisface identidad, recuperación ni custodia. La admisión serializada observada estaba concedida a #771; #784 continúa esperando su turno. No se ha desplegado esta continuación ni se presenta la vista previa como un flujo operativo en producción.

La principal avanzó después a `432d5741ca68ff77ee52863e4100c822e92646cc` (#771/#772). La continuación conserva sus cambios de capas y utiliza `auth/authEvents` para la revisión de audio. Antes de esa segunda integración, el binario compilado de la primera integración y el navegador pasaron de nuevo el recorrido completo: audio v3/v4/v3 histórica, cuatro cuentas, batería compartida, responsables en ambas cuentas, borrador concurrente, reserva modificada, ajuste de masterización y carga entre proyectos. No cambió ningún archivo de backend, Cabal ni resolver entre esas dos integraciones. El descriptor del binario coincide con las 957 operaciones y dos montajes Raw del snapshot actual; los huecos documentales heredados siguen abiertos. Las dos suites web dirigidas (tres pruebas) y el comprobador de capas pasaron con la segunda integración. La reproducción final de navegador y las comprobaciones hospedadas se informan separadamente de esa evidencia.

El inspector rechazado no inició mantenimiento ni escribió en producción. Una consulta posterior de nombres confirmó ausentes `/opt/tdf/release-config` y `/opt/tdf/release-tools`. El propietario confirmó que existen almacenamiento y clave de recuperación independientes; se recibió su destinatario público age y faltan los datos de Storage Box y la configuración y aceptación operativas, sin trasladar la identidad age privada al servidor. La API pública observada sigue en `d4bb96030b1182e165ff39b614315c6a2554aee7`, sana y con `/productions` en HTTP 404. No hay un commit desplegado de este módulo.

### Carrera de la siguiente acción en la interfaz

Una repetición del EP detectó que el botón de una segunda invitación podía seguir activo mientras la primera operación esperaba su proyección nueva; `run` descartaba la segunda pulsación al conservar `saving=true`. `ProductionWorkspacePage` mantiene ahora deshabilitados los controles nativos de la producción durante escritura y actualización, y muestra «Guardando cambios…». `ProductionWorkspacePage.test.tsx` retiene ambas promesas, comprueba la imposibilidad de una segunda acción en cada espera y verifica que la invitación siguiente utiliza la revisión nueva. La prueba falla en el componente anterior por permanecer habilitado el botón y pasa con la corrección. Pasaron las tres suites dirigidas, cuatro pruebas. El EP completo volvió a pasar después de este arreglo, con reproducción real v3/v4/v3, las cuatro cuentas, la batería compartida, ambos responsables, historia, edición concurrente, reserva y ajuste de fechas y privacidad entre proyectos. La compilación web con esta corrección pasó también: cinco preloads, 394.849 bytes gzip de JavaScript inicial.

La integración posterior conserva también la principal `d0d2ae343ba5564b3f922fd39d99025dd262e55b` (#778). Las filas del contrato y del catálogo se reconciliaron por objetos completos de los tres árboles; cualquier edición independiente incompatible habría rechazado la reconciliación. Las 17 pruebas de preservación pasaron. La compilación conjunta pasó (cinco preloads, 394.845 bytes gzip de JavaScript inicial). Los archivos de backend, sesión/autorización web y del módulo de producción permanecen idénticos al flujo corregido del EP ya probado; los cambios entrantes pertenecen a las alternativas de pago. Se mantiene separado su alcance de la ejecución de navegador del módulo.

### Aislamiento del borrador al cambiar de proyecto

La revisión posterior encontró que los formularios sin clave podían conservar campos del proyecto anterior mientras sus callbacks ya apuntaban al proyecto seleccionado. El área específica del proyecto se monta ahora con `Fragment key={p.id}`; incluye invitaciones, tareas, audio y cronograma, sin reiniciar borradores por un mero cambio de revisión en el mismo proyecto. La regresión prepara una invitación y una tarea en A, selecciona B y exige campos nuevos y la fecha de B antes de permitir una invitación de B. Falla con el componente anterior por conservar el usuario de A y pasa con la corrección. Las tres suites dirigidas pasan con cinco pruebas. Esta evidencia de interfaz complementa las pruebas de autorización de la API; no amplía por sí sola el alcance del modelo TLA+.

El EP completo se repitió también con el aislamiento del proyecto y pasó. La compilación y el lint pasaron; cinco preloads, 394.843 bytes gzip iniciales. La preparación operativa avanzó después de la identificación del Storage Box en la cuenta autenticada: el propietario creó la subcuenta e instaló la clave pública dedicada mediante su Terminal. Dos conexiones SFTP distintas verificaron la carga y recuperación exacta de un archivo de texto sintético cifrado con el destinatario age indicado. Esta comprobación no es una copia de datos de producción ni demuestra todavía custodia de la identidad privada, recuperación con el host perdido o protección de los objetos retenidos frente al escritor. Esos controles y la integración protegida del nuevo commit permanecen separados de la prueba funcional del módulo.
