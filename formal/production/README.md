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

El verificador valida el SHA-256 del JAR. TLC exploró por separado 1.087 estados distintos del grafo de tres tareas, 20 estados de revisión, 20 del escenario de decisiones, 16 del plan con dos ediciones o confirmaciones como máximo y 6 del permiso de lectura. El escenario de lectura usa dos proyectos y dos versiones. Cada configuración insegura produjo el contraejemplo previsto. La exploración comprueba seguridad en estos límites, sin hipótesis de equidad. Se abstraen la red, los bytes, el sistema de archivos, los relojes reales y el mecanismo de bloqueo de PostgreSQL.

La prueba de persistencia crea un contenedor PostgreSQL 17 sin red y utiliza conexiones independientes que se solapan dentro de la transacción. Comprueba el EP, una única tarea de batería para tres canciones, el aprobador, dos identificadores de archivo, v3/v4, tres carreras, retención del archivo, planificación inversa y privacidad del segundo proyecto. La prueba de calendario de la interfaz separa extremos de sesión contiguos, horarios que se solapan y tareas que comparten un vencimiento. Los criterios de reproducción en navegador y operación en producción se registran por separado en la PR.

## Evidencia del recorrido en navegador

Con el backend compilado, las dependencias de la raíz instaladas, Docker, Chromium y el paquete Python Playwright, ejecutar desde la raíz:

```sh
python3 scripts/test-production-workspace-browser.py --backend /ruta/absoluta/tdf-hq-exe
```

La prueba crea un PostgreSQL desechable a partir del esquema de producción y aplica las 259 migraciones. Arranca el backend real y la aplicación web, crea cuatro sesiones autenticadas y abre el EP desde la navegación web. La artista usa una ventana de 390 píxeles. La ingeniera comparte dos WAV de un segundo. Chromium decodificó y reprodujo v3 (330 Hz, SHA-256 `ea23a0b81765da59b1526c576ddd3bf1e16e8f185eed6f31754b32fdc58246f7`), v4 (550 Hz, `334e25294fa6db746bae4e044ebd12fb60b7247aabb086f6c9a5697f0d2fedf1`) y de nuevo la v3 histórica. Se verificaron las próximas acciones desde ambas cuentas, el rechazo de una decisión obsoleta, el historial, la reserva compartida, la modificación de la reserva antes del ajuste, la carga entre dos proyectos y la denegación de acceso desde una cuenta ajena. El recorrido combina la interfaz con llamadas autenticadas a la API para preparar el equipo, las tareas y las reservas. Se ejecutó sobre la restauración local.

## Migración y recuperación

El manifiesto de migraciones añade `2026-11-18_production_workspace`. La migración conserva los registros existentes. Para revertir la interfaz, usar una imagen web anterior. Para el servicio, usar el procedimiento de recuperación de Hetzner y conservar las tablas y los archivos históricos. El disparador de retención sigue protegiendo un audio ya vinculado al volver a una imagen anterior. La comprobación del manifiesto y del esquema debe ejecutarse sobre una restauración aislada antes de liberar el backend.
