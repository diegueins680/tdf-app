# Tu escena, conectada — monitoreo Quito del 2026-08-27

Fecha de verificación: 2026-08-27 a las 13:42, zona horaria America/Guayaquil.

## Estado operativo

Se auditó de forma no mutante el bloque de cinco mensajes enviado el 2026-08-26. No se envió una
nueva invitación, seguimiento, respuesta, publicación, correo, WhatsApp ni anuncio en esta
continuación.

Los identificadores exactos de Instagram, nombres del CRM y valores individuales de `utm_content`
permanecen únicamente en los sistemas privados de operación; no se replican en este repositorio.

## Respuestas e interacción

- Dos de los cinco destinatarios tienen ahora una reacción positiva al mensaje de campaña.
- Un tercer destinatario comenzó a seguir a TDF después del envío, pero no escribió un mensaje.
- Los otros dos hilos permanecen sin respuesta ni señal adicional.
- Ninguno de los cinco envió una respuesta textual, una baja o una solicitud gestionable.
- General y From Ads no mostraron una respuesta nueva de esta cohorte.
- Requests quedó en cero; Hidden Requests conservó una solicitud antigua y ajena a la campaña.

Las dos reacciones y el seguimiento se registran como interacción. No equivalen a consentimiento
para reclutamiento, activación o seguimiento, y no se cuentan como conversión.

## Atribución y CRM

La búsqueda exacta de los cinco contactos devolvió una sola coincidencia por destinatario. Ninguna
de las cinco fichas muestra una cuenta de usuario creada; todas siguen requiriendo completar el
correo antes de poder crearla desde el CRM.

No existe evidencia nueva de registro, perfil completo, publicación, hallazgo en búsqueda pública o
conversión atribuible a `tu_escena_conectada_piloto_quito`.

## Salud y concurrencia

`Removed content and messaging issues`, `Availability to people under 18` y
`Features you can't use` conservaron sus indicadores correctos.

Durante la revisión apareció una respuesta de TDF a una consulta de estudio ajena a la campaña sin
que esta continuación hubiera escrito o enviado ese texto. Eso prueba que otra sesión u operador
estaba usando la misma cuenta. Se detuvieron las mutaciones de Instagram para evitar respuestas o
contactos duplicados; la acción concurrente no se atribuye a esta campaña.

## Otras campañas

- Las cuatro automatizaciones de WhatsApp continúan en `Sin crear`; no se creó borrador, audiencia ni
  activación.
- La campaña de testers internos móviles conserva el estado local `draft`; no se envió prueba ni
  broadcast.
- No se abrió el flujo de pago ni se modificó pauta de Meta en esta revisión.

## Próxima acción segura

Gestionar únicamente una respuesta textual significativa o un alta atribuible. No enviar
seguimiento por una reacción o un follow, ni ampliar el volumen frío para compensar la ausencia de
activación.

Antes de reclutar la cohorte de activación, completar el ensayo aislado con dos personas adultas y
los controles definidos en `docs/activation-pilot/quito-creator-activation-2026-08-27.md`. Cualquier
nueva operación en Instagram debe empezar verificando que no existe otra sesión activa sobre la
cuenta.

## Continuación técnica — 13:46 a 13:49

Una nueva revisión de Primary no encontró respuestas textuales de la cohorte. No se envió ni editó
ningún mensaje.

La verificación del entorno exacto de activación encontró que el resultado público del perfil de
referencia todavía usa la imagen de respaldo. El API de producción devolvió `imageUrl: null`, y
`/version` confirmó que el backend continúa en el commit anterior a la reconciliación de medios.
Por tanto, la compuerta de imagen de la Fase B sigue abierta aunque el cambio ya esté fusionado.

El plan local de la versión actual de `origin/main` pasó como dry-run e incluye la migración
registrada `2026-08-26_music_directory_profile_media_reconciliation`. El preflight remoto, también
sin mutaciones, encontró dos bloqueos:

- `EVENT_DISCOVERY_ENABLED` está en `true` en ambas Machines y el carril exige suspenderlo durante
  el despliegue;
- la imagen inmutable del commit actual no existe en Docker Hub porque el workflow de Build Image
  fue cancelado antes de ejecutar el job de Docker.

No se reejecutó el workflow, no se cambió discovery, no se tomó snapshot, no se aplicó la migración
y no se desplegó backend. Reejecutar el build de imagen y realizar después el rollout/migración son
acciones externas separadas que requieren confirmación explícita del operador.

## Continuación técnica — imagen inmutable publicada

Con autorización explícita del operador, se reejecutó únicamente el intento 2 del workflow
[Build Image 33044669050](https://github.com/diegueins680/tdf-app/actions/runs/33044669050)
para el commit exacto `0d5126a34a03726f9ac452f0aa2f58be1fb18be6`. Pasaron las compuertas de
repositorio, UI, persona web, backend, contratos y migraciones, incluido el ensayo idempotente de la
migración y la verificación automática contra el esquema de producción. El job de Docker terminó
correctamente y publicó la etiqueta inmutable correspondiente.

La inspección de solo lectura del registro confirmó:

- índice OCI `sha256:122a600772bd0d99a23adc12abb674eb468f04f746226e85554f77d300f2a26e`;
- manifiesto de imagen `linux/amd64`
  `sha256:4d37fd60dba5611e4d92d94f8993643b8f9d46b65b5568f5d7a4397dd322e28e`;
- `SOURCE_COMMIT` y `GIT_SHA` iguales al commit autorizado;
- `RUN_MIGRATIONS=false` y `AUTO_APPLY_PRODUCTION_MIGRATIONS=true`.

La autorización terminó en la publicación y validación de la imagen. No se cambió discovery, no se
tomó snapshot, no se aplicó ninguna migración, no se actualizó ninguna Machine y no se desplegó a
producción. El rollout protegido continúa siendo una compuerta separada que requiere autorización
explícita: suspender discovery, tomar y verificar el snapshot, ejecutar migración/canary/rolling,
restaurar discovery y comprobar la versión e imagen públicas.
