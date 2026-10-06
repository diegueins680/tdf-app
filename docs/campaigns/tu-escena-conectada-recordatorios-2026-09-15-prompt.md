# Tu Escena Conectada — recordatorios autorizados para el 15 de septiembre

> **Plantilla archivada, no reejecutable.** La regla de copiar enlaces individuales antiguos queda sustituida para cualquier material futuro por dominio canónico y UTM sin datos personales. Ver [checkpoint del 18](tu-escena-conectada-continuacion-2026-09-18.md); no repetir el lote completado.

**COMPLETADO EL 14 DE SEPTIEMBRE, 19:17–19:24.** Los cinco recordatorios ya fueron enviados y verificados. No ejecutes este bloque ni envíes mensajes adicionales. Las instrucciones siguientes se conservan únicamente como historial de la programación original; el informe registra el resultado individual.

Ejecuta un solo bloque de recordatorios a invitados anteriores. Diego autorizó expresamente notificar a los usuarios por los canales disponibles y, el 14 de septiembre, aclaró: «Remind earlier campaign invitees». No se trata de buscar destinatarios nuevos. El bloque se difirió al alcanzar el límite anterior de diez mensajes. Después Diego elevó el máximo diario a 20; esta instrucción sustituye cualquier límite histórico de diez en los informes consultados. La programación de este bloque sigue siendo el 15 de septiembre a las 09:00.

Trabaja en `/Users/diegosaa/GitHub/tdf-app`. Cumple `AGENTS.md` y lee `docs/campaigns/tu-escena-conectada-seguidores-2026-09-11.md`, `docs/campaigns/tu-escena-conectada-seguidores-2026-09-14.md` y `docs/campaigns/tu-escena-conectada-recordatorios-2026-09-15.md`. La autorización del usuario para estos recordatorios sustituye la instrucción histórica de no hacer seguimiento por silencio; conserva cualquier baja u objeción real del destinatario.

## Destinatarios exactos

| Instagram | Nombre | CRM partyId |
| --- | --- | --- |
| comounmadrigal | Madrigal | 235 |
| da_pawn | Da Pawn | 233 |
| phia.dj.music | PHIA dj | 234 |
| yosoysheen | Sheen | 236 |
| miguelgallardokeys | Miguel Gallardo | 238 |

En la última lectura del 14 de septiembre cada uno tenía una ficha única, `hasUserAccount=false`, y ningún correo, teléfono o WhatsApp. Revalida antes de enviar. Si algún destinatario no es elegible, omítelo; no amplíes esta lista. Excluye de este bloque a los cinco recordados el 14: `t.duck_prod`, `diegocarvajalmusica`, `lilith.tarantino`, `dannytagle`, `_scarboy._`.

## Ejecución

1. Ejecuta solamente el 15 de septiembre de 2026, entre 09:00 y 20:00 de `America/Guayaquil`. Revisa el reloj antes de cada envío. Si el trabajo se despierta en otra fecha, detén y documenta; no recuperes envíos vencidos automáticamente.
2. Consulta los informes del día para descontar mensajes ya enviados. Máximo cinco destinatarios en este bloque y **20 mensajes de campaña por día de America/Guayaquil**, contando invitaciones y recordatorios de todos los canales. Este trabajo no es recurrente. Si se reintenta después de un fallo, conserva los resultados ya confirmados y no repitas envíos.
3. Verifica la sesión autenticada `tdf.records.label` mediante el perfil de la cuenta, no solo por la existencia de cookies. Usa la sesión administrada existente de OpenClaw, CDP local 18800. No introduzcas credenciales, cambies cuentas ni cierres perfiles persistentes. Si hay otro operador activo en la misma sesión, detente. No uses una sesión distinta para sortear restricciones.
4. Consulta el CRM completo mediante el API autenticado; la pantalla de contactos solo muestra 200. Confirma exactamente una ficha por handle y revisa coincidencias por nombre para detectar registros separados. No asumas que `hasUserAccount=false` excluye un registro duplicado separado. Si existe cuenta, perfil ya creado o identidad ambigua, omite el recordatorio genérico y documenta el caso para asistencia específica.
5. Abre el hilo exacto de cada handle y verifica destinatario, invitación original, respuestas, bajas, indicios de registro completado y cualquier recordatorio anterior. No envíes si hay negativa, baja, recordatorio previo o entrega ambigua. Una reacción positiva no exige otro saludo si ya fue respondida. No confíes en la vista previa del sidebar ni en contar texto que pueda estar citado: inspecciona las burbujas del propio hilo.
6. Envía como máximo un recordatorio por canal verificado, dentro del límite diario global. Instagram es el único canal verificado al preparar este bloque. Si aparecen otros datos de contacto en el CRM, verifica su asociación y disponibilidad antes de usarlos; no inventes direcciones ni busques datos personales fuera de la información de campaña. No envíes desde una cuenta de correo personal no identificada como TDF ni crees nuevas integraciones. Si no puedes validar un canal, documenta que no está disponible.
7. Envía un único mensaje natural en español, con el enlace original del destinatario, oferta de ayuda y baja. Para Da Pawn adapta el saludo al grupo. No promociones los eventos del 10 y 12 de septiembre porque ya terminaron.
8. Comprueba una burbuja enviada completa, compositor vacío y ausencia de errores. Registra inmediatamente hora, handle, canal y resultado. Ante estado ambiguo no reenvíes. Detén el bloque ante CAPTCHA, restricción, advertencia, error persistente o concurrencia; no intentes evadirlos. No pagues por mensajes prioritarios.
9. Actualiza el informe y la memoria del día mediante `apply_patch`. No crees usuarios ni contactos adicionales: estas cinco fichas ya existen. No atribuyas conversiones sin evidencia de registro. Resume enviados, omitidos, canales y bloqueos; distingue programado, enviado y leído.

## Texto

> Hola, [nombre]. Te dejamos el enlace de Tu Escena Conectada para crear o continuar tu perfil de artista: [enlace individual]. Si te quedaste en algún paso, responde aquí y te ayudamos. Si prefieres no recibir más mensajes, dínoslo. — TDF Records

Enlace individual, sustituyendo únicamente `<handle>` por uno de los cinco identificadores de la tabla:

`https://www.tdfrecords.net/login?signup=1&intent=artist&roles=Artista&redirect=%2Fmi-artista&utm_source=instagram&utm_medium=dm&utm_campaign=tu_escena_conectada_piloto&utm_content=artista_prueba_web`

No prometas activación sin aprobación: la verificación del 14 de septiembre encontró producción en `1157258b6a6551d49708fa9eb21ab893b6a051f1`, sin el endpoint nuevo. Este trabajo no autoriza un despliegue ni cambios de seguridad. El texto anterior sirve sin hacer esa promesa; si alguien pregunta por una aprobación pendiente, registra y explica el estado verificado, sin conceder roles manualmente.

## Herramientas y límites

La CLI vigente es `/Users/diegosaa/.local/bin/openclaw`. Para acciones de navegador, usa el targetId completo: los alias `tN` pueden fallar. Los helpers de lectura usados en la preparación están en `/private/tmp/tdf-campaign-continuation-audit.mjs` y `/private/tmp/tdf-campaign-replies-audit.mjs`; lee el contenido antes de reutilizarlos porque el segundo consulta los destinatarios del 14, no esta lista. No ejecutes `/private/tmp/tdf-instagram-send-reminders.mjs`: corresponde al grupo anterior y contiene una promesa de producto no desplegado.
