# Prueba cerrada de TDF Mobile en Android

La campaña usa el correo de TDF Records y consulta los contactos del CRM por
ID. La vista previa incluye destinatarios, remitente, asuntos, textos, plazos y
una huella SHA-256. Preparar la vista previa solo lee el CRM. El registro
aprobado y el temporizador habilitado permiten ejecutar los envíos.

## Preparación y aprobación

1. Revisa el conteo del canal Alpha en Play Console y comprueba que el enlace
   directo de incorporación abre con una cuenta habilitada. La renovación web
   de la [PR #761](https://github.com/diegueins680/tdf-app/pull/761) se gestiona
   por separado.
2. Prepara un CSV privado con cabecera `party_id,email,permission_ref`. Incluye
   solo contactos aptos para recibir la invitación, con una referencia a su
   autorización de contacto. Excluye integrantes actuales, bajas y personas ya
   invitadas recientemente. La API de contactos no expone el consentimiento
   por correo. Verifica estos datos contra el registro disponible antes de
   generar la vista previa. Si falta el historial anterior, revisa los hilos
   del buzón o usa un segmento cuya elegibilidad puedas acreditar.
3. Con `TDF_CRM_TOKEN` en el entorno, genera la vista previa. El archivo
   contiene datos personales y se comparte únicamente con quien aprueba.

   ```sh
   python3 scripts/android-beta-email.py --state /ruta/privada/campaign.sqlite \
     preview /ruta/privada/cohorte.csv /ruta/privada/vista-previa.json \
     --sender info@tdfrecords.net
   ```

4. Presenta la vista previa y su huella para aprobación. Usa exactamente el
   archivo aprobado. Ejecuta `activate` y `console-count` solo después de
   recibir aprobación de la audiencia y de la secuencia.

   ```sh
   python3 scripts/android-beta-email.py --state /ruta/privada/campaign.sqlite \
     activate /ruta/privada/vista-previa.json --approved-sha256 HUELLA_APROBADA
   python3 scripts/android-beta-email.py --state /ruta/privada/campaign.sqlite \
     console-count CONTEO_VERIFICADO
   ```

## Secuencia

| Paso | Disparador |
| --- | --- |
| Invitación | Registro aprobado, contacto vigente en el CRM y conteo inicial de Play Console |
| Recordatorio | Un solo correo después de 72 horas si aún no llegó respuesta |
| Acceso habilitado | El operador registra que la cuenta de Google ya está autorizada en Alpha |
| Seguimiento | Un correo a partir del día 7 de la incorporación verificada y antes del día 14 |

El proceso lee las respuestas en el hilo mediante IMAP. Una respuesta sin
clasificación automática suspende el recordatorio y queda disponible para
revisión en el buzón. Una dirección de Google Play en la primera línea registra
una solicitud inequívoca. La frase «Ya me uní» se guarda como declaración
pendiente de verificación. Las bajas, las devoluciones detectadas y los rechazos
de SMTP detienen la secuencia del contacto. Los intentos con resultado incierto
quedan registrados para revisión sin reintento automático. La aceptación SMTP
se registra por separado de las respuestas y del estado de Google Play.

El límite es de cinco intentos en 24 horas. La captación se detiene si el
conteo registrado de Play Console llega a 12 o el lote alcanza su meta de
incorporaciones verificadas. La meta es siete y se amplía si el conteo
registrado cae por debajo de cinco. Actualiza el conteo durante la captación y
antes de decidir si se cumplieron los 14 días consecutivos.

## Operación

Las siguientes acciones son subcomandos del script y requieren el mismo
`--state`. Sustituye `ID` por el ID del contacto en el CRM.

| Acción | Comando |
| --- | --- |
| Consultar estados e intentos | `status` |
| Registrar una solicitud revisada en el buzón | `record ID requested --google-email CUENTA_GOOGLE` |
| Registrar la habilitación en Play Console | `record ID access` |
| Registrar incorporación con evidencia del estado de la cuenta en Google Play | `record ID verified --verified-at FECHA_ISO_CON_ZONA` |
| Registrar instalación confirmada | `record ID installed` |
| Detener correos para un contacto | `record ID stop` |
| Registrar retiro del programa | `record ID withdrawn` |
| Actualizar total del canal | `console-count TOTAL` |
| Pausar o reanudar el lote aprobado | `pause` o `resume` |

Para la fecha de incorporación, usa la fecha acreditada por la evidencia. Si
solo puedes verificar que ya figura inscrito en este momento, usa la fecha
actual y apóyate en Play Console para el período oficial. Conserva evidencia,
credenciales y direcciones de Google en archivos privados.

## Temporizador de producción

El servicio usa Python 3 y un usuario de sistema dedicado
`tdf-android-beta-email`. Instala el script en
`/opt/tdf/production/android-beta-email.py` y los dos archivos systemd como
unidades del sistema. Usa `/var/lib/tdf-android-beta-email/campaign.sqlite`
como registro privado y ejecuta las operaciones sobre él como ese usuario.
Conserva el mismo registro para cualquier reanudación y evita ejecutar
réplicas con registros distintos.

Configura `/etc/tdf/android-beta-email.env` con permisos `0600` de root y
los valores `TDF_CRM_TOKEN`, `TDF_CAMPAIGN_MAIL_USER` y
`TDF_CAMPAIGN_MAIL_PASSWORD`. El usuario del buzón debe coincidir con el
remitente aprobado. Los hosts opcionales `TDF_CAMPAIGN_SMTP_HOST` y
`TDF_CAMPAIGN_IMAP_HOST` usan `mail.webador.com` por defecto. IMAP lee
`INBOX` salvo que se configure `TDF_CAMPAIGN_IMAP_FOLDER`.

Tras la aprobación y el registro del conteo inicial, habilita
`tdf-android-beta-email.timer` con `systemctl enable --now`. El servicio
revisa la campaña cada 15 minutos. Consulta `systemctl status` y
`journalctl -u tdf-android-beta-email.service` para revisar las ejecuciones.
El diario muestra conteos de intentos y errores sin listar direcciones.
