# Campañas automáticas de adquisición

La aplicación incluye cuatro secuencias de WhatsApp orientadas a generar ingresos sin activar envíos por el solo hecho de desplegar el código:

| Clave | Campaña | Objetivo | Destino |
| --- | --- | --- | --- |
| `music-services` | TDF · Single listo para lanzar | Vender mezcla y mastering a artistas con material grabado | `/reservar` |
| `domo-bookings` | Domo del Pululahua · Reservas piloto | Conseguir visitas y cotizaciones para usos de baja complejidad | `/domo-del-pululahua` |
| `managed-operations` | TDF Ops · Implementación para estudios y venues | Vender diagnóstico e implementación operativa gestionada | `/tdf` |
| `marketplace-validation` | TDF Marketplace · Validación de compradores y artistas | Validar demanda antes de ampliar producto o invertir en pauta | `/marketplace` |

Cada campaña tiene tres mensajes, parámetros UTM propios, tope diario y seguimiento por contacto. El panel está en **Configuración → Campañas automáticas** (`/configuracion/campanas-automaticas`) y requiere permisos de administración.

## Flujo de operación

1. Crear una o todas las campañas. Esta acción solo guarda borradores.
2. Seleccionar contactos del CRM. El backend acepta únicamente contactos con número válido y consentimiento de WhatsApp activo.
3. Abrir la vista previa para comprobar nombre, texto, URL y plantilla del siguiente mensaje.
4. Confirmar que las plantillas están aprobadas en Meta y activar la campaña de forma explícita.
5. Revisar los contactos y marcar conversiones o detener seguimientos cuando corresponda.
6. Pausar la campaña en cualquier momento. Completarla es una acción terminal y detiene todas las inscripciones aún programadas.

El worker revisa mensajes vencidos cada 60 segundos. Un bloqueo asesor de PostgreSQL evita que dos réplicas procesen campañas al mismo tiempo.

## Reglas de seguridad

- Crear, inscribir o previsualizar no envía mensajes.
- Una campaña no puede activarse sin al menos un contacto consentido y una secuencia activa.
- El endpoint de activación exige la confirmación explícita `templatesApproved=true`; no depende únicamente de la casilla del panel.
- El consentimiento se vuelve a validar inmediatamente antes de cada envío.
- Cada intento consume el tope diario de la campaña, incluso si el proveedor devuelve un error.
- Una respuesta entrante posterior al último envío detiene el seguimiento de ese contacto.
- Los comandos `SALIR`, `STOP`, `CANCELAR` y `BAJA` revocan inmediatamente el consentimiento global y detienen todas las secuencias programadas para el número.
- La revocación del consentimiento, una conversión, la detención manual, un error del proveedor o el final de la secuencia impiden mensajes posteriores.
- Un contacto solo puede inscribirse una vez en cada campaña.
- Cada entrega tiene un registro auditable y también se guarda en el historial de WhatsApp con origen `campaign_automation`.
- Si el proceso se interrumpe después de registrar un intento y no puede comprobar su resultado, el contacto se detiene con `delivery_outcome_unknown` para evitar un envío duplicado.
- La campaña del Domo no confirma disponibilidad, permisos, aforo ni una reserva: estos se validan y acuerdan fuera de la automatización.

El panel permite seleccionar contactos que tienen teléfono para facilitar la operación, pero esa selección no sustituye el consentimiento: el backend rechazará individualmente cualquier contacto sin consentimiento activo.

## Plantillas requeridas en Meta

Las siguientes plantillas de WhatsApp Business deben existir y estar aprobadas antes de activar una campaña:

- Música:
  - `tdf_music_services_intro_v1`
  - `tdf_music_services_fit_v1`
  - `tdf_music_services_close_v1`
- Domo:
  - `tdf_domo_bookings_intro_v1`
  - `tdf_domo_bookings_visit_v1`
  - `tdf_domo_bookings_close_v1`
- Operaciones gestionadas:
  - `tdf_managed_ops_intro_v1`
  - `tdf_managed_ops_audit_v1`
  - `tdf_managed_ops_close_v1`
- Marketplace:
  - `tdf_marketplace_validation_intro_v1`
  - `tdf_marketplace_validation_value_v1`
  - `tdf_marketplace_validation_close_v1`

Todas usan el idioma `es` y exactamente dos parámetros en el cuerpo, en este orden:

1. `{{1}}`: nombre visible del contacto.
2. `{{2}}`: URL absoluta de destino con los parámetros UTM de la campaña.

El texto aprobado en Meta debe coincidir con el cuerpo mostrado en la vista previa del panel. Si se cambia el texto, debe crearse una nueva versión de la plantilla y actualizarse el nombre en el código; no se debe reutilizar una plantilla aprobada con otro contenido.

## Migración y despliegue

Producción mantiene `RUN_MIGRATIONS=false`. La migración se debe revisar y ejecutar **antes** de desplegar el binario:

```sh
cd tdf-hq
psql "$DATABASE_URL" -v ON_ERROR_STOP=1 \
  -f sql/2026-07-29_campaign_automation.sql
```

La migración es aditiva y crea:

- `campaign_automation`
- `campaign_automation_step`
- `campaign_enrollment`
- `campaign_delivery`

Lista de verificación del primer lanzamiento:

1. Respaldar la base de datos y aplicar la migración.
2. Desplegar backend y panel administrativo con la configuración vigente de WhatsApp Business.
3. Registrar y conseguir aprobación de las doce plantillas en Meta.
4. Crear las cuatro campañas como borradores.
5. Empezar con una sola campaña, pocos contactos consentidos y un tope diario bajo.
6. Previsualizar cada mensaje y verificar que la URL pública resuelva al entorno correcto.
7. Activar manualmente y comprobar entregas, respuestas, detenciones y conversiones.
8. Aumentar el volumen únicamente después de validar el piloto y la calidad de los contactos.

Ni la migración ni el despliegue crean campañas, inscriben contactos o activan envíos automáticamente.
