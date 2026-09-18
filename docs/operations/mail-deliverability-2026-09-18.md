# Entregabilidad de tdfrecords.net — 18 de septiembre de 2026

Estado: diagnóstico y correcciones verificables, **no resolución completa del spam**.
Los originales, destinatarios, rebotes y configuración privada se conservan fuera de Git.

## Infraestructura comprobada

- DNS administrado en Webador; autoritativos Openprovider (`ns1.openprovider.nl`,
  `ns2.openprovider.be`, `ns3.openprovider.eu`). El sitio usa Cloudflare Pages;
  eso no convierte Cloudflare en el administrador de la zona de correo.
- Buzón y submission: Webador, `mail.webador.com:587`, STARTTLS/TLS 1.3 y
  autenticación comprobados. Lectura del buzón por IMAP TLS 993 comprobada.
- TDF App en Fly y los scripts de invitaciones usan `info@tdfrecords.net` tanto
  en From como en MAIL FROM. Los mensajes observados salen por MailChannels.
- DKIM real: `d=tdfrecords.net; s=jouwweb`, RSA 2048; validado por Gmail.
  `jouwweb._domainkey` apunta a `website-rendering._domainkey.jouwweb.nl`.
- También existen selectores `sparkpost` y `mandrill` (claves RSA 1024). Son
  indicios de configuración, **no prueba de envíos activos**. Se preservaron.
- No se encontraron integraciones adicionales de envío en el código/configuración
  examinados. No se puede excluir otro cliente personal, subdominio o servicio
  sin sus registros o informes agregados.

| Ruta | Finalidad | Remitente/sobre/firma | Volumen observado | IP de salida observada |
|---|---|---|---|---|
| Cliente SMTP Webador | Personal/comercial | info / info / tdfrecords.net | Volumen global pendiente | 23.83.213.25 en prueba |
| TDF.Email | Acceso, alta, recuperación, cursos, pedidos, reservas, tickets, transferencias, espera y reembolsos | info / info / tdfrecords.net | Sin contador global de entrega | 23.83.221.25 antes; 23.83.218.254 después |
| Script de invitaciones Android | Invitación voluntaria | info / info / tdfrecords.net | 82 intentos: 80 aceptados inicialmente, 2 rechazados | 23.83.222.70 en muestra |
| Broadcast administrativo | Campaña a usuarios registrados | Mismo SMTP | Campaña del 8/9 identificada en rebotes | Variable/compartida |
| Webador/formularios, SparkPost/Mandrill | Pendiente de confirmar actividad | Pendiente | Pendiente | Pendiente |

## Evidencia y causas

1. SPF, DKIM y alineación DMARC pasan en seis mensajes históricos de Gmail y
   en las tres pruebas nuevas. Todos estaban en INBOX/CATEGORY_PERSONAL al leerlos.
   No se obtuvo todavía un original realmente situado en Spam del receptor.
2. La aplicación no añadía `Date` ni `Message-ID` antes de entregar DATA al
   relay. Bibliotecas y mensajes originales confirman que los añadían intermediarios:
   la firma anterior no cubría esos campos. Es una deficiencia comprobada de
   formato/trazabilidad; no demuestra por sí sola la causa de cada clasificación.
3. Cinco rebotes permanentes de la invitación del 18/9 se correlacionaron con
   Message-ID y destinatario directo: inexistencia de buzón/dominio o relay denied.
   Algunos destinos ya fallaron en agosto o el 8/9. La lista incluía cuentas de
   prueba y destinos erróneos. No se deben reintentar hasta corregir/verificar la dirección.
4. Otro rebote correlacionado fue `4.2.2` por buzón lleno. No se convierte en
   exclusión permanente. Un fallo de reenvío hacia `connect.tdf.com` se investiga
   aparte; no justifica excluir automáticamente el buzón original.
5. El broadcast toma cuentas registradas, sin un consentimiento promocional,
   mecanismo de baja ni exclusiones persistentes de rebotes. Registro en TDF no
   prueba consentimiento de marketing. Webador excluye boletines de su servicio.
6. No hay evidencia suficiente para atribuir el problema a una IP compartida,
   bloqueo general del dominio, abuso o reputación de enlaces. Los proveedores
   receptores empresariales y las quejas permanecen pendientes de acceso/evidencia.
   Una consulta a Spamhaus devolvió `127.255.255.254`: es un error de acceso del
   resolvedor, no un listado positivo. No se solicita retirada sin prueba válida.

## DNS y reversión

| Nombre | Tipo | TTL | Antes | Cambio |
|---|---|---:|---|---|
| @ | TXT | 3600 | `v=spf1 include:_spf.webador.com ~all` | Conservado |
| _dmarc | TXT | 3600 | `v=DMARC1; p=none` | `v=DMARC1; p=none; rua=mailto:info@tdfrecords.net` |
| @ | MX | 3600 | `0 mail.webador.com` | Conservado |

SPF tiene un solo registro. Cadena: dominio → `_spf.webador.com` →
`_s00040434.autospf.email` → mecanismos `ip4`; **2 mecanismos de consulta** en la
evaluación actual, sin `a`, `mx`, `exists` ni `redirect` adicionales. No se aplana
ni se eliminan redes administradas por el proveedor. Las IP observadas están en
`23.83.208.0/20`. PTR y resolución directa de la primera IP observada coinciden.

La interfaz Webador guardó únicamente el cambio de `rua`; comparación de todos
los controles: un valor distinto, ningún otro registro modificado. El destinatario
de informes es un buzón bajo control verificado por IMAP; no requiere autorización
DMARC de dominio externo. Se conserva `p=none` y la alineación relajada existente.
No se publican `ruf` ni políticas estrictas sin conocer todos los emisores.

La propagación requiere observación: Google DNS ya devolvió el nuevo valor mientras
los autoritativos consultados desde este entorno aún devolvían el anterior.
No se afirma convergencia global ni recepción de informes: la primera lectura dio cero.

Reversión DNS: editar solo `_dmarc` al valor anterior con TTL 3600 y verificar los
tres autoritativos y resolutores. Conservar todos los MX, DKIM, A/CNAME y buzones.

## Cambio de aplicación

- Generar un UUID por intento y fecha UTC antes de SMTP; el mismo Message-ID queda
  en la firma DKIM y en los registros. Un solo Date y Message-ID por mensaje.
- El registro común distingue `smtp_attempt`, `smtp_accepted` y `smtp_error delivery=unknown`;
  no escribe destinatario, asunto, contenido ni texto arbitrario de excepción.
  El resto de registros históricos de otros handlers no se ha rediseñado.
  Una excepción después de DATA puede dejar la entrega incierta; no se reintenta
  automáticamente ni se interpreta como rechazo confirmado.
- `SMTP_UNDELIVERABLE_RECIPIENTS` es una lista revisada de fallos de entrega, con
  normalización/validación. Rechaza antes de conectar a SMTP y no comunica un éxito
  falso al llamador. No almacena bajas promocionales ni cambia cuentas de usuarios.
- El endpoint de broadcast rechaza envíos reales con el SMTP de Webador; conserva
  vista previa. Los flujos transaccionales individuales permanecen disponibles.
- Sin migración de base de datos ni cambios de proveedor. Las exclusiones privadas
  se instalarán como secreto en el despliegue revisado; no están activas solo por
  abrir este PR. La ingesta automática de rebotes no modifica esta lista.

Reversión de código: revertir el PR por el flujo protegido, generar una nueva imagen
del main compatible y usar el despliegue protegido. No volver a una imagen antigua
que deshaga otras correcciones de identidad/migraciones. Guardar el valor previo del
secreto; eliminar una exclusión solo con evidencia de corrección del destinatario.

## Pruebas reales (cuenta Gmail del titular)

| Prueba | UTC | SMTP | SPF/DKIM/DMARC | Carpeta | Relay |
|---|---|---|---|---|---|
| Cliente SMTP, texto | 16:07:51 | Aceptado | PASS/PASS/PASS alineados | Principal | Good |
| Plantilla actual de TDF | 16:36:05 | Aceptado | PASS/PASS/PASS alineados | Principal | Junk |
| Plantilla corregida de TDF | 16:41:14 | Aceptado | PASS/PASS/PASS alineados | Principal | Neutral |
| Recuperación HTTP de producción | 16:21:56 | No comprobado; HTTP 200 no lo demuestra | Sin mensaje nuevo observado | No comprobada | — |

Las dos plantillas se ejecutaron desde el código real `TDF.Email.sendTestEmail` en
local con el SMTP/configuración de producción. **No equivalen a un despliegue en Fly**.
La corrección añade Date/Message-ID a `h=` de DKIM; Gmail conserva el UUID de TDF.
La variación Junk → Neutral es una observación puntual, no una medición causal de
reputación. No se cambiaron filtros ni remitentes seguros. No se pudo auditar si
el titular tenía una regla previa; el histórico de relación condiciona la muestra.
Outlook/Hotmail y un buzón empresarial: pendientes de cuentas autorizadas y acceso
a la carpeta/encabezados. Tampoco se dispararon compras, reembolsos o campañas reales
para simular esas plantillas.

Validación local: seis ejemplos Hspec, incluida una propiedad QuickCheck de cien
casos y el rechazo de un destinatario excluido antes de SMTP; compilación de Email
con Stack/GHC 9.10.3; cinco pruebas del monitor; auditoría formal sin errores.
Los checks hospedados y la revisión independiente del PR son requisitos antes de
fusionar/desplegar. La rama protegida exige una aprobación; no se usa bypass.

## Observación y próximos hitos

`scripts/mail-deliverability-monitor.py --output <directorio-privado>` lee DNS,
rebotes e informes agregados del buzón existente, sin enviar mensajes. Limita tamaño
descomprimido, no acepta entidades XML, usa BODY.PEEK y conserva métricas sin contenido
ni destinatarios. La ventana es de 14 días, INBOX/Junk, máximo 200 mensajes por carpeta;
declara truncamiento y fallos de acceso. No proporciona quejas ni reputación Postmaster.

Se instaló `net.tdfrecords.mail-deliverability` como LaunchAgent local diario a las
09:30 (America/Guayaquil), con copia estable del monitor y resultados privados en
`tmp/mail-deliverability/`. Requiere que el Mac/sesión y el acceso Fly estén disponibles;
no es vigilancia alojada 24/7 ni envía alertas externas. Verificar `reports/latest.json`,
`monitor.log` y `monitor-error.log`. Desactivar de forma reversible con
`launchctl bootout gui/$(id -u)/net.tdfrecords.mail-deliverability`; conservar evidencias.

- 19/9/2026: convergencia DNS y primeros informes; si autoritativos siguen antiguos,
  escalar a Webador con la diferencia guardado/publicado y exportación de registros.
- 20/9: comprobar recepción efectiva de XML en el buzón; ausencia no implica éxito.
- 25/9: revisar emisores, alineación y rebotes; solicitar acceso a Google Postmaster
  y datos de MailChannels/SNDS mediante Webador. Bajo volumen puede ocultar métricas.
- 2/10: comparar al menos dos semanas; endurecer DMARC solo con inventario completo,
  emisores legítimos alineados y reenvíos revisados. No hay fecha automática de endurecimiento.
- Antes de reactivar campañas: verificar consentimiento, bajas, exclusiones y adecuación
  del servicio. No contratar ni migrar sin propuesta de costo/impacto/reversión aprobada.

## Fuentes oficiales consultadas

- [Gmail: requisitos](https://support.google.com/mail/answer/81126?hl=en): todos los
  remitentes a Gmail personal necesitan autenticación y TLS; los masivos (aproximadamente
  5.000/día) requieren SPF, DKIM, DMARC y, para suscripciones/marketing, baja de un clic.
  Evitar tasa de spam de 0,3%; objetivo inferior a 0,1%. No se acreditó ese volumen en TDF.
- [Microsoft Postmaster](https://substrate.office.com/ip-domain-management-snds/postmaster)
  y [diagnóstico](https://substrate.office.com/ip-domain-management-snds/Postmaster/Troubleshooting):
  Outlook.com/Hotmail aplica requisitos de autenticación adicionales a dominios con
  más de 5.000/día. Un tenant empresarial tiene políticas propias; no se extrapola
  la clasificación de Gmail ni Outlook.com a Microsoft 365 empresarial.
- [SPF RFC 7208](https://www.rfc-editor.org/rfc/rfc7208.html) y
  [formato RFC 5322](https://www.rfc-editor.org/rfc/rfc5322.html).
- [Webador: SMTP/IMAP](https://help.webador.com/hc/en-us/articles/29426718005777-Configure-a-mailbox-in-your-email-client),
  [boletines](https://help.webador.com/hc/en-us/articles/33369910060177-Can-I-send-newsletters-with-Webador),
  [DMARC](https://help.webador.com/hc/en-us/articles/29426760985105-Can-I-add-a-DMARC-record),
  [spam](https://help.webador.com/hc/en-us/articles/29441862912017-Why-are-my-emails-not-sending-or-landing-in-spam).
- [Spamhaus: códigos de respuesta](https://docs.spamhaus.com/datasets/docs/source/70-access-methods/data-query-service/040-dqs-queries.html).
