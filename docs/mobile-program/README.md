# TDF Mobile: adquisición y programa de testers

Registro de implementación y distribución. La actualización de analytics siguiente sustituye las observaciones anteriores de PostHog; se conserva el historial con sus horas para trazabilidad.

## Analytics y privacidad — 5 de octubre, actualización posterior a las 17:00 UTC

El propietario autorizó crear la organización TDF Records y el proyecto **TDF Production EU 294698** con `info@tdfrecords.net`. El proyecto existe, sus cinco consultas de dashboard funcionan y hay recepción comprobada de eventos de QA desde una preview. **Esto no acredita analytics en producción:** la clave web continúa pendiente del despliegue de las protecciones y avisos, y los builds firmados iOS32/Android24 no equivalen a distribución externa aprobada. [Informe canónico de configuración, fuentes de artefactos y límites de medición](analytics-2026-10-05.md).

La web añade `/cuenta/eliminar`, una solicitud autenticada y específica de eliminación completa, enlazada desde la página legal que abre la app. La confirmación exige un recibo autenticado del nuevo endpoint de backend, y la cola administrativa filtra antes de paginar. Debe desplegarse ese backend antes de considerar operativo el formulario; una API antigua no acredita recepción. Reutiliza la cola existente; no obliga a enviar un email ni afirma eliminación inmediata. [Procedimiento de verificación de identidad, tramitación manual y confirmación](account-deletion-operations.md). El propietario confirmó que gestionará estas solicitudes en `info@tdfrecords.net`. La publicación pública en App Store sigue bloqueada por el rechazo vigente y la evidencia física pendiente; este cambio no la autoriza ni afirma aprobación de Apple.

## Admisión Android comprobada en Console — 5 de octubre, 12:37 UTC

La consola autenticada confirma **1.0.1 (23), “Available to selected testers”**, publicada el 5 de octubre, en **178 países/regiones**, con publicación gestionada desactivada. Esto amplía la observación API de las 05:27: el release cerrado está disponible, pero no es distribución pública ni acredita una instalación física.

El acceso usa tres listas de correo seleccionadas (68, 1 y 11 entradas); los conteos pueden solaparse y no equivalen a testers inscritos. El dashboard muestra **4 testers con opt-in**. Open Testing está bloqueado hasta obtener acceso a producción; esta cuenta debe mantener **al menos 12 testers inscritos continuamente durante 14 días** antes de solicitarlo. No se alteraron listas ni se cambió a Google Groups, porque eso podría retirar accesos existentes. No se exportaron identidades. El enlace de opt-in, consultado con la sesión existente, muestra “You are a tester”; su enlace de descarga abre la ficha de TDF con opción de instalación y fecha de actualización 5 de octubre. No se alteró esa inscripción ni se solicitó instalación remota.

La landing conserva la solicitud consentida y añade **“Ya tengo acceso: abrir Google Play”**, visible solo con estado, capacidad y vigencia comprobados. Quien ya recibió admisión usa su misma cuenta de Google; los nuevos interesados solicitan acceso. La capacidad disponible significa que el canal admite testers autorizados, no admisión automática para todos. El enlace revalida vigencia al pulsar y su evento mide un clic, no opt-in ni instalación.

Se guardó y envió a revisión el canal de feedback de Play: `https://www.tdfrecords.net/app?feedback=1&utm_source=google_play&utm_medium=closed_testing`. Fue el único cambio pendiente en Publishing overview; no se envió una release de producción. El backend existente recibe solicitudes y feedback; el operador admite solo los correos consentidos en la lista seleccionada, sin reemplazar miembros ni cargar CSV que sobrescriba la lista. La aprobación de una solicitud y la inscripción efectiva en Play son pasos distintos.

En la observación de las 12:37 UTC, PostHog todavía no mostraba una organización accesible. Ese estado histórico quedó sustituido por la creación autorizada y la evidencia descritas en la actualización de analytics anterior; la recepción en producción se verifica por separado.

Fuentes oficiales: [admisión por correo, límites y feedback de Google Play](https://support.google.com/googleplay/android-developer/answer/9845334), [requisitos de producción y Open Testing](https://support.google.com/googleplay/android-developer/answer/14151465). Evidencia agregada: [distribution-2026-10-05.json](distribution-2026-10-05.json). Los apartados siguientes conservan las observaciones históricas, sustituidas por esta actualización donde difieran.

## Actualización verificada — 5 de octubre de 2026

La web de [TDF Mobile](https://www.tdfrecords.net/app) está desplegada desde [PR #479](https://github.com/diegueins680/tdf-app/pull/479), merge `5efd7ff31d55c1eb5b66b2f7710ea4ef87328727`; CI y deployment Cloudflare pasaron. La matriz específica de navegación/accesibilidad pasó 20/20 y la comprobación de producción pasó en Chromium, Firefox y WebKit. Los resultados completos y artefactos están en el informe de ese PR.

**iOS 1.0.1 (31):** firmado desde `TDF-mobile/main` `7a1fca3696dada4263c70716c70b763e492807cc`, que incluye los cambios de feedback y la corrección de categorías de #131. [Pipeline firmado](https://github.com/diegueins680/TDF-mobile/actions/runs/37261008115) y [EAS Submit](https://expo.dev/accounts/cuco.saa/projects/tdf-mobile/submissions/0b8442e5-cc27-44f7-a511-a73afca0035a) finalizaron correctamente. Apple confirmó `VALID`; se verificó el acceso de la cuenta demo existente a la API actual (HTTP 200, sin registrar credenciales), se envió a Beta App Review y Apple devolvió `APPROVED`. Build 31 está asignado al grupo externo del [enlace TestFlight existente](https://testflight.apple.com/join/7k3VE2JJ), junto al 19 vigente. La página pública muestra la invitación, pero no el número de build: la asignación del 31 se comprobó por API. No equivale a una instalación física comprobada.

Las instrucciones de prueba de build 31 están en español e inglés. La privacidad de TestFlight apunta ahora a `https://www.tdfrecords.net/mobile-app/privacy.html`: HTTP 200 y contenido idéntico al dominio antiguo antes del cambio. App Store permanece `REJECTED` con release `MANUAL`; no se envió una nueva versión a App Review ni se publicó en App Store. Google OAuth, Universal/App Links y lectores de pantalla siguen pendientes de prueba física; `devicectl` y `adb` no detectaron dispositivos conectados.

**Android:** la credencial de envío ya vinculada a EAS permitió consultar Play. Tras ejecutar y verificar el [pipeline firmado 23](https://github.com/diegueins680/TDF-mobile/actions/runs/37265560883) sobre `7a1fca3696dada4263c70716c70b763e492807cc` (560 tests), se subió el AAB con hash coincidente, se validó el canal cerrado y Google aceptó el commit solicitando revisión. Una lectura independiente a las 05:27 UTC confirma `alpha` 1.0.1 (23), `completed`; `internal`, code 12, `completed`. `completed` en la API no demuestra aprobación de revisión, publicación gestionada ni instalación: esos extremos siguen sin verificar. Los tracks `production` y `beta` no contienen releases. El hash de 22 coincide con el recibo del pipeline de `12a472ecb68e9a9c0bcba81baf0fafa551d59253`. Ecuador está habilitado en closed testing. Una validación de Open Testing en un edit temporal devolvió `FAILED_PRECONDITION`; el edit se eliminó sin commit. No se deduce el motivo preciso ni se eluden los requisitos de Google. La API de testers solo permite grupos Google, no las listas de correos de Console: admisión y capacidad siguen sin verificar. El manifiesto registra `closed_testing` / `approval_required` / `unknown` y conserva el formulario de solicitud sin exponer un botón de instalación que prometa acceso.

El certificado de firma real de los APK generados por Play para code 23 (`08:76:…:8E:B0`) coincide con `assetlinks.json` desplegado, comprobado mediante `generatedApks`. Esto valida la asociación declarada; no sustituye abrir un enlace en un dispositivo real.

**Analytics, observación histórica previa a la creación autorizada:** entonces solo se había verificado transporte interceptado y no se había encontrado una clave productiva. Ese bloqueo de acceso quedó resuelto; el [informe de activación posterior](analytics-2026-10-05.md) distingue el proyecto y los eventos de preview comprobados de la activación productiva y las instalaciones aún no acreditadas.

[Evidencia resumida sin datos personales](distribution-2026-10-05.json). Los siete días de vigencia del manifiesto se cuentan desde cada observación real; al vencer, la UI deriva al formulario. No existe una sincronización automática con cuentas privadas de tiendas. La siguiente actualización exige repetir la comprobación de estado, grupo/capacidad y destino.

Fuentes: [Apple, testers externos](https://developer.apple.com/help/app-store-connect/test-a-beta-version/invite-external-testers/); [Google, representación de tracks](https://developers.google.com/android-publisher/api-ref/rest/v3/edits.tracks); [Google, limitación de listas de testers](https://developers.google.com/android-publisher/api-ref/rest/v3/edits.testers); [Google, acceso a producción y Open Testing](https://support.google.com/googleplay/android-developer/answer/14151465).

## Evidencia de distribución

Auditoría iniciada sobre `tdf-app/main` `fa30e358de631e4773d142ec83ca6d75d42047f0` y `TDF-mobile/main` `d2fd9399126e3b456c310497fb7d9edba09f981d`, obtenidos de GitHub, en worktrees aislados. Los checkouts originales tienen trabajo ajeno y se conservaron.

### iOS

App Store Connect API, 2026-10-04 16:30–16:41 UTC: app **6779786470**, bundle **com.tdfrecords.app**, versión **1.0.1**. La versión de App Store está **REJECTED**, con liberación manual. No se comprobó disponibilidad pública de App Store. El ID 6754828747 presente en documentación histórica no es el configurado/observado actualmente.

- Build **29**: `VALID`, no expirado; `IN_BETA_TESTING` interno; `READY_FOR_BETA_SUBMISSION` externo, sin Beta App Review submission. Artefacto firmado GitHub [36519965499](https://github.com/diegueins680/TDF-mobile/actions/runs/36519965499), commit `12a472ecb68e9a9c0bcba81baf0fafa551d59253`. No confundir con el `main` actual.
- Grupo interno: un tester agregado, acceso a todos los builds. No se exportaron identidades.
- Grupo externo: enlace público habilitado, sin límite particular habilitado, cero testers agregados. Contiene builds **19** (vigente) y **17** (expirado).
- Build **19**: `BETA_APPROVED`, Beta App Review **APPROVED**. EAS production `1fc2fc0b-f93a-4d11-8eb6-2b7f97dc0207`, commit `1ec9160ebcec19fff406c11b09ad0dc39bf76203`, runtime `1.0.1`, channel `production`. No confundir los varios simuladores con número 19 con este build de tienda.
- [Invitación TestFlight vigente](https://testflight.apple.com/join/7k3VE2JJ): la página pública muestra TDF Records Mobile e instrucciones para aceptar/instalar. La integración dirige a esta beta existente, no publica ni cambia el build 29.
- Capacidad contractual: hasta 10.000 externos; hasta 100 internos con rol autorizado. El cupo observado es una instantánea, no una reserva.
- Diego confirmó en esta sesión que **no hay evidencia física reciente de Google OAuth**. Nueva publicación/release mantiene ese gate. Motivo detallado del rechazo de App Review no comprobado; requiere Resolution Center. El requisito de cuenta demo para Beta Review está activo; no se extrajeron credenciales.

Los JSON de `observations/` preservan estados, timestamps, IDs de build y conteos. No contienen tokens, claves privadas ni listas de personas.

### Android

Package confirmado en source: **com.tdf.records**. El build firmado más reciente observado en GitHub es [36519962875](https://github.com/diegueins680/TDF-mobile/actions/runs/36519962875), commit `12a472ecb68e9a9c0bcba81baf0fafa551d59253`. Version code y artefactos de ese run deben contrastarse con la evidencia de GitHub retenida; su presencia no demuestra publicación en Play.

La URL `https://play.google.com/apps/testing/com.tdf.records` dirige al login de Google cuando se consulta anónimamente; no confirma elegibilidad. La herramienta del navegador conectado devuelve `Browser is already in use`. No se tomó control de otra sesión. EAS confirma una credencial Play vinculada y submissions Android finalizadas; eso **no verifica** tracks activos, aprobación, testers, países ni acceso de producción. Android permanece `unavailable` en la integración pública, con solicitud de acceso; aquí significa **no hay acceso comunitario verificado**, no que no exista ningún build.

El historial indica Alpha/22 en revisión el 30 de septiembre, pero no se usa como estado vigente. El operador debe verificar Play Console, aprobar el acceso de un tester real y capturar track, release status, version codes, países y enlace de opt-in antes de cambiar la configuración. Open Testing se prefiere cuando la cuenta/app sea elegible; de lo contrario Closed con grupo explícito o admisión manual. No asumir que las condiciones de cuentas personales nuevas aplican a esta cuenta sin comprobar su tipo.

### Expo / EAS

Proyecto consultado: `218aca4d-c096-4892-a353-c1dd7df23448`, `@cuco.saa/tdf-mobile`. Profiles en main: development, preview, production, ios-simulator, interaction-e2e. Producción usa channel production, runtime 1.0.1 y versión remota; preview distribuye APK interno. Los workflows nativos firmados GitHub son una vía distinta de EAS Build: la lista EAS no sustituye su inventario.

Consulta de canales: production, preview, notification-phone-qa, ux-isolated-android, interaction-e2e, todos no pausados y vinculados a ramas de igual nombre; sin updateGroups devueltos. `eas update:list --branch production --limit 5 --json --non-interactive` devolvió lista vacía. No se publicó OTA. La submission iOS `d2a6ee83-7c1a-406e-979c-3c79e6460b73` está cancelada; Apple sí tiene build29 mediante la vía de upload posterior. No atribuir ese upload a una submission EAS exitosa.

## Implementación

- URL estable `/app`; landing pública ES/EN y ambas plataformas siempre seleccionables. Detección de iPhone/iPad/Android solo establece prioridad, no bloquea alternativas. No se hace un redirect automático fuera de TDF.
- Única configuración de distribución: `tdf-hq-ui/public/mobile-distribution.json`. El validador exige coherencia canal/plataforma/destino/identidad, HTTPS, fechas y política de admisión para closed testing. Solo URLs Apple/Google exactas; nunca destinos arbitrarios ni Expo artifacts como tienda.
- La landing refresca configuración cada minuto, recalcula expiración y revalida antes del clic. Caducidad, cupo lleno/desconocido o error inicial llevan a solicitud de acceso. **No existe sincronización en tiempo real de cupos con las tiendas**; actualizar la evidencia antes de `validUntil` y ante un cierre de cupo. Esto falla cerrado al caducar, pero no promete resolver carreras con cupos de Apple.
- CTAs en PublicBranding: navegación, footer, /tdf, inicio, fans/comunidad, perfiles; tarjeta contextual descartable durante 30 días en otras superficies móviles relevantes. Footer compacto; sin interstitial ni modal obligatorio. Menú de sesión persistente. Invitación posterior a signup conserva la navegación original, no añade campos al registro.
- Admisión sin formularios cuando existe enlace externo abierto. Solicitud voluntaria con email y consentimiento cuando no existe acceso comprobado. Se reutiliza `/feedback` y sus catálogos publicados; los operadores reciben la solicitud por el flujo existente y deben resolver admisión. Una solicitud guardada **no** significa tester admitido ni lista Google modificada.
- Feedback web y nativo: bug, UX, idea, comentario; selección de categoría publicada por intención; solicitudes/comentarios usan permissions/question/suggestion según disponibilidad, con Idea como alternativa informativa del catálogo vigente y tipo explícito en el contenido. No se usa el default Bug para solicitudes ni comentarios generales; se prefiere P4 para entradas informativas. Captura PNG/JPEG opcional de máximo 5 MB; descripción hasta 4.000 caracteres, conservada en memoria si falla. El formulario nativo permite excluir la metadata técnica. No se agregan tokens, emails de sesión, logs ni IDs de otras personas a la metadata/analytics. Los videos no se aceptan en este incremento porque no se verificó soporte seguro del endpoint para ese tipo/tamaño.
- Mobile: entry persistente en Perfil y Acerca de. Tras tres sesiones separadas al menos 30 minutos, tarjeta opcional; dismiss 30 días, opt-out persistente. El primer arranque observado no se denomina instalación.

## Analytics y atribución

Reutiliza PostHog. `mobile_promo_viewed` usa IntersectionObserver, no simplemente montaje. Eventos: `mobile_promo_dismissed`, `mobile_testing_interest_clicked`, `mobile_platform_selected`, `mobile_testing_join_clicked`, `mobile_store_clicked`, `mobile_testing_request_submitted`, `mobile_first_open`, `mobile_feedback_opened`, `mobile_feedback_submitted`.

Propiedades web: platform, surface, entry_surface, locale, authenticated, distribution_status cuando se conoce, source/medium/campaign. UTMs se conservan entre superficies TDF y se leen del mecanismo first-party ya existente; solo etiquetas alfanuméricas/guion de hasta 80 caracteres. No se transmiten querystrings completos, emails ni tokens. El clic se captura antes de navegar; la telemetría no bloquea el destino y un bloqueo del navegador/adblock puede perderlo. No se promete entrega perfecta.

`join_clicked` significa clic hacia opt-in, no aceptación observada. `request_submitted` solo tras 2xx del backend. `feedback_submitted` solo tras 2xx; nunca contiene texto/capturas/contacto. `mobile_first_open` es primer arranque observado de esta integración; reinstalar/borrar datos puede repetirlo. No hay Install Referrer/AdServices/deferred deep linking implementado: no atribuir instalaciones individuales a un UTM ni unir identidades de tienda. Los clientes sin clave PostHog hacen no-op; validar que la configuración de deployment esté habilitada antes de declarar recepción productiva.

## Deep links y dominios

Consulta HTTPS live devolvió JSON de asociación válido en `www.tdfrecords.net/.well-known/apple-app-site-association` y `assetlinks.json`: iOS team `83J23NPXG7`, bundle correcto; Android package correcto y dos fingerprints. Source mantiene ambos hosts www y pages.dev para /eventos/ y /conversacion/. `/app` permanece web, evitando abrir una ruta nativa no soportada. No se inventaron certificados ni se ampliaron rutas capturadas.

Los tests existentes de Expo Router comprueban resolución/allowlist, pero **no sustituyen handoff físico firmado**. Sigue pendiente probar abrir links en dispositivos con/sin app y verificar certificados contra Play App Signing actual. Se conservaron callbacks OAuth y compatibilidad pages.dev. URLs legales nativas pasan al dominio TDF comprobado; la página histórica /mobile-app enlaza /app y conserva las rutas de privacidad, soporte, términos y borrado.

## Operación y paso a producción

Antes de modificar `mobile-distribution.json`: comprobar consola y URL como tester elegible; registrar timestamp, build y commit, Beta Review/track, cupos y países. Para iOS public, exigir App Store realmente distribuida y URL id6779786470. Para Android public, exigir production publicado y package correcto; no basta draft/completed upload. Cambiar estado y URL en la misma revisión; las superficies derivan el CTA sin reimplementarse. No usar badges de tienda para beta. Se usan enlaces de texto accesibles; no se reproducen ni alteran insignias oficiales.

Solicitudes: revisar el feedback recibido, coordinar con el usuario solo con su consentimiento, admitir en el canal autorizado y verificar instalación/primer uso por separado. Retiro: TestFlight/Play Testing y contacto de privacidad para solicitudes. No se enviaron campañas, correos de invitación ni mensajes a terceros durante este trabajo.

## Fuentes primarias y decisiones

- [Apple: invitar testers externos](https://developer.apple.com/help/app-store-connect/test-a-beta-version/invite-external-testers/) y [TestFlight](https://developer.apple.com/testflight/): aprobación, grupos y capacidad; distinguir interno/externo/público.
- [Google: tracks de testing](https://support.google.com/googleplay/android-developer/answer/9845334) y [requisitos de cuentas personales nuevas](https://support.google.com/googleplay/android-developer/answer/14151465): opt-in, elegibilidad y requisitos de producción, sin extrapolar tipo de cuenta.
- [Expo: runtime versions](https://docs.expo.dev/eas-update/runtime-versions/) y [deploy updates](https://docs.expo.dev/eas-update/deployment/): commit/build/channel/runtime son dimensiones distintas; sin OTA especulativa.
- [Apple: associated domains](https://developer.apple.com/documentation/xcode/supporting-associated-domains) y [depurar universal links](https://developer.apple.com/documentation/technotes/tn3155-debugging-universal-links): asociación HTTPS no acredita una prueba de dispositivo.
- [Android: App Links](https://developer.android.com/training/app-links/about): huella de firma y verificación del sistema necesarias.
- [WCAG 2.2](https://www.w3.org/TR/WCAG22/): semántica, foco, reflow y tamaño de objetivos. Controles nuevos de al menos 44px web/48dp nativo, sin animación obligatoria. Axe/browser no certifican VoiceOver/TalkBack físicos.

La invitación contextual, el consentimiento independiente, las opciones de descarte y la separación de clic/admisión/instalación son decisiones del diseño; no se afirma haber hecho un experimento A/B ni haber demostrado incremento de conversión.

## QA registrado

- Web: 44 pruebas focalizadas (distribución, formulario, PublicBranding, login) pasaron; lint y typecheck pasaron.
- El primer build excedió el presupuesto al precargar analytics. Tras carga diferida, build pasó: 5 preloads, 362.010 bytes gzip iniciales, límite intacto.
- Browser: 19/20 pasaron en Chromium desktop/phone/tablet, Firefox y WebKit; la primera ejecución coincidió con reconstrucción de assets y un arranque tablet no encontró la ruta. Se retuvo el fallo; la repetición del caso tablet sobre build estable pasó (1/1), sin aflojar aserciones.
- Mobile: release:check pasó; 89 suites/558 tests de base con cambios; pruebas finales específicas de feedback/participación/perfil pasaron. Expo Doctor 17/17 con red; 12 pruebas Python de guards de firma/artefactos pasaron.
- No hay certificación de instalación/VoiceOver/TalkBack física, ni recepción productiva PostHog, ni despliegue afirmado.

## Integración revisada y comprobaciones adicionales

- Mobile PR [127](https://github.com/diegueins680/TDF-mobile/pull/127) integrado con aprobación independiente en `abf4d1dd5735a4acf96272f73f3dfb20a364fd87`. No implica distribución de un binario nuevo.
- Mobile PR [128](https://github.com/diegueins680/TDF-mobile/pull/128) sincroniza el registro `/app` y los tipos con la API canónica del parent. `main` mobile contenía declaraciones generadas de pagos no presentes en `tdf-app/main`; regenerar desde el OpenAPI vigente conserva el checkout nativo y pasa TypeScript y 18 tests de checkout/feedback, además de 10 del registro. No se incorporó backend de ramas ajenas.
- El primer CI web ejecutó 2.691 tests: 2.690 pasaron y el registro detectó correctamente que faltaba `/app`. Corregido en el catálogo compartido; 37 pruebas focalizadas posteriores pasan. Se corrigieron cuatro expresiones señaladas por el lint de CI, sin desactivar reglas.
- La ejecución local completa tuvo además dos suites con timeouts bajo carga. Su repetición aislada pasó (9 tests); no se cambiaron aserciones ni timeouts.
- Preview Cloudflare del primer commit: https://24ea74f9.tdf-app.pages.dev/app; manifiesto de distribución descargado y comprobado. Es preview, no producción.
- La auditoría adicional `audit:features` detecta una omisión previa, `/configuracion/fuentes-videos`, ajena a la landing. No se oculta ese diagnóstico ni se afirma que esta auditoría adicional esté verde.

### Actualización de main durante la revisión

El repositorio principal incorporó pagos PR #414 en `fdac8e76523befee1603f49f6c7cf7d00762931b` mientras se revisaba este trabajo. Se integró esa base conservando la carga diferida de rutas y la invitación posterior a signup. Mobile [PR #130](https://github.com/diegueins680/TDF-mobile/pull/130) regenera los dos archivos compartidos contra esta API nueva, incluyendo ahora las declaraciones de pagos y las 159 entradas del catálogo. No se modifica código de checkout en esa sincronización. Typecheck y 27 pruebas de registro/checkout pasan; CI mobile también pasa.

La web corregida sobre esta base pasa 37 pruebas focalizadas y el build: 318.464 bytes gzip iniciales. La matriz previa completa pasó 20/20; se repite sobre esta base con una aserción adicional de selección de archivo mediante Enter. Los formularios de la beta antigua se orientan a `/app`, sin prometer que el binario TestFlight 19 incluya el nuevo formulario nativo.

### Pin final y QA del candidato

Mobile #130 integrado con aprobación independiente en `2b31ed16f6f2daa0c25da4091d8aa372a0014fce`, fijado por el parent. La matriz final sobre main actualizado terminó **20/20** en Chromium desktop/phone/tablet, Firefox y WebKit, con axe, reflow, dismiss persistente y selección de adjunto mediante Enter. Las cinco regresiones de procedencia de catálogos también pasan; no se eliminaron pruebas ni se relajaron límites.

El pin tiene el mismo árbol que el candidato `ce6cc51` validado por Mobile Validate [37248275111](https://github.com/diegueins680/TDF-mobile/actions/runs/37248275111). El código fuente nativo nuevo aún necesita un artefacto firmado y distribución autorizada: los builds públicos/beta existentes no contienen estos cambios.

### Correcciones de revisión

La revisión automática detectó siete problemas de UX/datos que se corrigieron antes del merge: invitación tras alta Google; invitación dentro del contenido visible de ambos layouts; eliminación del banner duplicado en perfiles; carga explícita del manifiesto; estados públicos sin dependencia del cupo beta; comentarios generales y solicitudes fuera de triage de bugs. Las instrucciones de instalación también distinguen beta, tienda pública y preventa. La corrección equivalente nativa está en [PR #131](https://github.com/diegueins680/TDF-mobile/pull/131), con pruebas del categoryId efectivamente enviado.

Nueva comprobación ASC 2026-10-05 02:41 UTC: último upload 29 VALID y App Store 1.0.1 REJECTED, sin cambio. Se inició el pipeline firmado [iOS build 30](https://github.com/diegueins680/TDF-mobile/actions/runs/37256446684) del commit `2b31ed16f6f2daa0c25da4091d8aa372a0014fce`; no hace submission ni publicación y precede al ajuste de categoría de #131. Un resultado de build no sustituye la validación física pendiente.
