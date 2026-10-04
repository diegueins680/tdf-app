# TDF Mobile: adquisición y programa de testers

Estado de trabajo, 4 de octubre de 2026. **No es una declaración de despliegue ni de publicación en tiendas.**

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
- Feedback web y nativo: bug, UX, idea, comentario; selección de categoría del catálogo por código, con fallback al default publicado y tipo explícito en el contenido. Captura PNG/JPEG opcional de máximo 5 MB; descripción hasta 4.000 caracteres, conservada en memoria si falla. El formulario nativo permite excluir la metadata técnica. No se agregan tokens, emails de sesión, logs ni IDs de otras personas a la metadata/analytics. Los videos no se aceptan en este incremento porque no se verificó soporte seguro del endpoint para ese tipo/tamaño.
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
