# Acceso bilingüe y recuperación — incremento en validación

Hallazgos UX-260917-006 (parcial), 010 y 011. Español sigue siendo predeterminado; se reutiliza i18next y la preferencia existente. Se traducen acceso/registro/recuperación, nombres accesibles, estados y navegación pública. No se afirma que el resto de módulos, respuestas del backend o documentos legales estén traducidos. Los enlaces legales y la versión de consentimiento no cambian. Se retiran promesas de duración no medidas y jerga de permisos; no se atribuye mejora causal de conversión.

Un enlace incompleto ahora lleva con una acción a solicitar otro enlace, conservando exclusivamente el redirect que acepta readSafeRedirectPath. Antes hacían falta dos acciones (ir a login y abrir recuperación). El fallo de transporte conserva el correo y no declara envío exitoso.

Axe confirmó contraste 2,45:1 de la ayuda de contraseña sobre fondo oscuro en tablet, ES/EN. Se usa el color semántico del tema. Fuente consultada 2026-09-17: [WCAG 2.2](https://www.w3.org/TR/WCAG22/#contrast-minimum); objetivo 4,5:1. [MUI 6 Dialog](https://v6.mui.com/material-ui/api/dialog/) documenta duración/transición; se investiga un cierre intermitente de WebKit sin ampliar timeouts ni relajar assertions.

Evidencia inicial: 9/9 pruebas de componentes con i18next real; typecheck y lint dirigidos pasan. Navegador tras contraste: 8/10 pasan, dos fallos de cierre en WebKit; repetición aislada WebKit 2/2 pasa. Esto no acredita todavía cierre estable. Comandos de desarrollo usados: config temporal playwright.locale.config.mjs (Vite dev, puerto4187); repetir el mismo grep PW-PER-LOCALE sobre el bundle de producción antes de promover. No equivale a medición p75, estudio de usuarios ni cumplimiento WCAG completo.

Rollback: revertir sólo este incremento web; no modifica esquema, permisos ni API. Pruebas de regresión: formularios, consentimiento Google, sesión, rutas seguras, textos públicos y recuperación. Publicación pendiente de gates exactos/revisión y coordinación del despliegue concurrente.

Actualización de verificación: fuente `f36a6391c`, bundle local de producción, `npm run test:e2e:web -- --grep PW-PER-LOCALE`: **10/10 aprobados**, cinco perfiles, 1,6 minutos, sin ampliar timeouts ni cambiar assertions. Log seleccionado en evidence/locale-production-browser.log. La repetición hospedada de CI [35273466427](https://github.com/diegueins680/tdf-app/actions/runs/35273466427) pasó: 66 casos aprobados y 10 omitidos explícitamente, incluyendo los diez de idioma/recuperación. El primer fallo hospedado ocurrió en npm ci/node-datachannel antes de ejecutar pruebas; repetir sólo jobs fallidos resolvió esa instalación. Dos fallos locales previos en WebKit se conservan como evidencia histórica; el pase actual no demuestra ausencia de intermitencia en todas las condiciones. Todos los gates aplicables están SUCCESS; merge/producción retenidos por coordinación.

### Continuación: destino en el correo de recuperación

El hilo https://github.com/diegueins680/tdf-app/pull/422#discussion_r4042084948
confirmó que conservar `/login?redirect=…` no conservaba ese destino en el correo.
Se añade un parámetro query opcional `redirect` a `/v1/password-reset`; el cuerpo
sigue siendo `{email}`. El servidor conserva exclusivamente destinos locales
acotados y los codifica dentro del enlace generado. La pantalla de reset mantiene
la validación del destino y los permisos de la nueva sesión. No se concede acceso
por conocer un enlace ni se cambia la política de tokens. Los clientes antiguos
siguen funcionando. Desplegar el backend antes del cliente para completar el
round trip; revertir el cliente es compatible y no requiere revertir datos.

Verificación local en curso: API/rutas/login 30/30; se corrigió una expectativa del
harness porque URL codifica el fragmento Unicode. La primera corrida de navegador
pasó 8/10; dos scans Chromium incluyeron el fondo atenuado del modal. Se añadió
la precondición verificable `#root[aria-hidden=true]` antes de axe, sin excluir reglas
ni elementos: diagnóstico Chromium 2/2. Se mantiene la corrida inicial; la matriz
final y compilación Haskell siguen pendientes. No se afirma entrega SMTP real.

Cierre local: matriz de producción 10/10; API/rutas/login 30/30; binario de pruebas
construido con Stack/GHC 9.10.3: passwordResetLink 5 ejemplos y 100 casos QuickCheck,
cero fallos. Compilación completa y gates del head publicado siguen pendientes.
