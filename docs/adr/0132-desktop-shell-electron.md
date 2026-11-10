# ADR 0132 — TDF App Desktop: shell Electron sobre la web canónica

Status: proposed (implementación entregada en `tdf-desktop/`; la distribución pública requiere firma y aprobación del propietario del producto)

Fecha: 2026-11-08

## Contexto

TDF necesita una aplicación descargable para macOS y Windows en la que las
personas inicien sesión con su cuenta de TDF y trabajen con los mismos datos,
permisos y flujos de la plataforma web (`tdf-hq-ui` contra `api.tdfrecords.net`).
Los flujos prioritarios son cargas y lanzamientos (archivos grandes, reanudación,
reintento sin duplicados), música y radio con controles multimedia del sistema,
operación del estudio y gestión de eventos, campañas y ventas.

Restricciones relevantes del sistema actual:

- El backend solo admite orígenes `http(s)` explícitos en su política CORS con
  credenciales (`tdf-hq/src/TDF/Cors.hs`, requisito AUTH-CORS-001). Un origen de
  esquema propio (`tdf-desktop://app`) exigiría cambiar y volver a verificar esa
  frontera.
- El reproductor global ya centraliza la Media Session API, el teclado y la
  cola (`formal/system/player-session.md`, MEDIA-MUSIC-PLAYER-001).
- Las cargas de másteres ya son multipart reanudables con `Idempotency-Key`
  (`tdf-hq-ui/src/api/musicReleases.ts`).
- El flujo de autenticación emite una cookie de sesión y un token Bearer opcional
  desde `/login`; el token nunca se persiste en el cliente.

## Opciones evaluadas

| Opción | Inicio de sesión | Archivos grandes | Controles multimedia | Instaladores en CI | Riesgo |
| --- | --- | --- | --- | --- | --- |
| Electron, renderer alojado (web canónica) | Idéntico a la web: mismo origen, cookies y CORS sin cambios | Chromium: `File`, XHR por parte, reanudación existente | Chromium entrega teclas de hardware, Now Playing (macOS) y controles multimedia (Windows) a la Media Session de la página | electron-builder: DMG/ZIP firmados y notarizados, NSIS firmado; `electron-updater` | Depende de conexión al arrancar (igual que la web); contenido remoto gobernado por política de navegación |
| Electron, renderer empaquetado con esquema propio | Requiere admitir `tdf-desktop://app` en CORS del backend (cambio Haskell + nuevas pruebas de frontera) | Igual | Igual | Igual | Divergencia de versiones web/escritorio; cambio de seguridad en backend no verificable en esta etapa |
| Tauri 2 (WKWebView / WebView2) | Igual que la web si se aloja la web | Streams vía comandos Rust (más plumbing) | WKWebView no implementa Media Session; haría falta `souvlaki`/plugins nativos y duplicar la lógica del reproductor | `tauri-action` cubre DMG/MSI/NSIS | Motores distintos por plataforma; la suite Playwright del repo está calibrada en Chromium |

## Decisión

Construir TDF App Desktop como un shell Electron (`tdf-desktop/`) que aloja la
aplicación web canónica en su origen de producción (`https://www.tdfrecords.net`)
y comparte rutas, componentes, API y reglas de negocio sin duplicarlos.

El shell no implementa lógica de negocio. Añade: ventana endurecida (sandbox,
aislamiento de contexto, sin Node en la página), política de navegación que solo
permite el origen autorizado y páginas internas empaquetadas, apertura de enlaces
externos en el navegador del sistema, validación de enlaces profundos
`tdf-desktop://open?path=/ruta` antes de navegar, puente mínimo `window.tdfDesktop`
expuesto solo en el origen autorizado y verificado de nuevo en el proceso
principal, menú nativo de reproducción conectado a la misma sesión de reproducción
que la Media Session, canal de actualización del shell con descargas verificadas
e instalación explícita, y paquetes de prueba marcados `UNSIGNED-TEST` cuando no
hay credenciales de firma.

La web incorpora una sesión de reproducción compartida (`tdf-hq-ui/src/player/playbackSession.ts`)
para que el reproductor de lanzamientos y la radio compartan una única Media
Session y los comandos del sistema actúen sobre el audio activo; añade zonas de
arrastre, cancelación por archivo y reintento con la misma `Idempotency-Key` en
el Studio de lanzamientos; reutiliza una sola `Idempotency-Key` por transición de
ciclo de vida hasta que el servidor confirma; y oculta el inicio con Google dentro
del shell porque Google Identity Services rechaza contextos embebidos.

## Consecuencias

- Sin cambios en el backend ni en la frontera CORS verificada.
- La app de escritorio recibe las correcciones web al desplegarse la web; el canal
  de actualización gobierna solo el shell nativo.
- Inicio con Google y conexión de Google Drive quedan fuera del shell hasta que
  exista un cliente OAuth de escritorio aceptado por el backend.
- Las compras con proveedores externos (Stripe, PayPal) abandonan el origen
  autorizado y se completan en el navegador del sistema.
- La distribución oficial requiere Developer ID + notarización (macOS) y firma
  Authenticode o Azure Trusted Signing (Windows); hasta entonces solo se emiten
  paquetes de prueba identificados.
