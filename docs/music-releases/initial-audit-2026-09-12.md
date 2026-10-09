# Auditoría inicial — 2026-09-12

## Estado encontrado

El monorepo combina API Haskell/Servant/Persistent/PostgreSQL (`tdf-hq`), web React/Vite/MUI (`tdf-hq-ui`) y una app Expo en submódulo. El despliegue existente usa contenedores para la API y Cloudflare Pages para web. Había cambios de otras iniciativas en el árbol de trabajo; se preservaron y no se hizo reset, commit, push ni deploy.

Componentes reutilizados:

- identidad `party`, perfiles de artista, enriquecimiento/verificación y control administrativo estricto;
- catálogo canónico de géneros y estados editoriales;
- shell web persistente y radio anterior como ruta de compatibilidad;
- checkout canónico, intentos/bindings del proveedor, Datafast, PayPal, webhooks y evidencia cifrada;
- feed, perfiles públicos, búsqueda y notificaciones existentes;
- PostgreSQL, Fly/Render y pipeline de migraciones revisadas.

El catálogo musical previo (`ArtistRelease` y audio legado) no representaba versiones inmutables, derechos separados, deals territoriales, colaboradores externos ni cadena de recursos. El player de página podía competir con el audio del shell. El exportador DDEX anterior no era una implementación ERN 4 validada y estaba desactivado. No había credenciales verificables de almacenamiento de producción, DPID, aceptación de licencia DDEX ni proveedores de pago de producción disponibles en esta sesión.

## Brechas y riesgos iniciales

- Falta de un agregado canónico independiente de DDEX y de una máquina de estados protegida en servidor.
- Másteres sin cuarentena/promoción inmutable, multipart reanudable ni pipeline observable.
- Autorización territorial susceptible a datos declarados por el navegador.
- Publicación, sustitución y retirada sin transacción idempotente única.
- Compras musicales separadas del checkout canónico o sin entitlement verificable.
- Analítica de plays susceptible a incrementar por eventos ingenuos.
- DDEX con riesgo de combinar ERN, perfiles, AVS, diccionario y coreografía incompatibles.
- Alta probabilidad de filtración bajo embargo si búsqueda, páginas y assets no usan la misma consulta pública.
- Árbol de trabajo muy modificado y un error de compilación preexistente ajeno en `TDF.Server.SocialEventsHandlers` (`emrPartyId` recibe `Text` donde espera `Maybe Text`).

## Decisiones y orden de implementación

1. Migración aditiva y reversión guardada; UUID opacos, snapshots y bitácora.
2. Dominio/validadores puros y permisos en base de datos + API.
3. Multipart directo a cuarentena, checksum, promoción privada e jobs idempotentes.
4. Workflow editorial, scheduler transaccional y vista pública que aplica embargo/retirada.
5. Único player en el shell y fuentes autorizadas por servidor.
6. Reutilización de checkout y webhooks canónicos para crear entitlements y descargas auditadas.
7. Eventos versionados, deduplicación y agregados separados.
8. Adaptador DDEX versionado, bloqueado por feature flag, DPID verificado y XSD oficial local.
9. Despliegue gradual por banderas; contenido legado permanece operativo y entra a saneamiento, nunca se rellena inventando metadatos.

## Herramientas comprobadas

Disponibles localmente: GHC/Stack, Node/npm/Jest/TypeScript, PostgreSQL 16 vía Docker, `ffmpeg`, `ffprobe`, `jq`, `xmllint`, `curl`, `zip`, `unzip` y utilidades SHA-256. No se comprobó acceso válido a buckets, CDN, Datafast/PayPal de producción, DPID oficial ni infraestructura desplegada.
