# Evidencia de verificación

Fecha: 2026-08-16. Esta evidencia corresponde a la rama de implementación; no demuestra un
despliegue en producción. Las capturas usan fixtures sintéticos identificados como tales y no
contienen personas, eventos, direcciones, credenciales ni verificaciones inventadas.

## Gates automatizados

| Gate | Resultado |
| --- | --- |
| Backend Hspec/QuickCheck | 2.276 ejemplos, 0 fallos |
| Web Jest | 151 suites, 1.604 pruebas, 0 fallos |
| Móvil Jest | 51 suites, 264 pruebas, 0 fallos |
| Migración PostgreSQL 16 | Pass: base histórica inmutable; migración incremental de reputación con doble aplicación, rollback/reaplicación, backfill seco/aplicado/rollback/reaplicado, privacidad, claim, reseña verificada API/agregado, pérdida/restauración de elegibilidad, visibilidad del autor y moderación, alertas, merge, 10.000 documentos, taxonomías e invitaciones con participantes exactos, bloqueo y expiración |
| OpenAPI/clientes | Pass: seguridad pública, PII, reseñas/elegibilidad sin IDs comerciales, catálogos de servicio/moneda, separación patrocinada e idempotency keys; tipos web/móvil regenerados |
| Feature registry | Pass: 131 features, 146 rutas web, 44 rutas móviles |
| Autoridad de catálogos | Pass: 722 fingerprints revisados; 0 decisiones nuevas o caducas pendientes |
| Auditoría formal del repositorio | 4 pruebas, 0 fallos; QuickCheck cubre invariantes del dominio |
| Manifiesto de release | 37 pruebas, 0 fallos; SHA inmutable, ancestry, reanudación segura de catálogos y streaming SQL validados |
| TypeScript | Pass en web y móvil |
| Lint | Pass: 0 errores; web conserva 94 warnings preexistentes, móvil 0 warnings |
| Build web | Pass; presupuesto inicial 406.689 bytes gzip |
| Expo web export | Pass; 57 rutas estáticas |
| Axe WCAG 2.2 AA | 0 violaciones en cinco superficies auditadas; quedan dos comprobaciones `incomplete` en búsqueda y móvil, y una en el perfil con reseñas, para revisión humana |

El export de Expo conserva un warning de resolución de `event-target-shim` proveniente de
`react-native-webrtc`. Las suites web conservan warnings históricos de React `act(...)`; no hubo
fallos. No se ejecutó una entrega real de email/push ni una compra, y ningún fixture se marcó como
transacción o entrega real.

## Evidencia visual reproducible

- [`web-desktop-search-list.png`](screenshots/web-desktop-search-list.png): búsqueda dominante,
  ciudad, filtros, tabs, ranking orgánico y patrocinado separado.
- [`web-desktop-search-map.png`](screenshots/web-desktop-search-map.png): mapa abierto con ubicación
  aproximada y aviso de privacidad.
- [`web-desktop-profile-reviews.png`](screenshots/web-desktop-profile-reviews.png): perfil público con
  agregado y reseña sintética respaldada por una interacción verificable; el fixture declara que no
  representa una contratación real.
- [`web-mobile-search.png`](screenshots/web-mobile-search.png): web responsiva a 390 px.
- [`mobile-expo-directory.png`](screenshots/mobile-expo-directory.png): render real React Native Web
  de la pestaña principal móvil.
- [`mobile-expo-directory-results.png`](screenshots/mobile-expo-directory-results.png): resultados
  orgánicos móviles después del encabezado y el bloque patrocinado separado.
- [`accessibility-results.json`](screenshots/accessibility-results.json): resultado Axe estructurado.
- [`browser-errors.json`](screenshots/browser-errors.json): errores no esperados del navegador. La
  respuesta `401` de `/session` es deliberada para probar el visitante anónimo y se excluye de este
  archivo.

Reproducción:

```sh
npm run build --prefix tdf-hq-ui
cd tdf-mobile && npx expo export -p web
docker run --rm -v "$PWD/..:/workspace" --entrypoint node PLAYWRIGHT_IMAGE \
  /workspace/scripts/capture-music-directory-visuals.mjs
```

`PLAYWRIGHT_IMAGE` debe ser una imagen revisada que contenga Playwright y Chromium 1219. Las pruebas
de negocio con dos cuentas reales, entrega por proveedores, binarios iOS/Android y revisión con
VoiceOver/TalkBack permanecen como gates de release, no como éxitos simulados.
