# Reproducción nativa en navegadores

Fecha: 2026-09-15. Corrida final: 20/20, código 0 (15 casos nativos y 5 de
regresión con motor sustituido), sin omisiones ni reintentos. Los fallos previos
y los límites de esta evidencia se conservan a continuación.

Esta es la evidencia de la primera entrega nativa. La continuación de
[controles compactos](compact-player-controls.md) incorpora acceso a calidad,
repetición y volumen en pantallas pequeñas y amplía los escenarios de teclado;
sus resultados y hashes se documentan separadamente. Los controles ocultos
descritos aquí corresponden al estado de esta primera corrida.

La suite anterior comprueba shell, navegación y accesibilidad con un motor
multimedia sustituido explícitamente. Se conserva. Esta nueva suite usa
`HTMLAudioElement` sin modificar sus métodos, propiedades, reloj o eventos.
Genera un máster sintético y ejecuta el pipeline real para obtener AAC/FLAC;
sirve sus bytes por HTTP loopback con rangos. No usa audio de terceros.

Los metadatos y la autorización de API son fixtures declarados: esta suite no
sustituye el E2E HTTP/PostgreSQL/S3 ni certifica CDN. Las peticiones externas se
bloquean y la API configurada es local. El servidor de frontend no reutiliza
puertos ocupados. El setup conserva un único servidor de medios durante la
corrida y limpia exclusivamente su directorio temporal al terminar.

Se comprueban avance real del reloj, eventos nativos, duración, seek desde el
control de UI, identidad del elemento durante navegación SPA, cambios AAC/FLAC,
pausa/reanudación, siguiente pista, finalización natural y repetición. Se
adjuntan observaciones del motor por test. Las vistas móviles no se presentan
como pruebas de dispositivos físicos ni como Safari distribuido por Apple.
En teléfono los controles de calidad/repetición están ocultos en el layout
actual; en tablet también está oculto el selector de calidad. En esas ramas
se comprueba calidad automática y avance natural, no los controles manuales
ocultos. No contar sus ramas condicionales como cobertura móvil completa.

```sh
PLAYWRIGHT_PORT=4188 PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-native-player-run \
  npx playwright test --config=playwright.music-native.config.mjs

npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/player/PlayerProvider.test.tsx src/player/queue.test.ts \
  src/player/analytics.test.ts --watch=false --reporters=default
```

También existe `npm run test:music-player-native` en la raíz. Requiere las
dependencias del frontend/Playwright, sus navegadores instalados, FFmpeg,
ffprobe, jq y las herramientas de checksum usadas por el pipeline. La suite
no instala navegadores ni descarga medios durante su ejecución. El puerto
elegido debe estar libre; los artefactos se conservan fuera del repositorio
y pueden incluir trazas/capturas automáticas de fallos, no aceptación manual.

Fuentes primarias consultadas el 2026-09-15:

- [WHATWG: algoritmo y estados de los elementos multimedia](https://html.spec.whatwg.org/multipage/media.html).
- [Playwright: navegadores, codecs y límites frente a Safari](https://playwright.dev/docs/browsers).
- [Playwright: setup global, paso de configuración y teardown](https://playwright.dev/docs/test-global-setup-teardown).
- [MUI: step y shiftStep de los controles de seek](https://mui.com/material-ui/api/slider/).

No hay migración nueva, despliegue ni reemplazo del player legado en esta entrega.

## Defectos reproducidos y corrección

La primera ejecución nativa autorizada en Chromium terminó con 1 test pasado
y 1 fallido: después de `ended`, repetir la pista dejaba el elemento al final.
Seleccionar de nuevo el mismo índice no cambiaba la fuente y por tanto no
reiniciaba el motor. Se rebobina y reproduce explícitamente cuando la siguiente
selección es la misma; también se reinicia la posición persistida al completar.

Cinco regresiones unitarias fallaron antes de corregir el provider: repetición
de pista/cola/release con un único elemento, rechazo tardío de `play()` que
borraba la fuente nueva, y resolución tardía que revertía una pausa explícita.
Ahora se validan generación de fuente e intento de reproducción antes de
aplicar una respuesta asíncrona. Pausa y desmontaje invalidan los intentos
pendientes; desmontaje detiene el elemento. Estas pruebas usan dobles de
HTMLMediaElement y complementan, no sustituyen, la prueba nativa.

Después del cambio pasaron 3 suites Jest / 11 tests (provider, cola, analítica),
código 0. ESLint dirigido y sintaxis Node pasaron. La primera invocación sin
escalación falló antes de probar por `listen EPERM` del sandbox; no cuenta como
fallo del producto ni como prueba aprobada. La repetición usó servidores locales
con permiso, sin alterar autoplay, relojes ni prototipos del navegador.

La primera corrida completa posterior al fix terminó con 11/15, código 1:
Chromium 9/9, WebKit 2/3 y Firefox 0/3. Firefox tuvo timeouts en inicio, gesto
de seek y creación de página; su evidencia del primer fallo contiene AAC
reproduciéndose, eventos nativos confiables y `readyState=4`, sin error de
decodificación. WebKit no alcanzó el segundo `ended` dentro del plazo y la
observación posterior mostró posición 23,52 s de 23,98 s, todavía reproduciendo.
La carga del host fue alta, pero eso solo no demuestra la causa de cada fallo.

Se corrigió el teardown para tolerar que no se haya creado la página. El gesto
de seek ahora pausa, usa `PageUp` de 10 s y flechas de 1 s, verifica el valor del
input y reanuda. Evita que una automatización lenta termine la pista mientras
aún está operando sobre el control. No se amplió el timeout ni se quitaron
aserciones. La repetición aislada de Firefox pasó 3/3, código 0, en 1,3 min:
`/private/tmp/tdf-native-player-firefox-isolated`. La pasada final incluye los
15 casos nativos y los 5 casos anteriores con motor sustituido; estos últimos
no se contabilizan como decodificación real.

El typecheck global terminó con código 2 por la colisión entre `ReleaseArtwork`
y `releaseArtwork` en trabajo concurrente ajeno a esta entrega. Al inspeccionarlo
después, el helper ya se llamaba `resolveReleaseArtwork`; no se editó ese trabajo.
La tentativa de cancelar un compilador no ejecutó `kill`: su comprobación de
proceso padre falló. No se cuenta ese typecheck como aprobado ni como cancelado.
La repetición posterior mediante `npm run build --workspace=tdf-hq-ui` sí terminó
con código 0: TypeScript, Vite y presupuesto inicial (376936 bytes gzip, 5
preloads). Vite mantiene la advertencia de chunks mayores de 500 kB; no se
amplió el presupuesto. Ese build precede a la corrección adicional de posición
descrita a continuación. Se repitió después sobre las fuentes finales:
**TypeScript + Vite + presupuesto, código 0**, 376937 bytes gzip de JS inicial
y 5 preloads; Vite completó en 24,39 s. Permanece la misma advertencia de chunks.

## Revisión de trazas: posición atribuida a otra pista

La matriz completa pasó 20/20 en 388685 ms, sin skips, flakies ni reintentos,
con reporte `/private/tmp/tdf-native-player-final/results.json` (SHA-256
`dfa4a9d8e95a4079f61adc76c3330cbf4bc9f0e6999e7fffc758a143eee92d11`).
Sin embargo, revisar las observaciones descubrió un defecto que las aserciones
anteriores no detectaban: la pista segunda empezaba alrededor del segundo 9,
heredando la posición de la primera. Esa pasada no constituye aceptación de
la posición inicial entre pistas.

`pause()` puede encolar un `timeupdate` de la fuente anterior mientras se espera
la autorización de la siguiente. El nuevo listener guardaba entonces ese
tiempo bajo el ID de la pista nueva. Una sexta regresión de provider reprodujo
el problema (esperado 0, obtenido 8). Ahora la atribución de progreso exige
coincidencia entre pista, fuente cargada y `currentSrc`; se invalida la fuente
al cambiarla o desmontar el motor. Esto protege también la atribución del
tiempo escuchado. La prueba nativa exige ahora que el primer evento confiable
`playing` de la pista segunda tenga posición menor de 2 s.

Después de esta corrección pasaron 3 suites / 12 tests Jest y ESLint dirigido,
código 0. La matriz nativa más regresión también terminó con código 0:
**20/20 en 264003 ms**, inicio `2026-09-15T14:11:36.989Z`, cero omisiones,
fallos o reintentos. Cada proyecto pasó tres casos nativos y uno de regresión.
El reporte contiene 15 adjuntos de observaciones nativas y 5 informes axe:
`/private/tmp/tdf-native-player-position-final/results.json`, SHA-256
`6b7a67295505f17e090907c91f420fc6e5c4412619e6f3b901504ea4e905e5a9`.

La revisión de los eventos del primer test confirma la posición inicial de
la segunda pista, ahora sin herencia de los 8–9 s de la primera:

| Proyecto | Primer `playing` de la segunda pista | Estado final del motor |
| --- | ---: | --- |
| Chromium escritorio | 0,004254 s | `readyState=4`, sin error |
| Chromium teléfono emulado | 0,021333 s | `readyState=4`, sin error |
| Chromium tablet emulada | 0 s | `readyState=4`, sin error |
| Firefox | 0 s | `readyState=4`, sin error |
| WebKit | 0 s | `readyState=4`, sin error |

Se verificó después la ausencia del directorio sintético propio y de listener
en el puerto 4188. Se conservan los reportes; no se eliminaron archivos del
usuario ni recursos remotos.

Código congelado para esa repetición (SHA-256; no sustituye un commit):

```text
c05f4940afc28c7eff169fefa2554377579d0c4b4167943a336abfa3e8c17377  tdf-hq-ui/src/player/PlayerProvider.tsx
c645a502e520c40ca65b5733984877e12ee99d2367efcbf2c34a3f687d41e45f  tdf-hq-ui/src/player/PlayerProvider.test.tsx
009fe07c412cc2d677811d84482695e9b63581a711e27406eace05968751252c  e2e/web/music-player-native.spec.mjs
5502b8319ab18fbfbd2320c8e5f9313df7d5b097dd888d393fce0d4aa81d7bfc  e2e/web/music-player.spec.mjs
23c9a14d58758ce579f130b5e8a2368c9563af73ceaf8c3a160c751420cc9365  e2e/web/fixtures/native-music-setup.mjs
27b6f7979539b08843d2572c0e623ed80622686860db2463acb4d4df306fcd4b  playwright.music-native.config.mjs
```

Rollback de esta entrega: revertir únicamente las correcciones de provider
introducidas aquí mediante revisión del diff propio, sin descartar otros
cambios del árbol de trabajo. No hay transformación de datos persistidos ni
backfill; se mantiene el formato local `tdf-global-player/v1`, el motor legado
y las banderas existentes. Los tests son opt-in y no publican recursos. Un
rollback de código no equivale a haber probado un rollback de producción.

Pendientes de aceptación: controles compactos todavía ocultos, dispositivos
físicos y escucha humana, lector de pantalla real, sesiones largas/red
degradada, API/autorización reales dentro del navegador y CDN/proveedor remoto.
No se hicieron migraciones, despliegue, commits, PR ni validación manual de UI
durante esta entrega.
