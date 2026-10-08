# Navegador conectado a API, PostgreSQL y S3 locales

Fecha: 2026-09-15. Continuación de la verificación nativa del player y del
worker Linux. No es un despliegue ni una certificación de proveedor/CDN.

Continuación posterior: [persistencia de Studio y creación de borradores](studio-persistence.md)
amplía esta suite con un tercer caso por navegador. Los 10/10 registrados
aquí corresponden al corte anterior; consultar ese informe para el resultado
de la matriz ampliada y sus límites editoriales.

## Auditoría y decisiones

El harness anterior del navegador decodificaba derivados reales, pero
sustituía respuestas de catálogo, autorización y sesión. La integración
HTTP ya disponía de una base desechable, usuarios sintéticos, revisión,
publicación y objetos procesados reales. Se reutiliza esa integración y se
inserta la fase Playwright después de publicar y antes de las pruebas de
compra, corrección y retiro. No se duplican endpoints, checkout ni modelos.

Incompatibilidades encontradas y corregidas:

- El worker guarda `bitrate_kbps: 0` para FLAC. Las páginas pública y de
  biblioteca lo clasificaban como `low`. Ambas usan ahora `musicSourceQuality`
  y el tipo autorizado `audio/flac`. Un bitrate AAC elevado no demuestra
  codificación sin pérdidas; valores ausentes/no finitos usan `high`.
- CORS no admitía `Idempotency-Key`, usado por creación y compras. Se permite
  ese encabezado manteniendo intacta la política de orígenes y denegando el
  encabezado geográfico del navegador.
- Seis acciones de la página musical enviaban `returnTo` al login, que solo
  lee `redirect`. Se reutiliza `buildLoginRedirectPath`, con su sanitización
  existente; no se modifican las políticas globales de acceso ni los cambios
  concurrentes de onboarding.
- Una preview podía desplazar el audio completo al compartir el bitrate bajo.
  La selección del provider prioriza fuentes completas ya autorizadas; conserva
  el comportamiento y límite anteriores cuando solo se autorizó una preview.
  Una regresión reprodujo selección de `preview.m4a` en lugar de `full.m4a`;
  el navegador exige ahora `stream-low.m4a` y duración completa antes de FLAC.
- La radio legacy se montaba al autenticar incluso dentro del catálogo nuevo.
  No se monta en `/musica` ni sus rutas hijas (también con `#radio`); sigue
  disponible en sus rutas anteriores. No se elimina el player legacy.
- Favoritos permitía mutar antes de cargar el estado del servidor, y duplicaba
  ese estado en un `Set` local. Ahora se deriva de la consulta, se bloquea el
  control durante consulta/mutación y se vuelve a consultar tras la confirmación. Los
  fallos de carga se muestran explícitamente, sin habilitar una acción incierta.
- Las claves de caché de favoritos/playlists/historial no distinguían cuentas.
  Se agregó la identidad de sesión a ambos consumidores; una regresión
  reprodujo el favorito anterior después de cambiar de cuenta sin desmontar.

El cambio de calidad es de presentación/selección: no modifica bytes,
normalización, másteres ni sus hashes. FLAC corresponde al derivado
normalizado, no a una promesa de identidad binaria con el máster original.

## Ejecución y aislamiento

```sh
# Compilar previamente la API actual con el toolchain del repositorio:
cd tdf-hq
stack build tdf-hq:exe:tdf-hq-exe tdf-hq:exe:tdf-ddex-render --fast
stack exec -- runhaskell -isrc test/MusicCorsProbeMain.hs
cd ..
npm run test:music-browser-integration
```

Requiere las imágenes locales previamente verificadas, Docker sano, FFmpeg,
Stack, PostgreSQL CLI, dependencias web y los navegadores Playwright. No
descarga imágenes ni instala dependencias automáticamente. El comando usa
`--with-linux-worker --with-browser`; `--with-api` mantiene el modo anterior.

- MinIO HTTPS, PostgreSQL TLS y worker Linux son los del harness existente.
  La API Haskell continúa ejecutándose en el host.
- Vite ocupa exclusivamente `127.0.0.1:4187`, con `--strictPort` y sin reutilizar
  otro servidor. API, almacenamiento y el intermediario usan puertos locales
  asignados por el sistema. CORS de API y MinIO permite ese origen concreto.
- Un intermediario HTTP de prueba dirige solicitudes exclusivamente a la API
  loopback seleccionada y añade `CF-IPCountry: EC`, sobrescribiendo valores
  entrantes. No modifica respuestas, CORS, cookies ni cuerpos. Simula el dato
  de geolocalización del perímetro; no certifica geolocalización real.
- No hay `route.fulfill`, audio sustituido ni sesión inyectada. El navegador
  bloquea destinos HTTP ajenos a los tres orígenes exactos. Los objetos se
  descargan directamente de MinIO con URLs firmadas por la API.
- Se ignoran errores de certificado **solo en estos contextos de navegador**
  porque el certificado MinIO es efímero/autofirmado. Node/curl y el worker
  verifican el CA. No presentar esta prueba como confianza TLS del navegador
  en un CDN real.
- No se pasan credenciales de PostgreSQL/S3 a Playwright/Vite. El login usa
  una contraseña aleatoria de personas sintéticas, eliminada junto con su
  base. Traces y video están apagados; observaciones de red solo guardan
  origen, ruta, estado o fallo, nunca cookies, cuerpos ni queries firmadas.
  Los artefactos siguen siendo privados/locales; revisar antes de compartir.
- Los informes permanecen en el directorio `tdf-music-browser-results-*`
  indicado por consola. Contenedores, redes, base, objetos y certificado
  siguen sujetos a la limpieza del harness, también tras un fallo.

Casos de navegador en cinco proyectos (Chromium desktop/teléfono/tablet,
Firefox y WebKit): visitante reproduce, carga portada, navega conservando el
mismo audio y reloj, cambia a FLAC, verifica rangos y denegación de máster;
usuario entra desde favorito mediante login real, guarda favorito/playlist,
reproduce y consulta historial, recarga restaurando sesión y verifica datos
persistidos. Finalmente elimina su favorito y playlist de prueba.

## Evidencia conservada

- Preflight `ai:doctor`: 15 correctos, 3 advertencias, 0 errores. Árbol sucio,
  autenticación GitHub no válida en ese contexto y loop apuntando a main pero
  sin polling. No se hizo pull, commit, push ni se activó el loop.
- Primer intento integrado: sandbox impidió abrir el socket de Docker.
  Intento autorizado: motor HTTP 500 incluso en `/version`; Desktop figuraba
  activo. Diego autorizó reiniciarlo. `docker desktop restart` terminó 0 y
  la siguiente ejecución pudo correr la imagen y los servicios locales.
- Regresión de calidad: 7 fallos y 5 aciertos antes del fix. CORS: 1 fallo
  y 2 aciertos antes; 3/3 después usando el middleware WAI real, sin servidor.
- Regresión de retorno al login: primer fixture tenía un matcher Jest no
  instalado y funciones de consulta omitidas; se corrigió el fixture. Luego
  4/4 casos reprodujeron realmente `missing redirect` antes de corregir la
  página. La repetición conjunta final pasó 5 suites / **41 tests**, 17,809 s.
- Después se reprodujo el conflicto preview/completo (1 fallo / 20 aciertos
  del provider). Con la corrección, repetición conjunta **43/43**, 5 suites,
  código 0, 54,67 s. Incluye el caso de visitante con solo preview autorizado.
- Tests del intermediario, aislamiento Linux y base: **13/13**, código 0.
  Dos casos Linux usan un doble de comandos para probar limpieza: no son
  ejecuciones Docker. El test del intermediario sí usa HTTP loopback real.
- Regresión nativa de calidad: **5/5**, código 0, sin skips ni retries,
  371005,696 ms. API/sesión siguen siendo fixtures en esta suite separada.
  Informe `/private/tmp/tdf-player-worker-quality-authorized/results.json`,
  SHA-256 `a5ab6667f1f4e16547ac977df846cbad389facbfd7be8f3f56d8140792e9ba3e`.
  Primer intento sin autorización había fallado por `listen EPERM`.
  Esta corrida precede al cambio de prioridad completo/preview y al arreglo
  de retorno al login; no se atribuye como repetición de esos cambios.
- Primera matriz con API real: **5/10**, código 1. Pasaron los cinco casos
  de visitante; los cinco autenticados perdieron el destino tras login.
  Informe `/private/tmp/tdf-music-browser-results-M0UGVR/results.json`,
  SHA-256 `947713be22c67a3e04602d354d8cdb1f6a22b9c4057ddd576306852fcf70b81c`.
  Inicio `2026-09-15T15:35:00.882Z`, 233154,478 ms. No alcanzó compras/retiro.
  El destino erróneo `/buscar` produjo además errores 500 del directorio:
  esta base musical no instala sus tablas `profession` y
  `directory_public_search_document`. No se ocultaron con mocks ni se
  presentan esas páginas ajenas como validadas por este harness.
- Build backend normal fue interrumpido expresamente (130) al advertir el
  cambio de optimización; solo se señaló su grupo de procesos verificado.
  Repetición `--fast`: compilación, enlace e instalación código 0; warnings
  del linker por `-U`/`-lm`, no cambios de dependencias.
- Build frontend iniciado antes del último fix de retorno terminó 0:
  TypeScript/Vite, 12460 módulos, 376938 bytes gzip iniciales, 5 preloads.
  Se mantiene aviso de chunks mayores de 500 kB. No contar ese build como
  typecheck posterior al último cambio; repetir sobre fuentes finales.

Segunda matriz: **4/10**, código 1, inicio `2026-09-15T15:50:17.693Z`,
333147,276 ms. Informe `/private/tmp/tdf-music-browser-results-YSC3sC/results.json`,
SHA-256 `e5dfbb8ae9dec24bfb63bdf8d517eb0f05a3e9fd701719d08c08a9131f775464`.
Cuatro casos autenticados detectaron dos elementos audio (radio y player),
uno perdió el estado de favorito, y el visitante Firefox agotó el chequeo de
`currentSrc` al pasar a calidad baja. Las observaciones de red de ese Firefox
sí muestran `stream-low.m4a` con HTTP206; no se determinó la causa exacta del
timeout. El harness siguiente pausa antes de cambiar calidad, conserva las
aserciones de fuente/duración, reanuda después y registra ambos atributos de
fuente/rango de estado al finalizar. No se amplían tiempos ni reintentos.
También normaliza favoritos preexistentes desde la UI después de cargar su
snapshot, para no propagar datos dejados por un proyecto fallido.

Las tres regresiones de visibilidad de radio fallaron antes del fix; los cinco
casos anteriores/de rutas ajenas seguían verdes. Se reprodujeron por separado
el botón de favorito activo antes del snapshot y la reutilización de caché
de otra cuenta.

### Resultado final

La tercera ejecución de `npm run test:music-browser-integration` terminó
**código 0**: **8/8** preflight de imagen Linux, **10/10** casos de navegador
y **18/18** escenarios API/HTTPS/S3. Después del navegador también pasaron
la descarga del máster intacto, permisos, reembolso canónico sintético,
corrección auditable y retiro idempotente.

Informe `/private/tmp/tdf-music-browser-results-CwS2Uz/results.json`:

```text
Inicio UTC: 2026-09-15T16:04:07.130Z
Duración navegador: 216261,139 ms
Esperados: 10; fallos: 0; skips: 0; flaky/retries: 0
SHA-256: b016bedbabb78352fedf431194bc5aa4023ae58fc023167643c2f1eaed835909
```

Las observaciones de los cinco casos autenticados no contienen respuestas
HTTP >=400. En los cinco de visitante, el único error HTTP registrado es el
404 esperado para el máster privado. Cada proyecto pasó login, favorito,
playlist, historial, recarga y restauración de sesión real. El cambio de
calidad de esta matriz se opera en pausa y se reanuda después: no convertirlo
en una afirmación de que el timeout anterior en reproducción quedó explicado.

Validaciones finales adicionales:

- **53/53 Jest**, 6 suites, código 0, 18,09 s: controles, fuentes, retorno al
  login, consulta/mutación de favoritos, cambio de cuenta y rutas de radio.
- ESLint dirigido y comprobaciones de sintaxis/diff: código 0.
- Build frontend iniciado después de todos los cambios de runtime: código 0,
  TypeScript + Vite 12460 módulos / 1m3s + presupuesto, 376940 bytes gzip
  iniciales y 5 preloads. Persiste warning de chunks >500 kB; sin ampliar límites.
- Configuración de suites/intermediario: **4/4 Node**, código 0 (dos casos
  del intermediario ya están incluidos en los 13/13 anteriores).
- El comando general excluye únicamente las dos suites con infraestructura
  especial; sus configuraciones dedicadas restauran explícitamente la selección.
  `--list` confirmó 61 casos generales, 25 nativos y 10 integrados, todos código 0.
  **Enumerar no equivale a ejecutar** los 61/25 casos. No se omiten casos de
  las corridas dedicadas ni se habilita esta infraestructura automáticamente en CI.
- Consulta Docker por etiquetas: cero contenedores S3/Linux y cero redes Linux.
  Sin listeners en 4187/4188. Los recursos sintéticos fueron eliminados y los
  informes se conservan. No quedó un proceso de prueba propio pendiente.

Huellas de runtime verificadas tras la matriz (SHA-256):

```text
TDF/Cors.hs                 3e936246216b4c722eeb3ee66f1d21dda58e0319e6240b5ca3b7a9e9730603a3
player/sourceMetadata.ts   2d2f6271f7718eb899b75b8afd5c1e1b73d77bf5db665438e42c5d3eea3dd55a
player/PlayerProvider.tsx  a1a36f6ab3a24a6ba935d358b9904f8fcf83ea193fe5172c9829c2f688fcf9dc
MusicReleasePublicPage.tsx bb372c28a1836c324343addb7ecb1d3094857d20d891286dbaf38054132ef2e5
MusicLibraryPage.tsx       329e773ad263a5409e3a38261da4775858977388cbd55a6f9c9838cf4955e332
radioRouteVisibility.ts    192c36ea04bb9b223d2f9d73dfd2ca628d79dc2e615be0fba30ab12767ff8a7f
```

Se revisaron código, resultados y observaciones de red/estado; no hubo
interacción manual con la UI, dispositivos físicos ni un lector de pantalla.

## Despliegue, rollback y límites

No hay migración nueva. Se aplican únicamente las migraciones existentes en
la base sintética. Los cambios de runtime requieren desplegar frontend y API;
no se cambia imagen del worker, proveedor, almacenamiento ni banderas remotas.
Rollback de código: revertir únicamente los cambios de esta entrega en el
helper, provider, consumidores musicales, visibilidad de radio y encabezado
adicional de CORS; no deshacer cambios ajenos del árbol. No hay
datos productivos que revertir. Quitar `--with-browser` recupera el harness
HTTP anterior.

Pendientes fuera de esta evidencia: creación/carga/revisión editorial por UI
(se preparan por API real), pago externo y webhooks reales, CDN/IAM/lifecycle,
archivos de varios GiB, API Linux completa, dispositivos físicos, lector de
pantalla humano, prueba de operación/restore/rollback remoto y puertas
DDEX/licencia/identificadores ya documentadas. Las compras del harness usan
evidencia canónica sintética, no un cobro a PayPal/Datafast. Las métricas no
son contabilidad certificada de regalías.

Fuentes primarias consultadas el 2026-09-15: [Playwright webServer](https://playwright.dev/docs/test-webserver),
[control de red](https://playwright.dev/docs/network),
[opciones HTTPS](https://playwright.dev/docs/api/class-testoptions#test-options-ignore-https-errors),
[Fetch/CORS](https://fetch.spec.whatwg.org/#http-cors-protocol) y
[RFC 9639, registro FLAC](https://www.rfc-editor.org/rfc/rfc9639.html#section-12.1).
