# Controles compactos y teclado del player

Fecha: 2026-09-15. Implementación local verificada: matriz 25/25 y, tras el
último ajuste de teclado, 10/10 casos dirigidos y 25/25 unitarios. No es
despliegue ni aceptación de dispositivos físicos.

## Auditoría y decisión

La entrega anterior dejó calidad manual oculta en teléfono/tablet y repetición
oculta en teléfono. Volumen, aleatorio y repetición ya existen en el provider;
no hace falta un segundo motor, cambiar APIs ni crear preferencias paralelas.
El atajo global de Espacio ignoraba únicamente inputs/selects: interceptaba
botones, enlaces, comboboxes y contenido de diálogos, además de teclas ya
consumidas, modificadas, repetidas o de composición.

El árbol de trabajo sigue en `main`, con cambios musicales y ajenos sin commit;
no se cambia de rama ni se descartan archivos. `ai:doctor` terminó con 15 OK,
3 advertencias y 0 errores: árbol sucio, credencial GitHub no válida en ese
contexto y configuración de loop apuntando a main. No se activó el loop ni
se alteraron credenciales. Las herramientas locales de UI y FFmpeg están
disponibles. No se necesitan credenciales de storage/pagos para esta entrega.

Decisión: reutilizar Dialog de MUI **6.5.0**, la versión instalada, y el estado
del provider. El panel Opciones de reproducción es accesible en todos los
tamaños; contiene calidad, los cuatro modos de repetición, aleatorio, volumen
y mute. La barra mantiene accesos rápidos en escritorio y dos filas compactas
por debajo de `lg` (1200 px en el tema predeterminado). Los cinco botones de la
barra compacta conservan objetivos de al menos 44×44 CSS px, sin agregar un
sexto botón que comprima el título en 320 px.

Se mantiene el bloqueo/restauración de foco de MUI, nombre de diálogo, cierre
por Escape, foco inicial en Cerrar al entrar y transiciones desactivadas con
movimiento reducido. La barra respeta safe-area inferior y ofrece foco visible.
No se usan focus traps propios ni se altera autoplay. Los atajos globales dejan
el teclado a controles interactivos y diálogos, y respetan eventos consumidos,
modificadores del sistema, repetición y composición.

Fuentes primarias consultadas:

- [Dialog de MUI v6](https://v6.mui.com/material-ui/react-dialog/): comportamiento modal, tamaños y slots existentes, verificados también contra los tipos locales.
- [WAI-ARIA: diálogo modal](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/): foco contenido, cierre y restauración al invocador.
- [WAI-ARIA: botones](https://www.w3.org/WAI/ARIA/apg/patterns/button/): activación por Espacio/Enter.
- [WCAG 2.2: tamaño mínimo de objetivos](https://www.w3.org/WAI/WCAG22/Understanding/target-size-minimum.html): referencia de accesibilidad; los 44 px son la decisión táctil de esta interfaz, no una afirmación de certificación WCAG completa.
- [WHATWG: preload y estados multimedia](https://html.spec.whatwg.org/multipage/media.html#attr-media-preload): se conserva `preload="metadata"`; preparar una fuente pausada no implica iniciar reproducción ni exigir precarga completa.

## Orden y pruebas

1. Reproducir interferencia de teclado con regresiones del provider.
2. Conectar panel compacto, foco y objetivos táctiles al estado existente.
3. Exigir calidad/repetición nativas en todos los proyectos, sin ramas que
   omitan controles móviles. Agregar un cuarto escenario nativo con 320 px,
   paisaje, volumen/mute, preferencias, teclado/foco, axe y un solo motor.
4. Repetir matriz y build sobre las fuentes finales, conservar evidencia y
   documentar límites antes de entrega.

Diez regresiones nuevas fallaron sobre el manejador de teclado anterior.
Después del fix pasaron 3 suites / 22 tests Jest, código 0. El primer lint
detectó autofocus declarativo y atributos/keys incompletos en fixtures; se
corrigieron con foco al entrar al diálogo y fixtures completos. La primera
invocación de navegador sin escalación falló antes de ejecutar tests por
`listen EPERM` en loopback; no es un fallo del producto ni una prueba aprobada.
Las corridas posteriores usan autorización para sus servidores locales.

La primera corrida autorizada de teléfono terminó 4/5, código 1: el primer
caso no encontró la UI del player a tiempo aunque el audio avanzaba. La traza
muestra el módulo de GlobalPlayer respondido y el nuevo chunk de Tune todavía
pendiente; no se observó una excepción de React. No se amplió el timeout. Los
otros cuatro casos pasaron, incluido el nuevo escenario de panel/foco/volumen.
Se revisó visualmente su captura automática a 320 CSS px: controles legibles,
sin solapamiento en el panel; esto no equivale a interacción manual en teléfono.

Esa revisión detectó además `Cargando…` después de cambiar calidad estando
pausado. Una nueva regresión del provider lo reprodujo: tras `canplay`, esperaba
`paused` pero conservaba `loading`. Se corrigió con `loadedmetadata`/`canplay`,
solo cuando la pista y la URL coinciden con la fuente vigente, el elemento
está pausado y no hay intención de autoplay. No sobrescribe errores ni inicia
reproducción. Pasaron después **3 suites / 24 tests Jest**, código 0, incluidos
ambos eventos; ESLint dirigido y sintaxis Node también terminaron con código 0.

La primera matriz completa de esta entrega terminó **23/25**, código 1, en
8,8 minutos: únicamente falló el segundo seek de repetición en Chromium
escritorio/tablet. La traza confirmó que `isVisible(Pausar)` devolvía false
antes de restaurar el árbol accesible tras cerrar el diálogo, por lo que el
test omitía pausar y movía el seek con el audio avanzando. El helper ahora
espera explícitamente la reaparición de la región del player antes de decidir
si debe pausar; conserva las aserciones, el reloj real y los timeouts.

La matriz de **25 casos** (20 nativos y 5 de regresión con motor sustituido)
terminó **25/25, código 0**, en 582378 ms (inicio `2026-09-15T14:49:03.177Z`),
sin omisiones ni reintentos. Cada uno de los cinco proyectos pasó 5/5.
Reporte `/private/tmp/tdf-compact-player-verified/results.json`, SHA-256
`dbd08d0da00ae39e94bf27531864c5e1fdccb97e1c7ff2c0910068713f4567bd`.
Incluye 20 observaciones nativas, 5 informes axe del panel, 5 de la barra y una
captura automática del panel en teléfono. Comando reproducible:

```sh
PLAYWRIGHT_PORT=4188 PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-compact-player-run \
  npm run test:music-player-native
npm test --workspace=tdf-hq-ui -- --runTestsByPath \
  src/player/PlayerProvider.test.tsx src/player/queue.test.ts \
  src/player/analytics.test.ts --watch=false --reporters=default
npm run build --workspace=tdf-hq-ui
```

Huellas al iniciar esa repetición (SHA-256, no un commit). Runtime y harness
de navegador permanecieron sin cambios; el archivo unitario se amplió durante
la corrida para reproducir el caso de listbox descrito después:

```text
7c441a89f482cb813ccbbef631fa3a4282a6b9e6bcd25fc2e03a4be475ee7800  tdf-hq-ui/src/player/GlobalPlayer.tsx
55c5647269f92b70daa9019ebcad455d847997c3ca622c28d8354a9cf60afbe6  tdf-hq-ui/src/player/PlayerProvider.tsx
67694dcd08a435dd003edbeb2343a8e5d276e866e3738d8ac217c19f199f1db3  tdf-hq-ui/src/player/PlayerProvider.test.tsx
c8207d26e862fca17a6e09b741eb1cb36ef877cc88284dcdc28eaaedfe21e6c9  e2e/web/music-player-native.spec.mjs
67bc335508139b6555425e0b7b1fde9863887df9fe33d28457860108a7fe7f49  e2e/web/music-player.spec.mjs
```

La revisión del manejador de teclado detectó un caso adicional en los menús de
Select: su listbox se monta mediante portal fuera del diálogo. MUI consume el
typeahead que encuentra coincidencias, pero puede dejar sin consumir una letra
sin coincidencia. La prueba del provider con un portal real reprodujo que `m`
activaba mute en una lista de repetición. Se añadió protección explícita de
listboxes/opciones y un gesto real de navegador: teclear `m` sin coincidencia
en el menú de repetición no debe silenciar. El único cambio de runtime después
de la matriz 25/25 es agregar esos dos roles al filtro de atajos. Las pruebas
agregan también apertura mediante tap en paisaje para contextos táctiles.

La verificación final dirigida selecciona **10 casos** (5 nativos de opciones
y 5 de regresión de teclado/navegación) en los cinco proyectos, con las nuevas
aserciones. No presentar la matriz previa como si hubiera probado ese último
gesto: su reporte y los hashes finales se registran por separado.

Terminó **10/10, código 0**, en 183282 ms, inicio
`2026-09-15T14:59:49.506Z`, sin omisiones ni reintentos. Reporte
`/private/tmp/tdf-compact-player-keyboard-final/results.json`, SHA-256
`95679f959851bb6b5b2e8a21f18a881162ab9bfe2a39e01e62713e45c666e8ff`.
La selección excluye explícitamente los otros 15 casos nativos ya ejecutados;
no se los cuenta como repetidos sobre este último ajuste.

Las 3 suites / **25 tests Jest finales** pasaron, código 0 (5,707 s), sin
omisiones; ESLint dirigido, `node --check` de ambos specs y `git diff --check`
pasaron. Se corrigieron también roles de los fixtures señalados por lint;
no se deshabilitaron sus reglas. La revisión visual de la captura automática
final confirmó que el panel sigue legible a 320 px y ya no muestra el estado
de carga persistente en la barra pausada. No hubo interacción manual en un
dispositivo físico. Se verificó ausencia del directorio sintético propio y
de listener en 4188 al terminar. Se conservan los reportes, sin borrar datos
del usuario ni objetos remotos.

El build final `npm run build --workspace=tdf-hq-ui` terminó con **código 0**:
TypeScript, Vite (12459 módulos, 1 min 3 s) y presupuesto inicial (376939 bytes
gzip, 5 preloads). Se conserva la advertencia de chunks mayores de 500 kB;
no se amplió el presupuesto ni se suprimió la advertencia.

Huellas finales verificadas:

```text
7c441a89f482cb813ccbbef631fa3a4282a6b9e6bcd25fc2e03a4be475ee7800  tdf-hq-ui/src/player/GlobalPlayer.tsx
c350e51a9748580e77d30c2f4f985437e4cd61d51e40c5a9f458db7858b069f2  tdf-hq-ui/src/player/PlayerProvider.tsx
7897078bbb2d11a0ae75964144281120696bb6fadd63907be90daaaa7d614537  tdf-hq-ui/src/player/PlayerProvider.test.tsx
7f3619b850ebe1d347264816795e47098cfb0c06896897656f6673c8c6de8ea5  e2e/web/music-player-native.spec.mjs
67bc335508139b6555425e0b7b1fde9863887df9fe33d28457860108a7fe7f49  e2e/web/music-player.spec.mjs
```

```sh
PLAYWRIGHT_PORT=4188 PLAYWRIGHT_ARTIFACT_DIR=/private/tmp/tdf-compact-player-keyboard-final \
  npx playwright test --config=playwright.music-native.config.mjs \
  --grep 'PW-MUSIC-NATIVE-04|PW-MUSIC-01'
```

## Compatibilidad y límites

Sin migraciones, APIs nuevas, dependencias nuevas ni cambios en
`tdf-global-player/v1`. La normalización existente del motor no cambia. No se
retiran el player legado ni las pruebas anteriores. Los fixtures de API son
explícitos: los medios sí pasan por FFmpeg y el decodificador nativo, pero esto
no certifica API real en navegador, CDN, dispositivos físicos, lector de
pantalla humano ni sesiones prolongadas. La política de permisos del servidor
no cambia. No hay despliegue, commit ni PR en esta entrega.

Rollback: revertir solo los cambios propios de GlobalPlayer/atajos del provider
y sus tests con revisión del diff, sin resetear el árbol compartido. No hay
backfill ni transformación de datos que revertir. Las opciones conservan el
formato persistido previo; un rollback pierde su acceso compacto, no los datos.
