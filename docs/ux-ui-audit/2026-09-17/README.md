# Auditoría UX/UI TDF — checkpoint del 17 de septiembre de 2026

**PARTIALLY COMPLETE. La auditoría inicial integral sigue abierta.** Este checkpoint
no reduce el encargo a los hallazgos ya observados ni acredita cobertura de las
superficies pendientes. Todos los hallazgos confirmados, incluidos los menores,
sus dependencias y las regresiones introducidas siguen dentro del alcance.

## Checkpoint de continuación — 2026-09-18 02:04 UTC

**PARTIALLY COMPLETE.** Este bloque sustituye estados históricos contradictorios.
La cobertura inicial de411 entradas continúa abierta; no se limita a las PR activas.

- #406: aprobación independiente exacta de912f8326b, todos los gates aplicables
  y cero hilos pendientes; fusionada sin squash como
  `db69e0534e87ffcb9b3ccf3a856a7fc85e839f16` a01:34:52UTC. Incluye el filtro que
  excluye bandas de las reclamaciones y la protección de activación contra ABA de
  sesión/navegación.13 pruebas de componente, HTTP/PostgreSQL17 y modelos TLC
  con controles negativos pasan. Identidades separadas y revisión de titularidad
  permanecen obligatorias.
- #422 integra main mediante merge31f92a2c5; incorpora la intención invitado→
  registro→confirmación de seguimiento, validación Live Session mediante /session
  y UX015–020. Web:216 suites/2074 pruebas;43 focalizadas; typecheck/build/lint
  y quality:repo aprobados. El último cambio de color conserva5 preloads/364315
  bytes gzip, bajo410KiB. El head publicado anterior8c2e5f9 no contiene todo esto;
  la entrega nueva requiere sus propios gates y aprobación independiente.
- Bundle de producción contra backend/PostgreSQL17 aislados:84 casos de42 rutas
  públicas a320/1280px sin axe, overflow, pageerror ni5xx. Recorrido final en
  Chromium/Firefox/WebKit: signup real, follow, completion autoritativa, reload,
  teclado, dos temas, cuatro anchos y200%texto; cero violaciones axe/pageerror.
  WebKit había conservado texto oscuro tras cambiar de tema; reparación focalizada
  en Typography y evidencia antes/después. No es certificación WCAG, SMTP/OAuth,
  cobertura de todos los roles ni medida de rendimiento de campo.
- Producción: Build Image35295902009 SUCCESS y release protegido terminado a
  02:03:41UTC. Ambas máquinas sirviendo db69e053, db/status ok. Imagen inmutable
  `sha256:bf94f72ea7c45ff30a2fad939179f80592a8eacd51ea02a42f9891c5e7d4df76`.
  Snapshot `vs_alV97Vxv1gacAOlLxK3OGM`, volumenvol_re89q7o0w7ynpx1r,
  creado01:43:42UTC. Preflight, ledger/checksums, esquema, lease, canary y rolling
  aprobados; no rollback. Flags existentes conservados. El túnel Fly falló;
  el mecanismo documentado verificó HTTP público fijado a cada máquina. Webmain
  db69 tiene Cloudflare check84bed2dd-9730-473e-b96f-a4c2f620065b SUCCESS.
  Observación posterior y pruebas de journeys productivos se registran aparte.
- Móvil: gitlink de#422 y ambos builds STORE provienen de
  `efa2555c5400dfa736bc95a12414a790b82adda5`, ya contenido en mainfac7c2.
  Main móvil actual50e680a añade sólo una cabecera opcional de tipos generados
  (PR#92, otro workstream); no se importa ni se atribuye a los artefactos existentes.
  Android1.0.1(9), build1fbcb51e-b035-4ba9-a6f5-b244bf67bae3, sigue sin envío
  exitoso:656ce093-dbb9-4990-9b1e-8df40398f54a falló permisos. El usuario dijo
  «Los completaré» para la lista corregida Expo; falta confirmación de ese cambio.
- iOS1.0.1(18), build154bc17c-ff2d-4f87-b383-75973ee25824, subido mediante
  EASd3c2977f-c136-4c03-81c8-51ecbc8c110f. Apple build
  f72c49ae-2da2-4beb-b0eb-a26bdef674b2 VALID y enlazado al borrador1.0.1;
  descripción/keywords/URLs es-MX guardados y releídos. Free Apps Agreement
  Active observado en Business del equipo83J23NPXG7. Estado PREPARE_FOR_SUBMISSION:
  faltan capturas, cuestionario de edad, privacidad, acceso de revisión y requisitos
  de distribución aplicables. No revisión enviada ni publicación. Simulador de
  fuenteefa build efc9a326-c925-4aa0-9176-767efbc845e0 FINISHED e instalado;
  SHA256 del archivo8e1a446a49bbc84915f300b206945e4614566692dfc8ab5fd562bfc93a641e55.
  Sus recorridos actuales están en ejecución; no acredita dispositivo físico.

Siguiente acción: publicar el incremento integrado de#422 y observar sus gates;
continuar cobertura autenticada/operativa y QA nativa mientras se obtiene revisión
exacta y se completan requisitos de tienda. No reenviar Android sin cambio confirmado.
Para recuperar backend usar el informe del release y la imagen previa compatible;
no reescribir migraciones ni restaurar binarios incompatibles. Snapshot con retención
5d; el rollback no se ejercitó porque no hubo fallo.

Fuentes primarias: [permisos Expo/Play](https://github.com/expo/fyi/blob/main/creating-google-service-account.md),
[acuerdos Apple](https://developer.apple.com/help/app-store-connect/manage-agreements/view-agreements-status),
[builds de Apple](https://developer.apple.com/documentation/appstoreconnectapi/get-v1-builds).

## Actualización verificada — 2026-09-18 00:36 UTC

- El usuario aprobó #422 en `38256cf721d92380c8cfc0b6d19e10bd8703c233`, incluidas
  las traducciones. CI detectó dos fallos exclusivamente en pruebas: contrato fuente
  que esperaba enlaces sólo ingleses y coerciones de tipos rechazadas por ESLint.
  Se corrigieron sin modificar comportamiento de producto; 13 pruebas auth, lint
  completo y `quality:repo` pasan. El siguiente head requiere revisión renovada por
  la protección que descarta aprobaciones al cambiar código.
- #406 conserva aprobación exacta sobre `1fe6195a0`; backend CI continúa pendiente.
- Build Image de main `6b48449e7` terminó exitosamente. No se ha desplegado.
- iOS STORE `154bc17c-ff2d-4f87-b383-75973ee25824`, versión1.0.1(18),
  fuente `efa2555`, terminó FINISHED; Android1.0.1(9) sigue IN_PROGRESS.
  Ningún build acredita subida, revisión ni publicación en tienda.
- HTTP real contra backend integrado y PostgreSQL17 aislado: signup de dos cuentas
  sintéticas sin permisos elevados, intención persistida, primera acción inventada
  rechazada, salida opcional persistida/idempotente, aislamiento de cuentas,
  nuevo login conserva estado y logout revoca su token. Evidencia:
  [onboarding HTTP](evidence/onboarding-isolated-http.log). No SMTP/OAuth ni recorrido
  de navegador/productivo se infieren de esa prueba.
- Backend integrado compiló; cinco ejemplos de recuperación y100 QuickCheck pasan.
  Se conserva pendiente la cobertura integral, release guardado y publicación móvil.

## Checkpoint vigente — 2026-09-18 00:20 UTC (17 de septiembre en Ecuador)

Este bloque y los estados por hallazgo sustituyen las pausas y capacidades
históricas descritas más abajo. **PARTIALLY COMPLETE**: la cobertura inicial
integral y la publicación móvil aún no están cerradas.

- El usuario confirmó la finalización del despliegue ajeno y entregó la coordinación
  de producción. La pausa anterior está levantada; respetar la lease vigente.
- Main remoto verificado: `437fdddea907ba7a20b8626242bf2aed108d6789`.
  #421 merge `6b48449e7f934e91b9450c44be3b43873922ffd9`; #425 merge
  `437fdddea907ba7a20b8626242bf2aed108d6789`. #423/#424 ya están integradas;
  no se reabren sus workstreams ni se limpian ramas ajenas.
- #406 head `1fe6195a08a73574ee597cc0d227bcdf1a9157b2`: aprobación independiente
  exacta confirmada, observación de reclamaciones reparada y resuelta; CI pendiente.
  Incorpora UX-013/014 y preserva identidades separadas/revisión de titularidad.
- #422 recibe la recuperación de destino en el correo, traducciones ES/EN y
  políticas españolas de la misma versión. Su aprobación anterior no cubre estos
  cambios; necesita gates y revisión exactos nuevos. No atribuirlos al head remoto
  anterior `00b9029a807d275cbb0fc6879d2644d3f2709e44`.
- Móvil #89 merge `d22b49eb68151924617b13cc763178b7fb2cb2ba`; #90 merge
  `9e478efae4bbeebadf3b378250c49070d3fda636`, introduce contrato `c63290b`.
  Nuevo sucesor `efa2555` conserva idioma de políticas y contrato de recuperación;
  publicar las páginas españolas del padre antes de distribuirlo.
- Producción API observada saludable en `9f0da14de84919f8fba391fa752b7df7412a600d`:
  esto no acredita desplegar #421/#406/#422. Imagen de6b48449e7 sigue construyéndose
  en run35286741467. Ninguna mutación backend en esta continuación todavía.
- Web #425: Cloudflare check56e5751e-108c-4bc1-b9d8-e7820dd762f9 y Vercel
  deployment6514224951 exitosos sobre437fddd; https://tdf-app.pages.dev/login
  abre con getters local/sessionStorage denegados, conserva entrada y sin pageerror.
  FanHub con persistencia real autenticada aún requiere observación.
- Staging `tdf-hq-studio-audit-staging.fly.dev/health` responde db/status ok;
  no equivale a recorrido autenticado. Fuente editable, Node, Stack/GHC9.10.3,
  Docker/PostgreSQL17 aislado, tres motores de navegador, Fly y EAS disponibles.
  Disco27GiB; iOS18.3 sim disponible, Maestro2.5.1; idb sin companion.
  No Android/dispositivo físico ni VoiceOver/TalkBack verificados.

Verificación nueva: backend de1fe6195a0 compilado; esquema productivo aislado y
migraciones/reinicio idempotentes pasan; HTTP real de reclamaciones pasa con ocho
solicitudes simultáneas, privacidad, denegación y comprobante persistido.
TLC1.7.2/Alloy6.2.0 actuales y controles negativos pasan, incluyendo49 estados del
nuevo modelo de preparación y contraejemplo al quitar su bloqueo. Los límites
constan en [artist-management-claims](../../artist-management-claims.md).

Recuperación: cinco ejemplos Hspec/100 QuickCheck y30 pruebas API/login/rutas pasan.
Matriz final de bundle de producción20/20 (idioma, recuperación, políticas,
foco/axe; Chromium desktop/teléfono/tablet, Firefox y WebKit). La corrida previa
19/20 midió contraste del fondo atenuado detrás del modal; se conserva. Ahora se
comprueban modalidad, aislamiento del fondo y Tab dentro del diálogo; axe analiza
el diálogo activo y la página completa después de cerrar, sin cambiar umbrales.
No es certificación WCAG integral ni entrega SMTP real.

Suite web final215/215,2.064/2.064 tests; typecheck/lint y backend integrado
se registran por separado al terminar. El registro conserva además los22 IDs
históricos originales para conciliar sus sucesores; no son22 defectos nuevos ni
una orden de reimplementar trabajo ya integrado.

Móvil: release:check y77 suites/458 tests del sucesor pasan. Build iOS INTERNAL
`acce0957-ec70-40a6-8f9e-8192db3b7839`,1.0.1(17), fuente74d784c, instalado;
recorrido ES registro→Safari términos→retorno conserva email sin enviar signup.
Evidencia en este directorio. Ese binario no contiene los cambios posteriores.
#91 ya está fusionada como `fac7c2cfac14db1b81883e184764be7f7d28716a`.
EAS confirmó credenciales remotas de ambas plataformas y aceptó builds STORE de
fuente `efa2555c5400dfa736bc95a12414a790b82adda5`: Android1.0.1(9),
`1fbcb51e-b035-4ba9-a6f5-b244bf67bae3`; iOS1.0.1(18),
`154bc17c-ff2d-4f87-b383-75973ee25824`. Estado inicialNEW; sin envío ni publicación.
La limitación histórica de firma iOS ya no aplica. No hay tienda pública,
revisión aprobada ni rollout acreditados.

Siguiente ejecución: gates/revisión y merge #406; publicar cliente móvil compatible;
merge #422 tras revisión; artefacto inmutable y release backend guardado (snapshot,
preflight, lease, canary, smoke, observación), conservando flags efectivos. Completar
las filas no probadas, conciliación histórica y recorridos autenticados/native;
firmar/subir/enviar/publicar móvil y comprobar disponibilidad por plataforma.
No detenerse en estos incrementos ni declarar la auditoría integral terminada.

Fuentes de esta continuación: [patrón de diálogo W3C](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/)
(fondo inactivo atenuado y foco modal), [Maestro launchApp](https://docs.maestro.dev/reference/commands-available/launchapp)
(`stopApp:false` para retorno sin reinicio), consultadas17 de septiembre2026 local.

## Evidencia histórica anterior al checkpoint vigente

## Estado y procedencia

- Workspace original `main` en `17a33eca11d585d84435af85340beece9b51d14e`, con cambios
  ajenos web/backend/móvil y memoria: conservado. Trabajo aislado en
  `/private/tmp/tdf-ux-ui-20260917`, rama `audit/ux-ui-complete-20260917`.
- Baseline raíz actualizado: `d6244296925e20ccb6b2261290204415201843f3`.
  El remoto avanzó después a `6f77a7eec`; debe conciliarse antes de merge.
- `.gitmodules` enlaza `tdf-mobile` a `diegueins680/tdf-mobile`; GitHub lo identifica
  como `diegueins680/TDF-mobile`. Baseline raíz fija `88751192c758b49edad88131f4f90094d150e7ee`.
  Se calificó móvil main `092785f`; parche móvil publicado `a1791d0` (seguimiento pendiente).
- Se consultaron 72 PRs raíz, 15 móviles y las incidencias #128/#130. Se inspeccionaron
  especialmente #359/#368 (onboarding con dependencias de eventos y gates rojos),
  #406/#87 (activación de artista) y el trabajo concurrente de recuperación crítica.
  No se importaron stacks completos ni se atribuyeron sus pruebas a este parche.
- Protección raíz: una aprobación independiente, descartada con nuevos cambios.
  `enforce_admins=false` no autoriza saltarse esa revisión. Móvil respondió 404 a la
  consulta de branch protection; se siguen exigiendo los gates y validación del encargo.

## Cobertura, roles y límites

[coverage.csv](coverage.csv) conserva las409 entradas iniciales y añade dos traducciones estáticas verificadas (411 filas actuales): 155 funciones registradas,
188 declaraciones de rutas (incluidos wrappers/aliases), pantallas Expo y HTML
estático. **No son 409 recorridos probados ni 409 pantallas únicas.** Por fila se
registran origen, acceso, dispositivos, idiomas y estados requeridos. El inventario
se reproduce con `node scripts/ux-audit-inventory.mjs` desde la raíz.

[role-access.json](role-access.json) ejecuta el evaluador real del cliente sobre
31 roles individuales, todos sus pares, anónimo y autenticado sin grants (498
combinaciones). Agrupa resultados idénticos sin omitir combinaciones. Los módulos
proceden de `modulesForRole` como baseline semilla; **el servidor usa grants
canónicos de la base de datos**, reglas por registro, flags y revisiones. La unión
de roles no implica ampliar todo permiso: strict-admin limita combinaciones.
La matriz no sustituye pruebas HTTP ni cubre todas las combinaciones de tres o más
roles, datos privados, scopes por registro o configuraciones dinámicas de producción.

Evidencia actual de navegador: fixtures sintéticos aislados de login, signup,
recuperación, búsqueda, reflow 320px, entradas, reservas, Domo y marketplace; desktop,
teléfono y tablet Chromium, recorrido crítico Firefox/WebKit. No usuarios reales,
OAuth real, cobros, correos ni modificaciones de clientes. Operaciones internas,
administración y variantes de cada ruta siguen con cobertura runtime pendiente.
No afirmar WCAG 2.2 AA integral por los checks axe de estos recorridos.

## Hallazgos y plan completo

[findings.json](findings.json) es el registro estable con evidencia, impacto,
severidad/confianza, causa, cambio, dependencias, aceptación y método de verificación.
La prioridad de ejecución es:

1. UX-001: coordinar recuperación del backend con el despliegue concurrente.
2. UX-002/003/007: readiness veraz, JSON UTF-8 y acceso sin almacenamiento del navegador.
3. UX-004/005: recuperar consentimiento móvil y coordinar la referencia compatible.
4. UX-006: completar coherencia ES/EN en llegada, acceso y navegación pública.
5. Revalidar cada hallazgo histórico del informe de 2026-09-05 y sus seguimientos;
   conservar IDs históricos al reconciliarlos, sin asumir resuelto por un informe o PR.
6. Completar todos los recorridos pendientes de coverage.csv: permisos múltiples,
   privacidad/bloqueos, reservas, pagos, colaboración, operaciones, administración,
   iOS y Android. Confirmar hipótesis antes de añadir cambios obligatorios.
7. Resolver cada hallazgo confirmado, ejecutar gates sobre la revisión final,
   obtener revisiones requeridas, mergear, actualizar submódulo, staging, despliegue,
   observación y distribución/publicación móvil. Un flag apagado no equivale a rollout.

Hipótesis pendientes: efecto sobre abandono/engagement y rendimiento real. No se
inventaron tasas, entrevistas, p75 ni mejoras causales. Las ideas de nuevas funciones
ajenas al alcance permanecen separadas; no se implementó ninguna.

## Capacidades y evidencia ejecutada

- Dependencias Node web/móvil instaladas. Navegadores Chromium/Firefox/WebKit
  disponibles. Varias acciones requirieron salir del sandbox para red, sockets,
  metadatos Git, Stack y simuladores; no se interpretaron fallos de sandbox como
  fallos del producto. `/usr/bin/java` no funciona; se usó OpenJDK 21.0.12.1 temporal.
- Web baseline: 210 suites / 2.005 pruebas; build y presupuesto inicial pasan,
  357.180 bytes gzip; análisis estático sin hallazgos. Esto no mide LCP/INP/CLS de campo.
- Navegador baseline: 28 aprobadas / 4 omisiones explícitas. Logs preservados en
  [evidence/](evidence/). El storage-denial agregado antes del fix falló 2/2, con
  capturas antes; tras el fix dirigido pasó 2/2. Regresión final ampliada pendiente.
- Móvil viejo: 326/327, timeout de 5 s en TicketCheckout anónimo. Main móvil: 456/456.
  Parche de consentimiento: 457/457 y lint. Al iniciar Metro se generaron tipos Expo
  más estrictos y se descubrió otro bloqueo TS2322 en navegación externa; corrección
  explícita de la rama `web` y revalidación final pendientes. No se borraron tipos
  generados para ocultar el error ni se ampliaron timeouts.
- Readiness: `stack exec -- runghc -isrc -itest /private/tmp/tdf-ux-startup-spec.hs`
  ejecutó dos ejemplos Hspec, cero fallos. El runner temporal importa
  `TDF.StartupResponseSpec`; la suite normal también lo importa desde `test/Spec.hs`.
  Esto no sustituye compilación/gates completos del backend.
- `npm run quality:repo` pasó antes de las correcciones finales. Sus fixtures que
  imprimen git push/release no son operaciones del producto. El análisis heurístico
  llamado `verify:formal` no se cuenta como model checking.
- Metro arrancó en `localhost:8187`; ningún dispositivo se conectó. No había simulador
  arrancado ni dispositivo adb conectado. No hay evidencia VoiceOver/TalkBack ni
  hardware físico. Disco local con ~2,3 GiB libres al comprobarlo: evitar builds
  nativos grandes sin capacidad suficiente.

## Verificación formal real

Se ejecutaron herramientas fijadas existentes, no solo el audit de texto:

```sh
JAVA_BIN=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA2TOOLS_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
ALLOY_JAR=/private/tmp/tdf-event-ops-tools/alloy-6.2.0.jar \
bash scripts/verify-event-operations-formal.sh

TDF_SOCIAL_JAVA=/private/tmp/tdf-event-ops-java/openjdk@21/21.0.12.1/libexec/openjdk.jdk/Contents/Home/bin/java \
TLA_JAR=/private/tmp/tdf-event-ops-tools/tla2tools-1.7.2.jar \
TDF_SOCIAL_RESULTS=/private/tmp/tdf-ux-social-models bash scripts/social/check-models.sh
```

Ambos terminaron con código 0. Eventos: ocho configuraciones TLC positivas, tres
controles negativos que detectaron exactamente la propiedad esperada; Alloy una
situación SAT y ocho comprobaciones UNSAT. Social: cuatro modelos positivos y doce
controles negativos esperados. Logs del runner en evidence; los logs detallados
sociales están en `/private/tmp/tdf-ux-social-models`.

Los límites exactos constan en los `.cfg` y las relaciones Alloy de
`formal/event-operations` y `formal/social`, junto a sus documentos de supuestos.
Se modelan estados finitos, operaciones atómicas y fairness explícita cuando se
exige progreso. No se demuestra red exactamente-una-vez, concurrencia ilimitada,
proveedores reales, accesibilidad ni experiencia subjetiva. **Falta cerrar la
trazabilidad/conformidad de autenticación, onboarding, recuperación y todos los
handlers auditados.** El mero éxito de modelos existentes no verifica el producto
completo ni el despliegue de esos modelos.

## Producción y distribución

Fly autenticado tras intervención del usuario. Estado observado: dos máquinas
`tdf-hq`, imagen `sha256:a480d2cd98ab4475277bbc2362e8f0ca4202b044bc80f23d4fdc8269c36d55ba`,
SOURCE_COMMIT `954e995f1cd08f363afe2b0e34ccf02dcd80ea8c`; fallos de arranque por
cuatro capacidades RSVP. SQL de solo lectura confirmó las filas anunciada/en
venta/en vivo/pospuesta. No se alteraron datos, flags, secretos ni máquinas.

El usuario confirmó **otro despliegue en curso**. La intervención de producción
queda bloqueada hasta su finalización o traspaso. No se debe competir por rollout,
rollback ni leases. Staging real `tdf-hq-studio-audit-staging` existe, pero su última
comprobación muestra JSON `starting` aunque Fly la marca passing: falta validación
actual de servicio y cuentas sintéticas controladas. La app errónea
`tdf-studio-audit-staging` no existe; no se creó infraestructura nueva.

EAS autenticado y proyecto `218aca4d-c096-4892-a353-c1dd7df23448` consultable. Últimos
artefactos observados incluyen iOS interno 1.0.1(17), ID
`5c01ac50-e944-4b8e-80dc-32180115d170`, fuente `43185bb4…`; Android STORE 1.0.1(8), ID
`d785bc2f-f5ce-4193-a4e0-0cb22711a49c`, fuente `7f0a42b6…`. Son históricos; **no prueban
publicación ni contienen este parche**. Submit Android está configurado en
`internal/draft`; iOS usa ASC `6779786470`. No se confunde TestFlight, build STORE,
subida o revisión pendiente con disponibilidad pública.

## Recuperación y siguiente acción exacta

Los cambios de storage y consentimiento no requieren migración; se revierten por
commit o artifact previo compatible. El cambio readiness debe conservarse al
reparar disponibilidad; revertirlo restaura la falsa salud. No revertir la base de
datos ni deshabilitar capacidades para adaptar un ejecutable antiguo sin un plan
revisado. Antes de cualquier backend rollout: snapshot, preflight, imagen inmutable,
lease, canary, smoke fijado por máquina, rolling y observación. Preservar flags
vigentes mediante el canal existente; los informes históricos no autorizan cambiarlos.

Pendientes de cierre: terminar pruebas/revisión exactas; resolver contrato de artista
antes de actualizar referencia móvil; completar cobertura y hallazgos abiertos;
confirmar entrega del despliegue concurrente; validar staging y dispositivos;
construir/subir/enviar/revisar/publicar móviles y verificar URLs/versiones. No hay
merge, despliegue o publicación de esta entrega acreditados en este checkpoint.

## Fuentes primarias consultadas el 2026-09-17

- [WCAG 2.2](https://www.w3.org/TR/WCAG22/): estados identificables, idioma,
  contraste, reflow, foco visible/no oculto; controles automáticos no certifican AA.
- [React Native accessibility](https://reactnative.dev/docs/accessibility): alertas
  y live regions para recuperación; revisar por plataforma con lector real.
- [Expo submission](https://docs.expo.dev/deploy/submit-to-app-stores/) y
  [automated submissions](https://docs.expo.dev/build/automate-submissions/):
  build, upload, TestFlight/internal, revisión y publicación son estados distintos.

## Cierre de verificación del primer incremento

Web final: 210 suites / 2.005 pruebas, cero fallos. Navegador final estable: 38 aprobadas y cuatro omisiones, incluyendo denegación de acceso/operaciones de storage en cinco perfiles, axe tras fuentes/animaciones asentadas y capturas revisadas. El primer recorrido falló antes de la corrección. Móvil PR [#89](https://github.com/diegueins680/TDF-mobile/pull/89), commit `74d784ceb366a3200c1f68ceb6082ef0ba9e2b7c`, gates hospedados validate y Datadog SUCCESS. EAS aceptó build de simulador iOS `acce0957-ec70-40a6-8f9e-8192db3b7839`; pendiente, no firmado para tienda ni publicado. Se retiraron únicamente node_modules temporales móviles de esta copia después de pruebas y subida para recuperar espacio; reinstalar con npm ci para repetir gates.


## Checkpoint de continuación

- PR web [#421](https://github.com/diegueins680/tdf-app/pull/421): checks aún en curso; el control de listas señaló la nueva pareja de sesiones sintéticas del inventario. Se clasifican como constantes técnicas de auditoría, sin cambiar permisos, umbrales ni exclusiones. El submódulo local candidato difiere del gitlink publicado: sus cuatro decisiones de catálogo nuevas y cuatro antiguas no deben confundirse con el único fallo observado en CI del padre.
- EAS iOS simulador **FINISHED**, build `acce0957-ec70-40a6-8f9e-8192db3b7839`, fuente `74d784ceb366a3200c1f68ceb6082ef0ba9e2b7c`, versión 1.0.1 (17), canal preview / INTERNAL. [Build y artefacto](https://expo.dev/accounts/cuco.saa/projects/tdf-mobile/builds/acce0957-ec70-40a6-8f9e-8192db3b7839). Es un ejecutable para simulador, no una publicación en tienda. Instalación/recorrido pendientes: el disco local volvió a 555 MB libres; se pidió liberar 10 GB sin tocar copias de trabajo.
- Dependencias móviles temporales reinstaladas con npm ci. Verificación de repositorio posterior a integrar main aprobada.
- Producción sigue bajo coordinación pendiente con el otro despliegue confirmado por el usuario. No se realizó ninguna mutación productiva de esta entrega.
- Segundo incremento en `/private/tmp/tdf-ux-locale-20260917`, rama `fix/auth-locale-20260917`: traducciones de acceso/registro/recuperación y navegación pública, todavía en implementación y sin evidencia de cierre.

### Ampliación de UX-260917-007

El caso anterior de navegador usaba catálogos vacíos: no cubría la aplicación de preferencias después de recibir idiomas/monedas. El componente real se reprodujo con catálogos sintéticos no vacíos y dos fallos `SecurityError` (getter / escritura); tras proteger la caché opcional, ambas pruebas y las dos de normalización pasan. `import.meta.env` se lee opcionalmente para ejecutar el componente fuera de Vite. Los primeros intentos del harness fallaron por un mock incompleto y por env ausente; esos errores no se cuentan como reproducciones del defecto de storage. Comandos: `npm run test --workspace=tdf-hq-ui -- --runTestsByPath src/contexts/LocalePreferencesContext.storage.test.tsx src/contexts/LocalePreferencesContext.test.ts`. Se añadió el escenario de catálogo no vacío al recorrido de navegador: 10/10 aprobados en los cinco perfiles, con axe y conservación del formulario.

El usuario volvió a confirmar que el despliegue paralelo sigue en curso: continuar sólo trabajo independiente. El artefacto iOS se descargó (49 MB, SHA-256 `e61c0ff2b70bf453405b12324f09d4db2f5a0828bbb258fb8a77e1a220a95b5e`), pero todavía no está instalado/ejecutado. El usuario informó limpieza de disco; la relectura local fluctúa entre 404 MB y 1,6 GB libres, no los 10 GB solicitados.

### Checkpoint: FanHub y primera ejecución iOS

[FanHub: contrato, modelo y conformidad](fanhub-onboarding.md) documenta UX-260917-012, reutilizando únicamente el incremento pertinente de PR #368. Suite web actual 212/212, 2.031/2.031 pruebas; 14/14 recorridos FanHub y runner formal ampliado pasan. Son datos sintéticos y modelos finitos, no persistencia real ni auditoría exhaustiva cerrada. El registro de las PR paralelas de idiomas contiene UX-010/011 y se conciliará al integrar ambas ramas.

El artefacto iOS acce0957-ec70-40a6-8f9e-8192db3b7839 de fuente 74d784ceb366a3200c1f68ceb6082ef0ba9e2b7c se extrajo, instaló y arrancó en el simulador aislado TDF-UX-Audit-20260917 (iPhone 16, iOS 18.3, UDID 70262ECC-FDDB-470A-A73B-4776FAE364C1). [Captura inspeccionada](evidence/ios-74d784c-first-launch.png): bienvenida en inglés según idioma inicial del dispositivo, selector Español/English y acciones crear cuenta/acceder. No se ejecutó registro, OAuth ni lector de pantalla. Se apagó este simulador cuando el disco cayó a 87 MB; la siguiente lectura tras limpieza muestra 6,3 GiB, aún inferior a 10 GB. No se borraron archivos ajenos. Maestro está disponible; idb carece de idb_companion. Android, dispositivos físicos y publicación siguen pendientes.

PR #422 pasó los gates exactos f36a6391c tras repetir sólo los jobs fallidos de instalación nativa transitoria; el log de navegador acredita 66 aprobados/10 omitidos, incluidos los diez casos ES/EN. PR #421 ya tiene backend-quality y quality SUCCESS en 19d8dc830 y aprobación independiente de tdfrecords sobre ese SHA; el merge sigue retenido por coordinación de producción. El usuario reiteró que el otro despliegue continúa: ningún merge que dispare producción ni mutación productiva por esta entrega.
