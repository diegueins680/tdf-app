# Auditoría UX/UI TDF — checkpoint del 17 de septiembre de 2026

**PARTIALLY COMPLETE. La auditoría inicial integral sigue abierta.** Este checkpoint
no reduce el encargo a los hallazgos ya observados ni acredita cobertura de las
superficies pendientes. Todos los hallazgos confirmados, incluidos los menores,
sus dependencias y las regresiones introducidas siguen dentro del alcance.

## Continuación — 2026-09-18 04:52 UTC

Release guardado #422 `f19bb8855e8a9bc5b73ee4c3199c631540eb0df9` completado
04:38:48Z. Canary y segunda máquina confirman db/status OK y SHA exacto; snapshot
`vs_bNkX7ky3wqjsZg5nPk5OQJ` completado04:32:12Z. Imagen inmutable
`sha256:ebf4d755ff87d866782540791433a6fdbcbfb7629deaac95e93ac6e3889c792d`.
Read-only04:42:04Z:104 migraciones, lease0. Sin rollback. El túnel local de Fly
falló; el mecanismo documentado usó smoke público fijado por máquina.
`evidence/production-release-f19.json`. Recuperación: artefacto db69 compatible
según preflight, usando de nuevo el release guardado y verificando ledger actual;
no restaurar base ni sobrescribir despliegues posteriores. Handoff anotado en memoria.

Cloudflare f19 `fa76ec18-b275-42e7-842e-18cd54c26c16` SUCCESS. Después #426
se integró como `a81e48ea9e68b495562ae1f6592654c043dc97eb`; la web pública
ya coincide en todos sus assets con su deployment `93ae1f5b-b0c1-43af-b25e-291fb845f18e`.
No se revierte esta entrega independiente. API observada f19; es estado separado.

Apple: confirmación explícita del titular sobre derechos de terceros guardada y
leída por API (`USES_THIRD_PARTY_CONTENT`); categoría Music, precio gratuito y
disponibilidad175 regiones tras release. App Privacy publicada,19 tipos vinculados
a identidad/sin tracking, según código+manifiestos IPA. iOS1.0.1(19) enviado:
App Review **WAITING_FOR_REVIEW**, TestFlight externo **WAITING_FOR_BETA_REVIEW**.
`evidence/ios19-store-submitted.json` y `ios19-beta-submission.jsonl`. No publicación
pública ni aprobación acreditada. Condición no-comerciante permanece activa.

Android1.0.1(10) pasa de draft a rollout completo de Alpha por API; readback
`evidence/android-alpha-release10.json`. Console confirma comprobaciones previas
y cambios en revisión, incluyendo lista68. No se acredita disponibilidad nueva;
cero invitaciones enviadas aún. Se mantienen82 contactos previstos (14 requieren
cuenta Google) y1dirección inválida excluida; nunca publicar la audiencia privada.

#429: revisión encuentra inicialización de pago sin cliente utilizable; se verifica
SDK antes de reservar, se conserva entrada al fallar, se permite cancelar y se
frenan respuestas antiguas/doble envío.17pruebas component/logic pasan; modelo
CheckoutReadiness221estados distintos,3propiedades y liveness condicional pasan,
3controles negativos detectan sus invariantes. Runner TLC1.7.2/Alloy6.2.0 completo
aprobado. Nombres únicos de regiones/progresos ES/EN y locales FullCalendar
registrados; botón pagar localizado. Build/lint/typecheck pasan,364882bytes gzip
con5preloads dentro del presupuesto. Verificación de navegador del sucesor en curso.

Manager/Engineer/Artist/Accounting/Teacher:198llegadas por rol,99rutas×320/1280,
sesión/backend/PostgreSQL17 reales; cero axe/pageerror/5xx/overflow.28llegadas
focalizadas tienen cero axe/pageerror/overflow; integración Google Calendar devuelve
500 por OAuth no configurado en entorno aislado, pendiente de verificación real.
No acredita acciones, combinaciones de roles, estados ni accesibilidad con lector.
La prueba de bundle sin `VITE_API_BASE=/api` consultó rutas raíz del servidor
estático y falló; se corrige el harness, sin contar ese intento como defecto/producto
o evidencia positiva. No hubo escritura productiva en esa ejecución local.

Siguiente: terminar regresiones de navegador y gates, publicar #429/resolver5hilos,
renovar revisión independiente si GitHub la descarta; continuar cobertura inicial.
Tiendas requieren decisiones externas; producción Google exige12personas/14días.

## Continuación — 2026-09-18 04:18 UTC

#422 fusionada por merge normal como `f19bb8855e8a9bc5b73ee4c3199c631540eb0df9`:
head209b21b07, revisión independiente exacta, todos los checks aplicables aprobados,
nueve hilos resueltos. La API asíncrona de GitHub confirmó el SHA; el CLI normal
rechazó la PR por pertenecer a un stack. El sucesor209 añade traducción de errores
de transporte en confirmación de recuperación;10 casos ES/EN,5 perfiles/3 motores
pasan y confirman eliminación del token de la URL. #429 integra ese merge y su
base pasa a main. Backend productivo aún db69: Build Image35306320944 en curso;
no confundir merge con despliegue. Continúa coordinación del release guardado.

Bundle operacional dbf548172: build/typecheck y presupuesto364618 bytes gzip/5preloads
pasan. Manager:198 llegadas(99rutas×320/1280), sesión/backend/PostgreSQL17 reales,
cero axe/overflow/pageerror/5xx. `evidence/manager-runtime-dbf.json`. Engineer en curso;
las llegadas no certifican todas las acciones, estados, permisos de recursos ni WCAG.

Android: CSV derivado de90 credenciales activas→83direcciones únicas con sintaxis
válida; Play rechaza1dominio y14cuentas Google inexistentes. Se crea lista68, se
selecciona para Alpha preservando las listas1+11. Publishing overview indica lista
en revisión. Draft10 sustituye sólo borrador vacío8 y conserva activo4;
`evidence/android-alpha-draft10.json`. Cero invitaciones enviadas todavía:82cuentas
contactables,14 necesitan indicar/asociar una cuenta Google. No publicar PII.

Apple: usuario declara inexistentes groserías/alcohol-drogas/contenido sexual-violento;
cuestionario guardado/readback200, UGC/social/publicación de mensajes declarados
presentes. Enlaces privacidad/borrado guardados. Build19 interno IN_BETA_TESTING,
externo READY_FOR_BETA_SUBMISSION; no revisión externa enviada. Credencial AppReview
existente comprobada por login real y simulador; dos capturas actuales procesadas.
Se revisaron manifiestos reales del IPA19 (`evidence/ios19-privacy-manifests.json`),
no sólo dependencias; AppPrivacy sigue en preparación. No publicación pública.

Fuentes primarias consultadas2026-09-18: [Apple edades](https://developer.apple.com/help/app-store-connect/reference/app-information/age-ratings-values-and-definitions),
[Apple privacidad](https://developer.apple.com/app-store/app-privacy-details/) y
[Stripe SDK](https://support.stripe.com/questions/stripe-mobile-sdk-privacy-details).
La clasificación separa frecuencia editorial confirmada por titular de funciones
observadas; la privacidad incluye SDKs y vínculo de datos a identidad.

## Revisión de fallos de acceso y correo — 2026-09-18 03:40 UTC

Dos observaciones posteriores a3def67665 se reparan en este sucesor: API de auth
expone códigos estables para red/timeout/401/arranque; login/Google/signup traducen
esos códigos conservando detalles útiles de otros errores.27 pruebas focalizadas,
TypeScript/lint y25 recorridos de bundle en5 perfiles/3 motores pasan. Los recorridos
interceptan red/401; conservan formulario y destino. No son OAuth/SMTP reales.

UX029 registra regresión introducida en el CTA compartido: vuelve a «Ver detalles»;
sólo los dos correos de curso usan «Ver detalles del curso». Compilación Stack9.10.3
--fast y8 Hspec/100 QuickCheck de recuperación pasan. No hubo cambio de modelos
ni de transiciones; los gates formales del nuevo head deberán terminar igualmente.
Aprobación3def preservada como histórica; el nuevo código exige revisión exacta.

## Verificación adicional — 2026-09-18 03:38 UTC

- Recepción:198 llegadas a99 rutas×320/1280, sesión/API/PostgreSQL17 reales,
  cero axe y errores de página; se conservan denegaciones según autorización.
  Evidencia operational-reception-after.json. No sustituye otros estados/roles.
- Reconciliados doce IDs históricos sin rehacer fixes: ocho web ya contenidos
  en maindb69, cuatro nativos en móvil1ec9160/PR94.9 suites/47 pruebas web y
  4 suites/60 móviles pasan. Sus límites de runtime/producción siguen explícitos.
- Apple: capturas reales de simulador19/source1ec,1320×2868RGB, sin edición,
  procesadas COMPLETE:2f053679-4332-4145-aef5-17a8ecd2b020 y
  d877801c-3fc0-415b-8215-7766e0f01980; set1204e861-3128-48fb-bfae-e358b30a9ec3.
- Play: servicio activo y envío exitoso; build10 internal DRAFT, sin testers
  asignados a ese canal. Closed testing existente tiene sólo1 inscrito; Google
  exige12 durante14 días consecutivos para solicitar producción. Este bloqueo
  humano es distinto del permiso API ya resuelto. Notas ES/EN preparadas.
- #4223def67665 recibió aprobación exacta; nueva revisión señaló localización
  de fallos de login y CTA genérico de email. Reparación/test en su worktree;
  no se fusiona ni despliega con discusiones o checks pendientes.

## Actualización de gates y tiendas — 2026-09-18 03:28 UTC

La aprobación independiente de #4226479e0350 quedó verificada, pero CI
35302282394 detectó diez fallos de una expectativa E2E antigua: el enlace de
recuperación ahora conserva `lang`. La corrección cambia sólo esa prueba: comprueba
idioma en el enlace, en la petición del correo y al cerrar el diálogo, además del
destino y cuerpo compatibles. Los diez casos ES/EN en cinco perfiles y tres motores
pasan localmente con el bundle de producción. La revisión y CI del sucesor siguen
siendo necesarias; no se vuelve a ejecutar un fallo determinista sin corregirlo.

Play Console confirma cuenta de servicio Active y sólo app com.tdf.records,
seis permisos de publicación solicitados y tres implícitos, sin admin/finanzas.
Android envío f22acacc-5d30-4659-9078-a58f056f859b FINISHED: build1.0.1(10),
canal internal, estado DRAFT. No equivale a rollout ni disponibilidad pública.
iOS envío7233bd21-1d94-424c-b792-c0234f497949 FINISHED; Apple build
`da874897-8526-480d-96ae-044cf075d491` VALID, versión19, asociado y leído de
vuelta en borrador1.0.1 PREPARE_FOR_SUBMISSION. Capturas/declaraciones/revisión y
publicación siguen pendientes. Simulador source1ec9160 builddd963682 también
FINISHED, SHA25639109632b00852cc4c162ce8b4331440f520d035972a3ec93ddbe07c42ff3d07;
no se confunde con distribución de tienda. Backend productivo permanece db69.

## Cobertura operativa y tiendas — 2026-09-18 03:15 UTC

El sucesor operativo conserva IDs y añade UX022–028 confirmados por navegador:
scroll con teclado, nombres de controles/carga, contraste claro, alertas estrechas,
SDK de pago opcional, landmarks y objetivos táctiles. Código dccd66606 integrado con
#4226479e0350 mediante d4f720e51.198 casos admin y198 customer,99rutas×2anchos;
28 repeticiones focalizadas finales sin axe. Estos son casos de llegada inicial,
no aceptación de todos los estados ni certificación WCAG.15 pruebas existentes,
lint y build pasan; el bundle integrado d4f720e51 usa364500bytes gzip/5preloads y sus28 casos
focalizados pasan. Se normalizó coverage.csv a CSV estándar:411 filas/8columnas,
roundtrip verificado, sin perder evidencia ni definiciones JSON de permisos.
Se excluye como evidencia de persistencia el bundle intermedio mal configurado.

Play Console observado: cuenta7746420275596660022, com.tdf.records ya en Closed
testing. La cuenta de servicio de EAS faltaba por completo; ahora se añadió y se
leyó estado Active con acceso sólo a TDF. Seis permisos solicitados por Expo más
los tres permisos de lectura/políticas/enlaces que Play implica automáticamente;
Admin, finanzas, pedidos y respuestas a reseñas permanecen desmarcados. Envío del
build10 actualizado:f22acacc-5d30-4659-9078-a58f056f859b en ejecución; no publicación
inferida. iOS19 subido vía7233bd21-1d94-424c-b792-c0234f497949; falta verificar
processing y actualizar asociación del borrador. Simulador del mismo source:
dd963682-9ce6-442b-a7a9-6d178860c15e solicitado tras fallo TLS de upload; no se
redujo integridad/validación. Maestro en dispositivo6.9 tuvo XCTest killed antes
de iniciar; no se acredita QA por ese intento.

#4226479e0350 tiene las tres discusiones resueltas, CI en ejecución y requiere
renovar aprobación independiente. Producción sigue db69 según el último release
verificado. No compete con el operador de identidad ni inicia otro rollout aún.

## Checkpoint de continuación — 2026-09-18 03:10 UTC

**PARTIALLY COMPLETE.** #422 f935ba1e6 recibió aprobación exacta y todos sus
checks aplicables pasaron; tres observaciones posteriores exigieron este sucesor:
credencial Live Session independiente de cookies, idioma en correo/enlace de
recuperación y políticas inglesas para locales web sin traducción propia. Se
preserva la aprobación como evidencia histórica, no como aprobación del nuevo código.

- Recuperación: query `locale` opcional, cuerpo `{email}` compatible; correo y CTA
  ES/EN, enlace `lang`, destino seguro existente.8 Hspec/100 QuickCheck;12 pruebas
  de contexto/idioma/políticas. Chromium, Firefox y WebKit abren el enlace inglés
  desde almacenamiento vacío y previamente español.
- Live Session: transporte con código explícito sin cookies ni mutación de sesión
  global, formulario público sin lecturas/escrituras CRM previas, borrador por
  propietario verificado/variante y descarte de recibos tardíos.15 pruebas focalizadas
  finales; TLC9+42 estados, controles negativos y runner Alloy completos. El navegador
  confirma fila PostgreSQL17 bajo la cuenta del código mientras la cookie de otra
  cuenta permanece intacta. UX021 confirma incompatibilidad `null` del contrato:
  cliente omite opcionales para backend antiguo y backend nuevo acepta null sin
  permitir aliases contradictorios;33 Hspec y HTTP/PostgreSQL de músico+canción pasan.
- Web completo antes de los últimos cambios pequeños:217 suites/2085 pruebas;
  las pruebas afectadas posteriores pasan. Build final5 preloads/364390 bytes gzip,
  bajo410KiB; quality:repo pasa tras actualizar la aserción estática obsoleta del
  orden de URLs legales. No se considera esa aserción un modelo formal.
- Móvil PR94 fusionada como f2b4823d5c79a5d8c04b6ff4e3f42f5f2588fde9;
  gitlink publicado1ec9160ebcec19fff406c11b09ad0dc39bf76203 compatible con el
  OpenAPI de esta rama.77 suites/461 pruebas y release:check pasan. Nuevos builds:
  Android1.0.1(10)89431913-7993-47bc-932f-5652c017a652;
  iOS1.0.1(19)1fc2fc0b-f93a-4d11-8eb6-2b7f97dc0207 FINISHED y subido a Apple.
  Ni subida ni build firmado acreditan revisión/publicación.
- Android: tras confirmación de permisos, envío f8eba9b9-b3a9-4b8c-9e82-2ad0fafc7864
  volvió a fallar `The caller does not have permission`. Inspección real de Play
  cuenta7746420275596660022: appcom.tdf.records existe en Closed testing, pero
  Usuarios y permisos sólo contenía al titular. Se está corrigiendo el acceso de
  la cuenta de servicio existente exclusivamente a esa app. No crear otro paquete.
- Cobertura operativa independiente en worktree ux-operational:198 casos admin y
  198 customer con sesión/backend/DB reales,99 rutas×2anchos; customer sin axe.
  Correcciones operativas de nombres, teclado, reflow, contraste y carga pendientes
  de PR sucesora.28 casos focalizados posteriores sin axe. Calendar500 corresponde
  a OAuth no configurado en entorno aislado; /docs en un intento anterior tuvo
  proxy OpenAPI incorrecto. Un bundle intermedio omitió VITE_API_BASE: no se cuenta
  como persistencia. La cobertura411 sigue parcial (otros roles/estados/nativo).

Producción backend sigue verificada en db69, sin otro rollout de esta rama.
Recuperación: informe protegido y snapshot ya registrados abajo; ninguna migración
histórica se cambia y no se necesitó rollback. Siguiente acción: publicar este head,
resolver las tres observaciones con evidencia, obtener revisión exacta y gates;
continuar sucesor operativo y tiendas. El operador de identidad sigue en su propia
rama; volver a comprobar coordinación/lease antes de otro rollout.

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

Conciliación de concurrencia: el bot publicó775934f75 sobre#422 durante el push.
Su filtro artist/band contradecía el contrato artist-only ya aprobado en#406.
Se conserva su atribución mediante merge, se reutilizan sus fixtures person-only,
mixed y canonical-person, y se conserva el handler exacto de main. La prueba
HTTP/PostgreSQL17 ampliada pasa: no selecciona bandas/personas, no elude un
canonical inválido, mantiene idempotencia y no concede gestión. Evidencia
[e2e de conciliación](evidence/artist-claim-kind-union-http.log).

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

### EXP-01 contract recovery — 2026-09-18

Current source lacked the experiment routes/config/handlers already consumed by the
mobile client. Models, DTOs, migration and recorded production migration existed;
this was an integration gap, not a request to launch the experiment. Focusedb62895313
restores the historical contract, adds serialized authoritative eligibility, and
keeps deployment false. Original migration/checksum/introduction commits are intact.
Current main4b0bc6ed7 (#428) was merged normally as0ebdb180d before regenerating the
mobile contract, preserving the newer notification endpoints. The initial generated
mobile diff would have removed274notification lines and was discarded; the integrated
schema adds only116experiment lines. Existing mobile19/10 source1ec artifacts are
unchanged by TypeScript-only contract declarations.

See the formal README and experiment-http-runtime.mjs for bounded properties and
actual PostgreSQL concurrency evidence. Pre-integration build/Hspec2578(0fail,1PG-only
pending), HTTP/PG,65release controls and full formal runner pass. Post-integration
checks, mobile companion merge, root review/merge and guarded paused deployment
remain outstanding. This finding is not accepted or released yet.

Mobile companion#98 passed validate/Datadog and merged after a fresh thread/head/base
check. Parent intentionally pins published/tested34971c451d443fd4805d91f57ac310a732497dd8,
which adds only generated experiment declarations to notification main5faeeed; it
does not import the separate provider-linking#97 runtime merely to obtain these types.
TypeScript43native tests and offline Metro pass; this declarations-only change does
not require rebuilding signed binaries. Store review/publication remains separate.
### Radio and modal layering (UX-260917-030)

Initial checkout retry runtime found the global radio layer at1400 intercepting
the modal footer after its error expands. The screenshot is a real compiled-bundle
reproduction, synthetic event/payment transport. All three radio surfaces now use
theme.zIndex.appBar. [MUI primary documentation](https://mui.com/material-ui/customization/z-index/)
consulted2026-09-18 specifies appBar1100,modal1300,snackbar1400; reusing the theme
keeps application controls below the modal. No audio change, provider or charge.

Integrated main a81 in68581b8f, preserving merge ancestry. Actual .gitmodules/tree
pins published mobile30686143f3108c6e2fe0ccbff016f570d2ef51f6, descendant of1ec
with generated contract changes only; existing native19/10 artifacts remain source1ec.
#429 sixth review identifies duplicate activity load labels, corrected distinctly.
Student198 arrivals: one real unnamed progress in /mis-clasificados; four directory
load states now named. Includes before evidence, follow-up runtime pending.
Remaining historical booking/marketplace/onboarding/campaign suites34tests pass.

### Verified review successor — 2026-09-18 05:03 UTC

Integrated b9a654f9b production bundle: TypeScript/build budget364975gzip/5preloads
pass.17checkout component/logic tests pass, including session switch while SDK is
pending.10durable Playwright ES/EN cases on desktop/phone/tablet+Firefox+WebKit
pass: readiness failure retains input,0intents, ordinary retry/cancel clicks remain
unobscured by radio. Screenshots/trace on failure remain configured in the repository.
Six ES/EN calendar/board/activity named-region cases pass with actual isolated API
and locale response fixture; initial English attempts inherited real account Spanish
preference, corrected fixture explicitly, not a product failure.
Public production web a81: English login, signup dialog/legal links, recovery/token
removal pass with storage allowed and denied;0pageerrors, no account/password mutation.
Evidence files distinguish synthetic checkout/locale responses from production reads.

Four slow-response ES/EN browser checks verify distinct activity loading names and
named directory profile loading. `evidence/operational-loading-es-en.json`.
Local integrated catalog gate, repository-quality and final affected-source lint pass.

### Reservation cancellation review — 2026-09-18 05:24 UTC

All applicable CI checks for60d754aab passed and its independent approval remains
recorded, but new threadPRRT_kwDOQPdUrM6joQEq blocks merge. Confirmed regression:
Cancel while `createPaymentIntent` is pending abandons the returned reservation.
The focused regression failed on that head. Cancellation remains available during
SDK loading; all dialog close paths now wait once reservation dispatch begins.
19 component/logic tests, affected lint and TypeScript/production build pass,
364991 gzip bytes/5preloads. CheckoutCancellation adds explicit dispatch/response
states, named safety properties, conditional liveness and an unsafe negative control.
No real payment was made. External interruption and transport ambiguity are outside
this small model, as documented in the formal record.

ReadOnly and Intern each completed198 real isolated API arrivals without observed
axe/page errors or overflow. StudioManager198 produced two observations under review:
transient button contrast on /label/artistas and unnamed teacher loading. These are
not acceptance for every role combination, resource action, native state or assistive
technology. Google still shows Android10 and the68-account list in review; no tester
email has been sent. Apple19 remains submitted, not public availability.

Current bundle rerun:10/10 browser cases pass; unsafe cancellation configuration
produces the named abandoned-reservation counterexample. Full pinned TLC1.7.2/Alloy6.2.0 runner passed, including the four checkout
negative controls; no merge or release of this successor is claimed.

### Payment confirmation review — 2026-09-18

PRRT_kwDOQPdUrM6jofR3 confirms a second cancellation boundary: while Stripe confirms
a payment, Escape/backdrop could invalidate its successful completion. The parent
now guards that phase and the child prevents duplicate confirmation before React's
next render.21component/logic tests pass, including delayed success/callback once
and provider rejection recovery. The model now has8distinct states, explicit payment
confirmation, NoLostPayment and its negative control. TypeScript/production build
passes at364988gzip/5preloads. No real charge or broad server recovery proof.

### Remaining initial coverage and web release — 2026-09-18

PR #429 merged normally as c00578da58d3a2a4541fceb08a6d1c463318d8a1 after
exact-head independent approval, applicable CI and resolved review threads. Cloudflare
c44d424e-3e93-4215-90b7-06a1fa8ffb5b succeeded; public assets matched at06:10UTC.
Ten ES/EN checkout cases passed on the deployed bundle across five profiles and
Chromium/Firefox/WebKit. Transport was synthetic, with zero payment intents; this
proves released UI readiness/retry/cancellation, not real card settlement. No duplicate
backend deployment was needed for this frontend-only increment.

A11Y-01 remained defective despite its historical source labels: MUI inputProps named
the hidden input, while each visible combobox announced only its value. SelectProps.labelId
now names each visible status using its row and column, alongside the selected value.
Calendar/creative progress indicators also have names. The actual320/1280 browser
probe confirmed the defect and corrected accessible tree. [MUI Select API](https://mui.com/material-ui/api/select/)
and [WCAG name/role/value](https://www.w3.org/WAI/WCAG22/Understanding/name-role-value.html)
were consulted2026-09-18 and checked against the installed MUI implementation.

The same keyboard probe confirmed UX-260917-032: PageShell's sticky header completely
covered both the first status and notes focus at320/1280. The shared content heading
now scrolls in normal flow; application navigation remains available. Actual after
hit-testing reaches both focused controls, with screenshots before/after. The durable
campaign regression covers all five tabs, row/column names, menu/Escape/next-field
keyboard interaction, focus visibility, axe and reflow. These checks do not substitute
for a human screen-reader session or prove all WCAG criteria.

Remaining role arrivals exposed more UX-260917-023 loading indicators: teachers,
teacher agenda, DDEX inbox, services/revisions, reports and conversations. All eight
additional loaders (including message loading) now have distinct ES/EN names. Fourteen
delayed-response cases pass against the actual isolated session/backend, using only
locale overrides and a synthetic read-only thread for message loading. No messages,
campaigns or budgets were submitted. Final TypeScript/build passes365423gzip initial
JavaScript with five preloads, below the existing410KiB budget; affected lint passes.
No representative field performance data or physical-device validation is claimed.

Apple read-only API at06:23 confirms version1.0.1/build19 VALID and WAITING_FOR_REVIEW;
external TestFlight remains WAITING_FOR_BETA_REVIEW. Play's actual publishing overview
still shows Android10 full Alpha rollout and added tester list in review. The82 prepared
invitations have not been sent; verified tester access is still required first. The
separate notification operator owns the4b0 backend rollout and builds20/11. Preserve
that release coordination and the current Apple review; do not withdraw or duplicate it.

Final campaign runtime:10/10 cases pass across five profiles and three engines,
each checking five tabs. The initial synthetic fixture403 lacked the real scheduling
module; corrected fixture authority before acceptance. MUI's documented accessible
name includes its current value, now correctly asserted. Earlier runs interrupted
for shared-host pressure are not counted as passes. Final14 ES/EN loading cases also
pass. Remaining-role evidence records completed198-case fixtures without claiming
unexecuted actions/states. Transient pipeline-tab focus and label-button contrast
observations did not recur after settling on either viewport; they remain hypotheses.
A confirmed actual navigation500 has its own focused successor PR #432/UX-260917-031.

Checkpoint 2026-09-18 07:06 UTC: all23 remaining role fixtures completed4554
static-route arrival cases (198 each), including StageManager, TourManager, Vendor
and Webmaster. Together with prior role evidence this covers the31 derived roles
at arrival, not all role combinations, dynamic resources or failure states. The CMS
helper contrast observation belongs to an inactive selector; the explicit WCAG1.4.3
inactive-component exception applies (primary W3C documentation checked today).

#431 merged as2f01b20b0 after exact-head approval and all applicable PR gates.
Its automatic image run35315895802 currently has a WebKit recovery-close failure;
backend build is still running. No experiment production rollout is claimed.
The current release baseline remains4b0bc6ed7, and the experiment must remain paused.

Fresh #433 CI exposed an additional horizontal-focus issue in Linux WebKit320px:
the focused notes field remained mostly clipped after Select dismissal. The source
now scrolls focused table controls into view; the existing center-hit regression
is retained. New verification is pending. This extends UX032, not a passing rerun.

Native UX033 is confirmed by the actual iOS accessibility tree: five visible tabs
were announced with a total of12, including hidden routes. Mobile PR101 atafbd5dcc7
corrects visible counts and reactive ES/EN labels, based on parent-pinned34971c451.
Hosted checks pass; corrected EAS simulator4696e1b9 is FINISHED. Actual corrected
simulator execution and signed production distribution remain pending. Prior simulator
variant1.0.1/build1 does not prove App Store build19 acceptance.

The nearest-scroll correction passes10 local cases in five profiles/three engines;
production build365428gzip and affected lint pass. Linux CI must confirm the same
regression on the successor head. Synthetic fixtures prevent campaign submissions.

## Checkpoint — 2026-09-18 07:36 UTC

#431 is deployed as2f01b20b0c2a2e2088570c3dc5deba6197266452 through the guarded
release lane: complete07:32:24Z, both machines healthy on the immutable artifact,
107migration ledger rows, no lease remaining and no rollback. Fresh snapshot
vs_OzXgpX4lyQnsnqNk2YZ6Vb9 completed07:25:22Z. Existing discovery/autopublish
settings remain true; onboarding experiment remains false. Actual legitimate demo
GETs return200 for persisted onboarding and paused assignment, with no assignment
or exposure; production assignment table remains empty. Public health/version
still match07:36UTC. Rollback reference is the prior compatible4b0 image captured
in the guarded report; no SQL rollback is needed for this contract-only release.

#433 merged6d49c0f055b24743a13991a5fbb94e3774c07c9a after exactdabb approval,
all applicable checks and no unresolved threads. Cloudflare345dbb80 succeeded;
public assets match the immutable deployment. Ten production-bundle campaign
cases pass in5profiles/3engines; transport is synthetic. Final local bundle365447
bytes gzip remains below410KiB. The earlier Firefox axe timeout under simulator
pressure passed unchanged after the owned simulator was shut down.

Mobile101 mergedebec681092ba12a9f8a58019e35ae52a6949eb6f. This increment pins
published source667193e15517ecb087fbbacf6041b1296814da78, preserving the349 API
contract and excluding unrelated provider/intake runtime changes from mobile main.
The actual afbd simulator artifact has identical application code; authenticated
return/relaunch and ES→EN→ES visible-tab labels passed, Spanish restored. Source
667 adds only the durable flow/evidence. iOS21 artifact0771998e is built and verified,
submissionc06c203d scheduled; Android12d16877f8 is building. These are separate
from Apple19 still waiting for App Review/beta review and Android10 now active in
Alpha. No current accessibility-fix store publication or human reader/device claim.

Play Console now verifies Alpha10 active in177regions and the saved68-person
registered-user list selected alongside the prior1/11 lists. Current active-account
recipient set matches the prior83 after reserved-address exclusions. The authorized
82-recipient invitation run is in progress with per-recipient receipts; never retry
an ambiguous DATA result or resend an accepted message. Private audience stays off
Git. Real12-person/14-day testing and production access remain external gates.

#432 successor9213b1334 also repairs UX034: actual HTTP proved web preference keys
were rejected by the strict decoder. Canonical/legacy schema compatibility and
negative/generative cases are implemented; hosted corrected HTTP gate and renewed
exact-head review remain required. Do not claim the earlier failed gate passed.
Coverage remains bounded:31-role static arrivals, targeted actual isolated journeys,
models and controlled browser cases are not every dynamic resource/state, physical
platform, human screen-reader session or representative field-performance evidence.

Invitation batch completed07:38:54UTC:80SMTP-accepted,2recipient-address rejections,
0ambiguous and0unattempted among82 intended recipients. One invalid-domain and two
reserved addresses were excluded earlier. No accepted invitation was resent. The
private ledger/audience and retry safeguards are durably stored with owner-only
permissions; delivery, reading, enrollment and actual testing are not inferred.
iOS21 submissionc06c203d FINISHED (upload), not App Review/publication. Android12
buildd16877f8 FINISHED; signed AAB verification and submissionc41e9287 both completed. Play confirms versionCode12 and the exact local SHA-256; the internal release remains draft. No Alpha12 rollout or public availability is claimed.


Checkpoint 2026-09-18 07:56 UTC (supersedes earlier store next actions):
- Root main advanced to f1ff05e6f (#430); its mobile pin7b7ecaf includes explicit Google account creation/linking. #435 must preserve that current contract when reconciling its667 tab fixes; do not downgrade to667 or import the unrelated intake stack. Exact06d5 independent approval and all applicable gates passed, but the gitlink now conflicts and one checkpoint review needs correction.
- Android12 Play readback matches AAB SHA-2561beade7326ad630b1e83038ff495dd50fc149c9e4696144d1087798887fdb541. iOS21 is VALID/IN_BETA_TESTING, build5ebb79bf-8bf0-4802-9dff-42239c97afbb. These are upload/beta states, not public publication.
- NEW release blocker UX035: another operator reproduced an Android11 fresh-install protected-notification-link crash (Maximum update depth exceeded). Source667 retains the identical RootLayout from224, so Android12/iOS21 promotion is held pending candidate mobile#102 and platform runtime verification. An iOS crash is not asserted. Alpha10 and Apple19 existing reviews stay untouched.
- The user asked this operator to perform the physical iPhone Google flow. Fresh xcrun xctrace/devicectl discovery found only this Mac and simulators, no attached physical device. The existing physical-production gate cannot be executed here; no request to repeat credentials or a previously established absence.
- Native cross-device persistence preparation uses an isolated simulator-only configuration targeting localhost18631/PostgreSQL test data. The first archive/upload failed ENOSPC before EAS accepted a build; no simulator execution is claimed. Preserve production demo sessions and do not send them to the isolated API.
- Invitation completion remains80SMTP-accepted/2recipient-refused/0ambiguous/0unattempted of82, one invalid-domain excluded. The private correction list contains the three unusable addresses, never committed. No enrollment or delivery inference.

Next: verify mobile#102 actual Android/iOS cold-start evidence, integrate only the already-required provider contract/tab fixes/cold-start repair, qualify and publish that source before advancing the parent gitlink; rerun affected gates and obtain any exact-head approval required by protection. #432 corrected HTTP/generative CI is still running; its9213 independent approval is current. Continue isolated native progress verification when host storage permits.

Additional initial coverage:92 missing-resource public arrivals across23 parameterized routes, ES/EN and320/1280px, completed against the actual isolated backend and dabb production bundle. No axe violations, horizontal overflow, page errors or HTTP5xx were observed. Evidence public-dynamic-missing-runtime.json; populated resources/actions, complete copy translation and other state combinations remain separate.

Historical navigation gate note:
Navigation HTTP gate35315995605 passed all16 controlled concurrent visits and
second-account isolation, then failed400 on settings. This confirmed UX034:
web/OpenAPI send favorite/pinned/pinOrder but the decoder accepted only npu-prefixed
record names. The successor accepts both strict schemas, rejects mixed/unknown
keys, and adds generated roundtrips plus actual HTTP compatibility/authority cases.
Full corrected HTTP gate is pending; no merge/release claim. NavigationVisit model
and its meaningful unsafe counterexample are unchanged; new wire tests connect the
existing SettingsPreserved property to the actual handler.

Current navigation status: #432 merged5c11577a5d31f079b3e070a7810a6b04a48d99f4 at08:06:26UTC after exact9213 approval/all gates. Corrected actual HTTP/PostgreSQL and generated schema tests pass. Build Image35322697541 pending; release blocked until provider-identity recovery cannot restore an email-authorizing legacy binary. No production mutation by this increment.

2026-09-18 native cross-device checkpoint: actual isolated iOS follow exposed UX037 (wrong artist namespace). Mobile #10460fccd5 now uses the existing FanHub contract. Native follow/reopen and real web reload persist one authoritative completion; accountA is unchanged.498tests/releasecheck plus final10focusedtests pass; see native-canonical-follow receipts and before/after images. #104 retains #102 as a runtime dependency; no Android/physicalGoogle/store qualification is implied. Root #438 merged499e8be7b with74tests/fullmodels and exact35172083a approval; provider production rollout still waits for immutablef1ff recovery image35323829378. Backend remains2f01 at08:51readback.
