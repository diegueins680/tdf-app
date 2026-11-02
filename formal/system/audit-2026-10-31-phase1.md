# Auditoría formal — Fase 1: inventario y reconciliación de autoridad (2026-10-31)

Encargo: [docs/prompts/formal-validation.md](../../docs/prompts/formal-validation.md).
Esta fase **mide**; no cambia código de producción ni modelos. Los hallazgos de lógica
de esta fase son preliminares: se confirman con modelo o prueba en las fases 2–5.

## 1. Candidato y entorno

| Elemento | Valor |
|---|---|
| Raíz `tdf-app` | `234ff6f7` (main, PR #643) + este informe |
| Gitlink móvil | `7586be92f447faaf05d1f7d7073af6321ad1496a` (inicializado exactamente; ver F-01) |
| TLA+ | `tla2tools.jar` 1.7.2, sha256 `fa18543e…0b22` (coincide con el pin del repo) |
| Alloy | 6.2.0, sha256 `6b8c1cb5…b78d` (coincide con el pin del repo) |
| SMT | Z3 4.13.3.0 + PyYAML 6.0.3 (`formal/system/requirements-verification.txt`) |
| No disponible en este entorno | GHC/Stack, PostgreSQL servidor, simuladores móviles |

## 2. Gates ejecutados

| Gate | Resultado |
|---|---|
| `scripts/specification-inventory.py --check` | PASS tras regenerar; el único cambio fue incluir el prompt nuevo como fuente de descubrimiento (no es un requisito aprobado) |
| `scripts/specification-conformance.py --check` | PASS |
| `scripts/check-public-routes.py` | PASS |
| `scripts/check-generated-api.mjs` | PASS (2 clientes, revisión móvil del gitlink) |
| `npm run verify:formal` | PASS — `audit:formal`: 0 critical, 0 error, 544 warning, 12 024 info |
| `npm run test:formal` | PASS (4/4) |
| `npm run verify:event-operations:formal` | PASS dentro de los límites finitos documentados (§9) |
| `scripts/verification-report.py` | Ejecución correcta; **release BLOCKED**; 11 obligaciones SMT demostradas |
| Tests Haskell, `check-state-machines.py` (PG17), inspección de API compilada | **No ejecutados** (sin GHC/PostgreSQL aquí); quedan para la fase 5 |

## 3. Autoridad

Se adopta la jerarquía ya vigente en [README.md](README.md#authority-and-conflict-resolution):
decisiones de producto aprobadas › ADRs y contratos de dominio › contratos API/datos/formales ›
implementación y tests › documentación general (`specs.yaml` es histórico). Es compatible
con el encargo; donde este dice "lo que tenga más sentido", se aplica el orden del encargo
(seguridad/dinero/privacidad › intención › coherencia › compatibilidad) **solo** cuando no
existe decisión aplicable, y se registra como `AUTHORITY-*` nuevo, nunca en silencio.

## 4. Línea base medida

| Métrica | Valor |
|---|---|
| Requisitos registrados | 159 (126 críticos, 30 no críticos, **3 sin clasificar**) |
| Conformidad | 159 `PARTIAL`; 0 `PASS`, 0 `FAIL` |
| Requisitos **demostrados** (implementación o refinamiento) | **0 / 159** |
| Obligaciones SMT demostradas | 11 (4 aritmética de pagos, 3 autorización, 4 expiración de recuperación) |
| Requisitos críticos sin modelo formal | 54 |
| Requisitos con ambigüedad declarada | 143 |
| Módulos TLA+ / configuraciones / Alloy | 124 / 533 / 2 |
| Pruebas no acotadas de modelos (TLAPS, invariantes inductivos) | **0** |
| Superficies materiales | 2 731, de las cuales **1 631 sin requisito** |
| Operaciones servidas (Servant compilado) | 914, 508 mutantes |
| Mutantes sin mención en ningún requisito (heurística por ruta) | 407 |
| Máquinas de estado declaradas | 20 |

### Superficies sin requisito por zona (las mayores)

| Sin mapear / total | Zona |
|---|---|
| 244 / 520 | `scripts` |
| 175 / 249 | `tdf-hq-ui/src/pages` |
| 134 / 251 | `tdf-hq/src/TDF` (backend) |
| 90 / 134 | `tdf-hq-ui/src/api` |
| 73 / 132 | `tdf-hq-ui/src/components` |
| 61 / 82 | `tdf-hq/test/integration` |
| 33 / 43 | `tdf-mobile/src/lib` |
| 30 / 37 | `tdf-mobile/src/api` |

### Rutas mutantes sin requisito, por prefijo

`/admin` 51 · `/music` 35 · `/merch` 29 · `/fans` 28 · `/directory` 23 · `/internships` 21 ·
`/social-events` 19 · `/public` 18 · `/catalog` 13 · `/marketplace` 11 · `/operations` 11.
(Las 71 sin combinador de autenticación están todas revisadas en `public-routes.json`.)

## 5. Cobertura P0 de dinero

| Flujo | Requisitos | Con modelo | Observación |
|---|---|---|---|
| Cotizaciones Domo (checkout Datafast/PayPal) | 2 (indirectos) | 0 | `Server/DomoQuoteCheckout.hs` (1 407 líneas) sin requisito propio |
| Propinas a artistas (Stripe Connect) | 1 (indirecto) | 0 efectivos | Ver F-06 |
| Datafast | 3 | 2 | Solo arranque, tickets y carrito; no cursos ni Domo |
| PlaceToPay / PayPhone | 2 | 1 | Adaptadores HTTP sin requisito |
| Cursos (checkout) | 3 | 2 | `Commerce/CourseCheckout.hs` sin requisito |
| Tickets | 14 | 8 | El dominio mejor cubierto |
| Reembolsos | 19 | 17 | Bien cubierto |
| Facturación SRI/Dátil | 3 | 3 | `Invoice/SRI.hs` (927 líneas) sin mapeo directo |
| Royalties | 2 | 1 | Sin código que lo use (AUTHORITY existente) |

Módulos de dinero y seguridad **activos** (importados por el servidor) sin requisito:
`API/Payments`, `Server/DomoQuoteCheckout`, `Invoice/SRI`, `Commerce/ProviderAdapter/{Http,PayPhone,PlaceToPay}`,
`Commerce/ProviderEventWorker`, `Commerce/ProviderCapabilities`, `Commerce/ProviderRuntimeConfig`,
`Commerce/CourseCheckout`, `Commerce/MarketplaceSales`, `Commerce/MarketplaceOperations`,
`Commerce/MerchReservationWorker`, `API/CommerceOperations`, `Server/PaymentAvailability`,
`Server/PaymentCapabilities`, `API/Admin` (799), `ServerInstagramOAuth` (1 016), `Contracts/Server`.

## 6. Análisis estructural de las 20 máquinas de estado

Se comprobó para cada una: alcanzabilidad desde el estado inicial, estados no declarados,
estados terminales y estados desde los que no se puede terminar.

| Máquina | Resultado |
|---|---|
| 16 de 20 | Sin anomalías estructurales |
| `music-directory#review` | `pending` es inalcanzable: el inicial es `published` (F-09) |
| `music-directory#claim` | `approved` era terminal: no había revocación de una reclamación aprobada por error o fraude (Q3). **Resuelto** con AUTHORITY-097 (`approved → revoked`, sección 30) |
| `revenue-platform#royalty_statement` | Ningún estado del camino exitoso es terminal; `paid → corrected → review → issued → payable → paid` permite volver a pagar sin un invariante de ajuste (ya `SPECIFICATION AMBIGUITY`; sin código) |
| `operations.control-center`, `SYS-MONEY-003`, `PAY-INVOICE-001` | Formato de transiciones en lista; se analizan en la fase 2 |

## 7. Hallazgos preliminares

Severidades de `audit:formal`. "Preliminar" = evidencia de lectura de código o de datos,
pendiente de modelo o prueba.

| ID | Sev. | Dominio | Hallazgo |
|---|---|---|---|
| F-01 | error | P1 contratos | El gitlink de `tdf-app` fija el móvil en `7586be9`, que solo existe en `feat/music-search-contract-20261026` (y `feat/campaign-feature-registry-20261029`). `main` del móvil (`80d57fd`, 2026-10-21) es ancestro pero le faltan 12 commits, entre ellos los contratos de importes exactos de música (`ec17381`), el recibo de transferencia de tickets (`d832bb3`) y la moderación de memorias de eventos. No hay ramas `release/tdf-shipped-*`. La verificación de clientes generados valida contra una revisión que no está en `main` del móvil; si la rama se borra, el gitlink deja de poder obtenerse. |
| F-02 | error | Garantía | Ningún requisito está demostrado (0/159). Los 124 modelos TLA+ solo tienen chequeo acotado de TLC; no hay invariantes inductivos ni TLAPS. Lo único demostrado son 11 obligaciones SMT. |
| F-03 | error | P0 | Hay módulos de cobro y seguridad activos sin requisito ni modelo (§5), en especial `DomoQuoteCheckout` (1 407 líneas), `Invoice/SRI`, los adaptadores PlaceToPay/PayPhone, `ProviderEventWorker`, `API/Admin` y `ServerInstagramOAuth`. |
| F-04 | warning | P0/P1 | 54 requisitos críticos sin modelo, entre ellos `PAY-CHECKOUT-001/003`, `PAY-DOMAIN-BINDING-001`, `SYS-ESCROW-001..003`, `AUTH-WEBHOOK-001`, `AUTH-CORS-001`, `AUTH-REGISTRY-001`, `AUTH-LOGOUT-001`, `AUTH-ENTRY-002` y `PRIV-WEBHOOK-001`. |
| F-05 | warning | Todos | 407 de 508 rutas mutantes servidas no aparecen en ningún requisito (heurística; el número exacto se fija en la fase 2 con mapeo por manejador). |
| F-06 | error | P0 dinero | `createArtistTip` (`tdf-hq/src/TDF/Server.hs:1214`), `POST /fans/artists/{id}/tips`, ruta pública: (a) no hay requisito ni modelo; (b) no hay clave de idempotencia: cada reintento crea una fila nueva y un PaymentIntent nuevo de Stripe; (c) inserta la fila `pending` **antes** de comprobar que Stripe está configurado, dejando filas huérfanas con un 500; (d) devuelve al cliente el texto de error del proveedor (`"Stripe tip error: " <> err`), lo que probablemente contradice `PRIV-ERROR-001`; (e) acepta propinas para cualquier perfil con cuenta Stripe, sin mirar su estado (Q2). |
| F-07 | warning | P0 dinero | Webhook de Stripe (`SocialEventsHandlers.hs:829`): la deduplicación lee (`getBy`) antes de despachar e inserta (`insertUnique`) después, en transacciones separadas, así que dos entregas concurrentes del mismo evento despachan las dos. Las guardas de `markCourseRegistrationStatus`, `markArtistTipStatus` y la primera rama de `markMarketplaceOrderStatus` leen y luego actualizan sin `WHERE` condicional, a diferencia de la rama de recuperación, que sí usa `updateWhere`. El efecto observable parece acotado (los estados destino son coherentes), pero no hay modelo que lo garantice. |
| F-08 | warning | Registro | Calidad del registro: 3 requisitos con `critical: null` (`DISCOVERY-NAV-001`, `NAV-MENU-COVERAGE-001`, `ART-ENRICH-REVIEW-001`); dominio no canónico `"Artist enrichment"`; 12 IDs cuyo prefijo no coincide con su dominio (`SYS-MONEY-*` y `SYS-ESCROW-*` → `PAY`). |
| F-09 | warning | P2 | `review`: el estado `pending` es inalcanzable (ver §6). O el inicial debe incluir `pending`, o sobra el estado. |
| F-10 | info | P0 seguridad | `requireSeedToken` (`Server.hs:1062`) compara el token con `/=`, en tiempo no constante, mientras el resto del código usa `constEq`. El riesgo es bajo porque la configuración rechaza el token en entornos hosted o de producción. |
| F-11 | info | Herramientas | `audit:formal` produce 12 568 hallazgos (544 warnings). Con tanto ruido pierde valor como señal; conviene separar las reglas de estilo de las semánticas. |

## 8. Preguntas al responsable y decisiones (actualizado 2026-10-31)

| Pregunta | Respuesta o recomendación adoptada |
|---|---|
| **Q1** ¿Qué revisión del móvil está publicada? | Los últimos builds de release firmados son iOS #18 y Android #14 del 2026-10-05, ambos sobre `main` del móvil en `6ebe6c5` (PR #135). Desde entonces no hay builds firmados, así que lo instalado por usuarios es como mucho `6ebe6c5`; el envío a tiendas no es observable desde el repo. El gitlink de `tdf-app` (`7586be9`) es `main` del móvil + 12 commits: diferencia de 6 archivos (tipos generados y publicación de memorias de eventos). En esa revisión pasan typecheck, lint y 653/653 tests. **Recomendación:** (1) avanzar `main` del móvil por fast-forward a `7586be9`; la rama `claude/prompt-formal-validation-b2jmz3` del móvil ya apunta ahí, lista para un PR. (2) Añadir un gate que exija que el gitlink sea alcanzable desde `main` del móvil. (3) Registrar `6ebe6c5` como base instalada y verificar la compatibilidad del backend también contra ese cliente. |
| **Q2** Propinas | Respondida: solo artistas activos o publicados; sin mínimo ni máximo de negocio. Implementada (AUTHORITY-090, PAY-TIP-001). |
| **Q3** ¿Revocar una reclamación aprobada? | **Recomendación:** sí. Añadir `approved → revoked`, solo para administradores estrictos, con motivo obligatorio y auditoría, que retire en la misma transacción los permisos de gestión concedidos por la reclamación. Una reclamación revocada es terminal; quien la hizo puede presentar otra. Requiere migración del CHECK de `directory_claim.status`; pendiente de implementar. |
| **Q4** Royalties | Respuesta: "no lo sé". Evidencia: ningún código del backend lee ni escribe `royalty_statement` ni las tablas de distribución, y la máquina declarada contradice el CHECK de la base (decisión de autoridad existente). **Recomendación:** fuera del alcance actual; clasificarlo como diseño no implementado (`NOT IMPLEMENTED`) y no exigirle conformidad hasta que exista código. Antes de implementarlo hay que añadir un estado terminal de liquidación y un invariante de ajuste que impida volver a pagar un estado ya pagado. |
| Apalache | **Recomendación adoptada** (AUTHORITY-091): v0.62.4, fijado por checksum, con controles negativos obligatorios. |

## 9. Chequeo de modelos de event-operations

`npm run verify:event-operations:formal`: **PASS dentro de los límites finitos documentados.**
TLC ejecutó 486 configuraciones: 123 positivas sin violaciones y 363 controles negativos, que
produjeron el contraejemplo esperado. Alloy ejecutó 30 comprobaciones: 13 aserciones sin
contraejemplo dentro del alcance, 2 escenarios satisfacibles y 15 controles negativos
satisfacibles. Es evidencia **acotada** (`verificado-acotado`), no una prueba (ver F-02).

## 10. Plan priorizado para las fases 2–5

1. **P0 dinero:** escribir requisitos y modelos para las propinas (F-06) y la deduplicación del webhook de Stripe (F-07): modelo TLA+ con entregas duplicadas y concurrentes, controles negativos e invariante inductivo demostrado con Apalache. Después vienen `DomoQuoteCheckout`, `CourseCheckout`, los adaptadores PlaceToPay/PayPhone y `ProviderEventWorker`.
2. **P0 seguridad:** `API/Admin`, `ServerInstagramOAuth` y `Contracts/Server`, y modelar `AUTH-WEBHOOK-001`, `AUTH-CORS-001`, `AUTH-REGISTRY-001` y `AUTH-LOGOUT-001`.
3. **Garantía (F-02):** incorporar Apalache con un pin fijado y empezar con invariantes inductivos de los modelos P0 que ya existen (reembolsos, ledger, admisión de tickets). TLAPS solo para los invariantes de dinero.
4. **P1 contratos:** resolver F-01 según Q1.
5. **Registro:** corregir F-08 y F-09, y sustituir la heurística de F-05 por un mapeo de cada ruta a su manejador.

## 11. Avance de la fase 2 — dinero P0 (2026-10-31)

### Hallazgo nuevo

| ID | Sev. | Hallazgo |
|---|---|---|
| F-12 | error | La ruta de propinas convertía `artistId` en una clave de `artist_profile`, mientras sus rutas hermanas y `apArtistId` usan el **PartyId**. Un cliente que usara el id público enviaría el dinero a la cuenta Stripe de otro artista. No había clientes, operación OpenAPI ni tests que la usaran, así que el defecto era latente. **Corregido.** |
| F-13 | error | `/fans/artists` y `/fans/artists/{id}` (y también `/artists`, la búsqueda, el perfil público por id o referencia y `/fans/clubs`) mostraban **todos** los perfiles de artista, incluidos los que el directorio oculta (privados, en moderación, suspendidos o fusionados). **Corregido** por decisión del responsable (AUTHORITY-092, `ART-PUBLIC-VISIBILITY-001`): una sola regla en `TDF.Profiles.ArtistVisibility`, compartida con las propinas; un artista oculto responde como uno inexistente. Las lecturas admin y del propio dueño no cambian. |

### Cambios

- `PAY-TIP-001` y `formal/payments/`: requisito, modelo y pruebas.
- `TDF.Commerce.ArtistTip`, módulo puro: admisión por listado público, cuenta de destino y comisión.
- `createArtistTip`:
  - `artistId` pasa a ser el PartyId.
  - Responde 503 antes de escribir nada si Stripe no está configurado.
  - Un artista oculto responde 404, igual que uno inexistente.
  - Sin onboarding de cobros responde 409.
  - El texto de error del proveedor solo se registra en el log; el cliente recibe un 502 fijo.
- Webhook de Stripe: las tres guardas leer-y-actualizar pasan a `UPDATE … WHERE` condicional (F-07).
- `scripts/verify-inductive-proofs.sh`, `npm run verify:inductive-proofs` y un paso de CI con Apalache fijado.

### Evidencia

- Apalache demuestra inductivamente `Safe` (ningún resultado terminal se sobrescribe) para el código nuevo, **sin supuestos sobre Stripe**, con hasta 6 entregas concurrentes y ejecuciones de cualquier longitud.
- El código anterior era seguro solo bajo el supuesto de que Stripe nunca emite dos resultados terminales distintos; sin ese supuesto, TLC y Apalache encuentran el contraejemplo.
- Propiedades QuickCheck: la admisión nunca acepta a un artista oculto ni inventa un destino, y la comisión es exactamente el piso de la división y siempre menor que el importe.
- **Primera obligación del sistema demostrada de forma no acotada fuera de SMT.**

### Decisiones del 2026-11-01

- **Q1:** abierto diegueins680/TDF-mobile#178, que avanza `main` del móvil hasta `7586be9` sin merge.
- **F-13:** aplicado; ver la tabla de hallazgos.
- **Evidencia SQL:** la consulta de visibilidad se extrae del código Haskell y se ejecuta en PostgreSQL (`scripts/test-artist-visibility-postgres.sh`, imagen fijada por digest y paso en CI) contra 8 escenarios: sin identidad, pública por sujeto, borrador, pública por enlace legado, privada más pública, fusionada, suspendida alcanzada por los dos caminos y enlace de otro tipo. Un control negativo, que quita la condición de no fusionado, hace fallar el test.

## 12. Fase 2 — cobros con proveedor y Domo (2026-11-01)

| ID | Sev. | Hallazgo |
|---|---|---|
| F-14 | error | En Domo, cursos, tickets, marketplace y service storefront, el handler de captura comprueba la expiración del hold con el reloj de la aplicación y **después**, en otra transacción, admite el pago bajo bloqueo. Si otro request expira el hold entre ambos pasos (con relojes perfectos; el desfase entre instancias lo agrava), la admisión rechaza el pago y el dinero queda capturado sin pago verificado ni excepción de reconciliación: el cliente solo ve un 409. **Corregido** con `recordVerifiedPaymentOrReconcile` (`PAY-CAPTURE-RECON-001`). El contraejemplo lo reproducen TLC y Apalache, y la corrección está demostrada de forma inductiva. |
| F-15 | warning | La máquina de estados de Domo permite `deposit_paid → cancelled`, `in_progress → cancelled` y `balance_due → cancelled` con dinero ya cobrado, pero no especifica qué pasa con ese dinero (reembolso total o parcial, retención). **Pregunta Q5 para el responsable.** |
| F-16 | info | Con un total pequeño y una tasa de depósito baja, el depósito redondeado puede ser 0 y la cotización se rechaza. Es seguro, pero conviene confirmar que es la intención de negocio. |

Evidencia: `PAY-DOMO-QUOTE-001` tiene 4 lemas demostrados en Z3 (sin cota) con testigos y controles de mutación, más propiedades QuickCheck sobre la función real.

## 13. Fase 2 — worker de eventos de proveedor (2026-11-01)

| ID | Sev. | Hallazgo |
|---|---|---|
| F-17 | error | Las escrituras de resultado del inbox de eventos de proveedor (`processed`, `ignored`, `retry`, `dead_letter`) solo exigían `processing_status = 'processing'`. Un worker colgado más de 15 minutos pierde su lease, otro lo reclama, y el primero podía después sobrescribir el resultado del segundo: marcar `dead_letter` un evento que el otro procesó bien, o marcar `retry` y provocar que el evento se procese otra vez. **Corregido** cercando cada escritura con el `attempt_count` reclamado (`PAY-PROVIDER-INBOX-001`). TLC reproduce el defecto; Apalache demuestra la corrección de forma inductiva. El doble efecto del worker reclamado (admisión del pago) queda cubierto por la idempotencia de `PAY-RETRY-001` y `PAY-CAPTURE-RECON-001`. |

## 14. Fase 2 — seguridad P0, primera pasada (2026-11-01)

| ID | Sev. | Hallazgo |
|---|---|---|
| F-18 | info | `/admin/**`: barrido de los 60 manejadores de `ServerAdmin.hs` y `Server/RecordsIngestion.hs`. Todos los manejadores hoja exigen `ensureStrictAdmin` o `ensureModule ModuleAdmin`; los enrutadores solo delegan. No hay rutas admin sin guarda. Convertido en gate: `scripts/check-admin-guards.py` (`AUTH-ADMIN-GUARD-001`) falla en CI si un manejador hoja pierde su guarda; es un chequeo sintáctico, no prueba que la guarda sea la correcta ni que preceda a todo efecto. |
| F-19 | error | `POST /instagram/oauth/exchange` (requiere acceso al módulo de bandeja social): la fila `social_sync_account` es única por `(platform, externalUserId)` y el intercambio hace `upsert` reasignando `party_id` al usuario actual. Dos efectos: (a) si la cuenta de Instagram ya estaba vinculada a otra party, se reasigna en silencio, sin auditoría, y las publicaciones sincronizadas pasan a atribuirse al nuevo usuario; (b) el acceso a la bandeja social lo tienen Admin, Manager, StudioManager, Reception y LiveSessionsProducer; `resolveInstagramDeliveryAccount` y el backfill de Meta eligen "la cuenta conectada más reciente" de toda la plataforma, así que cualquier usuario con acceso a la bandeja social que conecte su propia página convierte su token en el canal de envío de mensajes de la plataforma. **Corregido** (AUTHORITY-094, sección 25): las cuentas conectadas por OAuth pertenecen a la party que las conectó; solo las designadas por un administrador se usan para envío; reasignar a otra party exige rol Admin y queda auditada en `social_sync_account_event`. |

### Preguntas nuevas

- **Q6 (F-19):** ¿qué cuentas de Instagram/Facebook deben poder usarse como canal de envío de la plataforma: solo las configuradas por entorno y las designadas por un administrador, o cualquier cuenta conectada por un usuario con acceso a la bandeja social? ¿Debe poder un usuario reasignarse una cuenta de Instagram que otro usuario ya conectó?

## 15. CI del PR #691 (2026-11-01)

| ID | Sev. | Hallazgo |
|---|---|---|
| F-21 | warning | `CI Safe Install` falla por `npm audit`: aviso alto GHSA-2p57-rm9w-gvfp en `ip`, vía `webtorrent > torrent-discovery > bittorrent-tracker` (dependencia de desarrollo). **No lo causa esta rama**: `main` (`be87455`, PR #646) falla igual desde el 2026-11-01; `main` en `234ff6f7` pasaba el 2026-10-30, así que el aviso es nuevo. Corrección pendiente aparte (actualizar o aislar `webtorrent`). |
| F-22 | info | El informe de garantía (`verification-report.py`) marcaba las obligaciones SMT de autorización y recuperación como `UNPROVEN` por "Receipt source fingerprint mismatch": sus recibos fijan el hash de `tdf-hq.cabal`, que esta rama cambia solo por registrar siete módulos nuevos. `Auth.hs`, `Models.hs`, `RecordsIngestion.hs`, `ServerAuth.hs`, el SQL de expiración y Stack/lock son idénticos a `main`. **Resuelto**: revisión de fuente añadida a ambos recibos según el procedimiento de `auth-predicates.md` y pin refrescado; el informe vuelve a demostrar las 11 obligaciones (`executionPassed: true`, release sigue `BLOCKED` por diseño). |

## 16. Fase 2 — adaptadores de proveedor y precio de cursos (2026-11-01)

- `PAY-ADAPTER-BINDING-001`: propiedades QuickCheck sobre los adaptadores reales de PlaceToPay y PayPhone con respuestas generadas cuya referencia, importe o moneda coinciden o difieren en exactamente un campo. Solo se admite un resultado con coincidencia exacta; PlaceToPay solo reporta éxito con exactamente una transacción aprobada y no revertida; los callbacks de PayPhone nunca se consideran autenticados. La conversión de dinero ida y vuelta es exacta y rechaza fracciones de centavo. Sin hallazgos: el código cumple.
- `PAY-COURSE-PRICE-001`: lemas Z3 sin cota (depósito dentro del total con tasas 1..9999; saldo estrictamente positivo si el total supera 50,00) y propiedades sobre `calculateCoursePrice`. Sin hallazgos de corrección.
- F-20 (info): un curso en modo depósito con total ≤ 50,00 puede tener saldo 0, es decir, el "depósito" es el precio completo. Es seguro; confirmar que es la intención.

Estado de verificación tras esta fase: 22 obligaciones de modelo (TLC positivos, controles negativos y pruebas inductivas Apalache) pasan en `verify:inductive-proofs`; 11 obligaciones SMT del informe de garantía demostradas; 11 lemas Z3 de aritmética Domo/cursos demostrados con 5 testigos; gate de guardas admin con control de mutación.

## 17. Evidencia PostgreSQL e integración de `main` (2026-11-01)

- `scripts/test-provider-retry-runtime.sh` sobre `pgvector/pgvector:pg17` con el esquema completo: 224 casos de dominio de proveedor, 4 de comisiones e impuestos, 16 de concurrencia y reintentos, controles de mutación de expiración de intents, y **318 ejemplos de `provider-retry-runtime` sin fallos**, incluido el nuevo test del lease reclamado (F-17).
- Suite completa de Haskell: 4 077 ejemplos, 0 fallos (21 pendientes por base de datos).
- `main` avanzó hasta `be87455` (PR #646, privacidad del nombre de Google). Integrado por merge normal; inventario y trazabilidad regenerados; los recibos SMT de autorización y recuperación toman los pines de `main` y añaden la revisión del Cabal de esta rama (siete registros de módulos, sin otros cambios).

## 18. Seguridad P0 — webhooks de Meta (2026-11-01)

`AUTH-WEBHOOK-001` pasa de "sin modelo" a evidencia ejecutable:
- Propiedades QuickCheck sobre `verifyMetaWebhookSignature`: acepta exactamente el HMAC-SHA256 del cuerpo exacto bajo el secreto configurado (hex en cualquier caja); 503 sin secreto o con secreto en blanco, diga lo que diga la cabecera; 401 si falta la cabecera, está mal formada, o cambia cualquier byte del cuerpo, del secreto o del digest; solo son posibles éxito, 401 o 503.
- Gate estructural `scripts/check-webhook-signatures.py`: las cuatro rutas POST firmadas por Meta (`/webhooks/whatsapp`, `/hooks/whatsapp`, `/instagram/webhook`, `/facebook/webhook`) las sirve una función que llama al verificador antes de decodificar el cuerpo; control de mutación (quitar la llamada en Facebook) lo detecta. En CI.
- Sin hallazgos de corrección: el código ya cumplía. Queda abierta la ejecución de denegación a nivel HTTP por alias.

## 19. Seguridad P0 — frontera CORS (2026-11-01)

`AUTH-CORS-001`: propiedades QuickCheck sobre los predicados puros de `TDF.Cors` con orígenes generados. `isTrustedPreviewOrigin` acepta exactamente las dos raíces de Pages y una etiqueta de vista previa bajo ellas, solo por https y en cualquier caja; rechaza http, puertos, rutas, etiquetas más profundas, parecidos sin punto (`evil-tdfui.pages.dev`), sufijos añadidos y otros proyectos de Pages. La prueba de origen del borrado de cuenta exige la cabecera, rechaza el origen `null` y nunca se satisface a través del comodín. La derivación del origen desde `HQ_APP_URL` devuelve solo esquema y host y es idempotente. Sin hallazgos de corrección. La construcción del middleware (producción rechaza comodín y allow-all) sigue cubierta por `CorsSpec` y los controles de mutación de `verify-cors-boundary.py`.

## 20. Seguridad P0 — intención de cierre de sesión web (2026-11-01)

`AUTH-LOGOUT-001` pasa de "sin modelo" a demostrado sobre el modelo: `formal/auth/LogoutIntent.tla`. Apalache demuestra de forma no acotada que la restauración automática queda suprimida exactamente desde un cierre explícito hasta una autenticación interactiva nueva, y que un callback con revisión antigua nunca puede marcar como cerrada una sesión nueva. El control negativo (aceptar cualquier revisión) reproduce ese ataque. La correspondencia con `logoutIntent.ts` es por revisión; las ramas de fallo de almacenamiento del navegador siguen cubiertas solo por Vitest. Sin hallazgos de corrección.

Estado: 28 obligaciones de modelo en `verify:inductive-proofs` (cuatro modelos, todos con prueba inductiva y control negativo).

## 21. Seguridad P0 — registro de funcionalidades (2026-11-01)

`AUTH-REGISTRY-001`: el gate `check-admin-guards.py` comprueba ahora también que la entrada `admin.video-sources` del registro concede descubrimiento, vista y administración solo bajo administrador estricto con rol y módulo Admin, y que `RecordsIngestion.requireAdmin` sigue exigiendo exactamente esos predicados. Control de mutación sobre el registro verificado. Sin hallazgos.

## 22. Contratos P1 — gate del gitlink móvil (2026-11-01)

F-01: nuevo `scripts/check-mobile-gitlink-ancestry.sh` exige que el gitlink de `tdf-mobile` sea alcanzable desde `main` del móvil. Hoy falla (12 commits faltan en `main`), así que corre en CI como advisorio (`continue-on-error`) hasta que se fusione diegueins680/TDF-mobile#178; entonces debe pasar a bloqueante quitando esa línea.

## 23. Propinas — idempotencia por petición (2026-11-01)

Cierra la ambigüedad de `PAY-TIP-001`: cabecera opcional `Idempotency-Key` (1..128 caracteres ASCII seguros), huella SHA-256 de la petición con campos prefijados por longitud, índice único parcial `uq_artist_tip_idempotency (artist_profile_id, idempotency_key)` y migración `2026-11-01_artist_tip_idempotency.sql` con rollback. Misma clave y misma petición → se devuelve la misma propina y su PaymentIntent, recuperando el `client_secret` desde Stripe (no se almacena). Misma clave con otra petición, intento fallido o intento sin PaymentIntent → 409. Dos peticiones concurrentes con la misma clave las serializa el índice único. Propiedades QuickCheck sobre las reglas puras: la huella cambia con cualquier campo y distingue ausente de vacío; con una fila existente nunca se crea un cobro nuevo. La ruta cambia su superficie compilada (cabecera nueva): la instantánea `compiled-api-surface.json` se regenera desde el binario.

## 24. Decisiones aceptadas por el responsable (2026-11-01)

El responsable aceptó las recomendaciones de la sección 14 y del informe de avance: AUTHORITY-093 (Q5, reembolsos Domo por antelación con política versionada en base de datos), AUTHORITY-094 (Q6, canales de envío solo configurados o designados por administrador; cuentas OAuth son del usuario; reasignación solo por admin y auditada), AUTHORITY-095 (F-16/F-20, mínimo absoluto de depósito y paso a pago completo por encima del 90 %), AUTHORITY-096 (F-21, actualizar `webtorrent` en PR propio). Orden de aplicación: Q6 (seguridad), Q5 y F-16/F-20 (dinero), F-21.

## 25. Aplicación de las decisiones aceptadas (2026-11-01)

- **Q5 / AUTHORITY-093** (`PAY-DOMO-CANCEL-001`): política de reembolso versionada en `domo_cancellation_policy` (una sola versión aprobada, filas aprobadas inmutables salvo retiro; versión 1 sembrada con 31/7 días y 50 %), función pura `domoCancellationRefund` y 5 lemas Z3 sin cota (reembolso dentro de lo cobrado, conservación del depósito, monotonía en la tasa, retención total y nula) con testigos y controles. **Pendiente**: no existe hoy ningún endpoint de cancelación de cotizaciones Domo; la política está lista para que lo use.
- **F-16/F-20 / AUTHORITY-095** (`PAY-DEPOSIT-POLICY-001`): `applyDepositPolicy` (mínimo 5,00; pago completo si el depósito supera el 90 %) con propiedades, aplicada dentro de `calculateDomoQuote` y `calculateCoursePrice` (variantes `...With` aceptan una política explícita). Domo ya no rechaza una cotización cuyo depósito redondea a cero. Pendiente: leer los valores de la política desde la tarjeta de tarifas y la política de cursos aprobadas.
- **Q6 / AUTHORITY-094**: migración `social_sync_account_delivery_designation` (designación por administrador, `connected_by_party_id`, tabla de auditoría `social_sync_account_event`, test en PostgreSQL desechable en CI) y reglas puras `TDF.Social.AccountOwnership` con propiedades (todo cambio de dueño se audita o se rechaza). Cableado: el intercambio OAuth rechaza con 409, antes de escribir nada, una cuenta conectada por otra party salvo que el llamante sea Admin (entonces la reasigna, la audita y le quita vínculo de artista y designación); la selección de cuenta de envío y el backfill de Meta exigen `delivery_designated`; nueva ruta `POST /admin/social/accounts/{id}/delivery` solo para administradores estrictos, auditada. Las cuentas existentes no quedan designadas: un administrador debe designar la cuenta de envío; mientras, se usa solo el token configurado por entorno.
- **F-21 / AUTHORITY-096**: la causa es que la excepción de `audit-ci` para GHSA-2p57-rm9w-gvfp expiró el 2026-10-31. Se verificó que `bittorrent-tracker` 11.2.3 solo importa `ip` en `lib/server/parse-udp.js`, excluido del bundle del navegador, y que `webtorrent` 3.0.21 arrastra la misma cadena; no existe versión corregida de `ip`. La renovación de la excepción en `audit-ci.jsonc` requiere aprobación humana (el clasificador de permisos la bloqueó como posible elusión de CI); queda para el responsable.

## 26. Verificación del cableado de las decisiones (2026-11-01)

- Suite Haskell completa: 4128 ejemplos, 0 fallos, 21 pendientes (requieren base de datos). La instantánea de la API compilada se regeneró con el inspector: la única diferencia era la nueva ruta `POST /admin/social/accounts/{accountId}/delivery` (cuerpo `SocialDeliveryDesignation`).
- Los recibos SMT `auth-predicates.json` y `recovery-expiry-arithmetic.json` vuelven a estar ligados a las fuentes actuales: nueva entrada `sourceContextReviews` que cubre el `cabal` (seis módulos nuevos registrados) y, en el de autorización, `Models.hs` (bloque `SocialSyncAccount` y entidad `SocialSyncAccountEvent`; ninguna entidad que lean los predicados cambia).
- Gates: inventario y trazabilidad regenerados y verificados (`--check`); rutas públicas; barrido de guardas admin (47 manejadores, 0 sin guarda; la nueva ruta exige administrador estricto); firmas de webhooks de Meta; lemas Z3 de cancelación Domo y de cotizaciones; migración de designación en PostgreSQL desechable (ida, vuelta, re-aplicación, 3 formas rechazadas, eventos registrados); informe de aseguramiento con ejecución correcta (el estado `BLOCKED` de publicación es el estado permanente hasta que haya pruebas de refinamiento).
- `ServerAdminSpec` queda mapeado a `AUTH-ADMIN-GUARD-001` y `AUTH-SOCIAL-OAUTH-001` (prueba 403 de designación sin administrador estricto).
- Pendiente para el responsable: renovar la excepción de `audit-ci.jsonc` (F-21); fusionar TDF-mobile#178 y convertir el gate del gitlink en bloqueante. Pendiente técnico: endpoint de cancelación Domo que aplique `PAY-DOMO-CANCEL-001`; política de depósito leída de tarifas/cursos aprobados; pruebas con base de datos para propinas con clave y reasignación OAuth.

## 27. Cancelación de cotizaciones Domo (2026-11-02)

- **Endpoint público** `POST /public/domo/quotes/{id}/cancel` (token de consulta, confirmación explícita) y **endpoint de operador** `POST /admin/domo/quotes/{id}/cancel` (administrador estricto; `canceller` = `customer` o `tdf`). Ambos corren en una sola transacción con bloqueo `FOR UPDATE` de la cotización y su checkout: se libera la fecha, se cancelan cotización, checkout no pagado y retención, y se escribe un registro inmutable `domo_quote_cancellation` con versión de política, días de antelación (períodos completos de 24 h antes del inicio), importes cobrados, retenidos y a reembolsar.
- **Dinero cobrado**: la política aprobada (AUTHORITY-093) fija el reembolso. El operador crea en la misma transacción la solicitud de reembolso por exactamente el importe debido (clave idempotente `domo-cancel:<id>`; la aprobación exige otra party, regla ya existente del almacén de reembolsos). El cliente no es una party autenticada, así que su cancelación registra la obligación y una excepción de conciliación abierta (`domo_cancellation_refund_due`) que el operador liquida llamando al endpoint admin; la vista `domo_quote_cancellation_open_refund` lista las obligaciones abiertas.
- **Invariantes en el esquema**: `retenido + reembolso = cobrado`, `retenido <= depósito cobrado`, registro obligatorio para cancelar una cotización pagada (trigger de restricción diferido), reembolso enlazado solo por el importe exacto del checkout propio, inmutabilidad salvo el enlace único del reembolso, rollback rechazado con registros.
- **Verificación**: propiedades QuickCheck de `assessDomoCancellation`; pruebas Hspec sobre el esquema completo en PostgreSQL (`DomoCancellationRuntimeSpec`, dentro del gate `provider-retry-runtime`): cancelación sin pago, obligación del cliente y liquidación por operador exactamente una vez, retención del 50 % a 10 días, reembolso total por cancelación de TDF, retención total a 2 días, rechazo de cotizaciones inexistentes o vencidas, rechazo del esquema sin registro; ensayo de migración ampliado con sondas de la tabla de política y del registro; lemas Z3 ahora en CI; ruta pública registrada en `public-routes.json`; OpenAPI y tipos del UI regenerados; método `cancelQuote` en el cliente web.
- **Regresión detectada y corregida**: el lote de publicación (`scripts/lib/production-release.mjs`) afirmaba que `social_sync_account` tiene exactamente 13 columnas y 7 claves foráneas en el grupo social; la migración de designación (AUTHORITY-094) añadió 4 columnas y 2 claves. Sin esta corrección el gate `test-provider-retry-runtime` y la publicación a producción habrían fallado. Actualizado a 17 columnas (con tipos y nulabilidad de las nuevas) y 9 claves; verificado aplicando el lote dos veces sobre una base limpia (231 migraciones).
- Pendiente de producto: botón de cancelación en la página pública de la cotización (el cliente ya expone el método).

## 28. Política de depósito con valores aprobados (2026-11-02)

- Los valores de PAY-DEPOSIT-POLICY-001 dejan de ser constantes del código: el `policy_snapshot` de la tarjeta Domo aprobada admite `minimum_deposit_minor` y `full_payment_above_bps`, y cada versión de `course_checkout_policy` los lleva como columnas (migración `2026-11-02_course_checkout_deposit_policy`, con valores por defecto de AUTHORITY-095 para las filas existentes, así que ninguna cotización ya emitida cambia). Se validan al cargar (`validateDepositPolicy`): una tarjeta o política con valores inválidos no cotiza.
- La migración redefine el trigger de inmutabilidad de la política de cursos para que las dos columnas nuevas compartan la inmutabilidad de la fila aprobada; el rollback restaura la comparación original y se rechaza si alguna política aprobada lleva valores distintos de los por defecto.

## 29. Pruebas con base de datos de propinas con clave y vinculación OAuth (2026-11-02)

- La parte persistente de ambos manejadores se extrajo a módulos sin dependencias de proveedor: `TDF.Commerce.ArtistTipStore.reserveArtistTip` (búsqueda por clave, decisión pura de repetición, inserción única y relectura ante carrera) y `TDF.Social.AccountOwnershipStore` (`planInstagramConnections` decide y rechaza antes de escribir; `applyInstagramConnections` hace el upsert y registra el evento de auditoría). Los manejadores de Stripe y de Meta solo orquestan.
- Pruebas Hspec sobre SQLite real: propina con clave reservada una vez, pendiente hasta que exista el intento de pago, repetida con el intento almacenado, huella distinta rechazada, claves por artista, peticiones sin clave nunca deduplicadas; conexión Instagram propiedad de quien conecta y auditada una vez, reconexión del dueño sin evento y sin perder designación, otra party rechazada sin escribir, reasignación por administrador auditada (party origen y destino) y con pérdida de vínculo de artista y designación.

## 30. Revocación de reclamaciones aprobadas del directorio (Q3, AUTHORITY-097, 2026-11-02)

- El modelo canónico `docs/music-directory/formal-model.yaml#claim` añade `approved → revoked` (terminal) y el invariante I17; la relación `allowedClaimTransition` cambia en sincronía (gate `check-state-machines` estático verde, 10 validadores).
- Migración `2026-11-02_directory_claim_revocation`: CHECK de estado con `revoked`, CHECK que exige revisor, fecha y motivo no vacío, y trigger inmediato que rechaza una reclamación revocada con alguna concesión de gestión activa creada por ella. Rollback rechazado si existen revocaciones.
- Manejador `PATCH /directory/admin/claims/{id}/status` con `revoked`: 400 sin motivo; mismo guard de administrador estricto y revisor distinto del solicitante; desactiva las concesiones (`active=false`, `revoked_at`) antes de cambiar el estado; evento `claim.revoked`; repetición observacional; `revoked → approved` es 409.
- Modelo TLA+ `DirectoryClaimReview` extendido con la decisión `revoked` y el invariante `RevocationWithdrawsGrant`; control negativo `RevokeKeepsGrant` (la revocación que no retira la concesión viola el invariante). UI: la cola de reclamos ofrece «Revocar» en `approved` (la prueba de la página deriva las acciones del contrato). OpenAPI y pruebas HTTP reales ampliadas (400 sin motivo, 403 al propio solicitante, retirada de la concesión en la misma transacción, auditoría única, terminalidad); ensayo de migración con sondas del CHECK y del trigger.
- Deriva detectada de paso: `scripts/test-music-directory-migration.sh` (no cableado en CI) no aplicaba `2026-10-25_directory_saved_search_parity` que `main` introdujo con cambios en el manejador de búsqueda, así que el ensayo fallaba con 500 en `/directory/search`. Se añadió la migración a la lista de prerrequisitos; el ensayo vuelve a pasar completo con el binario actual.

## 31. Botón de cancelación, registro de listas y forma del workflow (2026-11-02)

- La página pública de la cotización Domo ofrece «Cancelar cotización» con confirmación explícita y motivo opcional, y muestra el resultado registrado (cobrado, retenido según política, reembolso debido; aviso de que un operador emite la solicitud). Prueba de página: la cancelación no se envía sin confirmar y el resultado se muestra.
- El registro de listas de catálogo (`docs/catalog-persistence`) recibe la sucesión del manifiesto de migraciones (227 → 233 entradas; el prefijo de 227 y toda la cadena de revisiones anteriores se conservan por hash canónico), la sucesión de la lista de acciones de reclamos del UI (añade `revoked`) y la clasificación de ocho constantes de ciclo de vida introducidas por la auditoría (CHECK de estados y tipo suma `Canceller`). Documento `formal-audit-phase2-main-reconciliation-20261102.json`.
- Los tests de forma del workflow de operaciones prohíben `continue-on-error`; el paso del gitlink móvil pasa a avisar desde el shell (sigue siendo consultivo hasta fusionar TDF-mobile#178), lo que vuelve a dejar en verde los jobs de ese workflow que leen el YAML.
