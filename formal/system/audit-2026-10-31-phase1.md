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

## 32. Registro de referencias a Party y guardia de archivo en las tablas sociales (2026-11-02)

- CI del job `backend-quality` fallaba en «Verify reversible identity reconciliation»: el gate `generate-identity-party-reference-view.mjs --check` exige que la vista `identity_known_party_reference` liste toda columna `PartyId` declarada en los modelos, y las cinco columnas de propiedad social de AUTHORITY-094 (`social_sync_account.connected_by_party_id`, `designated_by_party_id`; `social_sync_account_event.actor_party_id`, `from_party_id`, `to_party_id`) faltaban. Hallazgo adicional: la migración `2026-11-01_social_sync_account_delivery_designation` creó `social_sync_account_event` sin el trigger `identity_archive_reference_guard`, así que una party archivada podía quedar referenciada por un evento de vinculación (las reglas de fusión sí la descubrían por la clave foránea).
- Corrección: el archivo de la migración `2026-09-18_identity_review_dependencies` se regenera con el generador (227 referencias); su checksum anterior queda registrado como `compatibleAppliedChecksums` en el manifiesto para que producción, que ya la aplicó, no la vuelva a ejecutar; la migración `2026-11-02_identity_party_reference_refresh` republica la vista completa e instala la guardia en las dos tablas sociales con el mismo predicado de descubrimiento (clave foránea a `party`, sufijo `_party_id` o registro). Rollback restaura la vista anterior y conserva las guardias.
- Verificación: `scripts/test-identity-reconciliation.sh` completo sobre una base vacía (migración, repetición, concurrencia y privilegios); avance, rollback y avance de la nueva migración sobre el esquema completo con comprobación de los triggers creados (`party_id, connected_by_party_id, designated_by_party_id` y `actor_party_id, from_party_id, to_party_id`); pruebas del manifiesto y del lote de publicación (72) verdes.
- Registro de listas: sucesión del manifiesto (233 → 234) con documento `formal-audit-phase2-registry-refresh-reconciliation-20261102.json`. Es la primera vez que cambia un campo dentro de un prefijo ya revisado (el checksum compatible de la entrada 105); el documento conserva ambos hashes del prefijo (con y sin el campo) y la prueba del registro comprueba que el manifiesto difiere del revisado exactamente en ese campo, de modo que los hashes históricos siguen siendo verificables.

## 33. Estado de cierre e integración de `main` 1b6e970f (2026-11-02)

- `main` avanzó dos veces durante la fase (69b5cec5 → 1b6e970f: corrección de rangos del informe financiero de eventos y diagnósticos del worker de música). Fusionado sin cambios de código propios; el manejador de respaldo de Stripe conserva la forma de actualización condicional de PAY-TIP-001 y compila; 132 ejemplos de las specs tocadas pasan; inventario y trazabilidad regenerados; el gate de superficies nuevas contra `main` exigió mapear el archivo regenerado de la migración `2026-09-18` (ahora en AUTH-SOCIAL-OAUTH-001).
- **Bloqueos de «hecho» que no dependen de esta rama** (acciones del responsable):
  1. `audit-ci.jsonc`: la lista de permitidos del job `CI Safe Install` caducó (F-21); la edición desde esta sesión fue bloqueada por el clasificador de permisos. Hasta renovarla ese job falla en cualquier PR.
  2. Fusionar TDF-mobile#178 (incluye la fusión de `main` móvil f4c7aba) y, después, convertir el aviso del gitlink móvil en gate bloqueante (§22).
  3. Revisión independiente del PR #691: ninguna decisión del registro de listas ni del manifiesto infiere aprobación; todas exigen revisión humana y CI fresca en la cabeza actual.
- **Verificación continua**: cada fusión de `main` repite inventario, trazabilidad, superficies nuevas, gate de listas, manifiesto/lote de publicación, rutas públicas, máquinas de estado y compilación del backend antes de publicar la rama.

## 34. Cierre de F-01: gitlink móvil alcanzable desde `main` (2026-11-02)

- diegueins680/TDF-mobile#178 quedó fusionado (merge commit `bee23755`, aprobación del responsable; la fusión por API exigía una revisión aprobatoria y la protección de rama no admite la del autor). `main` del móvil contiene ahora el gitlink fijado `f4c7aba`, que a su vez incluye `7586be9` y la regeneración de tipos de la auditoría.
- El paso del workflow de operaciones «Check Mobile gitlink reachability from Mobile main» pasa de aviso a bloqueante: `scripts/check-mobile-gitlink-ancestry.sh` falla el job si el gitlink deja de ser alcanzable desde `main` del móvil. F-01 queda cerrado mientras el gate esté verde.
- F-21 cerrado en la misma jornada: la excepción de `audit-ci.jsonc` se renovó tras reevaluar el aviso (`ip` 2.0.1 sigue sin corrección upstream; ruta de dependencia sin cambios); `CI Safe Install` volvió a verde en 8cda1473. Horas después `main` integró el PR propio de webtorrent (#685: paquete npm retirado, bundle de navegador vendorizado con procedencia y verificación `check-browser-vendor.mjs`), que vacía la lista de permitidos; al fusionar `main` se adopta esa versión y la renovación queda superada.
- Pendiente abierto: el paso «Verify canonical provider retry concurrency and compatibility» falló de forma intermitente en CI (dos de tres ejecuciones; pasa en todas las ejecuciones locales sobre base limpia, 333 ejemplos). El paso ahora reemite la sección de fallos de hspec como anotaciones del workflow para que la próxima falla identifique la prueba.

## 35. Integración de `main` dd9adc5d: expiración de sesión de `api_token` (2026-11-02)

- `main` integró #686 (ventana de validez de los `api_token`) con migración `2026-10-31_api_token_session_expiry`, reestructuración del `.cabal` (biblioteca única `tdf-hq` de la que dependen ejecutables y suite de pruebas) y re-anclaje de los recibos SMT. La fusión en la rama de auditoría se resolvió así:
  - Manifiesto: la migración de `main` se intercala en la posición 227, justo tras el prefijo protegido y antes de las siete migraciones de la auditoría (235 entradas). La revisión de `main` (228 entradas, decisión `040c32c5`) queda como prefijo exacto; la vista de la auditoría (manifiesto sin esa entrada, 234 entradas) es byte a byte la revisada en `d9468b76`. Las migraciones son independientes.
  - Registro de listas: decisión sucesora `27def070` con dos padres (`previousDecision` = auditoría, `concurrentReviewHistory` = `main`), documento `formal-audit-phase2-session-expiry-main-reconciliation-20261102.json` con ambos hashes (prefijo 228 y vista 234); la prueba del registro verifica el prefijo de `main` como rebanada y las revisiones previas de la auditoría sobre la vista sin la entrada intercalada.
  - `.cabal`: se adopta la estructura de biblioteca de `main` y se registran los ocho módulos fuente de la auditoría en `exposed-modules` y sus once módulos de prueba en la suite.
  - Recibos SMT (`auth-predicates.json`, `recovery-expiry-arithmetic.json`): `reviewedSources` re-anclado a los archivos fusionados; `sourceContextReviews` conserva las revisiones de ambos lados y añade una entrada por archivo con el hash anterior de cada lado y el fusionado (`Auth.hs`, `Models.hs`, `ServerAuth.hs`, `.cabal`, `identity-lifecycle.md`); los verificadores Z3 vuelven a demostrar las 3 + 4 obligaciones sobre las fuentes fusionadas.
- Verificación local tras la fusión: compilación del backend sobre la nueva estructura, specs de credenciales, servidor y de la auditoría, gate de listas, pruebas del manifiesto y del lote de publicación, verificadores SMT, inventario y trazabilidad.

## 36. Causa de la intermitencia del gate de reintentos de proveedor (2026-11-02)

- El paso «Verify canonical provider retry concurrency and compatibility» falló en tres de seis ejecuciones de CI y nunca en local. El registro del servicio PostgreSQL de la tercera falla mostró la causa: `duplicate key value violates unique constraint "domo_event_quote_runtime_lookup_token_hash_key"` durante la siembra de un fixture de `DomoCancellationRuntimeSpec`.
- Defecto propio de la auditoría: el fixture construía el `lookup_token_hash` (64 hex) repitiendo 32 veces los dos primeros caracteres hexadecimales de un UUID aleatorio, así que solo había 256 valores posibles para una columna única; con una docena de fixtures por ejecución la probabilidad de colisión es del 23 %, coherente con la tasa observada. Corregido derivando el hash del UUID completo (`tokenHash`), tanto para `commerce_checkout_session` como para `domo_event_quote_runtime`.
- Segundo acoplamiento del mismo fixture, visible al reejecutar la spec sobre una base ya poblada: todos los fixtures usaban la `venue_key` por defecto, así que la restricción de exclusión de ventana activa (`ex_domo_event_quote_active_window`) chocaba con cotizaciones pagadas de ejecuciones anteriores en la misma fecha. Cada fixture lleva ahora su propia `venue_key`. Con ambas correcciones la spec pasa sobre una base limpia y sobre una base con restos de ejecuciones previas (18 ejemplos).
- La anotación de fallos añadida al paso no bastó porque el error ocurría en la siembra, antes de la sección «Failures» de hspec; el paso ahora anota también la cola de la salida del gate.

## 37. Hallazgos de la revisión automática (Codex) sobre el PR #691 (2026-11-04)

Diez hilos sin resolver, todos sobre código de esta auditoría. Ocho eran defectos reales; uno ya estaba corregido.

| Hallazgo | Resolución |
|---|---|
| CI: el paso del gate de reintentos capturaba `status=$?` después de un comando que, con `errexit`, ya había terminado el paso | `status=0; … \|\| status=$?`; el informe y las anotaciones se emiten antes de devolver el fallo. Explica por qué las anotaciones de §36 salían vacías |
| Propiedad de cabecera Meta «no hexadecimal» que a veces generaba un resumen válido | el primer carácter se sustituye siempre por `g` |
| Generador CORS con etiquetas solo numéricas | ya corregido (las etiquetas empiezan por letra) |
| Propina con clave: `insertUnique` no es atómico (comprobación y luego inserción) | `INSERT … ON CONFLICT (artist_profile_id, idempotency_key) WHERE idempotency_key IS NOT NULL DO NOTHING RETURNING id`; prueba SQLite con el mismo índice parcial |
| Propina pendiente sin PaymentIntent contestada como «usa una clave nueva» | nueva decisión `TipReplayInProgress`: reintentar con la misma clave; una clave nueva duplicaría el cobro |
| Validador SQL de cotizaciones Domo exige el depósito sin política (AUTHORITY-095) | migración `2026-11-04_domo_quote_deposit_policy_validator` (+ rollback) que replica `applyDepositPolicy` con los valores de la tarifa aprobada; sonda `scripts/test-domo-deposit-policy-validator.sh` (mínimo, umbral de pago total, valores por defecto) en CI; control: la sonda falla con el validador anterior |
| Carrera de vinculación Instagram: plan y aplicación en transacciones separadas | escrituras condicionadas al dueño observado al planificar (`updateWhereCount … AND party_id = ?`, `insertUnique` para cuentas nuevas); un plan obsoleto no escribe nada, el manejador deshace la transacción y responde 409; dos pruebas de plan obsoleto |
| Cancelación Domo frente a captura en vuelo | valla bilateral sobre el bloqueo de la fila del checkout: la cancelación sin dinero rechaza si hay intentos `created`/`requires_customer_action`/`processing`, y la admisión de intentos (`beginPaymentAttemptGuarded` + `domoAttemptAdmission`) rechaza cotizaciones canceladas o checkouts cancelados/vencidos; prueba con PostgreSQL de ambos lados |
| UI: al reabrir una cotización cancelada se perdía el resultado y el aviso decía «fecha retenida, depósito sin pagar» | estado cancelado derivado del estado persistido y recuperación del resultado registrado mediante la repetición idempotente de la cancelación; prueba de página |
| Reembolsos de cancelación Domo sin ruta de aprobación ni ejecución | el manejador compartido de aprobación de reembolsos PayPal reconoce los reembolsos ligados a `domo_quote_cancellation`, exige además el módulo Admin, enlaza el reembolso con la captura PayPal verificada del depósito y ejecuta el mismo flujo; sin cambio de contrato (no exige regenerar clientes ni otro PR móvil). Datafast sigue sin adaptador de reembolso (503) |

El registro de listas recibe la sucesión del manifiesto (235 → 236 entradas, decisión `438668da`, documento `formal-audit-phase2-domo-deposit-validator-reconciliation-20261104.json`).

## 38. Integración del PR #691 y cierre de F-07, F-08, F-09 y F-10 (2026-11-04)

- El PR #691 se fusionó en `main` (`1976242a`) con los nueve workflows en verde; la rama de trabajo se reinició desde `main` para el trabajo posterior.
- **F-10** (`requireSeedToken` y `ensureAdminSeedToken` comparaban el token con `/=`): nueva función `TDF.Auth.secretTokenMatches`, que compara los resúmenes SHA-256 de ambos valores con `constEq`; el tiempo no depende del contenido ni de la longitud del secreto. Ambos guardas la usan; pruebas de igualdad, prefijo, sufijo, vacío y mayúsculas.
- **F-09** (`music-directory#review`: `pending` inalcanzable): el código solo crea reseñas `published` (una interacción verificada, I09) y la moderación las pasa a `hidden` o `removed`. Se elimina `pending` del modelo y del CHECK de `directory_review.status` con la migración `2026-11-04_directory_review_status_without_pending` (+ rollback), que se niega a ejecutarse si existe alguna reseña `pending`. Mapeada a `SYS-STATE-001`; gate estático de máquinas de estado verde.
- **F-08** (registro): `DISCOVERY-NAV-001` y `NAV-MENU-COVERAGE-001` pasan a `critical: false` (navegación); `ART-ENRICH-REVIEW-001` pasa a `critical: true` y dominio `ID` (verificación de identidad de artistas). Los 9 identificadores cuyo prefijo difiere del dominio se conservan: son referencias estables usadas por especificaciones y pruebas, y renombrarlos rompería la trazabilidad sin ganancia semántica.
- **F-07** queda cerrado por lo ya hecho: las escrituras de resultado detrás de la deduplicación son `UPDATE … WHERE` condicionales, demostradas idempotentes por `formal/payments/StripeIntentOutcome.tla` (Apalache, hasta seis entregas concurrentes); un doble despacho no cambia el estado.
- Registro de listas: sucesión del manifiesto (236 → 237, decisión `ef3b8933`).

## 39. Cierre de F-11: señal de `audit:formal` (2026-11-04)

- Medición en `main` `1976242a`: 12 918 hallazgos (599 warnings), de los cuales 11 877 eran `magic-number` y 529 eran `unbracketed-resource`. Una muestra de los segundos mostró que prácticamente todos eran falsos positivos: la regla buscaba `with`, `lock`, `connect` o `acquire` sin distinguir mayúsculas, también dentro de comentarios, mensajes, nombres de pruebas y SQL (`WITH requested AS …`).
- **Regla de recursos corregida:** ahora se aplica sobre el código con comentarios (`--`, `{- -}` anidados), cadenas y literales de carácter reemplazados por espacios (`maskHaskellCommentsAndStrings`, que conserva los números de línea y no confunde operadores como `-->`), y solo sobre funciones que devuelven un handle o una conexión que hay que liberar (`openFile`, `openBinaryFile`, `openTempFile`, `openBinaryTempFile`, `connectPostgreSQL`, `takeResource`). Los pools de conexiones y los `Manager` de http-client quedan fuera: viven lo que vive el proceso o la prueba y reciclan sus recursos ociosos. La liberación se reconoce por `bracket*`, `with*`, `finally`, `allocate`, `onException` o `resourceForkIO`. Resultado en el árbol actual: 0 hallazgos; las adquisiciones reales (`MusicDdexRenderMain`, `Services/Stripe`, `TicketPaymentReceipt`) ya están protegidas.
- **Separación semántica/estilo:** cada regla tiene categoría (`FORMAL_RULE_CATEGORIES`). Semánticas: recursos, mutación de estado sin validar, mapa sin caso por defecto, precondición, frontera de error y exportaciones implícitas. Estilo: números mágicos, anidamiento, funciones largas, trampa booleana, documentación de invariantes, pruebas de propiedades y efectos no documentados. Una regla sin categoría cuenta como semántica, para que nunca se oculte en silencio.
- **CLI:** `audit:formal` acepta `--scope semantic|style|all` (por defecto `semantic`). El informe añade `scope`, `outOfScope` y `byRule` (conteo por regla de todas las categorías), así que la deuda de estilo sigue visible sin tapar la señal. Por defecto: 248 hallazgos (67 warnings) frente a 12 918; `--scope all` reproduce el total.
- Pruebas (`npm run test:formal`, 8): enmascarado de comentarios, cadenas, literales de carácter y operadores; la regla de recursos ignora prosa, SQL y mensajes y marca solo el handle sin liberar; categorías y selección por alcance; CLI por defecto, `--scope style`, `--scope all` y rechazo de un alcance desconocido.
- Requisito nuevo, no crítico: `SYS-VERIFY-AUDIT-SIGNAL-001` (implementación, pruebas y propiedad de la separación y de la regla de recursos).
- CI del PR #701: (1) `specification-contracts` rechazó el recibo SMT de autorización porque `TDF/Auth.hs` cambió (F-10); se re-fija con un registro de revisión: el cambio solo añade `secretTokenMatches` y su exportación, ningún predicado traducido la usa, y quitar esas líneas reproduce `main`. `verify-auth-predicates.py` (3 obligaciones PROVEN) y `verification-report.py` pasan con z3 4.13.3. (2) `backend-quality` falló en `test-studio-internship-audit-api-e2e.mjs`: la prueba lanza a la vez un retest y una reapertura y acepta ambos órdenes, pero luego exigía siempre 10 ejecuciones y 3 retests. Si el retest gana, registra una ejecución más (11 y 4). Ahora los conteos esperados dependen del resultado observado de la carrera; la falla no dependía de este cambio.

## 40. F-03 — emisión de facturas SRI por sesión (2026-11-04)

- **Hallazgo:** `POST /invoices/{sessionId}/generate` creaba una factura nueva y, por defecto, emitía un documento SRI nuevo (factura electrónica, documento legal) en cada llamada. `session_invoice` solo es única por par (sesión, factura), así que un doble clic, un reintento del cliente tras un timeout o una caída después de que el portal autorizara el documento producían facturas legales duplicadas para la misma sesión.
- **Decisión del responsable (2026-11-04):** exigir una clave de idempotencia.
- **Implementación (`PAY-INVOICE-ISSUE-001`, crítico):**
  - Nuevo campo `idempotencyKey` en el cuerpo (16–128 caracteres ASCII visibles), obligatorio cuando se emite al SRI y opcional sin SRI.
  - Antes de crear la factura, el manejador reserva `(sesión, clave)` en la nueva tabla `session_invoice_issue` (migración `2026-11-04_session_invoice_issue` + rollback) con `INSERT … ON CONFLICT DO NOTHING`, guardando el SHA-256 de la sesión y la petición decodificada, sin la clave ni la contraseña del certificado.
  - Misma clave y misma petición: se devuelve la respuesta guardada. Otra petición con la clave: 409. Reserva sin respuesta (en curso, o interrumpida cuando el SRI pudo haber emitido): 409, sin volver a emitir. La reserva solo se libera si falla la creación o el enlace de la factura, antes de ejecutar el script SRI.
  - El cliente web conserva la clave mientras la petición no cambia y usa una nueva tras cualquier edición o tras un éxito.
  - `TDF/Invoice/SRI.hs` queda mapeado a este requisito, lo que cierra la mitad SRI de F-03; queda `Contracts/Server`.
- **Observación sin cambio:** el `total` que devuelve el script es la suma sin IVA que el propio script calculó de la petición, así que la comprobación de total confirma el eco, no el cálculo del portal. Se registra como ambigüedad del requisito.
- **Pruebas:**
  - `SessionIssueSpec` (validación de la clave, invariancia y sensibilidad de la huella, tabla de decisión de replay, y reserva, compleción y liberación en SQLite con duplicado y ámbito por sesión): 54 ejemplos en verde junto con las pruebas SRI y del decodificador.
  - Pruebas web del helper de clave: 3.
  - Migración aplicada hacia delante, revertida y reaplicada dos veces sobre el esquema completo de PostgreSQL; las cuatro restricciones se probaron una a una.
- Recibos SMT: la entrada del `.cabal` (dos líneas de módulo) se re-fija con registro de revisión en ambos recibos; `verify-auth-predicates`, `verify-recovery-expiry` y `verification-report` pasan.
- Registro de listas: sucesión del manifiesto (237 → 238, decisión `050f3093`).
- **Revisión Codex de `a4d97f5e` (dos hallazgos, ambos corregidos):**
  - **P2:** el cliente SRI (RUC obligatorio) se cargaba después de reservar la clave y crear la factura, fuera de la liberación, así que un cliente sin RUC bloqueaba la clave para siempre. Ahora el cliente se carga y la petición SRI completa se valida (`validateSriScriptRequest`) antes de reservar nada: un error corregible es un 400 y la clave sigue disponible.
  - **P1:** el cliente web descartaba la clave tras cualquier HTTP 200, incluso con `sri.ok = false` o un estado no confirmado, cuando el portal pudo haber emitido el documento; un nuevo clic emitía con otra clave. Ahora la clave solo se descarta si no se pidió SRI o el SRI respondió `issued` (`invoiceIssueSettled`). En otro caso, repetir la misma petición reproduce la respuesta guardada, y un nuevo intento exige el botón explícito «Verifiqué el portal: nuevo intento». Se añaden 2 pruebas web.

## 41. F-03 — contratos: almacenamiento y envío (2026-11-04)

- **Hallazgos:**
  - `POST /contracts/{id}/send` respondía `{"status":"sent"}` sin enviar nada; solo comprobaba que el contrato existiera.
  - Los contratos se guardaban como JSON en `contracts/store`, una ruta relativa fuera del único volumen persistente del despliegue en Fly (`/data/assets`), así que cada redespliegue los perdía y sus PDF pasaban a dar 404.
- **Decisiones del responsable (2026-11-04):** enviar de verdad y guardar los contratos en una tabla de la base de datos.
- **`CONTRACT-STORE-001` (crítico):**
  - La migración `2026-11-04_contract_store` crea la tabla `contract`, con restricciones de id no nulo, forma del `kind` y payload objeto JSON. Su rollback se niega a ejecutarse mientras haya contratos.
  - Crear inserta la fila canónica con el creador. El PDF se sirve desde la tabla y cada lectura vuelve a validar el contenido.
  - Un contrato que solo exista en el almacén antiguo de archivos se valida y se importa en la primera lectura.
- **`CONTRACT-SEND-001` (crítico):**
  - El envío valida el destinatario y carga el contrato (404). Sin configuración de correo responde 503 sin registrar nada.
  - Después genera el PDF, inserta una fila en `contract_delivery` antes de contactar SMTP y envía el correo con el PDF adjunto (`sendContractEmail`).
  - Si SMTP acepta, se marca `accepted_at` y se responde `sent` con `deliveryId`. Si rechaza, se registra `failed_at` con un motivo acotado y se responde 502.
- El generador de PDF y el transporte de correo son dependencias inyectadas (`ContractsDeps`), así que las pruebas no necesitan LaTeX ni SMTP.
- **Pruebas:**
  - `StoreSpec` sobre SQLite: crear y leer el PDF; 503 sin registro; envío aceptado con PDF adjunto y entrega registrada; 502 con fallo registrado; 404 sin enviar.
  - PostgreSQL: migración hacia delante, rollback y dos aplicaciones más; cada restricción probada una a una; el rollback rechazado mientras existe un contrato.
- **Nota:** ni la web ni la app móvil llaman hoy a `send`; solo existen sus envoltorios de cliente. Con esto F-03 queda cerrado (DomoQuoteCheckout, adaptadores, ProviderEventWorker, API/Admin, ServerInstagramOAuth, SRI y contratos ya tienen requisito).
- Recibos SMT: la entrada del `.cabal` (un módulo de prueba) se re-fija con registro de revisión. Registro de listas: sucesión del manifiesto (238 → 239, decisión `d7fc1df6`).
- **Revisión Codex de `93d632f2` (P1):** la clave vivía solo en memoria del componente, así que recargar o salir de la página tras un resultado SRI incierto la perdía y la misma factura recibía una clave nueva. Ahora los intentos sin resolver se guardan en `localStorage` por huella (sesión y petición, sin la contraseña del certificado); se restauran al reenviar la misma petición y se borran solo cuando el resultado queda resuelto o el personal inicia un nuevo intento. Si el almacenamiento no está disponible, se mantiene el comportamiento en memoria. Se añaden 3 pruebas web (8 en total). Revisión de `89890965` (P2): se elimina el límite de 50 entradas, que podía expulsar en silencio un intento sin resolver; ahora solo se borra al resolverse o con el nuevo intento verificado (prueba añadida, 9 en total).
- **Revisión Codex de `5e3ba6e2` (P2):** una reserva sin respuesta devuelve 409 en cada reintento; ese camino de error no mostraba la acción «Verifiqué el portal: nuevo intento», y como la clave ahora persiste entre recargas, el personal quedaba bloqueado. La alerta de error ahora ofrece la misma acción verificada cuando hay un intento SRI sin resolver; el reintento normal conserva la clave.
- **Revisión Codex de `2cf457c4` (P1) y decisión del responsable (2026-11-04):** si el almacenamiento del navegador está bloqueado o lleno, la clave de un intento incierto aún podía perderse. El servidor ahora rechaza con 409 una clave nueva para una sesión cuya misma huella de petición ya tiene un intento sin resolver (sin respuesta, o con error SRI o estado distinto de `issued`), salvo que la petición incluya `sriPortalVerified: true`, que la web solo envía tras la acción explícita «Verifiqué el portal: nuevo intento». La garantía ya no depende del navegador. Pruebas: `responseSettled` y la búsqueda de intentos hermanos en SQLite (58 ejemplos backend); prueba web de que la marca no cambia la huella (10).
- **Revisión Codex de `2a3c0e50` (P1):** dos navegadores con claves distintas para la misma factura podían pasar ambos la comprobación de intentos hermanos antes de que se confirmara ninguna reserva. La comprobación y la reserva se ejecutan ahora en una sola transacción tras `pg_advisory_xact_lock` sobre (sesión, huella); la segunda petición espera a que la primera confirme su reserva, la ve como hermana sin resolver y recibe 409. Verificado en PostgreSQL que una segunda transacción con la misma clave de bloqueo espera (≈2,6 s) y otra clave no; prueba SQLite de la reserva protegida (59 ejemplos backend).
- **Revisión Codex de `51d87b3b` (P1 y dos P2):**
  - **P1:** el bloqueo asesor se ejecutaba con `rawExecute`, pero `pg_advisory_xact_lock` devuelve una columna y la ejecución como sentencia falla en PostgreSQL; toda petición con clave habría fallado. Ahora usa `rawSql` con un resultado consumible, igual que los demás bloqueos de `TDF.Server`.
  - **P2 servidor:** una clave ya existente se resuelve primero (reproducción, en curso o conflicto) y solo una clave nueva pasa por la comprobación de intentos hermanos, así que reintentar un intento más reciente ya no se rechaza por uno anterior sin resolver.
  - **P2 web:** la verificación del portal queda ligada a la huella de la petición verificada y solo se envía si la petición enviada coincide; no se aplica a otra sesión ni a una factura editada.
  - Se añade una prueba sobre PostgreSQL real (`TDF_SESSION_ISSUE_TEST_DB`, base desechable `tdf_session_issue_test`) que toma el bloqueo y reserva por la API de consultas, y comprueba que de dos claves nuevas concurrentes para la misma factura exactamente una reserva. Ejecutada localmente: 62 ejemplos backend; 10 pruebas web. La prueba de PostgreSQL aún no está conectada a CI.
- **CI de `ca3e5c5e` (backend-quality):** el gate de identidad falló porque las tablas `contract` y `contract_delivery` añaden dos referencias a Party (`created_by_party_id`, `requested_by_party_id`) que el registro `identity_known_party_reference` no incluía. En lugar de reescribir la migración ya aplicada 2026-09-18 (lo que exigiría otra suma compatible y rehacer los hashes del registro de listas), el generador ahora toma como destino la migración más reciente que contiene los marcadores; la migración no publicada `2026-11-04_contract_store` lleva el registro (229 referencias) y la guarda de party archivado en ambas tablas, y su rollback republica el registro anterior (227, idéntico al de 2026-11-02). Verificado: `test-identity-reconciliation.sh` y sus 9 pruebas; en PostgreSQL, 229 tras aplicar, 227 tras revertir y las dos guardas instaladas.

## 42. F-05 — mapa de rutas mutantes a requisitos (2026-11-06)

- **Prueba PostgreSQL de facturas configurada en CI (ejecución pendiente de verificar):** `scripts/test-session-invoice-issue.sh` crea una base desechable exclusiva `tdf_session_issue_test_$$` y ejecuta solo los ejemplos de PostgreSQL de `SessionIssueSpec` con `--fail-on=pending`, exigiendo que se reporten; una base sin configurar falla en lugar de pasar en silencio. El job de backend tiene configurada su ejecución junto a las demás comprobaciones con PostgreSQL. El autor informó 2 ejemplos locales sin fallos, pero no se encontró un recibo inmutable de esa ejecución; no se ejecutaron los dos casos reales durante la consolidación. Los controles locales con comandos simulados verifican rechazo de cero ejemplos, fallo y pending; no prueban el bloqueo ni la carrera en PostgreSQL. La ejecución hospedada real queda pendiente.
- **`SYS-ROUTE-REQUIREMENTS-001`:** `formal/system/route-requirements.json` tiene una entrada por cada operación POST, PUT, PATCH y DELETE del API compilado (`compiled-api-surface.json`), y `scripts/check-route-requirements.py` exige que el conjunto coincida exactamente, sin duplicados ni entradas obsoletas. Cada entrada tiene una base:
  - `named`: un requisito nombra «MÉTODO /ruta» (los parámetros se comparan por posición);
  - `path-named`: un requisito nombra la ruta sin método y es el único método mutante en ella;
  - `reviewed`: atribución revisada, con requisitos y nota obligatoria;
  - `unspecified`: deuda visible.
- Las bases `named` y `path-named` se recalculan desde el texto de los requisitos en cada comprobación, así que no pueden divergir; `--write` regenera el registro y conserva las entradas revisadas. 12 pruebas unitarias; el workflow formal ejecuta la comprobación y las pruebas.
- **Número exacto (sustituye la heurística de F-05):** de 513 rutas mutantes compiladas, 15 están atribuidas (14 `named`, 1 `path-named`) y **498 no aparecen en ningún requisito**. La cifra de la fase 1 (101 cubiertas de 508) contaba coincidencias de prefijo, que no establecen que un requisito cubra la ruta.
- **Límite:** la descripción compilada no incluye el nombre del manejador, así que la atribución sale del texto de los requisitos, no del análisis del manejador. Atribuir no es conformidad. Reducir las 498 rutas sin requisito es trabajo de especificación por dominio (empezando por `/social-events` con 70, `/admin` con 51 y `/music` con 35), no de herramientas; cualquier ruta nueva sin clasificar hace fallar CI.

## 43. F-04 — modelo de la emisión de facturas SRI por sesión (2026-11-06)

- **`PAY-INVOICE-ISSUE-001` deja de estar sin modelo.** `formal/payments/SessionInvoiceIssue.tla` modela peticiones para una sesión y una huella de petición, cada una con su clave y, opcionalmente, verificación del portal. Una petición puede detenerse en cualquier paso.
  - `OneRunPerKey`: el script SRI se inicia como máximo una vez por clave.
  - `NoBlindDuplicate`: dos intentos sin resolver con claves distintas nunca están abiertos a la vez salvo que uno se haya hecho tras verificar el portal.
- **Resultados informados por el autor, no reejecutados ni acreditados como CI actual** (`scripts/verify-inductive-proofs.sh`, configurado en el workflow formal; TLC/Apalache fijados no disponibles en esta consolidación):
  - TLC 1.7.2, 3 peticiones y 3 claves: sin violación (12 139 estados).
  - Apalache 0.62.4: `Init ⇒ IndInv`, `IndInv ∧ Next ⇒ IndInv′` e `IndInv ⇒ Safe` **demostrados** para cualquier conjunto no vacío de hasta cuatro peticiones y cuatro claves, en ejecuciones de cualquier longitud.
  - Control negativo: con la comprobación de intentos hermanos y la reserva en pasos separados (sin bloqueo asesor), TLC y Apalache encuentran la carrera revisada en el PR #701.
- **`CONTRACT-SEND-001` deja de estar sin modelo.** `formal/payments/ContractSend.tla`: «sent» solo después de que SMTP aceptara el PDF y la fila diga `accepted`; todo mensaje aceptado tiene fila de entrega aunque la petición se detenga; sin configuración (503) no se registra ni envía nada. El autor informa una demostración inductiva Apalache para hasta cuatro peticiones, con y sin configuración, y exploración TLC para tres; esos resultados y el control negativo de escribir la fila después de SMTP quedan pendientes de ejecución hospedada verificable en esta consolidación.
- **Límite:** la equivalencia entre una transacción bajo `pg_advisory_xact_lock` y un paso atómico del modelo, y entre `sendH` y sus pasos, se establece por revisión; `StoreSpec` conserva su evidencia anterior y los dos casos PostgreSQL nuevos requieren ejecución hospedada verificable, no hay refinamiento mecánico. Un transporte que acepta y después lanza una excepción deja la fila como fallida aunque el correo salió. Quedan 71 requisitos críticos sin modelo antes de la corrección siguiente.
- **Registro de modelos (corrección):** cuatro requisitos críticos tenían una prueba de solver que CI ya ejecuta, pero listada solo como prueba y no como modelo formal, así que contaban como «sin modelo». Ahora la listan en `formalModels`: `PAY-DOMO-QUOTE-001` y `PAY-COURSE-PRICE-001` (`domo_quote_arithmetic.py`, Z3), `PAY-DOMO-CANCEL-001` (`domo_cancellation_refund.py`, Z3) y `AUTH-REGISTRY-001` (`verify-auth-predicates.py`, SMT de los predicados de administrador estricto). Son lemas aritméticos o de predicados: el ciclo de vida de la cancelación y la denegación HTTP siguen sin modelo, como ya dicen sus campos `confidence` y `verification`. Los demás `verify-*.py` citados por requisitos críticos (CORS, errores, checkout, Google) son pruebas de ejecución con mutaciones, no modelos, y no se cuentan. Quedan **67** requisitos críticos sin modelo.
- **`PAY-CHECKOUT-003` (comisión y neto del vendedor de mercancía):** `formal/payments/merch_money_arithmetic.py` (Z3 4.13.3) asume exactamente las guardas de `calculateMerchMoney` sobre entradas Int64 y demuestra sin cota que la comisión es el piso de la cuota y está en `0..base`, que comisión, neto del vendedor y total caben en Int64, que `comisión + comisión del procesador + neto = total`, y que en `checkedMerchSubtotals` cada línea aceptada cabe en la suma aceptada. Cuatro controles quitan una guarda cada uno y encuentran el valor que la guarda excluye. Se ejecuta en el workflow formal. Quedan **66** requisitos críticos sin modelo.
- **`PAY-CHECKOUT-001` (productos y totales de checkout):** `formal/payments/checkout_money_arithmetic.py` (Z3 4.13.3) asume las guardas de `TDF.Commerce.Money` y demuestra que las líneas canónicas aceptadas (de 1 a 6, más un lema de paso para cualquier longitud) son positivas, no superan al padre, no cambian al estrecharse a Int64 y suman el padre; los importes del carrito heredado aceptados tampoco cambian al estrecharse. Los controles muestran que estrechar antes de calcular (desbordamiento circular de Int64) aceptaría importes erróneos. El rechazo atómico del carrito es propiedad de la transacción, probada en `tdf-hq/test`. Quedan **65** requisitos críticos sin modelo.
- **`PAY-DEPOSIT-POLICY-001` (anticipo mínimo y pago completo):** `formal/payments/deposit_policy_arithmetic.py` (Z3 4.13.3) demuestra, para toda política válida y entradas Int64, que un anticipo admitido deja un saldo de al menos una unidad, no supera la cuota aprobada y no es menor que el mínimo ni que el anticipo calculado; que un total no positivo cobra el total; y que subir el mínimo nunca convierte un pago completo en anticipo. Los controles muestran que las dos guardas de pago completo son necesarias. Quedan **64** requisitos críticos sin modelo.
- **`PAY-DOMAIN-BINDING-001` (admisión de pagos por enlace de proveedor):** `formal/payments/provider_binding_admission.py` (Z3 4.13.3) codifica conjunción por conjunción el predicado SQL `commerce_checkout_has_verified_payment` y se niega a ejecutarse si cambia el sha256 de su migración o si otra migración lo redefine. Demuestra que la evidencia admitida concuerda en checkout, proveedor, cuenta, entorno, referencia, recurso, moneda e importe (rama alojada: PlaceToPay y PayPhone) y en referencia de pedido, recurso, entorno, importe y moneda (rama directa), y que un intento no exitoso o discordante nunca se admite. Las 25 condiciones de la rama alojada son necesarias: quitar cualquiera rompe la concordancia. La evidencia manual y la guarda de reserva de Domo no se codifican (las cubre la matriz de PostgreSQL). Quedan **63** requisitos críticos sin modelo.

**Identidad de ejecución de esta consolidación (2026-11-06):** los cuatro programas Z3 nuevos de esta sección se ejecutaron con Z3 4.13.3 sobre el árbol limpio `743c5c995538420f6702c6df1ae002792f7e1eaa`, todos con salida 0. Sus fuentes y entradas de implementación permanecen idénticas en la corrección posterior de metadatos. Es evidencia local de las fórmulas codificadas, no de CI ni de refinamiento mecánico. No se ejecutaron aquí los nuevos modelos TLC/Apalache ni los dos casos reales PostgreSQL; se exige evidencia hospedada fresca antes de acreditar esos resultados. Las cifras históricas de deuda anteriores no son una declaración de conformidad global.

**Seguridad del runner PostgreSQL (consolidación):** no borra ninguna base antes de crear; instala la limpieza únicamente tras crear con éxito su nombre exclusivo. Rechaza `PGHOSTADDR`, `PGSERVICE`, `PGSERVICEFILE`, hosts ajenos a loopback (salvo `postgres` con `CI=true`) y puertos inválidos antes de invocar comandos de base. La conexión de `SessionIssueSpec` recibe solo `dbname=nombre_exclusivo`; host, puerto y credenciales permanecen en el entorno libpq sin concatenación. Los controles usan ejecutables propios simulados: no ejecutan PostgreSQL. El runner anterior y las mutaciones de limpieza prematura, nombre compartido y DSN con credenciales fallan esas regresiones.
- **Integración de `main` (PR #599, campañas de video):** `main` añadió tres rutas mutantes (`PUT /artists/video-campaigns/{campaignKey}`, `POST …/conversion`, `POST …/feature`). El gate de rutas las rechazó hasta clasificarlas, como debe; quedan como `unspecified`. Recuento actual: 15 de **516** rutas atribuidas, **501** sin requisito. Los informes de catálogo y los índices de especificación se regeneraron con las herramientas del repositorio.
- **`ID-SESSION-004` (expiración de sesiones interactivas):** `formal/auth/SessionExpiry.tla` modela los tokens (interactivos, de servicio y de restablecimiento), la ventana de validez en el reloj de la base de datos, las sesiones heredadas sin expiración y un reloj que puede moverse a cualquier valor, también hacia atrás. Apalache demuestra inductivamente que todo uso aceptado de una sesión interactiva ocurre antes de su expiración, que una sesión interactiva sin expiración nunca se acepta, que toda ventana dura entre 1 segundo y el Max-Age de la cookie, y que los tokens de restablecimiento nunca autentican (hasta cuatro tokens, cualquier movimiento del reloj). Un testigo de alcanzabilidad descarta que la prueba sea vacía. Controles: aceptar tokens sin expiración o admitir un TTL mayor que el Max-Age viola `Safe`. Quedan **63** requisitos críticos sin modelo.

## 44. F-05 — requisitos de las rutas `/social-events` (2026-11-07)

- **Decisión del responsable (2026-11-07):** especificar primero el dominio `/social-events`, a partir del comportamiento actual de los manejadores, para que el responsable lo revise.
- **Cobertura:** las 72 rutas mutantes de `/social-events` quedan atribuidas.
  - 26 se vinculan, revisadas y con nota, a requisitos existentes que ya especifican su comportamiento: `EVT-TICKET-001`, `EVT-TICKET-MANUAL-001`, `PAY-EVENT-REPORT-RANGE-001`, `MEDIA-STREAM-001`, `SOCIAL-EVENT-INVITATION-001`, `SOCIAL-EVENT-MEMORY-001` y `EVT-OWNER-CLAIM-001`.
  - 46 las nombran 15 requisitos nuevos, más el ya existente `EVT-DISCOVERY-001`. Los requisitos nuevos tienen estado `implemented but unspecified` y conformidad `IMPLEMENTED BUT UNSPECIFIED`: describen lo que hace el código, no una especificación aprobada.
  - Recuento global: 86 de 516 rutas mutantes atribuidas; **430** sin requisito.
- **Documento para revisión:** `formal/system/social-events-routes.md` recoge la autorización, validación, efectos y concurrencia de cada ruta (leídos del código) y los comportamientos que parecen no intencionados.
- **Defectos corregidos:**
  1. `PUT /social-events/artists/{id}` aceptaba `artistPartyId` del cuerpo sin comprobarlo, así que un propietario podía mover su perfil de artista a cualquier otra parte. Ahora solo un administrador estricto puede hacerlo, igual que al crear (`validateArtistProfilePartyChange`, con pruebas en `Spec.hs`).
  2. `PUT /social-events/me/city-subscriptions` sobrescribía el nombre y la zona horaria de una ciudad compartida con lo que enviaba el suscriptor. Ahora reutiliza la fila existente sin cambiarla (prueba de manejador en `FollowHandlerSpec`).
- **Pendiente de decisión** (campos `ambiguity`; lista completa en el documento de revisión):
  - el webhook de Stripe heredado no registra una excepción de conciliación cuando la evidencia no coincide (`PAY-STRIPE-WEBHOOK-001`, crítico);
  - los códigos promocionales son únicos entre todos los eventos;
  - la lista de espera admite duplicados concurrentes;
  - cualquier cuenta puede crear eventos;
  - fuera del modo canónico, los comentarios guardan un nombre de autor que envía el cliente;
  - «notificar» en la lista de espera e invitar a un evento no envían mensajes.

## 45. F-05 — requisitos de las rutas `/admin` (2026-11-07)

- **Cobertura:** las 53 rutas mutantes de `/admin` quedan atribuidas.
  - 11 se vinculan, revisadas, a requisitos existentes: `ID-SESSION-001`, `PRIV-RAG-002`, `OPS-ENRICH-001`, `ART-ENRICH-REVIEW-001` y `ART-PROFILE-MEDIA-001`.
  - 42 las nombran 13 requisitos nuevos (`implemented but unspecified`) y los ya existentes `PAY-DOMO-CANCEL-001` y `AUTH-SOCIAL-OAUTH-001`.
  - Recuento global: 138 de 516 rutas mutantes atribuidas; **378** sin requisito.
  - Documento de revisión: `formal/system/admin-routes.md`.
- **Defectos corregidos:**
  1. `PUT /admin/services/storefront/orders/{orderId}` leía el pedido y lo reemplazaba entero en otra transacción, así que podía revertir un pago registrado entretanto. Ahora es una sola transacción que escribe solo estado, entregables y notas, condicionada al estado comprobado (409 si cambió), con el estado normalizado. Prueba en PostgreSQL en `ProviderRetrySpec`.
  2. `POST /admin/social/unhold` por `externalId` devolvía a la cola mensajes ya respondidos (`sent`), y el trabajador automático podía responderlos de nuevo. Ahora los excluye. Prueba en `ServerAdminSpec`.
  3. `check-route-requirements.py` no reconocía una ruta seguida de dos puntos («POST /a/{id}/cancel:»). Ahora quita la puntuación final. Con ello se atribuyen las cancelaciones Domo administrativa y pública a `PAY-DOMO-CANCEL-001`.
- **Hueco del gate de guardas:** `AUTH-ADMIN-GUARD-001` y `check-admin-guards.py` solo recorren `ServerAdmin` y `RecordsIngestion`. Los manejadores `/admin` de cursos, tienda de servicios y operaciones de comercio quedan fuera, aunque comprueban acceso por sí mismos.
- **Pendiente de decisión** (campos `ambiguity`; lista completa en el documento de revisión):
  - envíos y reenvíos de WhatsApp por administradores sin comprobar consentimiento ni ventana de 24 h;
  - difusión de correo a usuarios registrados sin consentimiento de marketing;
  - el correo de prueba está abierto a StudioManager y Webmaster;
  - una inscripción a curso pagada puede cancelarse sin reembolso, y una inscripción heredada puede marcarse pagada solo con la URL de un recibo;
  - aprobar un candidato de identidad puede otorgar el rol Artist a cualquier parte;
  - `user_credential` no tiene clave única por parte;
  - la contraseña inicial viaja en claro en el correo de bienvenida.

## 46. F-05 — requisitos de las rutas `/music` (2026-11-07)

- **Cobertura:** las 35 rutas mutantes de `/music` quedan atribuidas: 13 revisadas contra requisitos existentes (`MEDIA-MUSIC-RELEASE-001`, `MEDIA-MUSIC-ASSET-001`, `MEDIA-MUSIC-DDEX-001`, `PAY-MUSIC-PURCHASE-REPLAY-001`, `PAY-MUSIC-OUTCOME-001`, `PAY-MUSIC-DOWNLOAD-001`) y 22 nombradas por 6 requisitos nuevos (`implemented but unspecified`). Recuento global: 173 de 516 rutas atribuidas; **343** sin requisito. Documento de revisión: `formal/system/music-routes.md`.
- **Defecto corregido (dinero):** las rutas de captura PayPal y estado Datafast de compras de música registraban el pago verificado con `recordVerifiedPayment` a secas. Los demás dominios usan `recordVerifiedPaymentOrReconcile`, que deja una excepción de conciliación abierta cuando dinero capturado con evidencia autoritativa no se admite (por ejemplo, un checkout caducado entretanto). Música respondía 502 sin registrar nada, así que dinero capturado podía quedar sin rastro. Ahora usa la variante con conciliación; `PAY-CAPTURE-RECON-001` nombra el dominio de música, y una prueba en PostgreSQL (`MusicPurchaseSpec`, 33 ejemplos) lleva una captura COMPLETED a un checkout caducado y encuentra la excepción abierta.
- **Pendiente de decisión** (campos `ambiguity`): creaciones concurrentes con una misma clave pueden crear dos lanzamientos; el miembro de equipo con permiso de subida no puede confirmar ni cancelar la sesión que ve; comentarios de petición de cambios y acciones por infracción cambian el estado sin fila de auditoría de transición; la identidad anónima de reproducción es elegida por el cliente.

## 47. F-05 — requisitos de las rutas `/merch` y `/fans` (2026-11-07)

- **Cobertura:** las 29 rutas mutantes de `/merch` y las 28 de `/fans` quedan atribuidas; ningún requisito existente especificaba ninguna de ellas. Las nombran 9 requisitos nuevos de mercancía (3 críticos: checkout, reembolsos y liquidaciones) y 7 de comunidad (`implemented but unspecified`). Recuento global: 229 de 516 rutas atribuidas; **287** sin requisito. Documentos de revisión: `formal/system/merch-routes.md` y `formal/system/fans-routes.md`.
- **Defecto corregido:** las elecciones de club de fans tienen estado y ventanas opcionales de candidatura y votación, pero ni `createCandidacy` ni `castVote` las consultaban: cualquiera podía postularse o votar en cualquier momento, incluso en una elección cerrada. `validateFanClubElectionPhase` rechaza (409) la elección cerrada y todo lo que quede fuera de una ventana definida; se aplica en la misma transacción que la búsqueda del objetivo. Pruebas en `ServerFanClubSpec` (32 ejemplos).
- **Pendiente de decisión** (campos `ambiguity`; lista completa en los documentos):
  - mercancía: la aprobación de un reembolso no mueve dinero (no hay ruta de ejecución en el módulo); una actualización de producto reescribe el stock de las variantes saltándose la ruta de stock con versión; un titular de `can_settings` puede otorgar `can_finance` a cualquiera; la aceptación de invitaciones ignora su caducidad; las transiciones de estado de producto y de cumplimiento leen y escriben sin guarda;
  - comunidad: cualquier cuenta puede inscribirse, publicar recuerdos y escribir a cualquier club sin seguirlo; no se restringe quién vota ni el voto propio; los autores no pueden borrar sus recuerdos; las respuestas de la directiva se sobrescriben sin aviso y no notifican al fan.

## 48. F-05 — requisitos de las rutas `/trials`, `/internships` y `/directory` (2026-11-07)

- **Cobertura:** las 19 rutas mutantes de `/trials`, las 21 de `/internships` y las 23 de `/directory` quedan atribuidas: 3 revisadas contra requisitos existentes (`ID-CLAIM-REVIEW-001`, `DIR-SAVED-SEARCH-001`, `ID-ARTIST-CLAIM-001`) y 60 nombradas por 15 requisitos nuevos (`implemented but unspecified`; críticos: auditoría de pasantías y moderación/fusiones del directorio). Recuento global: 292 de 516 rutas atribuidas; **224** sin requisito. Documentos de revisión: `formal/system/trials-routes.md`, `internships-routes.md` y `directory-routes.md`.
- **Defectos corregidos (escuela):**
  1. `PATCH /trials/v1/students/{id}` solo exigía acceso de escuela al personal y actualizaba la parte indicada, fuera quien fuera: el correo y teléfono de un administrador podían cambiarse por la ruta de alumnos. `validateStudentUpdateTarget` exige ahora que el objetivo sea alumno (rol Student, vínculo con un profesor o sesión de clase) para el personal (404 si no) y un alumno propio para el profesor (403). Pruebas en `PublicLeadSpec` (25 ejemplos).
  2. Los nombres de asignatura duplicados respondían 500 por el índice único; ahora 409 en creación (`insertUnique`) y en renombrado (comprobación previa).
- **Pendiente de decisión** (campos `ambiguity`; listas completas en los documentos):
  - escuela: asignar asignaturas otorga el rol Teacher a cualquier parte que nombre el personal; las comprobaciones de conflicto de profesor y sala no están bloqueadas (doble reserva posible); las compras toman el precio del cliente;
  - pasantías: las guardas de administrador solo miran roles (ignoran la revocación del módulo); las decisiones de permisos no tienen máquina de estados ni revisor distinto; dos fichajes concurrentes dejan dos entradas abiertas;
  - directorio: cualquier usuario puede preparar una reclamación de artista y con ello retirar un artista heredado de la búsqueda pública hasta que se publique su gemelo; las fusiones se ejecutan con solicitante = aprobador y sin comprobar sujeto ni tipo; las decisiones de verificación no tienen guarda de autorrevisión ni auditoría; la repetición de una decisión de moderación vuelve a aplicar la acción.

## 49. F-05 — requisitos de las rutas `/catalog`, `/marketplace` y `/public` (2026-11-07)

- **Cobertura:** las 13 rutas mutantes de `/catalog`, las 17 de `/marketplace` y las 18 de `/public` quedan atribuidas por 12 requisitos nuevos (`implemented but unspecified`; críticos: flujo de revisiones y concesiones de seguridad del catálogo, rieles de pago y revisiones de operadores del marketplace, y los tres checkouts públicos de cursos, Domo y entradas). Ningún requisito existente las especificaba directamente; `REC-CATALOG-001`, `PAY-CHECKOUT-001/002`, `PAY-CAPTURE-RECON-001`, `LEGAL-CONSENT-001`, `PAY-DOMO-*`, `EVT-TICKET-*` y `NOTIF-TICKET-001` cubren partes. Recuento global: 340 de 516 rutas atribuidas; **176** sin requisito. Documentos de revisión: `formal/system/catalog-routes.md`, `marketplace-routes.md` y `public-intake-routes.md`.
- **Defectos corregidos:**
  1. **Catálogo:** `approveRevisionHandler` solo rechazaba al autor en catálogos sensibles, pero el disparador `catalog_revision_workflow_integrity` rechaza `approved_by = created_by` en todos. En un catálogo no sensible (o sensible con `emergencyOverride`) el manejador publicaba el ítem, subía la versión del catálogo y escribía la auditoría antes de que la actualización de la revisión fallara con 42501: ítem publicado, revisión en revisión, reintento con 409. `validateCatalogApproval` rechaza ahora al autor en todo catálogo (403) antes de publicar nada. Pruebas en `Catalog/SecuritySpec`.
  2. **Marketplace:** todo `PUT /marketplace/orders/{id}` sobre un pedido canónico respondía 500: el checkout canónico crea el pedido en `awaiting_payment` (y el disparador de sincronización escribe `processing`), estados que `normalizeMarketplaceOrderStatus` no reconocía, así que `validateStoredMarketplaceOrderStatus` rechazaba el estado almacenado antes de cualquier edición. Los estados canónicos se aceptan ahora como estado actual y siguen rechazándose como destino. Pruebas en `ServerSpec`.
- **Pendiente de decisión** (campos `ambiguity`; listas completas en los documentos):
  - catálogo: `catalog_merge_operation` se escribe y nunca se lee (una fusión no fusiona nada); envío y rechazo no escriben auditoría; publicación y actualización de la revisión son transacciones separadas; las revisiones programadas nunca se publican; la desactivación directa evita el flujo de aprobación en catálogos sensibles; conflictos de versión de concesiones bajo el cerrojo responden 500;
  - marketplace: si la vinculación falla tras crear el recurso en el proveedor, el reintento crea un segundo recurso; la revisión de pagos manuales solo exige el módulo de facturación; un operador puede cancelar un pedido canónico sin liberar su reserva;
  - público: el token de consulta de cursos se deriva de la clave de idempotencia elegida por el cliente sin secreto del servidor (Domo y entradas usan HMAC); los límites de tasa de Domo y entradas se indexan por el correo que envía el atacante; las rutas QR de inventario no tienen límite, permiten subidas anónimas ilimitadas y cualquier poseedor del token puede cerrar el préstamo ajeno.
