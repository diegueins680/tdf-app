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
| `music-directory#claim` | `approved` es terminal: no hay revocación de una reclamación aprobada por error o fraude (pregunta Q3) |
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

## 8. Preguntas abiertas para el responsable

- **Q1 (F-01):** ¿qué revisión del móvil está publicada en las tiendas? ¿Se fusiona la rama fijada en `main` del móvil, o se cambia el gitlink a `main`?
- **Q2 (F-06):** ¿solo se puede dar propina a artistas publicados o activos? ¿Hay importe mínimo o máximo de negocio, aparte del límite de Stripe?
- **Q3 (§6):** ¿debe poder revocarse una reclamación de directorio ya aprobada?
- **Q4 (§6):** ¿los royalties están en el alcance actual o siguen siendo diseño sin implementación?

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
