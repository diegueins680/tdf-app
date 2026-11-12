# Marketplace de demanda y oferta — Fase 0: auditoría del estado actual

Fecha: 2026-11-08. Base auditada: `main` en `5c69dc30` (PR #709).
Alcance: módulos Directory (`/directory/*`), Commerce, Interactions, UI web del directorio y Fan Hub.
Método: lectura del contrato OpenAPI, SQL, handlers Haskell, páginas React y documentos de diseño
(ADR 0105, `docs/music-directory/*`, estrategia de comunidad e ingresos). La app desplegada es una
SPA (sin contenido sin JavaScript); el recorrido funcional en producción queda documentado en
`manual-gates` de esta carpeta como verificación pendiente con navegador.

## 1. Resumen ejecutivo

El objetivo (anuncios de búsqueda y de oferta, respuestas con precio, elección por quien busca,
reserva + pago + comisión, reputación y verificación) ya tiene construido entre el 50 % y el 60 %
de su base:

| Capacidad objetivo | Estado | Dónde vive |
| --- | --- | --- |
| Perfiles públicos multi-tipo administrados por una cuenta | **Implementado** | `directory_profile`, `directory_profile_manager`, `/directory/profiles*`, `DirectoryManagePage` |
| Anuncio de búsqueda con categoría, modalidad, fechas, ubicación, presupuesto y moneda | **Implementado** | `classified`, `ClassifiedCreate`, `createClassified` |
| Anuncio de oferta de servicios | **Parcial** | categorías `offering-services`, `room-studio-available`, `classes`, `equipment-sale-rental`; listados derivados automáticamente de perfiles de artista (`source_profile_id`) |
| Respuesta a un anuncio con precio | **Parcial** | `classified_application.proposed_amount_minor` (un solo monto, sin contraoferta ni términos) |
| Comparar respuestas y elegir | **Parcial** | `listApplications` (solo autor), `ApplicationRow` con preseleccionar/aceptar/rechazar; sin ordenar ni comparar por precio/reputación, sin cerrar las demás al aceptar |
| Negociación (contraoferta en ambos sentidos) | **No existe** | — |
| Invitar perfiles a un anuncio | **Implementado** | `directory_invitation` |
| Conversación privada tras aceptación | **Implementado** | `/directory/contact` + `directory_conversation_context` sobre el DM existente |
| Reserva/acuerdo al aceptar | **No existe para clasificados** | Existe `ServiceBookings` (reservas de estudio/servicios TDF) y `MarketplaceSales/Rentals` (equipos) |
| Pago en la app con comisión | **Existe la infraestructura, no la integración** | `CheckoutStore` (`platform_fee_minor`), `DepositPolicy`, `PaymentIntentStore`, proveedores Stripe/PayPal/Datafast/PayPhone/PlaceToPay, ledger inmutable, reembolsos |
| Liquidación al proveedor (pago a terceros) | **Parcial** | Propinas a artistas vía Stripe (`ArtistTipStore`, `platform_fee_cents`); no hay cuenta conectada/payout genérico para perfiles del directorio |
| Reseñas verificadas bidireccionales | **Implementado el motor, sin productor** | `directory_interaction` (kinds `booking`, `service_order`, `marketplace_order`, `event_collaboration`, `confirmed_collaboration`), `directory_review`, `review-eligibility`. **Nada en el directorio inserta `directory_interaction`**; `match_converted` solo registra un evento analítico |
| Verificación de perfiles | **Implementado** | `/directory/verifications`, cola admin, `directory.admin` |
| Moderación, reportes, bloqueos, rate limit, idempotencia, auditoría | **Implementado** | `directory_moderation_report`, `directory_profile_block`, `consumeRate`, `reserveIdempotency`, `directory_audit_event` |
| Búsqueda con texto, taxonomías, geo (Haversine), facetas, alertas de búsquedas guardadas | **Implementado** | `directory_search_document`, `/directory/search`, `directory_saved_search` + trigger de alertas |
| Internacionalización de datos | **Implementado** | catálogos de país/ciudad/área metropolitana/moneda/idioma; i18n es/en en UI |
| Notificaciones in-app | **Implementado** | tabla `notification`, `notifyClassifiedAuthor`, `notifyProfile`, `notification-context` en UI |
| Mobile | **No** | `tdf-mobile` (submódulo) no tiene pantallas del directorio |

Conclusión: la vía más corta y segura es **evolucionar `classified` + `classified_application` hacia
anuncios con intención (busco/ofrezco) y ofertas negociables**, y conectar la oferta aceptada con la
infraestructura de reservas y checkout ya existente, sin crear chat, catálogos ni identidad nuevos.

## 2. Inventario detallado

### 2.1 Datos (PostgreSQL, `tdf-hq/sql/`)

- `2026-08-14_music_directory_core.sql`: `profession`, `classified_category` (15 categorías sembradas,
  10 de búsqueda, 5 de oferta; `requirements.required[]` controla campos obligatorios por categoría),
  `compensation_type` (`exact`, `range`, `negotiable`, `exchange`, `unpaid` con `metadata.budget`),
  `metropolitan_area`, `directory_profile` (+ profesiones, instrumentos, géneros, servicios, idiomas,
  ubicaciones públicas y privadas, créditos), `directory_contact_preference`
  (`allow_classified_applications`, `minimum_profile_completeness`), `directory_profile_block`,
  `classified`, `classified_{profession,instrument,genre,location,attachment}`, `classified_application`,
  `directory_invitation`, `directory_conversation_context`, `directory_favorite`,
  `directory_saved_search`, `directory_interaction`, `directory_review`, `directory_moderation_report`,
  `directory_audit_event`.
- `2026-10-07_directory_artist_derived_listings.sql`: categoría `artist-profile` derivada; trigger que
  impide editar listados derivados; backfill con dry-run y apply.
- `2026-09-30_interaction_classified_expiry.sql`: expiración de clasificados integrada con interacciones.
- `2026-10-25_directory_saved_search_parity.sql`: predicado compartido búsqueda/alertas.

Observaciones:
1. `classified.status` ya incluye `filled`; no hay noción de cupos (`slots`) ni de "oferta ganadora".
2. `classified_application` guarda **un** monto; no hay historial de contraofertas, términos
   estructurados (fecha/hora, duración, lugar, inclusiones), vencimiento de la oferta ni enlace a
   reserva/pago.
3. `classified_application.status` tiene `accepted` y `converted`, pero `converted` no produce ningún
   registro de dominio (ni `directory_interaction`, ni reserva, ni orden).
4. `directory_interaction.interaction_kind` no tiene un valor para acuerdos del marketplace; el más
   cercano es `confirmed_collaboration`.
5. No hay tabla de **acuerdo/contrato** entre dos perfiles del directorio con términos, montos,
   depósito, saldo, comisión y estados.

### 2.2 API (`tdf-hq/docs/openapi/directory.yaml`, `TDF.API.Directory`, `TDF.Server.Directory`)

Público: `search`, `suggestions`, `taxonomies`, `profiles/{slug}`, `party-profiles/{partyId}`,
`profiles/{slug}/reviews`, `classifieds/{slug}`, `events/{eventId}`, `venues/{venueId}`.

Protegido: `age-assurance`, `profiles` CRUD + `status`, `classifieds` (GET gestionados, POST con
idempotencia, PATCH status), `classifieds/{id}/applications` (GET solo autor, POST con idempotencia y
rate limit 20), `applications/{id}/status` (PATCH; autor: viewed/shortlisted/accepted/rejected/
conversation_open/converted; postulante: withdrawn/conversation_open), `invitations` (GET/POST/PATCH),
`contact`, `review-eligibility`, `reviews`, `favorites`, `saved-searches`, `artist-claim-targets`,
`claims`, `verifications`, `reports`, admin (`claims`, `verifications`, `moderation`, `merges`).

Brechas de API:
1. No hay **"mis postulaciones/ofertas"** para el postulante (solo `applicationSummary` por id y el
   `notification-context`).
2. `PublicClassified` no expone cupos, conteo de ofertas, precio sugerido como tal ni reglas de
   negociación.
3. No hay endpoints de contraoferta, aceptación con creación de acuerdo, ni de acuerdo/pago.
4. `ClassifiedApplication` está tipado como `additionalProperties: true` (sin esquema): la UI usa
   `Record<string, unknown>`. Conviene tiparlo al introducir ofertas.
5. No hay endpoint para que el autor fije precio sugerido distinto del rango de presupuesto (el rango
   `budget_min/max` + `budget_negotiable` cumple ese papel; basta exponerlo mejor).

### 2.3 Políticas y verificación formal

- `TDF.Directory.Policy`: máquinas de estado de perfil, reclamo, clasificado, postulación e
  invitación; confidencialidad de postulaciones; elegibilidad de reseña verificada; mayoría de edad.
- `docs/music-directory/formal-model.yaml`: invariantes I01–I0x (I07 transiciones de clasificado, I08
  confidencialidad de postulaciones), precondiciones `publish_classified`, `submit_application`,
  `accept_application`. `test/TDF/Directory/PolicySpec.hs` cubre estas tablas.
- Pagos: `formal/payments`, ADR 0100–0131 (checkout enlazado a dominio, holds, evidencia, custodia,
  depósitos de servicios y cotizaciones autoritativas).

### 2.4 Comercio reutilizable

- `TDF.Commerce.DepositPolicy`: mínimo absoluto y umbral de pago total (PAY-DEPOSIT-POLICY-001).
- `TDF.Commerce.ServiceBookings`: estados de reserva (`BookingOnHold … BookingCompleted/Disputed`),
  cálculo de precio en unidades menores con depósito en bps. Diseñado para reservas de estudio con
  unidades de tiempo; el marketplace necesita precio acordado libre, no tarifa×unidades.
- `TDF.Commerce.CheckoutStore` / `PaymentIntentStore` / `PaymentRuntimeStore`: `platform_fee_minor`,
  enlace `domain_order_id`, estados canónicos (`2026-09-09_canonical_payment_lifecycle.sql`).
- `TDF.Commerce.Merch.calculateMerchMoney`: comisión por bps validada (0–10000).
- Proveedores: `ProviderAdapter/{Http,PayPhone,PlaceToPay,PayPalRefund}`, `Services/Stripe.hs`,
  `DatafastConfig`; capacidades por proveedor (`ProviderCapabilities`), gates de finalización de
  checkout (ADR 0126).
- `ArtistTipStore`: único flujo que paga a un tercero (artista) con `platform_fee_cents` vía Stripe:
  referencia para el payout a perfiles del directorio.

### 2.5 UI web (`tdf-hq-ui/`)

- `DirectorySearchPage` (`/inicio`, `/buscar`): búsqueda unificada de perfiles, clasificados, eventos
  y venues; filtros, facetas, mapa, favoritos, búsquedas guardadas.
- `DirectoryPublicDetailPage` (`/directorio/:slug`, `/clasificados/:slug`, `/eventos/:id`,
  `/venues/:id`): detalle público; en clasificado deriva a `/mis-clasificados?apply=…` para postular.
- `DirectoryManagePage` (`/mis-clasificados`): pestañas Perfiles, Clasificados (con postulaciones por
  anuncio y acciones preseleccionar/aceptar/rechazar/abrir conversación), Responder-contactar-invitar,
  Invitaciones; editor de perfil con tarifas por profesión.
- `DirectoryAdminPage` (`/admin/directorio`): reclamos, verificaciones, moderación, merges.
- `FanHubPage` (`/fans`, etiqueta "Comunidad"), `SocialWorkspace`, `CommunityDiscovery`: seguir
  artistas, clubes, eventos, descubrimiento. No enlaza hoy con el directorio/clasificados.
- `api/directory.ts`: cliente tipado por OpenAPI generado (`api/generated/types.ts`).
- Falta: vista del postulante de sus ofertas; comparación de ofertas; formulario de postulación con
  términos; pantalla de acuerdo/pago; reputación agregada visible en tarjetas.

### 2.6 Feature registry

`tdf-hq/assets/feature-registry.json` define `directory.search`, `directory.classified.public`,
`directory.classifieds.manage`, `directory.admin`, `marketplace`, `bookings.public`, con
`featureFlag`, `maturity`, `requiredAuth` y `permissions` por acción. Las nuevas pantallas del
marketplace deben registrarse ahí (web y alias móvil) con `maturity: "experimental"` y flag propio.

## 3. Brechas priorizadas

| # | Brecha | Impacto | Fase |
| --- | --- | --- | --- |
| B1 | Intención explícita busco/ofrezco y cupos en el anuncio | Permite feeds y reglas diferenciadas | 1 |
| B2 | Ofertas negociables (cadena de contraofertas, términos, vencimiento) | Núcleo del modelo inDrive | 1 |
| B3 | Aceptación que cierra el anuncio/cupo y rechaza el resto | Integridad del match | 1 |
| B4 | Vista "mis ofertas" para el oferente y comparación para el autor | Usabilidad básica | 1 |
| B5 | Acuerdo (`marketplace_agreement`) con montos, depósito, comisión y estados | Base de reserva y pago | 2 |
| B6 | Checkout enlazado al acuerdo con `platform_fee_minor`, depósito y saldo | Ingreso TDF | 2 |
| B7 | Payout al proveedor (cuenta conectada o manual verificado) | Cierre financiero | 2 |
| B8 | Productor de `directory_interaction` desde acuerdos completados | Habilita reseñas verificadas | 3 |
| B9 | Reputación agregada por perfil (rating, trabajos, respuesta) e insignias de verificación | Confianza | 3 |
| B10 | Calendario de disponibilidad y matching proactivo (alertas a perfiles compatibles) | Liquidez | 4 |
| B11 | Paridad móvil | Alcance | 5 |

## 4. Riesgos y mitigaciones

- **Migrar `classified_application` en caliente**: añadir columnas con valores por defecto y una
  tabla hija de rondas de negociación; las filas existentes se interpretan como oferta inicial
  (`round 1`, `kind='initial'`). Rollback deja las columnas nuevas sin usar o las elimina.
- **Comisión y pagos a terceros**: cada país/proveedor tiene capacidades distintas; mantener el gate
  por proveedor (ADR 0126) y empezar con depósito cobrado por TDF + liquidación manual verificada,
  antes de Stripe Connect.
- **Menores**: `requireAdult` ya protege publicar/postular; conservarlo en ofertas y acuerdos.
- **Confidencialidad (I08)**: las ofertas de otros oferentes no deben ser visibles entre sí; exponer
  al oferente solo su propia cadena y, opcionalmente, estadísticas agregadas (rango de ofertas).
- **Derivados de perfil de artista**: los anuncios derivados (`source_profile_id`) son de oferta;
  no admiten cupos ni negociación propia y se mantienen fuera de la Fase 1.

## 5. Verificación pendiente en producción (manual gates)

Recorrido a ejecutar con navegador y cuenta de prueba antes de desplegar la Fase 1:
1. `/buscar` → filtrar por clasificados → abrir `/clasificados/:slug`.
2. `/mis-clasificados` → crear perfil → crear clasificado → publicar.
3. Con otra cuenta: postular con monto → verificar notificación del autor.
4. Autor: preseleccionar, aceptar, abrir conversación → DM.
5. Confirmar que `expiresAt` por defecto (+30 días) y el tope de 90 días se cumplen.
