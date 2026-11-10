# Marketplace de demanda y oferta — Especificación funcional

Fecha: 2026-11-08. Vocabulario: **anuncio** = `classified`; **oferta** = `classified_application`
con su cadena de **rondas**; **acuerdo** = `marketplace_agreement` (Fase 2). Se conserva la
terminología de la UI actual ("clasificado", "postulación") como alias en i18n durante la
transición.

## 1. Actores

Todos son `Party` autenticados que administran uno o más `directory_profile`
(`directory_profile_manager`). No se crean roles de autorización nuevos.

| Actor | Perfil típico (`profile_kind`) | Publica | Responde |
| --- | --- | --- | --- |
| Músico / sesionista / DJ / técnico | `person`, `artist` | busco banda, busco fecha, ofrezco servicios | ofertas a bandas, venues, organizadores |
| Banda / proyecto | `band`, `project` | busco músico, busco venue, busco fecha, disponible para eventos (derivado) | ofertas a venues y organizadores |
| Venue / sala / estudio | `venue`, `studio` | busco banda para fecha, sala disponible | ofertas a organizadores y bandas |
| Organizador / promotor / wedding planner / agencia | `organization`, `company`, `agency`, `person` | busco banda, busco venue, busco técnico | — |
| Sello / escuela / distribuidora | `label`, `school`, `distributor` | oportunidades, clases | — |
| Administrador del directorio | rol `Admin` + módulo | — | modera, verifica, resuelve disputas |

Un mismo perfil puede publicar demanda y oferta. Las acciones requieren capacidad `publish`
(anuncios) o `contact` (ofertas, invitaciones) sobre el perfil, y mayoría de edad acreditada
(`requireAdult`).

## 2. Anuncios

### 2.1 Intención

Cada categoría declara `intent`: `seeking` (busco) u `offering` (ofrezco). El anuncio hereda la
intención y se muestra con un badge ("Busca" / "Ofrece"). La búsqueda admite filtro `intent`.

Categorías existentes y su intención:
- `seeking`: busco-musico, busco-banda-proyecto, busco-productor, busco-ingeniero-tecnico,
  busco-management, busco-venue, busco-fecha-concierto, audicion-convocatoria,
  trabajo-remunerado, colaboracion, oportunidad-sello-distribucion.
- `offering`: ofrezco-servicios, equipo-venta-alquiler, sala-estudio-disponible, clases,
  perfil-artista (derivado).

### 2.2 Campos de un anuncio de búsqueda

Existentes: título, categoría, descripción (Markdown 20–10 000), profesiones, instrumentos,
géneros, países/ciudades/áreas, modalidad (presencial/remoto/viaja), radio, `startsAt`/`endsAt`,
nivel de experiencia, tipo de compensación, presupuesto min/max + moneda, negociable, vencimiento
(≤ 90 días), adjuntos.

Nuevos (Fase 1):
- `slots` (1–100, por defecto 1): cuántas ofertas puede aceptar.
- `priceMode`: `open` (sin precio; solo ofertas), `suggested` (precio de referencia =
  `budget_max_minor`; se admiten contraofertas), `fixed` (precio cerrado; las ofertas solo pueden
  aceptar el precio, no contraofertar).
- `offerDeadlineAt`: fecha límite para ofertar (≤ `expiresAt`; por defecto igual a `expiresAt`).
- `terms` (JSON): duración estimada en minutos, aforo o tamaño esperado, equipo provisto / requerido,
  lugar textual (sin dirección exacta), zona horaria de las fechas, notas de pago (p. ej. "50 %
  adelanto"). La categoría puede marcar términos obligatorios en `requirements.required`
  (`terms.duration`, `terms.capacity`).

### 2.3 Campos de un anuncio de oferta

Los mismos, interpretados como lo que se ofrece: `budget_min/max` es la tarifa base o rango,
`priceMode` indica si la tarifa es negociable, `startsAt/endsAt` la ventana de disponibilidad,
`terms` lo que incluye (equipo, duración mínima, desplazamiento). `slots` representa cuántos
compromisos acepta en la ventana. Los anuncios derivados de perfil de artista no son editables y
no tienen negociación propia (se contacta vía perfil o se invita a un anuncio de búsqueda).

### 2.4 Ciclo de vida

Sin cambios en estados: `draft → pending_moderation → published → paused | filled | expired |
withdrawn | moderated`. Reglas añadidas:
- `published → filled` ocurre automáticamente cuando `slots_remaining = 0`; también manualmente.
- Al pasar a `filled`, `expired`, `withdrawn`, `rejected` o `moderated`, toda oferta en estado abierto
  (`submitted`, `viewed`, `shortlisted`, `countered_*`) pasa a `rejected` con `rejection_reason`
  (`slots_filled`, `classified_closed`) y se notifica. `paused` conserva las ofertas abiertas pero no
  admite nuevas (ofertar exige anuncio `published`).
- `expired → published` (renovación) reabre ofertas **no**: las rechazadas quedan rechazadas; se
  pueden crear nuevas.

## 3. Ofertas y negociación

### 3.1 Crear una oferta

Precondiciones (ya existentes + nuevas): anuncio publicado, vigente, `offerDeadlineAt` futuro,
`slots_remaining > 0`, el oferente no es el autor, pasa preferencias de contacto y bloqueos, mayoría
de edad, rate limit (20/h). Una oferta activa por (anuncio, perfil oferente).

Cuerpo: `applicantProfileId`, `message` (10–5 000), `portfolio[]`, `availability`, **`amountMinor` +
`currencyId`** (obligatorio salvo compensación `exchange`/`unpaid`), `terms` (misma forma que el
anuncio; propone cambios), `expiresAt` opcional de la oferta (por defecto 7 días o
`offerDeadlineAt`, lo que ocurra antes).

Reglas de precio:
- `priceMode=fixed`: `amountMinor` debe igualar `budget_max_minor` en la moneda del anuncio;
  no hay contraofertas; la oferta es una aceptación del precio.
- `priceMode=suggested|open`: cualquier monto ≥ 0 en una moneda del catálogo. Si la moneda difiere
  de la del anuncio se muestra ambas sin conversión (Fase 1); Fase 4 añade conversión indicativa.

La oferta inicial crea la ronda 1 (`kind='initial'`, `actor='applicant'`).

### 3.2 Contraofertar

`POST /directory/applications/{id}/counter` con `amountMinor`, `currencyId`, `terms?`, `message?`,
`expiresAt?`. Permitido para el autor cuando la oferta está en `submitted | viewed | shortlisted |
countered_by_applicant`, y para el oferente cuando está en `countered_by_author`. Alternancia
estricta: nunca dos rondas seguidas del mismo actor. Máximo 10 rondas por oferta. Cada ronda crea una
fila en `classified_offer_round` y actualiza `current_amount_minor`, `current_round`,
`last_actor_role`, `offer_expires_at`. Estados resultantes: `countered_by_author` o
`countered_by_applicant`.

### 3.3 Aceptar, rechazar, retirar

- **Aceptar** (`PATCH status=accepted`): el autor acepta en `submitted | viewed | shortlisted |
  countered_by_applicant`; el oferente acepta en `countered_by_author`. Efecto atómico: oferta
  `accepted` con `accepted_amount_minor` = monto de la última ronda; `slots_remaining -= 1`; si
  llega a 0, anuncio `filled` y resto de ofertas abiertas `rejected(slots_filled)`. Con
  `marketplace.agreements` activo (Fase 2) se crea el acuerdo y la oferta pasa a `converted` al
  confirmarse el depósito.
- **Rechazar**: solo el autor, desde cualquier estado abierto. **Declinar contraoferta**: el
  oferente puede `withdrawn` en cualquier momento previo a `accepted`.
- **Retirar tras aceptar**: `accepted → withdrawn` (oferente) libera el cupo (`slots_remaining +=
  1`) y, si el anuncio estaba `filled` por cupos y sigue vigente, vuelve a `published`. Se
  registra como cancelación en reputación (Fase 3).
- **Vencimiento**: una oferta cuya `offer_expires_at` pasa sin respuesta pasa a `expired`
  (nuevo estado terminal); se evalúa en lectura y por el cron de expiración existente.

### 3.4 Visibilidad (I08)

El autor ve todas las ofertas de su anuncio con perfil, mensaje, portafolio, cadena de rondas y
reputación. El oferente ve solo su propia oferta y rondas. Público y otros oferentes ven solo
`offerStats`: número de ofertas y, si hay ≥ 3, rango min–max agregado (sin identidades).

### 3.5 Invitaciones

Sin cambios funcionales; al aceptar una invitación vinculada a un anuncio con cupos, la UI ofrece
"Enviar oferta" prellenando el anuncio.

## 4. Conversación

Al aceptar (o en `shortlisted`, si el autor lo decide) se abre el DM existente mediante
`/directory/contact` con `contextKind=application`. La negociación de monto ocurre siempre por
rondas estructuradas, no por chat, para que el precio acordado sea inequívoco.

## 5. Acuerdo, pago y comisión (Fase 2)

- Al aceptar con el gate activo se crea `marketplace_agreement` con términos congelados: monto,
  moneda, fechas, lugar textual, inclusiones, política de cancelación, `commission_bps`
  (configurable por categoría/país; por defecto 1 000 = 10 %), `deposit_bps` (por defecto 3 000)
  ajustado por `DepositPolicy`.
- El autor (quien busca) paga el depósito mediante checkout enlazado (`domain_order_kind =
  marketplace_agreement`); el saldo se paga antes de `startsAt` o al marcar `completed` según la
  categoría. La comisión se descuenta de lo liquidado al proveedor (modelo take-rate del lado de la
  oferta) o se suma al comprador según configuración; por defecto se descuenta al proveedor.
- Estados: `pending_deposit` (48 h o hasta `startsAt`−24 h) → `confirmed` → `in_progress` (desde
  `startsAt`) → `balance_due` → `completed` (confirmación de ambas partes o automática 72 h después
  de `endsAt` sin disputa) | `cancelled` (según política; reembolsos vía `RefundStore`) | `disputed`
  (admin resuelve) | `expired` (depósito no pagado).
- Liquidación: v1 manual por operaciones con evidencia; v2 Stripe Connect.
- Nunca se usa la palabra "escrow" en UI ni contratos; se habla de "depósito" y "saldo".

## 6. Reputación y verificación (Fase 3)

- Al pasar a `completed` se inserta `directory_interaction(kind='marketplace_agreement',
  status='completed', verified_at=now())` entre ambos perfiles → ambos pueden reseñar (1–5 +
  texto) en 30 días.
- Agregados visibles: rating medio y conteo, trabajos completados, tasa de respuesta a ofertas (%
  de ofertas con respuesta en 48 h), tiempo medio de respuesta, cancelaciones tras aceptar.
- Insignias: Verificado (existente), Pago habilitado, Venue verificado, Respuesta rápida.

## 7. Notificaciones

In-app (tabla `notification`) y, cuando el gate de email esté activo, correo:
`marketplace.offer.new`, `marketplace.offer.countered`, `marketplace.offer.accepted`,
`marketplace.offer.rejected`, `marketplace.offer.expiring` (24 h antes), `marketplace.classified.filled`,
`marketplace.agreement.*` (Fase 2), `marketplace.review.requested` (Fase 3).

## 8. Permisos y seguridad

- Autor: gestiona anuncio y decide sobre ofertas; no ve PII del oferente más allá del perfil público.
- Oferente: crea/contraoferta/retira su oferta; no ve otras ofertas.
- Admin: lectura con propósito de auditoría (I08), moderación y disputas.
- Idempotencia obligatoria en POST de ofertas y rondas (`Idempotency-Key`), rate limit por
  cuenta (`consumeRate`), auditoría en `directory_audit_event` para cada transición y ronda.
- Menores: sin publicar, ofertar ni acordar (`minorMayPublishOrRespond`).

## 9. Métricas de éxito

| Métrica | Definición | Meta 90 días post-Fase 1 |
| --- | --- | --- |
| Anuncios de búsqueda publicados / semana | `classified.intent=seeking`, `published` | 30 |
| Ofertas por anuncio | mediana de `classified_application` por anuncio | ≥ 3 |
| Tiempo a primera oferta | mediana `first_offer_at − published_at` | < 24 h |
| Tasa de aceptación | anuncios con ≥ 1 `accepted` / publicados | ≥ 40 % |
| RAS | acuerdos o aceptaciones entre perfiles distintos / semana | 15 |
| GMV y take rate (Fase 2) | suma de acuerdos `completed`; comisión / GMV | — |

## 10. Casos de uso de referencia

1. **Wedding planner busca banda**: publica `seeking-band` con fecha, ciudad, 4 h, precio
   sugerido 1 200 USD, `slots=1`, términos (equipo de sonido provisto). Tres bandas ofertan
   (1 100, 1 300 con backline incluido, 1 200). Contraoferta a la segunda por 1 250; acepta; anuncio
   `filled`; DM; (Fase 2) depósito 30 % y saldo el día anterior.
2. **Bajista busca banda en su barrio**: `seeking-band` con instrumento bajo, ciudad/área,
   compensación `collaboration`, sin monto; las bandas responden con mensaje y portafolio; acepta
   hasta `slots=2`.
3. **Banda busca tecladista**: `seeking-musician`, instrumento teclados, ensayos semanales (fechas
   recurrentes en términos), `priceMode=open`.
4. **Venue busca banda para fecha**: `seeking-band` con `dateRange`, aforo 300, precio fijo 400
   USD (`fixed`): las ofertas solo aceptan el precio; el venue elige por reputación.
5. **Organizador busca venue**: `seeking-venue`, ciudad, fecha, aforo 500; los venues ofertan con
   tarifa y lo que incluye.
6. **Músico ofrece servicios**: `offering-services` con tarifa por hora y disponibilidad; recibe
   invitaciones y contactos; puede ser invitado a anuncios de búsqueda.
