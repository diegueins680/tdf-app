# Cursos públicos — checkout, cupos e inscripciones

The public course flow supports a backward-compatible lead mode and a provider-neutral checkout
mode. A submitted form, temporary seat hold, verified payment, enrollment, attendance, and course
completion are different states.

## Slugs
- `produccion-musical-feb-2026`
- Cohorts list for the selector page via `VITE_COURSE_COHORTS` (comma-separated).

## Database and rollout gate

- `course_registration` remains the compatible lead/admin record.
- `2026-08-17_course_checkout_runtime.sql` adds immutable approved policy versions, canonical
  checkout links, expiring seat holds, enrollment events, capacity locks, and verified-payment
  guards.
- Current database course prices are seeded as `draft`, `active=false`; activation requires an
  authorized review of price, tax, terms, cancellation, hold time, and payment schedule.
- `commerce.courses` defaults to disabled in production. `commerce.course_recurring_billing` is an
  independent gate and stays disabled until a real recurring merchant capability is verified.
- Rehearse apply/rerun/rollback and invariants with
  `npm run test:course-checkout-migration`.

## Backend endpoints
- **GET /public/courses/{slug}** → course metadata (sessions, syllabus, includes, CTA links).
- **POST /public/courses/{slug}/registrations** with `Idempotency-Key` → creates a canonical
  checkout and atomic seat hold when the domain gate and an approved policy are active. Otherwise it
  returns an honest `lead_received` response with `checkoutAvailable=false` and no reserved seat.
  - Request body:
    ```json
    {
      "fullName": "Ada Lovelace",
      "email": "ada@example.com",
      "phoneE164": "+593999001122",
      "source": "landing",
      "howHeard": "Instagram",
      "utm": { "source": "ads", "medium": "ig", "campaign": "feb2026" },
      "termsAccepted": true
    }
    ```
  - The server derives price, tax, schedule, and total from the active policy snapshot.
  - Duplicate requests are idempotent. A duplicate active email/seat request returns a conflict
    instead of creating multiple orders.
- **GET /public/courses/{slug}/registrations/{id}** with `X-Order-Lookup-Token` → customer-safe
  payment and enrollment tracking with enumeration-resistant not-found responses.
- **POST .../{id}/datafast/checkout** → creates/reuses the bound hosted Datafast checkout.
- **GET .../{id}/datafast/status?resourcePath=...** → verifies Datafast server to server. The return
  route alone never marks payment paid.
- **POST .../{id}/paypal/create** and **POST .../{id}/paypal/capture** → create and server-verify a
  bound PayPal Orders capture.
- Legacy Stripe payment-intent/subscription endpoints reject canonical runtime registrations; they
  remain only for historical compatibility while the shared rollout is gated.
- **PATCH /admin/courses/{slug}/registrations/{id}/status** (bearer auth, ModuleAdmin) → body `{ "status": "pending_payment" | "paid" | "cancelled" }`.
  - A canonical registration cannot be marked paid by this endpoint. It must already have a verified
    paid checkout; receipt upload is not payment proof.
- **GET /admin/courses/registrations/{id}/emails?limit=200** (bearer auth, ModuleAdmin) → persistent email audit trail for that registration (`sent` / `failed` / `skipped`, event type, timestamp, message).
- **GET /webhooks/whatsapp** → verification (hub.challenge echo when tokens match).
- **POST /webhooks/whatsapp** → listens for messages containing “inscribirme” (case-insensitive); creates/reuses a registration with `source=whatsapp` and replies with the landing link.

### WhatsApp env vars
- `WHATSAPP_TOKEN` (or `WA_TOKEN`)
- `WHATSAPP_PHONE_NUMBER_ID` (or `WA_PHONE_ID`)
- `WHATSAPP_VERIFY_TOKEN` (or `WA_VERIFY_TOKEN`)
- `COURSE_WHATSAPP_NUMBER` (or `WHATSAPP_CONTACT_NUMBER` / `WA_CONTACT_NUMBER`) — used for the wa.me CTA link.
- `HQ_APP_URL` — base URL for the landing link in replies (defaults to `https://tdf-app.pages.dev`).

## Frontend (tdf-hq-ui)
- Public routes:
  - Generic landing: `/curso/:slug`.
  - Secure checkout/order tracking: `/curso/:slug/orden/:registrationId` (requires the locally
    retained lookup token).
  - Production selector alias: `/curso/produccion-musical` (uses configured production cohorts).
  - Direct cohort: `/curso/produccion-musical-feb-2026`.
  - Legacy token flow: `/inscripcion/:slug?lead=<id>&t=<token>` still completes old lead invitations; `/inscripcion/:slug` without token params redirects to `/curso/:slug`.
- Uses generated API client (`npm run generate:api:ui`) to hit the public registration endpoint.
- Form auto-includes `source=landing` and UTM params from the URL (`utm_source`, `utm_medium`, `utm_campaign`, `utm_content`).
- The UI says `Solicitud recibida` when checkout is disabled and explicitly says no seat or payment
  exists. With checkout enabled it says `Cupo retenido temporalmente`, shows the expiry and amount,
  and offers only configured Datafast/PayPal methods. `Pago verificado` appears only after the server
  returns a verified paid state. API failure never renders a success state.
- Secondary CTA opens WhatsApp with the prefilled message “INSCRIBIRME Curso Produccion Musical”.

## Staging verification

1. Apply the migration twice and run the migration rehearsal.
2. Create and approve one non-production cohort policy matching `course.price_cents` and currency.
3. Enable `commerce.courses` only in the staging/sandbox environment.
4. Confirm simultaneous final-seat requests yield one hold and one conflict; let a hold expire and
   confirm capacity returns.
5. Exercise Datafast and PayPal sandbox create/pending/failure/verified paths. Check merchant,
   environment, amount, currency, order reference, and resource/capture binding evidence.
6. Confirm browser returns and failed provider calls never display paid/enrolled.
7. Reconcile checkout, provider attempt/binding, registration, runtime, and enrollment event.
8. Keep production disabled until provider credentials, approved policy, cancellation/refund owner,
   alerts, and rollback ownership are recorded.

## Quick local run
- Backend: `cd tdf-hq && stack build && stack run` (ensure env vars + migrations applied).
- Frontend: `cd tdf-hq-ui && npm install && npm run dev` then open `http://localhost:5173/curso/produccion-musical`.
