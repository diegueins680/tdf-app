# Curso de Producción Musical — Inscripciones

End-to-end flow for the in-person course **“Curso de Producción Musical – Ene 2026 / Feb 2026”**. Includes public landing, backend endpoints, WhatsApp webhook, and admin status update.

## Slug
- `produccion-musical-dic-2025`

## Database
- New table `course_registration` via Persistent + SQL mirror: `tdf-hq/sql/2025-11-21_course_registrations.sql`.
- Apply manually if needed:
  ```bash
  cd tdf-hq
  ./scripts/migrate_course_registrations.sh
  ```
- `RUN_MIGRATIONS=true` will create the table automatically on startup.

## Backend endpoints
- **GET /public/courses/{slug}** → course metadata (sessions, syllabus, includes, CTA links).
- **POST /public/courses/{slug}/registrations** → create/update a registration in `pending_payment`.
  - Request body:
    ```json
    {
      "fullName": "Ada Lovelace",
      "email": "ada@example.com",
      "phoneE164": "+593999001122",
      "source": "landing",
      "howHeard": "Instagram",
      "utm": { "source": "ads", "medium": "ig", "campaign": "dic2025" }
    }
    ```
  - Behavior: if a pending registration for the same slug + email/phone exists, it is updated; otherwise a new row is inserted.
- **PATCH /admin/courses/{slug}/registrations/{id}/status** (bearer auth, ModuleAdmin) → body `{ "status": "pending_payment" | "paid" | "cancelled" }`.
- **GET /webhooks/whatsapp** → verification (hub.challenge echo when tokens match).
- **POST /webhooks/whatsapp** → listens for messages containing “inscribirme” (case-insensitive); creates/reuses a registration with `source=whatsapp` and replies with the landing link.

### WhatsApp env vars
- `WHATSAPP_TOKEN` (or `WA_TOKEN`)
- `WHATSAPP_PHONE_NUMBER_ID` (or `WA_PHONE_ID`)
- `WHATSAPP_VERIFY_TOKEN` (or `WA_VERIFY_TOKEN`)
- `COURSE_WHATSAPP_NUMBER` (or `WHATSAPP_CONTACT_NUMBER` / `WA_CONTACT_NUMBER`) — used for the wa.me CTA link.
- `HQ_APP_URL` — base URL for the landing link in replies (defaults to `https://tdf-app.pages.dev`).

## Frontend (tdf-hq-ui)
- Public route: `/curso/produccion-musical-dic-2025`.
- Uses generated API client (`npm run generate:api:ui`) to hit the public registration endpoint.
- Form auto-includes `source=landing` and UTM params from the URL (`utm_source`, `utm_medium`, `utm_campaign`, `utm_content`).
- Success state locks the form and shows “¡Gracias! Hemos recibido tu inscripción…”. Errors prompt to retry or use WhatsApp.
- Secondary CTA opens WhatsApp with the prefilled message “INSCRIBIRME Curso Produccion Musical”.

## Quick local run
- Backend: `cd tdf-hq && stack build && stack run` (ensure env vars + migrations applied).
- Frontend: `cd tdf-hq-ui && npm install && npm run dev` then open `http://localhost:5173/curso/produccion-musical-dic-2025`.
