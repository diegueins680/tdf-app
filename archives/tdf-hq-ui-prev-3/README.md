# TDF HQ UI (React + Vite + MUI + React Query + FullCalendar + DnD)

Front-end for the TDF HQ backend (Servant + PostgreSQL). It includes:
- Parties screen (list, create, edit Instagram/phone).
- Bookings screen with FullCalendar (wired to `/bookings`).
- Basic Kanban for Mixing/Mastering (client-side demo, ready to POST when endpoints exist).

## Quick start (local)

```bash
npm i
cp .env.example .env
# set VITE_API_BASE to your backend; for local dev it's usually http://localhost:8080
npm run dev
```

Open http://localhost:5173.

## Environment

Create `.env` with:

```
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

## Deploy to Render (Static Site)

1. Push this repo to GitHub.
2. In Render → New → Static Site → pick this repo.
3. Build command: `npm ci && npm run build`
4. Publish directory: `dist`
5. Environment variables:
   - `VITE_API_BASE=https://<your-api>.onrender.com`
   - `VITE_TZ=America/Guayaquil`
6. Create the site. After build, open the URL Render gives you.

> Ensure your backend allows CORS from the static site origin. For dev you can use permissive CORS and later restrict to your static URL.

## Notes

- The calendar reads from `/bookings` and expects items with `{ bookingId, title, startsAt, endsAt }`.
- The Kanban is client-side only for now; when backend endpoints for pipelines exist, wire `onDragEnd` to POST changes.
- UI built with MUI v6 and a minimal custom theme.


**Note:** If you cloned without a lockfile, run `npm install` (not `npm ci`).
