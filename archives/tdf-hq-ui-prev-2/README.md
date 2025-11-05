
# TDF HQ UI (React + Vite + MUI + React Query)

Front-end for the TDF HQ API. Includes:
- Parties (CRM) list/create/edit
- Bookings calendar (FullCalendar) wired to `/bookings`
- Pipelines kanban for Mixing/Mastering (hello-pangea/dnd; in-memory for now)
- TS + Vite + MUI + React Query + React Router

## Configure

Create `.env` (local dev):
```
VITE_API_BASE=http://localhost:8080
VITE_TZ=America/Guayaquil
```

Install & run:
```bash
npm ci
npm run dev
```

## Render (Static Site)
1. Push this repo to GitHub.
2. In Render: New → Static Site → connect repo.
   - Build Command: `npm ci && npm run build`
   - Publish Directory: `dist`
3. Environment Variables:
   - `VITE_API_BASE=https://<your-api>.onrender.com`
   - `VITE_TZ=America/Guayaquil`
4. Save & Deploy.

> Ensure your API has CORS enabled (e.g., via `wai-cors`), allowing the Static Site origin.

## Pages
- `/parties` — list/create party, edit Instagram
- `/bookings` — calendar; click-drag to select a time range and create a booking
- `/pipelines` — basic kanban for Mixing/Mastering (local state, ready to wire to API)

## Notes
- When your backend exposes pipeline endpoints, POST stage changes on `onDragEnd` in `src/pages/PipelinesPage.tsx`.
- FullCalendar styling is included via its CSS imports.
