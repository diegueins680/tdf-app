# Artist Promotion Daily Report

This feature extends `LabelArtistsPage` with artist promotion schedule management and a per-artist daily PDF report.

## Timezone

- All report scheduling is managed in Ecuador time: `America/Guayaquil`.
- The PDF header explicitly labels the schedule as Ecuador time.
- Rows are ordered by local schedule time before rendering.

## Data Model

Backend entity: `ArtistPromoSlot`

Stored fields:

- `artistPartyId`
- `day`
- `startTime`
- `medium`
- `program`
- `interviewerHost`
- `bandMembers`
- `status` (optional)
- `notes` (optional)
- `createdAt`
- `updatedAt`

This model is the single source for both admin CRUD and the PDF report preview/download flow.

## Admin Endpoints

Under `/admin/artists/:artistId/promotions`:

- `GET ?day=YYYY-MM-DD`
  - List the day schedule rows for that artist.
- `POST`
  - Create a promotion row.
- `PUT /:promotionId`
  - Update a promotion row.
- `DELETE /:promotionId`
  - Delete a promotion row.
- `GET /report?day=YYYY-MM-DD`
  - Return the report preview payload with header + ordered rows.
- `GET /report/pdf?day=YYYY-MM-DD`
  - Generate and download the PDF (`OctetStream` + `Content-Disposition`).

## PDF Layout

The PDF uses the repo LaTeX/PDF generation path (`TDF.Handlers.InputList.generateInputListPdf`) and includes:

- clear day/date header
- Ecuador-time label
- rows ordered by time
- columns for:
  - medium
  - program
  - interviewer/host
  - participating band members
  - optional status
  - optional notes

## Admin UI

`tdf-hq-ui/src/pages/LabelArtistsPage.tsx` now includes:

- artist selector for the report context
- day selector
- CRUD form for daily promotion slots
- report preview table
- PDF preview button
- PDF download button

The editable day table and the preview both read from the same backend schedule data.
