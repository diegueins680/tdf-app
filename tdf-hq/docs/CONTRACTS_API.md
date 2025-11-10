# Contracts API

## Endpoints
POST /contracts — body: JSON payload conforme a schema.
GET /contracts/{id}/pdf — devuelve application/pdf.
POST /contracts/{id}/send — body: { email } envía PDF.

## Infra
- Render: script `scripts/latex/render.sh` (tectonic).
- Storage: S3/GCS (env `PDF_BUCKET`).
- Email: SendGrid/SMTP.

## DB
See migration in db/migrations/2025-11-03_create_contracts.sql
