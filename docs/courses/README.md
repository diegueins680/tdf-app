# Course publish payloads

Reusable payloads for the existing TDF course API.

## Guillermo Diaz drum course

Payload:

```bash
docs/courses/bateria-guillermo-diaz-abr-2026.course.json
```

Recommended public link after publishing:

```text
https://tdf-app.pages.dev/curso/bateria-guillermo-diaz-abr-2026
```

Publish with an admin bearer token:

```bash
TDF_ADMIN_TOKEN="..." node scripts/upsert-course.mjs docs/courses/bateria-guillermo-diaz-abr-2026.course.json
```

The helper defaults to the live API root:

```text
https://tdf-hq.fly.dev
```

Override the API root only for local or staging work:

```bash
TDF_ADMIN_TOKEN="..." TDF_COURSE_API_BASE="http://localhost:8080" node scripts/upsert-course.mjs docs/courses/bateria-guillermo-diaz-abr-2026.course.json
```

Notes:

- The course uses the public instructor image at `https://tdf-app.pages.dev/assets/tdf-ui/guillermo-diaz-bateria.jpg`.
- The April 2026 dates are valid as of 2026-04-19. If Diego publishes after the first session date, update the dates before POSTing because the admin API rejects past session dates.
- The payload leaves `whatsappCtaUrl` as `null` so the backend can build the standard WhatsApp CTA from the configured contact number and the course landing URL.
