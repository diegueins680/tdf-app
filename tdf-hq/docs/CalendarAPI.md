# Calendar API (draft)

Propuesta de endpoint para alimentar el calendario unificado del Domo.

## GET /calendar
Parámetros opcionales:
- `type`: `Rental` | `Artistic` | `Family`
- `status`: `Hold` | `Confirmed` | `Invoiced` | `Paid`

Header opcional:
- `X-Role: staff` → devuelve `title` y `status` completos. Sin header: modo público `free/busy`.

### Respuesta (staff)
```json
[{
  "title": "Verde70 – Rehearsal – 3h",
  "start": "2025-11-05T14:00:00Z",
  "end":   "2025-11-05T17:00:00Z",
  "ctype": "Artistic",
  "status": "Confirmed"
}]
```

### Respuesta (público)
```json
[{
  "title": null,
  "start": "2025-11-05T14:00:00Z",
  "end":   "2025-11-05T17:00:00Z",
  "ctype": "Artistic",
  "status": null
}]
```

## Notas de implementación
- Primera iteración puede ser mock o proxy a Google Calendar (calendarios "Domo – Rentals" y "Domo – Artístico").
- Mantener contrato JSON estable para TDF-ui y TDF-mobile.
- Agregar filtros por query param es opcional, pero recomendado.

## Futuro
- Persistir reservas en DB con `status` (Hold/Confirmed/Invoiced/Paid).
- Webhook/cron de expiración de holds a 48h.
- Cálculo de horas y mapeo a Revenue Tracker.
