
# Frontend (TDF-ui) — SOP Enforcement

- Muestra el modal al arrastrar una tarjeta a `Schedule`, recolecta metadatos de YouTube y llama a `POST /projects/:id/stage`.
- Maneja 400 con `missing` para enseñar qué requisitos faltan.

Archivos:
- `src/components/ScheduleModal.tsx`
- `src/kanban/withSopEnforcement.tsx`
- `src/hooks/useSopCompliance.ts`

Integra `withSopEnforcement` alrededor de tu Kanban y pasa tu cliente `api`.
