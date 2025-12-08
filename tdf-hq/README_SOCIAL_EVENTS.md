# Social Events Backend Scaffold

This folder contains initial scaffold for the Social Events feature.

Files added:

- `migrations/0001_create_social_events.sql` — SQL migration to create the core tables (artists, venues, events, rsvps, invitations, junction tables).
- `src/TDF/SocialEvents.hs` — Haskell module stub for server handlers. Implement handlers here and register them in `TDF.Server`.
- `src/TDF/DTO/SocialEventsDTO.hs` — Placeholder DTOs for early integration and testing. Replace with more complete DTOs and validations.

Next steps for backend implementers:

1. Review `migrations/0001_create_social_events.sql` and adjust types/constraints to match your `parties` table and other existing schemas.
2. Add Persistent model definitions in `TDF.Models` or equivalent and create Persistent migrations if your stack uses them.
3. Implement the handlers in `TDF.SocialEvents` and wire them into `TDF.Server`.
4. Add DTO validation and conversions between Persistent entities and DTOs.
5. Add tests and integration cURL examples (see `tdf-hq/AGENTS.md` for conventions).
