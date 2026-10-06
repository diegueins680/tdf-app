BEGIN;
-- Existing profiles and their grants are preserved. Roll back the application first.
UPDATE security_role_assignment_policy SET active=false, version=version+1, updated_at=now()
WHERE code='artist.self-service.artist' AND active;
COMMIT;
