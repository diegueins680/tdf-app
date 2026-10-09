BEGIN;

UPDATE security_role_assignment_policy
SET active = FALSE,
    updated_at = CURRENT_TIMESTAMP,
    version = version + 1
WHERE code = 'artist.invitation.artist'
  AND active = TRUE;

-- Remove only this migration's trigger code; keep codes added by others.
DO $$ DECLARE definition text; BEGIN
  SELECT pg_get_functiondef('security_validate_assignment_policy()'::regprocedure) INTO definition;
  IF position(',''artist-invitation-redeemed''' IN definition)>0 THEN
    EXECUTE replace(definition, ',''artist-invitation-redeemed''', '');
  END IF;
END $$;

COMMIT;
