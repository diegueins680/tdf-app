BEGIN;

-- Extend the existing validator without dropping independently added trigger
-- codes (e.g. artist-self-service-activated from 2026-09-16).
DO $$ DECLARE definition text; BEGIN
  SELECT pg_get_functiondef('security_validate_assignment_policy()'::regprocedure) INTO definition;
  IF position('''artist-invitation-redeemed''' IN definition)=0 THEN
    IF position('''verified-artist-claim''' IN definition)=0 THEN
      RAISE EXCEPTION 'Unrecognized automatic assignment policy validator';
    END IF;
    EXECUTE replace(definition, '''verified-artist-claim''', '''verified-artist-claim'',''artist-invitation-redeemed''');
  END IF;
END $$;

INSERT INTO security_role_assignment_policy (
  id,
  code,
  trigger_code,
  role_id,
  name_es,
  name_en,
  description_es,
  description_en,
  requires_verified_email,
  active,
  effective_from,
  effective_to,
  created_by,
  updated_by,
  approved_by,
  created_at,
  updated_at,
  version
)
SELECT
  '00000000-0000-4000-8000-000000000311'::uuid,
  'artist.invitation.artist',
  'artist-invitation-redeemed',
  role.id,
  'Artista por invitación de campaña',
  'Artist from campaign invitation',
  'Activa Artista al canjear una invitación explícita y reconocida; no crea roles administrativos.',
  'Activates Artist when an explicit recognized invitation is redeemed; it creates no administrative roles.',
  FALSE,
  TRUE,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  CURRENT_TIMESTAMP,
  CURRENT_TIMESTAMP,
  1
FROM security_role role
WHERE role.code = 'artist'
ON CONFLICT (code) DO UPDATE SET
  trigger_code = EXCLUDED.trigger_code,
  role_id = EXCLUDED.role_id,
  name_es = EXCLUDED.name_es,
  name_en = EXCLUDED.name_en,
  description_es = EXCLUDED.description_es,
  description_en = EXCLUDED.description_en,
  requires_verified_email = EXCLUDED.requires_verified_email,
  active = TRUE,
  effective_from = NULL,
  effective_to = NULL,
  updated_at = CURRENT_TIMESTAMP,
  version = security_role_assignment_policy.version + 1;

COMMIT;
