BEGIN;

CREATE OR REPLACE FUNCTION security_validate_assignment_policy()
RETURNS trigger
LANGUAGE plpgsql
AS $$
DECLARE
  role_allowed boolean;
BEGIN
  IF NEW.trigger_code NOT IN (
    'account-signup',
    'google-account-create',
    'verified-artist-claim',
    'artist-invitation-redeemed',
    'generated-account-create',
    'course-registration',
    'trial-inquiry',
    'teacher-subject-configured',
    'teacher-student-linked',
    'student-created',
    'artist-profile-created'
  ) THEN
    RAISE EXCEPTION 'unknown automatic security policy trigger' USING ERRCODE='23514';
  END IF;
  IF NEW.effective_from IS NOT NULL
     AND NEW.effective_to IS NOT NULL
     AND NEW.effective_to <= NEW.effective_from THEN
    RAISE EXCEPTION 'automatic security policy effective period is invalid' USING ERRCODE='23514';
  END IF;
  SELECT active AND automatic_assignable AND NOT emergency_administrator
    INTO role_allowed
    FROM security_role
   WHERE id = NEW.role_id;
  IF NOT COALESCE(role_allowed, FALSE) THEN
    RAISE EXCEPTION 'automatic security policies require an active, explicitly automatic, non-emergency role' USING ERRCODE='42501';
  END IF;
  IF NEW.created_by IS NOT NULL
     AND (NEW.approved_by IS NULL OR NEW.approved_by = NEW.created_by) THEN
    RAISE EXCEPTION 'automatic security policy changes require a distinct approver' USING ERRCODE='42501';
  END IF;
  RETURN NEW;
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
