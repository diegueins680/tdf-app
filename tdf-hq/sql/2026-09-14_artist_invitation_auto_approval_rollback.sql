BEGIN;

UPDATE security_role_assignment_policy
SET active = FALSE,
    updated_at = CURRENT_TIMESTAMP,
    version = version + 1
WHERE code = 'artist.invitation.artist'
  AND active = TRUE;

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

COMMIT;
