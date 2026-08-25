BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM course_registration_checkout_runtime)
     OR EXISTS (SELECT 1 FROM course_enrollment_event) THEN
    RAISE EXCEPTION 'Course checkout rollback blocked: runtime/enrollment evidence exists';
  END IF;
  IF EXISTS (
    SELECT 1 FROM course_checkout_policy
    WHERE approval_status = 'approved' OR active
  ) THEN
    RAISE EXCEPTION 'Course checkout rollback blocked: approved or active policies exist';
  END IF;
END $$;

DELETE FROM revenue_feature_flag
WHERE environment = 'production'
  AND flag_key IN ('commerce.courses','commerce.course_recurring_billing');

DROP TRIGGER IF EXISTS trg_course_checkout_sync_verified_payment
  ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_course_checkout_require_verified_payment
  ON commerce_checkout_session;
DROP TRIGGER IF EXISTS trg_course_registration_require_canonical_payment
  ON course_registration;
DROP FUNCTION IF EXISTS course_registration_require_canonical_payment();
DROP FUNCTION IF EXISTS course_checkout_sync_verified_payment();
DROP FUNCTION IF EXISTS course_checkout_require_verified_payment();
DROP FUNCTION IF EXISTS course_checkout_expire_holds(TIMESTAMPTZ, BIGINT);
DROP FUNCTION IF EXISTS course_checkout_expire_holds(TIMESTAMPTZ);

DROP TRIGGER IF EXISTS trg_course_checkout_validate_runtime
  ON course_registration_checkout_runtime;
DROP FUNCTION IF EXISTS course_checkout_validate_runtime();
DROP TRIGGER IF EXISTS trg_course_checkout_policy_history
  ON course_checkout_policy;
DROP FUNCTION IF EXISTS course_checkout_policy_history_capture();
DROP TRIGGER IF EXISTS trg_course_checkout_validate_policy
  ON course_checkout_policy;
DROP FUNCTION IF EXISTS course_checkout_validate_policy();

DROP TABLE IF EXISTS course_enrollment_event;
DROP TABLE IF EXISTS course_registration_checkout_runtime;
DROP TABLE IF EXISTS course_checkout_policy_history;
DROP TABLE IF EXISTS course_checkout_policy;

COMMIT;
