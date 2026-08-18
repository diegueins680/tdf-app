-- Provider-neutral course checkout, atomic expiring seat holds, and truthful
-- enrollment/payment separation. Existing registrations are preserved and are
-- not silently classified as canonical checkouts.
BEGIN;

CREATE TABLE IF NOT EXISTS course_checkout_policy (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  course_id BIGINT NOT NULL REFERENCES course(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL CHECK (length(btrim(policy_version)) BETWEEN 1 AND 80),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  price_minor BIGINT NOT NULL CHECK (price_minor > 0 AND price_minor <= 2147483647),
  tax_bps INTEGER NOT NULL DEFAULT 0 CHECK (tax_bps BETWEEN 0 AND 10000),
  payment_mode TEXT NOT NULL DEFAULT 'full'
    CHECK (payment_mode IN ('full','deposit')),
  deposit_bps INTEGER NOT NULL DEFAULT 10000
    CHECK (deposit_bps BETWEEN 1 AND 10000),
  hold_minutes INTEGER NOT NULL DEFAULT 15 CHECK (hold_minutes BETWEEN 5 AND 60),
  terms_version TEXT NOT NULL CHECK (length(btrim(terms_version)) BETWEEN 1 AND 80),
  terms_summary TEXT NOT NULL CHECK (length(btrim(terms_summary)) BETWEEN 1 AND 2000),
  cancellation_policy TEXT NOT NULL
    CHECK (length(btrim(cancellation_policy)) BETWEEN 1 AND 2000),
  transfer_allowed BOOLEAN NOT NULL DEFAULT TRUE,
  approval_status TEXT NOT NULL DEFAULT 'draft'
    CHECK (approval_status IN ('draft','approved','retired')),
  active BOOLEAN NOT NULL DEFAULT FALSE,
  approved_at TIMESTAMPTZ,
  approved_by TEXT,
  effective_from TIMESTAMPTZ,
  effective_until TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (course_id, policy_version),
  CHECK (
    (payment_mode = 'full' AND deposit_bps = 10000)
    OR (payment_mode = 'deposit' AND deposit_bps < 10000)
  ),
  CHECK (
    active = FALSE
    OR (approval_status = 'approved' AND approved_at IS NOT NULL AND approved_by IS NOT NULL)
  ),
  CHECK (effective_until IS NULL OR effective_from IS NULL OR effective_until > effective_from)
);

CREATE UNIQUE INDEX IF NOT EXISTS uq_course_checkout_active_policy
  ON course_checkout_policy(course_id)
  WHERE active;

CREATE TABLE IF NOT EXISTS course_checkout_policy_history (
  id BIGSERIAL PRIMARY KEY,
  policy_id UUID NOT NULL,
  course_id BIGINT NOT NULL,
  policy_version TEXT NOT NULL,
  snapshot JSONB NOT NULL,
  changed_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  changed_by TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS course_registration_checkout_runtime (
  registration_id BIGINT PRIMARY KEY REFERENCES course_registration(id) ON DELETE RESTRICT,
  course_id BIGINT NOT NULL REFERENCES course(id) ON DELETE RESTRICT,
  checkout_id UUID NOT NULL UNIQUE REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  policy_id UUID NOT NULL REFERENCES course_checkout_policy(id) ON DELETE RESTRICT,
  policy_version TEXT NOT NULL,
  lookup_token_hash TEXT NOT NULL UNIQUE CHECK (lookup_token_hash ~ '^[0-9a-f]{64}$'),
  create_idempotency_key TEXT NOT NULL UNIQUE CHECK (length(create_idempotency_key) BETWEEN 16 AND 128),
  create_request_sha256 TEXT NOT NULL CHECK (create_request_sha256 ~ '^[0-9a-f]{64}$'),
  enrollment_status TEXT NOT NULL DEFAULT 'seat_held' CHECK (enrollment_status IN (
    'seat_held','enrolled','waitlisted','transfer_requested','transferred',
    'cancelled','completed','expired'
  )),
  payment_schedule TEXT NOT NULL CHECK (payment_schedule IN ('full','deposit')),
  payment_status TEXT NOT NULL DEFAULT 'awaiting_payment' CHECK (payment_status IN (
    'awaiting_payment','processing','paid','partially_refunded','refunded',
    'disputed','chargeback'
  )),
  balance_status TEXT NOT NULL DEFAULT 'not_due' CHECK (balance_status IN (
    'not_due','due','awaiting_payment','processing','paid','partially_refunded',
    'refunded','waived','written_off','disputed','chargeback'
  )),
  seat_count INTEGER NOT NULL DEFAULT 1 CHECK (seat_count = 1),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  price_minor BIGINT NOT NULL CHECK (price_minor > 0),
  tax_bps INTEGER NOT NULL CHECK (tax_bps BETWEEN 0 AND 10000),
  tax_minor BIGINT NOT NULL CHECK (tax_minor >= 0),
  total_minor BIGINT NOT NULL CHECK (total_minor = price_minor + tax_minor),
  due_now_minor BIGINT NOT NULL CHECK (due_now_minor > 0 AND due_now_minor <= total_minor),
  balance_minor BIGINT NOT NULL CHECK (balance_minor = total_minor - due_now_minor),
  terms_version TEXT NOT NULL,
  terms_accepted_at TIMESTAMPTZ NOT NULL,
  hold_expires_at TIMESTAMPTZ NOT NULL,
  enrolled_at TIMESTAMPTZ,
  completed_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (hold_expires_at > created_at),
  CHECK (enrollment_status <> 'enrolled' OR enrolled_at IS NOT NULL),
  CHECK (enrollment_status <> 'completed' OR completed_at IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_course_registration_runtime_capacity
  ON course_registration_checkout_runtime(course_id, enrollment_status, hold_expires_at);
CREATE INDEX IF NOT EXISTS idx_course_registration_runtime_payment
  ON course_registration_checkout_runtime(payment_status, hold_expires_at);

CREATE TABLE IF NOT EXISTS course_enrollment_event (
  id BIGSERIAL PRIMARY KEY,
  registration_id BIGINT NOT NULL
    REFERENCES course_registration_checkout_runtime(registration_id) ON DELETE RESTRICT,
  from_status TEXT,
  to_status TEXT NOT NULL,
  actor_type TEXT NOT NULL CHECK (actor_type IN ('system','operator','provider','customer')),
  actor_id TEXT,
  reason_code TEXT,
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_course_enrollment_event_registration
  ON course_enrollment_event(registration_id, created_at, id);

CREATE OR REPLACE FUNCTION course_checkout_validate_policy()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.active AND NOT EXISTS (
    SELECT 1 FROM course
    WHERE id = NEW.course_id
      AND price_cents = NEW.price_minor
      AND upper(currency) = NEW.currency
      AND capacity > 0
  ) THEN
    RAISE EXCEPTION 'Active course policy must match the authoritative course price and currency';
  END IF;
  IF TG_OP = 'UPDATE' AND OLD.approval_status = 'approved'
     AND ROW(
       NEW.course_id, NEW.policy_version, NEW.currency, NEW.price_minor,
       NEW.tax_bps, NEW.payment_mode, NEW.deposit_bps, NEW.hold_minutes,
       NEW.terms_version, NEW.terms_summary, NEW.cancellation_policy,
       NEW.transfer_allowed
     ) IS DISTINCT FROM ROW(
       OLD.course_id, OLD.policy_version, OLD.currency, OLD.price_minor,
       OLD.tax_bps, OLD.payment_mode, OLD.deposit_bps, OLD.hold_minutes,
       OLD.terms_version, OLD.terms_summary, OLD.cancellation_policy,
       OLD.transfer_allowed
     ) THEN
    RAISE EXCEPTION 'Approved course checkout policy is immutable; create a new version';
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_checkout_validate_policy ON course_checkout_policy;
CREATE TRIGGER trg_course_checkout_validate_policy
  BEFORE INSERT OR UPDATE ON course_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION course_checkout_validate_policy();

CREATE OR REPLACE FUNCTION course_checkout_policy_history_capture()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN
    INSERT INTO course_checkout_policy_history(
      policy_id, course_id, policy_version, snapshot, changed_by
    ) VALUES (
      OLD.id, OLD.course_id, OLD.policy_version, to_jsonb(OLD), current_user
    );
    RETURN OLD;
  END IF;
  INSERT INTO course_checkout_policy_history(
    policy_id, course_id, policy_version, snapshot, changed_by
  ) VALUES (
    NEW.id, NEW.course_id, NEW.policy_version, to_jsonb(NEW), current_user
  );
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_checkout_policy_history ON course_checkout_policy;
CREATE TRIGGER trg_course_checkout_policy_history
  AFTER INSERT OR UPDATE OR DELETE ON course_checkout_policy
  FOR EACH ROW EXECUTE FUNCTION course_checkout_policy_history_capture();

CREATE OR REPLACE FUNCTION course_checkout_validate_runtime()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout commerce_checkout_session%ROWTYPE;
  registration course_registration%ROWTYPE;
  policy course_checkout_policy%ROWTYPE;
  course_capacity INTEGER;
  locked_course_slug TEXT;
  consumed_seats INTEGER;
BEGIN
  SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
  SELECT * INTO registration FROM course_registration WHERE id = NEW.registration_id;
  SELECT * INTO policy FROM course_checkout_policy WHERE id = NEW.policy_id;
  IF checkout.id IS NULL OR registration.id IS NULL OR policy.id IS NULL THEN
    RAISE EXCEPTION 'Course checkout runtime references missing canonical records';
  END IF;
  IF checkout.domain_type <> 'course_registration'
     OR checkout.domain_order_id <> NEW.registration_id::text
     OR checkout.total_minor <> NEW.due_now_minor
     OR checkout.currency <> NEW.currency THEN
    RAISE EXCEPTION 'Course checkout runtime does not match immutable checkout amount and identity';
  END IF;
  IF registration.course_slug <> (SELECT slug FROM course WHERE id = NEW.course_id)
     OR policy.course_id <> NEW.course_id
     OR policy.policy_version <> NEW.policy_version
     OR policy.currency <> NEW.currency
     OR policy.price_minor <> NEW.price_minor
     OR policy.tax_bps <> NEW.tax_bps
     OR policy.payment_mode <> NEW.payment_schedule THEN
    RAISE EXCEPTION 'Course checkout runtime does not match registration and approved policy';
  END IF;
  IF TG_OP = 'INSERT' AND (
       policy.approval_status <> 'approved'
       OR NOT policy.active
       OR policy.approved_at IS NULL
       OR policy.approved_by IS NULL
     ) THEN
    RAISE EXCEPTION 'New course checkout runtime requires an approved active policy';
  END IF;
  SELECT capacity, slug INTO course_capacity, locked_course_slug
    FROM course WHERE id = NEW.course_id FOR UPDATE;
  IF NEW.enrollment_status = 'seat_held' AND NEW.hold_expires_at <= NOW() THEN
    RAISE EXCEPTION 'Course seat hold must expire in the future';
  END IF;
  IF NEW.enrollment_status IN ('seat_held','enrolled','transfer_requested','completed') THEN
    SELECT
      (SELECT count(*)
       FROM course_registration_checkout_runtime existing
       WHERE existing.course_id = NEW.course_id
         AND existing.registration_id <> NEW.registration_id
         AND (
           (existing.enrollment_status = 'seat_held' AND existing.hold_expires_at > NOW())
           OR existing.enrollment_status IN ('enrolled','transfer_requested','completed')
         ))
      +
      (SELECT count(*)
       FROM course_registration legacy
       WHERE legacy.course_slug = locked_course_slug
         AND legacy.status = 'paid'
         AND legacy.id <> NEW.registration_id
         AND NOT EXISTS (
           SELECT 1 FROM course_registration_checkout_runtime linked
           WHERE linked.registration_id = legacy.id
         ))
      INTO consumed_seats;
    IF consumed_seats + NEW.seat_count > course_capacity THEN
      RAISE EXCEPTION 'Course capacity is exhausted';
    END IF;
  END IF;
  IF NEW.enrollment_status = 'enrolled' THEN
    NEW.enrolled_at := COALESCE(NEW.enrolled_at, NOW());
  END IF;
  NEW.updated_at := NOW();
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_checkout_validate_runtime
  ON course_registration_checkout_runtime;
CREATE TRIGGER trg_course_checkout_validate_runtime
  BEFORE INSERT OR UPDATE ON course_registration_checkout_runtime
  FOR EACH ROW EXECUTE FUNCTION course_checkout_validate_runtime();

CREATE OR REPLACE FUNCTION course_checkout_require_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.domain_type = 'course_registration'
     AND NEW.status = 'paid'
     AND OLD.status <> 'paid'
     AND NOT EXISTS (
       SELECT 1
       FROM commerce_payment_attempt attempt
       JOIN commerce_provider_binding binding
         ON binding.payment_attempt_id = attempt.id
        AND binding.provider = attempt.provider
        AND binding.environment = attempt.environment
        AND binding.merchant_account_ref = attempt.merchant_account_ref
        AND binding.merchant_reference = NEW.domain_order_id
        AND binding.amount_minor = NEW.total_minor
        AND binding.currency = NEW.currency
       WHERE attempt.checkout_id = NEW.id
         AND attempt.status = 'succeeded'
         AND attempt.environment = NEW.environment
         AND attempt.amount_minor = NEW.total_minor
         AND attempt.currency = NEW.currency
         AND (
           (attempt.provider = 'datafast' AND binding.resource_type = 'checkout')
           OR (attempt.provider = 'paypal' AND binding.resource_type = 'capture')
           OR (attempt.provider = 'stripe' AND binding.resource_type = 'payment')
           OR (
             attempt.provider IN ('bank_transfer','cash','pos')
             AND binding.resource_type = 'manual_evidence'
             AND EXISTS (
               SELECT 1 FROM commerce_manual_payment_evidence evidence
               WHERE evidence.checkout_id = NEW.id
                 AND evidence.payment_attempt_id = attempt.id
                 AND evidence.status = 'approved'
                 AND evidence.submitted_by IS NOT NULL
                 AND evidence.reviewed_by IS NOT NULL
                 AND evidence.submitted_by <> evidence.reviewed_by
             )
           )
         )
     ) THEN
    RAISE EXCEPTION 'Course checkout cannot become paid without bound verified payment evidence';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_checkout_require_verified_payment
  ON commerce_checkout_session;
CREATE TRIGGER trg_course_checkout_require_verified_payment
  BEFORE UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION course_checkout_require_verified_payment();

CREATE OR REPLACE FUNCTION course_checkout_sync_verified_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE runtime_registration BIGINT;
BEGIN
  IF NEW.domain_type <> 'course_registration' OR OLD.status = NEW.status THEN
    RETURN NEW;
  END IF;
  SELECT registration_id INTO runtime_registration
    FROM course_registration_checkout_runtime WHERE checkout_id = NEW.id;
  IF runtime_registration IS NULL THEN RETURN NEW; END IF;
  IF NEW.status = 'paid' THEN
    UPDATE course_registration_checkout_runtime
      SET enrollment_status = 'enrolled', payment_status = 'paid',
          enrolled_at = COALESCE(enrolled_at, NOW()), updated_at = NOW()
      WHERE registration_id = runtime_registration AND enrollment_status = 'seat_held';
    UPDATE course_registration SET status = 'paid', updated_at = NOW()
      WHERE id = runtime_registration AND status = 'pending_payment';
    INSERT INTO course_enrollment_event(
      registration_id, from_status, to_status, actor_type, reason_code, notes
    ) VALUES (
      runtime_registration, 'seat_held', 'enrolled', 'provider',
      'verified_payment',
      'Seat enrolled only after canonical checkout received verified payment evidence'
    );
  ELSIF NEW.status IN ('cancelled','expired') THEN
    UPDATE course_registration_checkout_runtime
      SET enrollment_status = NEW.status, updated_at = NOW()
      WHERE registration_id = runtime_registration AND enrollment_status = 'seat_held';
    UPDATE course_registration SET status = 'cancelled', updated_at = NOW()
      WHERE id = runtime_registration AND status = 'pending_payment';
    INSERT INTO course_enrollment_event(
      registration_id, from_status, to_status, actor_type, reason_code, notes
    ) VALUES (
      runtime_registration, 'seat_held', NEW.status, 'system',
      'checkout_' || NEW.status, 'Unpaid seat hold released'
    );
  ELSIF NEW.status = 'processing' THEN
    UPDATE course_registration_checkout_runtime
      SET payment_status = 'processing', updated_at = NOW()
      WHERE registration_id = runtime_registration
        AND payment_status = 'awaiting_payment';
  ELSIF NEW.status IN ('partially_refunded','refunded','disputed','chargeback') THEN
    UPDATE course_registration_checkout_runtime
      SET payment_status = NEW.status, updated_at = NOW()
      WHERE registration_id = runtime_registration;
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_checkout_sync_verified_payment
  ON commerce_checkout_session;
CREATE TRIGGER trg_course_checkout_sync_verified_payment
  AFTER UPDATE OF status ON commerce_checkout_session
  FOR EACH ROW EXECUTE FUNCTION course_checkout_sync_verified_payment();

CREATE OR REPLACE FUNCTION course_registration_require_canonical_payment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.status = 'paid' AND OLD.status <> 'paid'
     AND EXISTS (
       SELECT 1 FROM course_registration_checkout_runtime
       WHERE registration_id = NEW.id
     )
     AND NOT EXISTS (
       SELECT 1
       FROM course_registration_checkout_runtime runtime
       JOIN commerce_checkout_session checkout ON checkout.id = runtime.checkout_id
       WHERE runtime.registration_id = NEW.id AND checkout.status = 'paid'
     ) THEN
    RAISE EXCEPTION 'Canonical course registration cannot become paid before verified checkout payment';
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_course_registration_require_canonical_payment
  ON course_registration;
CREATE TRIGGER trg_course_registration_require_canonical_payment
  BEFORE UPDATE OF status ON course_registration
  FOR EACH ROW EXECUTE FUNCTION course_registration_require_canonical_payment();

-- The optional course scope lets checkout creation expire only the cohort row it
-- already locked. This avoids cross-course lock ordering while retaining the
-- one-argument global sweep used by the background/operator path.
DROP FUNCTION IF EXISTS course_checkout_expire_holds(TIMESTAMPTZ);
CREATE OR REPLACE FUNCTION course_checkout_expire_holds(
  at_time TIMESTAMPTZ DEFAULT NOW(),
  target_course_id BIGINT DEFAULT NULL
)
RETURNS INTEGER LANGUAGE plpgsql AS $$
DECLARE expired_count INTEGER;
BEGIN
  WITH expired AS (
    UPDATE commerce_checkout_session checkout
      SET status = 'expired', updated_at = at_time
      FROM course_registration_checkout_runtime runtime
      WHERE checkout.id = runtime.checkout_id
        AND checkout.domain_type = 'course_registration'
        AND checkout.status IN ('holding','awaiting_payment','failed')
        AND runtime.enrollment_status = 'seat_held'
        AND runtime.hold_expires_at <= at_time
        AND (target_course_id IS NULL OR runtime.course_id = target_course_id)
      RETURNING checkout.id
  ) SELECT count(*) INTO expired_count FROM expired;
  RETURN expired_count;
END $$;

-- Preserve current public course prices as reviewable policy drafts. Activating
-- a policy requires an explicit operational/tax/terms approval; the migration
-- never makes a historical lead form payable by itself.
INSERT INTO course_checkout_policy(
  course_id, policy_version, currency, price_minor, tax_bps, payment_mode,
  deposit_bps, hold_minutes, terms_version, terms_summary,
  cancellation_policy, approval_status, active
)
SELECT
  course.id,
  'course-v1-db-' || course.id::text,
  upper(course.currency),
  course.price_cents,
  0,
  'full',
  10000,
  15,
  'course-terms-draft-v1',
  'Borrador migrado desde el precio publicado. Requiere aprobación operativa, tributaria y legal antes de activar checkout.',
  'Política de cancelación pendiente de aprobación; no activar este borrador.',
  'draft',
  FALSE
FROM course
WHERE course.price_cents > 0
ON CONFLICT (course_id, policy_version) DO NOTHING;

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('commerce.courses', FALSE, 'production',
   'Requires approved active cohort policy, provider sandbox evidence, cancellation ownership, and staged rollout'),
  ('commerce.course_recurring_billing', FALSE, 'production',
   'Automatic renewals require verified merchant recurring capability and an approved subscription product')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
