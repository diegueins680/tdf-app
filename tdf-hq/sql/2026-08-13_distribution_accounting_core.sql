-- Versioned distribution, delivery evidence, usage, royalties, and payout gates.
-- Requires 2026-08-02_ddex_catalog_core and 2026-08-13_unified_checkout_core.
BEGIN;

CREATE TABLE distribution_product_version (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  product_key TEXT NOT NULL,
  version INT NOT NULL CHECK (version > 0),
  product_kind TEXT NOT NULL CHECK (product_kind IN ('single','ep','album','catalog_management','subscription','add_on')),
  locale TEXT NOT NULL DEFAULT 'es',
  name TEXT NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  price_minor BIGINT NOT NULL CHECK (price_minor >= 0),
  royalty_share_bps INT NOT NULL DEFAULT 0 CHECK (royalty_share_bps BETWEEN 0 AND 10000),
  terms_version TEXT NOT NULL,
  included_services JSONB NOT NULL DEFAULT '{}'::jsonb,
  status TEXT NOT NULL DEFAULT 'draft' CHECK (status IN ('draft','pending_approval','active','retired')),
  approved_by BIGINT,
  approved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (product_key, version),
  CHECK (status <> 'active' OR (approved_by IS NOT NULL AND approved_at IS NOT NULL))
);

CREATE UNIQUE INDEX uq_distribution_active_product
  ON distribution_product_version(product_key, locale)
  WHERE status = 'active';

CREATE TABLE distribution_release_version (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_id INTEGER NOT NULL REFERENCES catalog_release(id) ON DELETE RESTRICT,
  version INT NOT NULL CHECK (version > 0),
  state TEXT NOT NULL DEFAULT 'draft' CHECK (state IN (
    'draft','validation_failed','validated','ready_for_review','rights_review',
    'payment_due','paid','scheduled','package_generated','delivery_queued','sent',
    'acknowledged','partially_rejected','accepted','live','reporting',
    'takedown_requested','takedown_completed','archived'
  )),
  metadata_valid BOOLEAN NOT NULL DEFAULT FALSE,
  identifiers_valid BOOLEAN NOT NULL DEFAULT FALSE,
  assets_valid BOOLEAN NOT NULL DEFAULT FALSE,
  rights_complete BOOLEAN NOT NULL DEFAULT FALSE,
  splits_locked BOOLEAN NOT NULL DEFAULT FALSE,
  terms_version TEXT,
  terms_accepted_at TIMESTAMPTZ,
  immutable_snapshot JSONB NOT NULL,
  snapshot_sha256 TEXT NOT NULL,
  created_by BIGINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_id, version),
  UNIQUE (snapshot_sha256),
  CHECK ((terms_version IS NULL) = (terms_accepted_at IS NULL))
);

CREATE TABLE distribution_rights_declaration (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  rights_scope TEXT NOT NULL CHECK (rights_scope IN ('master','composition','neighboring','distribution')),
  territory_scope TEXT[] NOT NULL,
  ownership_basis TEXT NOT NULL,
  exclusive BOOLEAN NOT NULL DEFAULT FALSE,
  term_starts_on DATE NOT NULL,
  term_ends_on DATE,
  warranty_version TEXT NOT NULL,
  declared_by BIGINT NOT NULL,
  declared_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  evidence_object_key TEXT,
  UNIQUE (release_version_id, rights_scope),
  CHECK (cardinality(territory_scope) > 0),
  CHECK (term_ends_on IS NULL OR term_ends_on >= term_starts_on)
);

CREATE TABLE distribution_split_allocation (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  rights_scope TEXT NOT NULL CHECK (rights_scope IN ('master','composition','neighboring','distribution')),
  participant_party_id BIGINT NOT NULL,
  basis_points INT NOT NULL CHECK (basis_points BETWEEN 1 AND 10000),
  acceptance_version TEXT,
  accepted_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_version_id, rights_scope, participant_party_id),
  CHECK ((acceptance_version IS NULL) = (accepted_at IS NULL))
);

CREATE TABLE distribution_version_asset (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  catalog_asset_id INTEGER NOT NULL REFERENCES catalog_asset(id) ON DELETE RESTRICT,
  asset_role TEXT NOT NULL CHECK (asset_role IN ('master_audio','artwork','lyrics','video','rights_document','contract')),
  sha256 TEXT NOT NULL,
  validation_status TEXT NOT NULL CHECK (validation_status IN ('pending','valid','invalid','quarantined')),
  validation_evidence JSONB NOT NULL DEFAULT '{}'::jsonb,
  UNIQUE (release_version_id, catalog_asset_id, asset_role)
);

CREATE TABLE distribution_submission (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL UNIQUE REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  product_version_id UUID REFERENCES distribution_product_version(id) ON DELETE RESTRICT,
  checkout_id UUID REFERENCES commerce_checkout_session(id) ON DELETE RESTRICT,
  commercial_gate TEXT NOT NULL CHECK (commercial_gate IN ('payment_due','paid','waived')),
  accepted_price_minor BIGINT NOT NULL CHECK (accepted_price_minor >= 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  accepted_royalty_share_bps INT NOT NULL CHECK (accepted_royalty_share_bps BETWEEN 0 AND 10000),
  accepted_terms_version TEXT NOT NULL,
  waiver_reason TEXT,
  waiver_approved_by BIGINT,
  waiver_approved_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (
    (commercial_gate = 'waived' AND checkout_id IS NULL AND waiver_reason IS NOT NULL AND waiver_approved_by IS NOT NULL AND waiver_approved_at IS NOT NULL)
    OR (commercial_gate <> 'waived' AND checkout_id IS NOT NULL AND waiver_reason IS NULL AND waiver_approved_by IS NULL AND waiver_approved_at IS NULL)
  )
);

CREATE TABLE distribution_partner_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  partner_id INTEGER NOT NULL REFERENCES ddex_partner(id) ON DELETE RESTRICT,
  profile_key TEXT NOT NULL,
  profile_version INT NOT NULL CHECK (profile_version > 0),
  sender_dpid TEXT NOT NULL,
  recipient_dpid TEXT NOT NULL,
  ern_version TEXT NOT NULL,
  acknowledgement_version TEXT NOT NULL,
  transport TEXT NOT NULL CHECK (transport IN ('api','sftp','object_storage_drop')),
  credentials_reference TEXT NOT NULL,
  rules JSONB NOT NULL,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','staging','production')),
  status TEXT NOT NULL CHECK (status IN ('draft','verified','disabled')),
  verified_by BIGINT,
  verified_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (partner_id, profile_key, profile_version, environment),
  CHECK (credentials_reference !~* '(password|secret|token)=') ,
  CHECK (status <> 'verified' OR (verified_by IS NOT NULL AND verified_at IS NOT NULL))
);

CREATE TABLE distribution_package (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  partner_profile_id UUID NOT NULL REFERENCES distribution_partner_profile(id) ON DELETE RESTRICT,
  message_id TEXT NOT NULL,
  package_version INT NOT NULL CHECK (package_version > 0),
  xml_private_uri TEXT NOT NULL,
  manifest_private_uri TEXT NOT NULL,
  xml_sha256 TEXT NOT NULL,
  manifest_sha256 TEXT NOT NULL,
  asset_checksums JSONB NOT NULL,
  validation_evidence JSONB NOT NULL,
  generated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (partner_profile_id, message_id),
  UNIQUE (release_version_id, partner_profile_id, package_version)
);

CREATE TABLE distribution_delivery_attempt (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  partner_profile_id UUID NOT NULL REFERENCES distribution_partner_profile(id) ON DELETE RESTRICT,
  package_id UUID NOT NULL REFERENCES distribution_package(id) ON DELETE RESTRICT,
  operation TEXT NOT NULL CHECK (operation IN ('new_release','update','takedown')),
  prior_delivery_id UUID REFERENCES distribution_delivery_attempt(id) ON DELETE RESTRICT,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','staging','production')),
  state TEXT NOT NULL CHECK (state IN ('queued','sending','sent','retry','dead_letter','cancelled')),
  transport_reference TEXT,
  attempt_count INT NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  next_attempt_at TIMESTAMPTZ,
  sent_at TIMESTAMPTZ,
  last_error TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_version_id, partner_profile_id, operation),
  CHECK ((operation = 'new_release' AND prior_delivery_id IS NULL) OR (operation <> 'new_release' AND prior_delivery_id IS NOT NULL)),
  CHECK (state <> 'sent' OR (transport_reference IS NOT NULL AND sent_at IS NOT NULL))
);

CREATE TABLE distribution_status_evidence (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  delivery_attempt_id UUID NOT NULL REFERENCES distribution_delivery_attempt(id) ON DELETE RESTRICT,
  recipient_event_id TEXT NOT NULL,
  evidence_kind TEXT NOT NULL CHECK (evidence_kind IN ('mock','sandbox','provider_signed','manual_verified')),
  status TEXT NOT NULL CHECK (status IN ('acknowledged','partially_rejected','rejected','accepted','live','reporting','takedown_completed')),
  evidence_private_uri TEXT,
  payload_sha256 TEXT NOT NULL,
  live_url TEXT,
  occurred_at TIMESTAMPTZ NOT NULL,
  recorded_by BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (delivery_attempt_id, recipient_event_id),
  CHECK (status <> 'live' OR live_url IS NOT NULL)
);

CREATE TABLE distribution_recipient_status (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  release_version_id UUID NOT NULL REFERENCES distribution_release_version(id) ON DELETE RESTRICT,
  partner_profile_id UUID NOT NULL REFERENCES distribution_partner_profile(id) ON DELETE RESTRICT,
  current_status TEXT NOT NULL CHECK (current_status IN ('delivery_queued','sent','acknowledged','partially_rejected','rejected','accepted','live','reporting','takedown_requested','takedown_completed')),
  evidence_id UUID REFERENCES distribution_status_evidence(id) ON DELETE RESTRICT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (release_version_id, partner_profile_id),
  CHECK (current_status IN ('delivery_queued','sent','takedown_requested') OR evidence_id IS NOT NULL)
);

CREATE TABLE distribution_usage_report (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  partner_profile_id UUID NOT NULL REFERENCES distribution_partner_profile(id) ON DELETE RESTRICT,
  report_reference TEXT NOT NULL,
  report_family TEXT NOT NULL CHECK (report_family IN ('DSR','partner_csv','partner_json')),
  report_version TEXT NOT NULL,
  period_start DATE NOT NULL,
  period_end DATE NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  private_uri TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  correction_of UUID REFERENCES distribution_usage_report(id) ON DELETE RESTRICT,
  status TEXT NOT NULL CHECK (status IN ('ingested','normalized','reconciled','allocated','review','issued','corrected','rejected')),
  imported_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (partner_profile_id, report_reference, sha256),
  CHECK (period_end >= period_start)
);

CREATE TABLE distribution_usage_line (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  report_id UUID NOT NULL REFERENCES distribution_usage_report(id) ON DELETE RESTRICT,
  source_line_key TEXT NOT NULL,
  release_id INTEGER REFERENCES catalog_release(id) ON DELETE RESTRICT,
  resource_id INTEGER REFERENCES catalog_resource(id) ON DELETE RESTRICT,
  dsp TEXT NOT NULL,
  territory TEXT NOT NULL,
  usage_type TEXT NOT NULL,
  commercial_model TEXT NOT NULL,
  usage_count BIGINT NOT NULL CHECK (usage_count >= 0),
  gross_minor BIGINT NOT NULL,
  partner_deduction_minor BIGINT NOT NULL DEFAULT 0,
  net_minor BIGINT NOT NULL,
  normalized_data JSONB NOT NULL,
  UNIQUE (report_id, source_line_key),
  CHECK (net_minor = gross_minor - partner_deduction_minor)
);

CREATE TABLE royalty_statement (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  beneficiary_party_id BIGINT NOT NULL,
  period_start DATE NOT NULL,
  period_end DATE NOT NULL,
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  state TEXT NOT NULL CHECK (state IN ('ingested','normalized','reconciled','allocated','review','issued','payable','paid','disputed','corrected')),
  gross_minor BIGINT NOT NULL DEFAULT 0,
  deductions_minor BIGINT NOT NULL DEFAULT 0,
  net_minor BIGINT NOT NULL DEFAULT 0,
  correction_of UUID REFERENCES royalty_statement(id) ON DELETE RESTRICT,
  issued_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (beneficiary_party_id, period_start, period_end, currency, correction_of),
  CHECK (period_end >= period_start),
  CHECK (net_minor = gross_minor - deductions_minor),
  CHECK (state NOT IN ('issued','payable','paid') OR issued_at IS NOT NULL)
);

CREATE TABLE royalty_allocation_event (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  statement_id UUID NOT NULL REFERENCES royalty_statement(id) ON DELETE RESTRICT,
  usage_line_id UUID REFERENCES distribution_usage_line(id) ON DELETE RESTRICT,
  event_kind TEXT NOT NULL CHECK (event_kind IN ('earning','partner_deduction','tdf_share','participant_share','recoupment','reserve','release','refund','correction')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor <> 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  source_reference TEXT NOT NULL,
  correction_of UUID REFERENCES royalty_allocation_event(id) ON DELETE RESTRICT,
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (statement_id, event_kind, source_reference, correction_of),
  CHECK ((event_kind = 'correction') = (correction_of IS NOT NULL))
);

CREATE TABLE distribution_beneficiary_payout_profile (
  beneficiary_party_id BIGINT PRIMARY KEY,
  kyc_status TEXT NOT NULL CHECK (kyc_status IN ('not_started','pending','verified','rejected','expired')),
  tax_status TEXT NOT NULL CHECK (tax_status IN ('not_started','pending','verified','rejected','expired')),
  payout_account_status TEXT NOT NULL CHECK (payout_account_status IN ('not_started','pending','verified','rejected','locked')),
  encrypted_account_reference TEXT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE distribution_payout (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  statement_id UUID NOT NULL REFERENCES royalty_statement(id) ON DELETE RESTRICT,
  beneficiary_party_id BIGINT NOT NULL REFERENCES distribution_beneficiary_payout_profile(beneficiary_party_id) ON DELETE RESTRICT,
  environment TEXT NOT NULL CHECK (environment IN ('sandbox','staging','production')),
  method TEXT NOT NULL CHECK (method IN ('bank_transfer_manual','paypal_payouts','partner_payout')),
  status TEXT NOT NULL CHECK (status IN ('draft','pending_approval','approved','processing','paid','failed','cancelled')),
  amount_minor BIGINT NOT NULL CHECK (amount_minor > 0),
  currency TEXT NOT NULL CHECK (currency ~ '^[A-Z]{3}$'),
  idempotency_key TEXT NOT NULL,
  requested_by BIGINT NOT NULL,
  approved_by BIGINT,
  approved_at TIMESTAMPTZ,
  provider_reference TEXT,
  paid_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (beneficiary_party_id, method, idempotency_key),
  CHECK (status NOT IN ('approved','processing','paid') OR (approved_by IS NOT NULL AND approved_at IS NOT NULL)),
  CHECK (approved_by IS NULL OR approved_by <> requested_by),
  CHECK (status <> 'paid' OR (provider_reference IS NOT NULL AND paid_at IS NOT NULL))
);

CREATE OR REPLACE FUNCTION distribution_reject_immutable_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION '% records are immutable; append correction or successor evidence', TG_TABLE_NAME;
END $$;

CREATE TRIGGER trg_distribution_package_immutable
  BEFORE UPDATE OR DELETE ON distribution_package
  FOR EACH ROW EXECUTE FUNCTION distribution_reject_immutable_mutation();
CREATE TRIGGER trg_distribution_status_evidence_immutable
  BEFORE UPDATE OR DELETE ON distribution_status_evidence
  FOR EACH ROW EXECUTE FUNCTION distribution_reject_immutable_mutation();
CREATE TRIGGER trg_distribution_usage_line_immutable
  BEFORE UPDATE OR DELETE ON distribution_usage_line
  FOR EACH ROW EXECUTE FUNCTION distribution_reject_immutable_mutation();
CREATE TRIGGER trg_royalty_allocation_immutable
  BEFORE UPDATE OR DELETE ON royalty_allocation_event
  FOR EACH ROW EXECUTE FUNCTION distribution_reject_immutable_mutation();

CREATE OR REPLACE FUNCTION distribution_validate_locked_splits()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  version_id UUID;
  locked BOOLEAN;
BEGIN
  IF TG_TABLE_NAME = 'distribution_release_version' THEN
    version_id := COALESCE(NEW.id, OLD.id);
  ELSE
    version_id := COALESCE(NEW.release_version_id, OLD.release_version_id);
  END IF;
  SELECT splits_locked INTO locked FROM distribution_release_version WHERE id = version_id;
  IF locked AND (
    NOT EXISTS (SELECT 1 FROM distribution_rights_declaration WHERE release_version_id = version_id)
    OR EXISTS (
      SELECT 1
      FROM distribution_rights_declaration rights
      LEFT JOIN (
        SELECT rights_scope, SUM(basis_points) AS total_bps,
               BOOL_AND(accepted_at IS NOT NULL) AS all_accepted
        FROM distribution_split_allocation
        WHERE release_version_id = version_id
        GROUP BY rights_scope
      ) allocations USING (rights_scope)
      WHERE rights.release_version_id = version_id
        AND (COALESCE(allocations.total_bps, 0) <> 10000 OR COALESCE(allocations.all_accepted, FALSE) = FALSE)
    )
  ) THEN
    RAISE EXCEPTION 'Every declared rights scope must have accepted allocations totaling exactly 10000 basis points';
  END IF;
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END $$;

CREATE CONSTRAINT TRIGGER trg_distribution_split_total
  AFTER INSERT OR UPDATE OR DELETE ON distribution_split_allocation
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_locked_splits();
CREATE CONSTRAINT TRIGGER trg_distribution_rights_split_total
  AFTER INSERT OR UPDATE OR DELETE ON distribution_rights_declaration
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_locked_splits();
CREATE CONSTRAINT TRIGGER trg_distribution_version_split_total
  AFTER UPDATE ON distribution_release_version
  DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_locked_splits();

CREATE OR REPLACE FUNCTION distribution_protect_locked_rights()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE version_id UUID;
DECLARE locked BOOLEAN;
BEGIN
  version_id := COALESCE(NEW.release_version_id, OLD.release_version_id);
  SELECT splits_locked INTO locked FROM distribution_release_version WHERE id = version_id;
  IF locked THEN
    RAISE EXCEPTION 'Locked rights and split records are immutable; create a new release version';
  END IF;
  IF TG_OP = 'DELETE' THEN RETURN OLD; END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_rights_locked
  BEFORE INSERT OR UPDATE OR DELETE ON distribution_rights_declaration
  FOR EACH ROW EXECUTE FUNCTION distribution_protect_locked_rights();
CREATE TRIGGER trg_distribution_split_locked
  BEFORE INSERT OR UPDATE OR DELETE ON distribution_split_allocation
  FOR EACH ROW EXECUTE FUNCTION distribution_protect_locked_rights();

CREATE OR REPLACE FUNCTION distribution_validate_release_transition()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  allowed BOOLEAN := FALSE;
  commercially_cleared BOOLEAN := FALSE;
BEGIN
  IF OLD.splits_locked AND NOT NEW.splits_locked THEN
    RAISE EXCEPTION 'Locked distribution splits cannot be reopened; create a new release version';
  END IF;
  IF (OLD.release_id, OLD.version, OLD.immutable_snapshot, OLD.snapshot_sha256, OLD.created_by, OLD.created_at)
     IS DISTINCT FROM
     (NEW.release_id, NEW.version, NEW.immutable_snapshot, NEW.snapshot_sha256, NEW.created_by, NEW.created_at) THEN
    RAISE EXCEPTION 'Release version snapshot and provenance are immutable';
  END IF;
  IF NEW.state = OLD.state THEN RETURN NEW; END IF;
  allowed := (OLD.state, NEW.state) IN (
    ('draft','validation_failed'), ('draft','validated'), ('draft','archived'),
    ('validation_failed','draft'), ('validation_failed','validated'),
    ('validated','ready_for_review'), ('validated','draft'),
    ('ready_for_review','rights_review'), ('ready_for_review','draft'),
    ('rights_review','payment_due'), ('rights_review','ready_for_review'),
    ('payment_due','paid'), ('payment_due','archived'),
    ('paid','scheduled'), ('scheduled','package_generated'),
    ('package_generated','delivery_queued'), ('delivery_queued','sent'),
    ('sent','acknowledged'), ('sent','partially_rejected'),
    ('acknowledged','accepted'), ('acknowledged','partially_rejected'),
    ('partially_rejected','delivery_queued'), ('partially_rejected','accepted'),
    ('accepted','live'), ('accepted','takedown_requested'),
    ('live','reporting'), ('live','takedown_requested'),
    ('reporting','live'), ('reporting','takedown_requested'),
    ('takedown_requested','takedown_completed'), ('takedown_completed','archived')
  );
  IF NOT allowed THEN
    RAISE EXCEPTION 'Invalid distribution transition % -> %', OLD.state, NEW.state;
  END IF;
  IF NEW.state IN ('validated','ready_for_review','rights_review','payment_due','paid','scheduled','package_generated','delivery_queued','sent','acknowledged','partially_rejected','accepted','live','reporting')
     AND NOT (NEW.metadata_valid AND NEW.identifiers_valid AND NEW.assets_valid AND NEW.rights_complete AND NEW.splits_locked AND NEW.terms_accepted_at IS NOT NULL) THEN
    RAISE EXCEPTION 'Distribution validation, rights, splits, assets, identifiers, and terms gates are incomplete';
  END IF;
  IF NEW.state = 'paid' THEN
    SELECT EXISTS (
      SELECT 1 FROM distribution_submission submission
      LEFT JOIN commerce_checkout_session checkout ON checkout.id = submission.checkout_id
      WHERE submission.release_version_id = NEW.id
        AND (
          (submission.commercial_gate = 'paid' AND checkout.status = 'paid')
          OR submission.commercial_gate = 'waived'
        )
    ) INTO commercially_cleared;
    IF NOT commercially_cleared THEN
      RAISE EXCEPTION 'Distribution payment cannot be marked paid without a verified paid checkout or approved waiver';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_release_transition
  BEFORE UPDATE ON distribution_release_version
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_release_transition();

CREATE OR REPLACE FUNCTION distribution_validate_submission_gate()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  checkout_status commerce_checkout_session.status%TYPE;
  checkout_currency commerce_checkout_session.currency%TYPE;
  checkout_total_minor commerce_checkout_session.total_minor%TYPE;
BEGIN
  IF NEW.commercial_gate = 'paid' THEN
    SELECT status, currency, total_minor
      INTO checkout_status, checkout_currency, checkout_total_minor
      FROM commerce_checkout_session
      WHERE id = NEW.checkout_id;
    IF NOT FOUND OR checkout_status <> 'paid' OR checkout_currency <> NEW.currency OR checkout_total_minor <> NEW.accepted_price_minor THEN
      RAISE EXCEPTION 'Distribution submission does not match a verified paid checkout';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_submission_gate
  BEFORE INSERT OR UPDATE OF commercial_gate, checkout_id, accepted_price_minor, currency
  ON distribution_submission
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_submission_gate();

CREATE OR REPLACE FUNCTION distribution_validate_package()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE release_state TEXT;
DECLARE profile_status TEXT;
BEGIN
  SELECT state INTO release_state FROM distribution_release_version WHERE id = NEW.release_version_id;
  SELECT status INTO profile_status FROM distribution_partner_profile WHERE id = NEW.partner_profile_id;
  IF release_state <> 'scheduled' THEN
    RAISE EXCEPTION 'A package can be generated only for a scheduled immutable release version';
  END IF;
  IF profile_status <> 'verified' THEN
    RAISE EXCEPTION 'A package requires a verified recipient profile';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_package_gate
  BEFORE INSERT ON distribution_package
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_package();

CREATE OR REPLACE FUNCTION distribution_validate_delivery()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  package_release_version_id distribution_package.release_version_id%TYPE;
  package_partner_profile_id distribution_package.partner_profile_id%TYPE;
  profile_environment distribution_partner_profile.environment%TYPE;
  profile_status distribution_partner_profile.status%TYPE;
  release_state distribution_release_version.state%TYPE;
  delivery_enabled BOOLEAN := FALSE;
BEGIN
  SELECT release_version_id, partner_profile_id
    INTO package_release_version_id, package_partner_profile_id
    FROM distribution_package
    WHERE id = NEW.package_id;
  IF NOT FOUND
     OR package_release_version_id <> NEW.release_version_id
     OR package_partner_profile_id <> NEW.partner_profile_id THEN
    RAISE EXCEPTION 'Delivery package is not bound to this release version and recipient profile';
  END IF;
  SELECT environment, status
    INTO profile_environment, profile_status
    FROM distribution_partner_profile
    WHERE id = NEW.partner_profile_id;
  IF NOT FOUND OR profile_environment <> NEW.environment OR profile_status <> 'verified' THEN
    RAISE EXCEPTION 'Delivery environment must match a verified recipient profile';
  END IF;
  SELECT state INTO release_state FROM distribution_release_version WHERE id = NEW.release_version_id;
  IF NOT FOUND OR release_state NOT IN ('package_generated','delivery_queued','sent','partially_rejected','takedown_requested') THEN
    RAISE EXCEPTION 'Release version is not eligible for delivery';
  END IF;
  IF NEW.environment = 'production' THEN
    SELECT enabled INTO delivery_enabled FROM revenue_feature_flag
      WHERE flag_key = 'distribution.ern_delivery' AND environment = 'production';
    IF COALESCE(delivery_enabled, FALSE) = FALSE THEN
      RAISE EXCEPTION 'Production DDEX delivery is feature-disabled';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_delivery_gate
  BEFORE INSERT OR UPDATE OF package_id, release_version_id, partner_profile_id, environment
  ON distribution_delivery_attempt
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_delivery();

CREATE OR REPLACE FUNCTION distribution_validate_status_evidence()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  delivery_environment distribution_delivery_attempt.environment%TYPE;
  delivery_state distribution_delivery_attempt.state%TYPE;
BEGIN
  SELECT environment, state
    INTO delivery_environment, delivery_state
    FROM distribution_delivery_attempt
    WHERE id = NEW.delivery_attempt_id;
  IF NOT FOUND THEN RAISE EXCEPTION 'Unknown delivery attempt'; END IF;
  IF delivery_environment = 'production' AND NEW.evidence_kind IN ('mock','sandbox') THEN
    RAISE EXCEPTION 'Mock or sandbox evidence cannot transition production distribution records';
  END IF;
  IF delivery_state <> 'sent' THEN
    RAISE EXCEPTION 'Recipient evidence requires a delivery recorded as sent';
  END IF;
  IF NEW.evidence_kind = 'manual_verified' AND NEW.recorded_by IS NULL THEN
    RAISE EXCEPTION 'Manually verified recipient evidence requires an accountable operator';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_status_evidence_environment
  BEFORE INSERT ON distribution_status_evidence
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_status_evidence();

CREATE OR REPLACE FUNCTION distribution_validate_recipient_status()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  evidence_status distribution_status_evidence.status%TYPE;
  evidence_delivery_attempt_id distribution_status_evidence.delivery_attempt_id%TYPE;
BEGIN
  IF NEW.evidence_id IS NOT NULL THEN
    SELECT status, delivery_attempt_id
      INTO evidence_status, evidence_delivery_attempt_id
      FROM distribution_status_evidence
      WHERE id = NEW.evidence_id;
    IF NOT FOUND OR evidence_status <> NEW.current_status THEN
      RAISE EXCEPTION 'Recipient status must match its immutable evidence';
    END IF;
    IF NOT EXISTS (
      SELECT 1 FROM distribution_delivery_attempt attempt
      WHERE attempt.id = evidence_delivery_attempt_id
        AND attempt.release_version_id = NEW.release_version_id
        AND attempt.partner_profile_id = NEW.partner_profile_id
    ) THEN
      RAISE EXCEPTION 'Recipient evidence is not bound to this release version and partner profile';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_recipient_status_evidence
  BEFORE INSERT OR UPDATE ON distribution_recipient_status
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_recipient_status();

CREATE OR REPLACE FUNCTION distribution_protect_usage_report()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP = 'DELETE' THEN RAISE EXCEPTION 'Usage report evidence is immutable'; END IF;
  IF (OLD.partner_profile_id, OLD.report_reference, OLD.report_family, OLD.report_version,
      OLD.period_start, OLD.period_end, OLD.currency, OLD.private_uri, OLD.sha256, OLD.correction_of)
     IS DISTINCT FROM
     (NEW.partner_profile_id, NEW.report_reference, NEW.report_family, NEW.report_version,
      NEW.period_start, NEW.period_end, NEW.currency, NEW.private_uri, NEW.sha256, NEW.correction_of) THEN
    RAISE EXCEPTION 'Usage report evidence is immutable';
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_usage_report_immutable
  BEFORE UPDATE OR DELETE ON distribution_usage_report
  FOR EACH ROW EXECUTE FUNCTION distribution_protect_usage_report();

CREATE OR REPLACE FUNCTION distribution_validate_usage_report_gate()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE profile_environment TEXT;
DECLARE ingestion_enabled BOOLEAN := FALSE;
BEGIN
  SELECT environment INTO profile_environment FROM distribution_partner_profile WHERE id = NEW.partner_profile_id;
  IF profile_environment = 'production' THEN
    SELECT enabled INTO ingestion_enabled FROM revenue_feature_flag
      WHERE flag_key = 'distribution.dsr_ingestion' AND environment = 'production';
    IF COALESCE(ingestion_enabled, FALSE) = FALSE THEN
      RAISE EXCEPTION 'Production usage-report ingestion is feature-disabled';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_usage_report_gate
  BEFORE INSERT ON distribution_usage_report
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_usage_report_gate();

CREATE OR REPLACE FUNCTION distribution_validate_payout_gate()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  profile_kyc_status distribution_beneficiary_payout_profile.kyc_status%TYPE;
  profile_tax_status distribution_beneficiary_payout_profile.tax_status%TYPE;
  profile_payout_account_status distribution_beneficiary_payout_profile.payout_account_status%TYPE;
  statement_state royalty_statement.state%TYPE;
  statement_beneficiary_party_id royalty_statement.beneficiary_party_id%TYPE;
  statement_currency royalty_statement.currency%TYPE;
  statement_net_minor royalty_statement.net_minor%TYPE;
  auto_enabled BOOLEAN := FALSE;
BEGIN
  IF NEW.status IN ('approved','processing','paid') THEN
    SELECT kyc_status, tax_status, payout_account_status
      INTO profile_kyc_status, profile_tax_status, profile_payout_account_status
      FROM distribution_beneficiary_payout_profile
      WHERE beneficiary_party_id = NEW.beneficiary_party_id;
    IF NOT FOUND OR profile_kyc_status <> 'verified' OR profile_tax_status <> 'verified' OR profile_payout_account_status <> 'verified' THEN
      RAISE EXCEPTION 'Payout requires verified KYC, tax, and payout account gates';
    END IF;
    SELECT state, beneficiary_party_id, currency, net_minor
      INTO statement_state, statement_beneficiary_party_id, statement_currency, statement_net_minor
      FROM royalty_statement
      WHERE id = NEW.statement_id;
    IF NOT FOUND OR statement_state <> 'payable'
       OR statement_beneficiary_party_id <> NEW.beneficiary_party_id
       OR statement_currency <> NEW.currency
       OR NEW.amount_minor > statement_net_minor THEN
      RAISE EXCEPTION 'Payout must match a payable statement beneficiary, currency, and available amount';
    END IF;
  END IF;
  IF NEW.method <> 'bank_transfer_manual' AND NEW.status IN ('processing','paid') THEN
    SELECT enabled INTO auto_enabled FROM revenue_feature_flag
      WHERE flag_key = 'distribution.automatic_payouts' AND environment = NEW.environment;
    IF COALESCE(auto_enabled, FALSE) = FALSE THEN
      RAISE EXCEPTION 'Automatic distribution payouts are feature-disabled';
    END IF;
  END IF;
  IF NEW.environment = 'production' AND NEW.status IN ('processing','paid') THEN
    SELECT enabled INTO auto_enabled FROM revenue_feature_flag
      WHERE flag_key = 'distribution.manual_payouts' AND environment = 'production';
    IF COALESCE(auto_enabled, FALSE) = FALSE THEN
      RAISE EXCEPTION 'Production distribution payouts are feature-disabled pending explicit authorization';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE TRIGGER trg_distribution_payout_gate
  BEFORE INSERT OR UPDATE OF status, method, environment ON distribution_payout
  FOR EACH ROW EXECUTE FUNCTION distribution_validate_payout_gate();

INSERT INTO revenue_feature_flag(flag_key, enabled, environment, reason) VALUES
  ('distribution.intake', FALSE, 'production', 'Requires production private storage, legal terms, and operational ownership'),
  ('distribution.ern_delivery', FALSE, 'production', 'Requires DDEX license, DPID, contracted recipient profile, credentials, and authorization'),
  ('distribution.dsr_ingestion', FALSE, 'production', 'Requires contracted partner reports and reconciliation approval'),
  ('distribution.manual_payouts', FALSE, 'production', 'Requires KYC, tax, banking, legal, reconciliation, dual approval, and production authorization'),
  ('distribution.public_storefront', FALSE, 'production', 'Requires approved prices, support ownership, legal terms, and provider verification')
ON CONFLICT (flag_key, environment) DO NOTHING;

COMMIT;
