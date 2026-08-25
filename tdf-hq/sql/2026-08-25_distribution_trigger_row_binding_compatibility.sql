-- Replace ambiguous composite-row trigger variables without mutating the
-- already-applied 2026-08-13 distribution migration.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

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

COMMIT;
