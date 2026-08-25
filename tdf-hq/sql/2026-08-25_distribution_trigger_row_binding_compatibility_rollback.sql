-- Emergency rollback for the distribution trigger row-binding compatibility fix.
-- Freeze distribution writes and roll back the application before applying it.
BEGIN;

SET LOCAL lock_timeout = '10s';
SET LOCAL statement_timeout = '10min';

CREATE OR REPLACE FUNCTION distribution_validate_submission_gate()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE checkout commerce_checkout_session%ROWTYPE;
BEGIN
  IF NEW.commercial_gate = 'paid' THEN
    SELECT * INTO checkout FROM commerce_checkout_session WHERE id = NEW.checkout_id;
    IF NOT FOUND OR checkout.status <> 'paid' OR checkout.currency <> NEW.currency OR checkout.total_minor <> NEW.accepted_price_minor THEN
      RAISE EXCEPTION 'Distribution submission does not match a verified paid checkout';
    END IF;
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION distribution_validate_delivery()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE package distribution_package%ROWTYPE;
DECLARE profile distribution_partner_profile%ROWTYPE;
DECLARE release_state TEXT;
DECLARE delivery_enabled BOOLEAN := FALSE;
BEGIN
  SELECT * INTO package FROM distribution_package WHERE id = NEW.package_id;
  SELECT * INTO profile FROM distribution_partner_profile WHERE id = NEW.partner_profile_id;
  SELECT state INTO release_state FROM distribution_release_version WHERE id = NEW.release_version_id;
  IF NOT FOUND OR package.release_version_id <> NEW.release_version_id OR package.partner_profile_id <> NEW.partner_profile_id THEN
    RAISE EXCEPTION 'Delivery package is not bound to this release version and recipient profile';
  END IF;
  IF profile.environment <> NEW.environment OR profile.status <> 'verified' THEN
    RAISE EXCEPTION 'Delivery environment must match a verified recipient profile';
  END IF;
  IF release_state NOT IN ('package_generated','delivery_queued','sent','partially_rejected','takedown_requested') THEN
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
DECLARE delivery distribution_delivery_attempt%ROWTYPE;
BEGIN
  SELECT * INTO delivery FROM distribution_delivery_attempt WHERE id = NEW.delivery_attempt_id;
  IF NOT FOUND THEN RAISE EXCEPTION 'Unknown delivery attempt'; END IF;
  IF delivery.environment = 'production' AND NEW.evidence_kind IN ('mock','sandbox') THEN
    RAISE EXCEPTION 'Mock or sandbox evidence cannot transition production distribution records';
  END IF;
  IF delivery.state <> 'sent' THEN
    RAISE EXCEPTION 'Recipient evidence requires a delivery recorded as sent';
  END IF;
  IF NEW.evidence_kind = 'manual_verified' AND NEW.recorded_by IS NULL THEN
    RAISE EXCEPTION 'Manually verified recipient evidence requires an accountable operator';
  END IF;
  RETURN NEW;
END $$;

CREATE OR REPLACE FUNCTION distribution_validate_recipient_status()
RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE evidence distribution_status_evidence%ROWTYPE;
BEGIN
  IF NEW.evidence_id IS NOT NULL THEN
    SELECT * INTO evidence FROM distribution_status_evidence WHERE id = NEW.evidence_id;
    IF NOT FOUND OR evidence.status <> NEW.current_status THEN
      RAISE EXCEPTION 'Recipient status must match its immutable evidence';
    END IF;
    IF NOT EXISTS (
      SELECT 1 FROM distribution_delivery_attempt attempt
      WHERE attempt.id = evidence.delivery_attempt_id
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
DECLARE profile distribution_beneficiary_payout_profile%ROWTYPE;
DECLARE statement royalty_statement%ROWTYPE;
DECLARE auto_enabled BOOLEAN := FALSE;
BEGIN
  IF NEW.status IN ('approved','processing','paid') THEN
    SELECT * INTO profile FROM distribution_beneficiary_payout_profile WHERE beneficiary_party_id = NEW.beneficiary_party_id;
    IF NOT FOUND OR profile.kyc_status <> 'verified' OR profile.tax_status <> 'verified' OR profile.payout_account_status <> 'verified' THEN
      RAISE EXCEPTION 'Payout requires verified KYC, tax, and payout account gates';
    END IF;
    SELECT * INTO statement FROM royalty_statement WHERE id = NEW.statement_id;
    IF NOT FOUND OR statement.state <> 'payable'
       OR statement.beneficiary_party_id <> NEW.beneficiary_party_id
       OR statement.currency <> NEW.currency
       OR NEW.amount_minor > statement.net_minor THEN
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
