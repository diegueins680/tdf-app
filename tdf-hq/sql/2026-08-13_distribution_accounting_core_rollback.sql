-- Emergency rollback. Refuses teardown after distribution/royalty evidence exists.
BEGIN;

DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM distribution_release_version LIMIT 1)
     OR EXISTS (SELECT 1 FROM distribution_usage_report LIMIT 1)
     OR EXISTS (SELECT 1 FROM royalty_allocation_event LIMIT 1)
     OR EXISTS (SELECT 1 FROM distribution_payout LIMIT 1) THEN
    RAISE EXCEPTION 'Refusing distribution-core rollback: catalog, delivery, royalty, or payout records exist';
  END IF;
END $$;

DELETE FROM revenue_feature_flag WHERE flag_key IN (
  'distribution.intake','distribution.ern_delivery','distribution.dsr_ingestion','distribution.manual_payouts','distribution.public_storefront'
);

DROP TABLE distribution_payout;
DROP TABLE distribution_beneficiary_payout_profile;
DROP TABLE royalty_allocation_event;
DROP TABLE royalty_statement;
DROP TABLE distribution_usage_line;
DROP TABLE distribution_usage_report;
DROP TABLE distribution_recipient_status;
DROP TABLE distribution_status_evidence;
DROP TABLE distribution_delivery_attempt;
DROP TABLE distribution_package;
DROP TABLE distribution_partner_profile;
DROP TABLE distribution_submission;
DROP TABLE distribution_version_asset;
DROP TABLE distribution_split_allocation;
DROP TABLE distribution_rights_declaration;
DROP TABLE distribution_release_version;
DROP TABLE distribution_product_version;

DROP FUNCTION distribution_validate_payout_gate();
DROP FUNCTION distribution_validate_usage_report_gate();
DROP FUNCTION distribution_protect_usage_report();
DROP FUNCTION distribution_validate_recipient_status();
DROP FUNCTION distribution_validate_status_evidence();
DROP FUNCTION distribution_validate_submission_gate();
DROP FUNCTION distribution_validate_delivery();
DROP FUNCTION distribution_validate_package();
DROP FUNCTION distribution_validate_release_transition();
DROP FUNCTION distribution_protect_locked_rights();
DROP FUNCTION distribution_validate_locked_splits();
DROP FUNCTION distribution_reject_immutable_mutation();

COMMIT;
