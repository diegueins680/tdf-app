-- Remove only untouched documentation metadata introduced by the forward
-- migration. Environment-verified evidence is intentionally retained.
\set ON_ERROR_STOP on
BEGIN;

DELETE FROM commerce_provider_capability capability
USING commerce_provider_account account
WHERE capability.provider_account_id = account.id
  AND account.provider IN ('datafast','paypal','placetopay','payphone')
  AND capability.verification_status IN ('documented','contract_required')
  AND capability.verified_at IS NULL
  AND capability.source_reference LIKE '%#tdf-capability-catalog-2026-09-11';

COMMIT;
