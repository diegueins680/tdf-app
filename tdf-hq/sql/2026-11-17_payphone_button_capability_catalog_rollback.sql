-- Remove only untouched documentation metadata introduced by the forward
-- migration. Environment-verified evidence is intentionally retained.
\set ON_ERROR_STOP on
BEGIN;

DELETE FROM commerce_provider_capability capability
USING commerce_provider_account account
WHERE capability.provider_account_id = account.id
  AND account.provider = 'payphone'
  AND capability.verification_status = 'documented'
  AND capability.verified_at IS NULL
  AND capability.source_reference LIKE '%#tdf-payphone-button-2026-11-17';

COMMIT;
