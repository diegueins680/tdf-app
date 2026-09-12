-- Evidence-preserving rollback. Verified/activated accounts are retained and
-- require an explicit operator decision instead of being silently removed.
\set ON_ERROR_STOP on
BEGIN;

DELETE FROM commerce_provider_capability capability
USING commerce_provider_account account
WHERE capability.provider_account_id = account.id
  AND account.provider = 'bank_transfer'
  AND capability.payment_method = 'manual_bank_transfer'
  AND capability.capability = 'one_time'
  AND capability.verification_status = 'documented'
  AND capability.verified_at IS NULL
  AND capability.source_reference =
    'docs/payments/ecuador-payment-platform-audit-2026-09-11.md#manual-bank-transfer-control';

DELETE FROM commerce_provider_account account
WHERE account.provider = 'bank_transfer'
  AND account.enabled = FALSE
  AND account.status = 'disabled'
  AND account.contract_status = 'unverified'
  AND account.credential_status = 'absent'
  AND account.verified_at IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM commerce_provider_capability capability
    WHERE capability.provider_account_id = account.id
  );

COMMIT;
