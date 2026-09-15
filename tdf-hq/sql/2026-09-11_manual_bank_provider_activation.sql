-- Bring the existing staff-verified bank-transfer rail under the canonical
-- provider activation and exact method/capability gates. This migration does
-- not enable the rail or reinterpret historical manual-payment evidence.
\set ON_ERROR_STOP on
BEGIN;

INSERT INTO commerce_provider_account (
  provider, environment, status, contract_status, credential_status,
  settlement_currency, feature_flag_key, enabled, disabled_reason
)
SELECT
  'bank_transfer', environment, 'disabled', 'unverified', 'absent', 'USD',
  'checkout.bank_transfer', FALSE,
  'Activation requires settlement-account, review workflow and capability verification'
FROM unnest(ARRAY['sandbox','production']) AS environment
ON CONFLICT (provider, environment) DO NOTHING;

INSERT INTO commerce_provider_capability (
  provider_account_id, payment_method, capability, verification_status,
  source_reference
)
SELECT
  account.id, 'manual_bank_transfer', 'one_time', 'documented',
  'docs/payments/ecuador-payment-platform-audit-2026-09-11.md#manual-bank-transfer-control'
FROM commerce_provider_account account
WHERE account.provider = 'bank_transfer'
ON CONFLICT (provider_account_id, payment_method, capability) DO NOTHING;

COMMIT;
