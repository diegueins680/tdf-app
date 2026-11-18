-- PayPhone payment button: card and PayPhone-wallet payments on PayPhone's
-- hosted page, confirmed server-to-server with /api/button/V2/Confirm.
-- Seeds documentation only, like 2026-09-11_provider_capability_catalog.sql:
-- a row must still be moved to the environment-verified state, with
-- verified_at, before the router can offer it. This migration enables nothing.
\set ON_ERROR_STOP on
BEGIN;

WITH documented(provider, payment_method, capability, verification_status, source_reference) AS (
  VALUES
    ('payphone','card','one_time','documented','https://docs.payphone.app/boton-de-pago-por-redireccion'),
    ('payphone','card','server_verification','documented','https://docs.payphone.app/boton-de-pago-por-redireccion')
)
INSERT INTO commerce_provider_capability (
  provider_account_id, payment_method, capability, verification_status,
  source_reference
)
SELECT
  account.id, documented.payment_method, documented.capability,
  documented.verification_status,
  documented.source_reference || '#tdf-payphone-button-2026-11-17'
FROM documented
JOIN commerce_provider_account account
  ON account.provider = documented.provider
ON CONFLICT (provider_account_id, payment_method, capability) DO NOTHING;

COMMIT;
