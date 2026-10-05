-- Seed provider documentation as non-routable capability metadata. A row must
-- still be changed to the environment-specific verified state, with verified_at,
-- before the routing layer can use it. This migration enables nothing.
\set ON_ERROR_STOP on
BEGIN;

WITH documented(provider, payment_method, capability, verification_status, source_reference) AS (
  VALUES
    ('datafast','card','one_time','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','recurring','documented','https://developers.datafast.com.ec/pagos_recurrentes.aspx'),
    ('datafast','card','tokenization','documented','https://developers.datafast.com.ec/pagos_recurrentes.aspx'),
    ('datafast','card','three_ds','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','installments','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','void','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','full_refund','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','disputes','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','chargebacks','documented','https://developers.datafast.com.ec/index.aspx'),
    ('datafast','card','server_verification','documented','https://developers.datafast.com.ec/index.aspx'),

    ('paypal','paypal_wallet','one_time','documented','https://developer.paypal.com/api/orders/v2/'),
    ('paypal','paypal_wallet','recurring','documented','https://developer.paypal.com/subscriptions/integrate/'),
    ('paypal','paypal_wallet','authorize','documented','https://developer.paypal.com/v5/checkout/auth-capture/'),
    ('paypal','paypal_wallet','capture','documented','https://developer.paypal.com/v5/checkout/auth-capture/'),
    ('paypal','paypal_wallet','void','documented','https://developer.paypal.com/v5/checkout/auth-capture/'),
    ('paypal','paypal_wallet','full_refund','documented','https://developer.paypal.com/docs/api/payments/v2/'),
    ('paypal','paypal_wallet','partial_refund','documented','https://developer.paypal.com/docs/api/payments/v2/'),
    ('paypal','paypal_wallet','disputes','documented','https://developer.paypal.com/docs/api/customer-disputes/v1/'),
    ('paypal','paypal_wallet','chargebacks','documented','https://developer.paypal.com/docs/api/customer-disputes/v1/'),
    ('paypal','paypal_wallet','signed_webhook','documented','https://developer.paypal.com/api/rest/webhooks/rest/'),
    ('paypal','paypal_wallet','server_verification','documented','https://developer.paypal.com/api/rest/webhooks/rest/'),
    ('paypal','paypal_wallet','connected_accounts','contract_required','https://developer.paypal.com/platforms/seller-onboarding'),
    ('paypal','paypal_wallet','split_settlement','contract_required','https://developer.paypal.com/docs/multiparty/checkout/multiseller-payments/'),
    ('paypal','paypal_wallet','seller_payouts','contract_required','https://developer.paypal.com/docs/payouts/'),

    ('placetopay','card','one_time','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','card','three_ds','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','card','installments','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','card','payment_link','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','card','signed_webhook','documented','https://docs.placetopay.dev/en/checkout/notification/'),
    ('placetopay','card','server_verification','documented','https://docs.placetopay.dev/en/checkout/query-session/'),
    ('placetopay','bank_redirect','one_time','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','bank_redirect','signed_webhook','documented','https://docs.placetopay.dev/en/checkout/notification/'),
    ('placetopay','bank_redirect','server_verification','documented','https://docs.placetopay.dev/en/checkout/query-session/'),
    ('placetopay','deuna_qr','one_time','documented','https://docs.placetopay.dev/en/payments/external-redirects/deuna/'),
    ('placetopay','deuna_qr','signed_webhook','documented','https://docs.placetopay.dev/en/checkout/notification/'),
    ('placetopay','deuna_qr','server_verification','documented','https://docs.placetopay.dev/en/checkout/query-session/'),
    ('placetopay','payment_link','one_time','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','payment_link','payment_link','documented','https://docs.placetopay.dev/en/checkout/create-session/'),
    ('placetopay','payment_link','signed_webhook','documented','https://docs.placetopay.dev/en/checkout/notification/'),
    ('placetopay','payment_link','server_verification','documented','https://docs.placetopay.dev/en/checkout/query-session/'),

    ('payphone','payphone_wallet','one_time','documented','https://docs.payphone.app/api-sale'),
    ('payphone','payphone_wallet','server_verification','documented','https://docs.payphone.app/api-sale')
)
INSERT INTO commerce_provider_capability (
  provider_account_id, payment_method, capability, verification_status,
  source_reference
)
SELECT
  account.id, documented.payment_method, documented.capability,
  documented.verification_status,
  documented.source_reference || '#tdf-capability-catalog-2026-09-11'
FROM documented
JOIN commerce_provider_account account
  ON account.provider = documented.provider
ON CONFLICT (provider_account_id, payment_method, capability) DO NOTHING;

COMMIT;
