CREATE TABLE marketplace_order (
 id uuid PRIMARY KEY, cart_id uuid, buyer_name text NOT NULL, buyer_email text NOT NULL,
 buyer_phone text, total_usd_cents integer NOT NULL, currency text NOT NULL, status text NOT NULL,
 payment_provider text, stripe_payment_intent_id text, stripe_idempotency_key text, paypal_order_id text,
 paypal_payer_email text, datafast_checkout_id text, datafast_resource_path text, datafast_payment_id text,
 datafast_result_code text, datafast_result_description text, datafast_payment_brand text, datafast_auth_code text,
 datafast_acquirer_code text, paid_at timestamptz, created_at timestamptz NOT NULL DEFAULT now(), updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE marketplace_order_item(id uuid PRIMARY KEY, order_id uuid, listing_id uuid, quantity integer, unit_price_usd_cents integer, subtotal_usd_cents integer);
CREATE TABLE commerce_checkout_session(id uuid PRIMARY KEY, domain_type text, status text, environment text);
CREATE TABLE marketplace_order_checkout_runtime(order_id uuid, checkout_id uuid, create_idempotency_key text, create_request_sha256 text, order_kind text, fulfillment_method text, domain_status text, hold_expires_at timestamptz, tracking_reference text);
CREATE TABLE marketplace_rental_order_runtime(order_id uuid, start_date date, end_date date, duration_days integer, rental_charge_usd_cents bigint, security_deposit_usd_cents bigint, deposit_status text, deposit_deduction_usd_cents bigint, terms_version text, timezone text, condition_out text, condition_in text);
CREATE TABLE commerce_payment_attempt(id uuid, checkout_id uuid, provider text, operation text, status text, updated_at timestamptz);
CREATE TABLE commerce_manual_payment_evidence(id uuid, checkout_id uuid, payment_attempt_id uuid, status text, submitted_at timestamptz);
CREATE TABLE marketplace_sale_fulfillment_event(id uuid, order_id uuid, to_status text, created_at timestamptz);
CREATE TABLE marketplace_rental_event(id uuid, order_id uuid, to_status text, created_at timestamptz);
CREATE TABLE commerce_payment_intent(checkout_id uuid, status text, currency text, amount_minor bigint, authorized_minor bigint, captured_minor bigint, refunded_minor bigint);
CREATE TABLE revenue_feature_flag(flag_key text, environment text, enabled boolean);
INSERT INTO revenue_feature_flag VALUES ('commerce.provider.bank_transfer','sandbox',false);
ALTER TABLE commerce_payment_intent ADD COLUMN id uuid DEFAULT gen_random_uuid();
CREATE TABLE commerce_payment_amount_component(payment_intent_id uuid, component_type text, source text, currency text, amount_minor bigint);
CREATE TABLE commerce_provider_account(id uuid PRIMARY KEY, provider text, environment text, enabled boolean, credential_status text, contract_status text, feature_flag_key text, merchant_account_ref text);
CREATE TABLE commerce_provider_capability(provider_account_id uuid, payment_method text, capability text, verification_status text);
