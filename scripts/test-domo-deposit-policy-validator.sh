#!/bin/sh
# PAY-DEPOSIT-POLICY-001: the Domo quote runtime insert validator must accept
# exactly the deposit TDF.Commerce.DepositPolicy.applyDepositPolicy produces
# for the approved rate card (2026-11-04_domo_quote_deposit_policy_validator)
# and reject the unadjusted basis-point deposit whenever the policy changes it.
# Runs on a fully migrated disposable database inside a rolled-back transaction.
set -eu
db="${TDF_DOMO_VALIDATOR_DATABASE_URL:?Set a fully migrated disposable database URL}"
psql "$db" -X -q -v ON_ERROR_STOP=1 <<'SQL'
BEGIN;
-- One probe: a quote of total 501760 (subtotal 448000 + 12% tax) against the
-- given approved rate card version, with the given deposit. Returns 'ok' or the
-- validator's error message; every probe runs in its own subtransaction.
CREATE FUNCTION pg_temp.probe(product UUID, deposit BIGINT) RETURNS TEXT
LANGUAGE plpgsql AS $$
DECLARE
  runtime_id UUID := gen_random_uuid();
  quote_id UUID := gen_random_uuid();
  checkout_id UUID := gen_random_uuid();
  pv commerce_product_version%ROWTYPE;
  starts TIMESTAMPTZ := now() + make_interval(days => 40 + (random() * 300)::int);
  expires TIMESTAMPTZ := now() + INTERVAL '15 minutes';
BEGIN
  SELECT * INTO pv FROM commerce_product_version WHERE id = product;
  BEGIN
    INSERT INTO commerce_quote(id, domain_type, domain_subject_id, version, status, currency,
      subtotal_minor, tax_minor, total_minor, deposit_minor, expires_at)
    VALUES (quote_id, 'domo_event_quote', runtime_id::text, 1, 'sent', 'USD',
      448000, 53760, 501760, deposit, expires);
    INSERT INTO commerce_quote_line(quote_id, line_number, product_type, product_id, product_version,
      description, quantity, unit_amount_minor, subtotal_minor)
    VALUES (quote_id, 1, 'domo_rate_component', 'venue_base', pv.product_key || '-v' || pv.version,
      'Domo event', 1, 448000, 448000);
    INSERT INTO commerce_checkout_session(id, domain_type, domain_order_id, quote_id, status, environment,
      currency, subtotal_minor, total_minor, customer_email, lookup_token_hash, idempotency_key, expires_at)
    VALUES (checkout_id, 'domo_event_quote', 'domo-quote:' || runtime_id, quote_id, 'holding', 'sandbox',
      'USD', deposit, deposit, 'probe@example.com', md5(random()::text) || md5(random()::text),
      'deposit-policy-probe:' || runtime_id, expires);
    INSERT INTO commerce_checkout_line_item(checkout_id, line_number, product_type, product_id,
      product_version, description, quantity, unit_amount_minor, subtotal_minor, total_minor, snapshot)
    VALUES (checkout_id, 1, 'domo_initial_deposit', pv.product_key, pv.product_key || '-v' || pv.version,
      'Domo initial deposit', 1, deposit, deposit, deposit, '{}'::jsonb);
    INSERT INTO domo_event_quote_runtime(id, quote_id, checkout_id, product_version_id, venue_key,
      lookup_token_hash, create_idempotency_key, create_request_sha256, customer_name, customer_email,
      event_type, guests, starts_at, ends_at, setup_starts_at, duration_hours, setup_hours, catering,
      production, transport, quote_status, fulfillment_status, currency, subtotal_minor, tax_minor,
      total_minor, deposit_minor, balance_minor, tax_basis_points, deposit_basis_points,
      rate_card_version, rate_card_rules_sha256, timezone, terms_version, hold_expires_at)
    VALUES (runtime_id, quote_id, checkout_id, product, 'deposit-policy-probe:' || runtime_id,
      md5(random()::text) || md5(random()::text), 'deposit-policy-probe:' || runtime_id, repeat('3', 64),
      'Probe Customer', 'probe@example.com', 'wedding', 80, starts, starts + INTERVAL '8 hours',
      starts - INTERVAL '2 hours', 8, 2, FALSE, FALSE, FALSE, 'sent', 'date_held', 'USD',
      448000, 53760, 501760, deposit, 501760 - deposit,
      (pv.pricing_rules->>'legacy_tax_basis_points')::int, pv.deposit_basis_points,
      pv.product_key || '-v' || pv.version,
      encode(digest(convert_to(pv.pricing_rules::text, 'UTF8'), 'sha256'), 'hex'),
      pv.policy_snapshot->>'timezone', pv.policy_snapshot->>'terms_version', expires);
    RAISE EXCEPTION USING ERRCODE = 'P0001', MESSAGE = 'probe-ok';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM = 'probe-ok' THEN RETURN 'ok'; END IF;
    RETURN SQLERRM;
  END;
END $$;

-- Rate card versions copied from the approved fixture, each with a policy that
-- changes the basis-point deposit of 501760.
CREATE TEMP TABLE probe_card(label TEXT, id UUID, bps INT, policy JSONB, expected BIGINT, unadjusted BIGINT);
INSERT INTO probe_card VALUES
  -- 40%: 200704 raised to the 300000 minimum, still below the 90% threshold.
  ('minimum raises the deposit', gen_random_uuid(), 4000, '{"minimum_deposit_minor": 300000}', 300000, 200704),
  -- 95%: 476672 exceeds the default 90% threshold, so the whole total is due now.
  ('threshold switches to full payment', gen_random_uuid(), 9500, '{}', 501760, 476672),
  -- 40% under the defaults (minimum 500, threshold 90%): unchanged.
  ('defaults leave the deposit unchanged', gen_random_uuid(), 4000, '{}', 200704, 200704);
INSERT INTO commerce_product_version(id, domain_type, product_key, version, name_es, name_en,
  pricing_model, currency, pricing_rules, tax_code, deposit_basis_points, policy_snapshot, source,
  status, approved_by, approved_at)
SELECT c.id, base.domain_type, 'deposit_policy_probe_' || row_number() OVER (), 1, base.name_es, base.name_en,
  base.pricing_model, base.currency, base.pricing_rules, base.tax_code, c.bps,
  base.policy_snapshot || c.policy, base.source, 'draft', NULL, NULL
FROM probe_card c CROSS JOIN commerce_product_version base
WHERE base.id = '81000000-0000-0000-0000-000000000001';
INSERT INTO commerce_rate_card_review(domain_type, product_version_id, baseline_source,
  baseline_snapshot, candidate_snapshot, status, requested_by, reviewed_by, reviewed_at)
SELECT 'domo', id, 'deposit-policy-probe', '{}'::jsonb, '{}'::jsonb, 'approved', 2, 1, now() FROM probe_card;
UPDATE commerce_product_version SET status = 'active', approved_by = 1, approved_at = now()
WHERE id IN (SELECT id FROM probe_card);

DO $$
DECLARE
  card RECORD;
  accepted TEXT;
  rejected TEXT;
BEGIN
  FOR card IN SELECT * FROM probe_card LOOP
    accepted := pg_temp.probe(card.id, card.expected);
    IF accepted <> 'ok' THEN
      RAISE EXCEPTION '% : policy deposit % rejected: %', card.label, card.expected, accepted;
    END IF;
    IF card.unadjusted <> card.expected THEN
      rejected := pg_temp.probe(card.id, card.unadjusted);
      IF rejected NOT LIKE 'Domo quote tax or deposit does not match%' THEN
        RAISE EXCEPTION '% : unadjusted deposit % was not rejected by the policy check: %',
          card.label, card.unadjusted, rejected;
      END IF;
    END IF;
    RAISE NOTICE 'deposit policy probe passed: %', card.label;
  END LOOP;
END $$;
ROLLBACK;
SQL
echo "Domo deposit policy validator: minimum, full-payment threshold and default cases passed."
