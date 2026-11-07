-- Synthetic authoring records only. Outcome transitions below use real triggers.
-- Call inside a disposable-database transaction and roll that transaction back.
CREATE FUNCTION pg_temp.music_payment_state_fixture()
RETURNS TABLE(order_id UUID, checkout_id UUID, attempt_id UUID)
LANGUAGE plpgsql AS $$
DECLARE
  buyer BIGINT;
  release_key UUID := gen_random_uuid();
  version_key UUID := gen_random_uuid();
  asset_key UUID := gen_random_uuid();
  rule_key UUID := gen_random_uuid();
  order_key UUID := gen_random_uuid();
  checkout_key UUID := gen_random_uuid();
  attempt_key UUID := gen_random_uuid();
  previous_replication_role TEXT := current_setting('session_replication_role');
BEGIN
  PERFORM set_config('session_replication_role', 'replica', TRUE);
  INSERT INTO party(display_name, is_org, primary_email, created_at)
  VALUES ('Music state fixture', FALSE, order_key::TEXT || '@example.test', NOW())
  RETURNING id INTO buyer;
  INSERT INTO music_release(id, artist_party_id, canonical_slug, release_kind, created_by)
  VALUES (release_key, buyer, 'music-state-fixture-' || order_key, 'single', buyer);
  INSERT INTO music_release_version(
    id, release_id, version_number, state, title, display_artist, published_at,
    created_by, approved_by, approved_at, release_at_utc, release_timezone,
    immutable_snapshot, snapshot_sha256
  ) VALUES (
    version_key, release_key, 1, 'published', 'Music state fixture', 'Fixture artist',
    NOW() - INTERVAL '1 hour', buyer, buyer, NOW() - INTERVAL '2 hours',
    NOW() - INTERVAL '1 hour', 'UTC', '{}'::JSONB, repeat('b', 64)
  );
  UPDATE music_release SET published_version_id = version_key WHERE id = release_key;
  INSERT INTO music_asset(
    id, release_version_id, asset_role, storage_provider, bucket_name, object_key,
    media_type, byte_size, sha256, processing_state, immutable, created_by, ready_at
  ) VALUES (
    asset_key, version_key, 'cover_original', 's3_compatible', 'fixture',
    'music/state-fixture/' || order_key, 'application/octet-stream', 4,
    repeat('a', 64), 'ready', TRUE, buyer, NOW()
  );
  INSERT INTO music_availability_rule(
    id, release_version_id, listening_policy, download_policy, purchasable,
    price_minor, currency, downloadable_asset_id
  ) VALUES (rule_key, version_key, 'none', 'purchase', TRUE, 250, 'USD', asset_key);
  INSERT INTO commerce_checkout_session(
    id, domain_type, domain_order_id, status, environment, currency,
    subtotal_minor, total_minor, customer_email, customer_party_id,
    lookup_token_hash, idempotency_key, expires_at
  ) VALUES (
    checkout_key, 'music_download', order_key::TEXT, 'awaiting_payment', 'sandbox',
    'USD', 250, 250, order_key::TEXT || '@example.test', buyer,
    order_key::TEXT, order_key::TEXT, NOW() + INTERVAL '30 minutes'
  );
  INSERT INTO music_purchase_order(
    id, buyer_party_id, release_version_id, availability_rule_id, checkout_id,
    state, gross_minor, net_minor, currency, idempotency_key
  ) VALUES (
    order_key, buyer, version_key, rule_key, checkout_key,
    'awaiting_payment', 250, 250, 'USD', order_key::TEXT
  );
  INSERT INTO commerce_payment_attempt(
    id, checkout_id, provider, environment, operation, status, amount_minor,
    currency, merchant_account_ref, idempotency_key
  ) VALUES (
    attempt_key, checkout_key, 'paypal', 'sandbox', 'capture', 'processing',
    250, 'USD', 'music-state-fixture', attempt_key::TEXT
  );
  INSERT INTO commerce_provider_binding(
    payment_attempt_id, provider, environment, merchant_account_ref,
    resource_type, provider_resource_id, provider_resource_path,
    merchant_reference, amount_minor, currency
  ) VALUES (
    attempt_key, 'paypal', 'sandbox', 'music-state-fixture', 'capture',
    'FIXTURE-' || attempt_key, '/v2/payments/captures/FIXTURE-' || attempt_key,
    order_key::TEXT, 250, 'USD'
  );
  PERFORM set_config('session_replication_role', previous_replication_role, TRUE);
  RETURN QUERY SELECT order_key, checkout_key, attempt_key;
END;
$$;
