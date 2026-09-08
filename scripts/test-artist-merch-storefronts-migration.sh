#!/bin/sh
set -eu

TDF_MERCH_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TDF_MERCH_CONTAINER="tdf-merch-migration-test-$$"
TDF_MERCH_DATABASE="tdf_merch_test"
TDF_MERCH_ROLLBACK_DATABASE="tdf_merch_rollback_test"

cleanup() {
  docker rm -f "$TDF_MERCH_CONTAINER" >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

docker run --rm -d \
  --name "$TDF_MERCH_CONTAINER" \
  -e POSTGRES_HOST_AUTH_METHOD=trust \
  -e POSTGRES_DB="$TDF_MERCH_DATABASE" \
  postgres:16-alpine >/dev/null

attempt=0
until docker exec "$TDF_MERCH_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_DATABASE" >/dev/null 2>&1; do
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 45 ]; then
    echo "Merch migration test database did not become ready" >&2
    exit 1
  fi
  sleep 1
done

# The official image briefly exposes its bootstrap server before restarting
# into the long-lived postmaster.
sleep 5
until docker exec "$TDF_MERCH_CONTAINER" pg_isready -U postgres -d "$TDF_MERCH_DATABASE" >/dev/null 2>&1; do
  sleep 1
done

psql_exec() {
  database="$1"
  shift
  docker exec -i "$TDF_MERCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$database" "$@"
}

apply_file() {
  database="$1"
  file="$2"
  docker exec -i "$TDF_MERCH_CONTAINER" psql -v ON_ERROR_STOP=1 -U postgres -d "$database" < "$file" >/dev/null
}

prepare_dependencies() {
  database="$1"
  apply_file "$database" "$TDF_MERCH_ROOT/tdf-hq/sql/init_schema.sql"
  apply_file "$database" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-08-13_unified_checkout_core.sql"
  psql_exec "$database" <<'SQL' >/dev/null
CREATE TABLE directory_profile (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  subject_party_id BIGINT NOT NULL REFERENCES party(id),
  profile_kind TEXT NOT NULL,
  public_name TEXT NOT NULL,
  slug TEXT NOT NULL UNIQUE,
  profile_status TEXT NOT NULL,
  visibility TEXT NOT NULL,
  moderation_status TEXT NOT NULL
);
CREATE TABLE directory_profile_manager (
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  account_party_id BIGINT NOT NULL REFERENCES party(id),
  active BOOLEAN NOT NULL,
  can_manage BOOLEAN NOT NULL,
  source_claim_id UUID,
  PRIMARY KEY(profile_id,account_party_id)
);
CREATE TABLE directory_verification (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  profile_id UUID NOT NULL REFERENCES directory_profile(id),
  status TEXT NOT NULL
);
SQL
}

prepare_dependencies "$TDF_MERCH_DATABASE"
apply_file "$TDF_MERCH_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"
apply_file "$TDF_MERCH_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"

psql_exec "$TDF_MERCH_DATABASE" <<'SQL' >/dev/null
INSERT INTO party(id,display_name,is_org,created_at) VALUES
  (900001,'Synthetic Band',TRUE,now()),
  (900002,'Synthetic Owner',FALSE,now()),
  (900003,'Synthetic Collaborator',FALSE,now()),
  (900004,'Synthetic Other Seller',FALSE,now()),
  (900005,'Synthetic Admin',FALSE,now()),
  (900006,'Synthetic Buyer',FALSE,now());
SELECT setval(pg_get_serial_sequence('party','id'), 900100, TRUE);

INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status)
VALUES('91000000-0000-4000-8000-000000000001',900001,'band','Synthetic Band','synthetic-band','published','public','allowed');
INSERT INTO directory_profile_manager(profile_id,account_party_id,active,can_manage,source_claim_id)
VALUES('91000000-0000-4000-8000-000000000001',900002,TRUE,TRUE,'91000000-0000-4000-8000-000000000099');

INSERT INTO merch_store(
  id,directory_profile_id,seller_party_id,primary_owner_party_id,slug,display_name,application_note,
  application_idempotency_key,application_request_sha256
) VALUES(
  '92000000-0000-4000-8000-000000000001','91000000-0000-4000-8000-000000000001',900001,900002,
  'synthetic-band','Synthetic Band','Solicitud sintética para verificar el piloto de merch.',
  'synthetic-store-application-001',encode(digest('synthetic-store-request','sha256'),'hex')
);

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM merch_store_member WHERE store_id='92000000-0000-4000-8000-000000000001'
      AND party_id=900002 AND member_role='owner' AND invitation_status='accepted'
      AND can_catalog AND can_stock AND can_orders AND can_fulfillment AND can_finance AND can_settings
  ) THEN RAISE EXCEPTION 'Primary owner membership was not created'; END IF;
END $$;

UPDATE merch_store SET application_status='approved', operational_status='active', reviewer_notes='Synthetic approval.',
  reviewed_by=900005, reviewed_at=now(), activated_at=now(), updated_at=now()
WHERE id='92000000-0000-4000-8000-000000000001';

INSERT INTO merch_store_member(
  store_id,party_id,member_role,invitation_status,can_catalog,can_stock,can_orders,can_fulfillment,
  can_finance,can_settings,invited_by,invitation_idempotency_key,invitation_request_sha256,accepted_at
) VALUES(
  '92000000-0000-4000-8000-000000000001',900003,'collaborator','accepted',TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,900002,
  'synthetic-member-001',encode(digest('synthetic-member-request','sha256'),'hex'),now()
);

INSERT INTO merch_store_policy(id,store_id,version,shipping_policy,return_policy,status,effective_at,created_by)
VALUES(
  '93000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001',1,
  'Despachamos pedidos nacionales en el plazo indicado para cada producto.',
  'Aceptamos solicitudes de devolución según los mínimos de protección al consumidor.',
  'active',now(),900002
);

INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status)
VALUES('91000000-0000-4000-8000-000000000002',900004,'band','Synthetic Other Seller','synthetic-other-seller','published','public','allowed');
INSERT INTO directory_profile_manager(profile_id,account_party_id,active,can_manage,source_claim_id)
VALUES('91000000-0000-4000-8000-000000000002',900004,TRUE,TRUE,'91000000-0000-4000-8000-000000000098');
INSERT INTO merch_store(
  id,directory_profile_id,seller_party_id,primary_owner_party_id,slug,display_name,application_note,
  application_idempotency_key,application_request_sha256
) VALUES(
  '92000000-0000-4000-8000-000000000002','91000000-0000-4000-8000-000000000002',900004,900004,
  'synthetic-other-seller','Synthetic Other Seller','Solicitud sintética para probar el aislamiento entre tiendas.',
  'synthetic-store-application-002',encode(digest('synthetic-store-request-002','sha256'),'hex')
);
INSERT INTO merch_store_policy(id,store_id,version,shipping_policy,return_policy,status,effective_at,created_by)
VALUES(
  '93000000-0000-4000-8000-000000000002','92000000-0000-4000-8000-000000000002',1,
  'Política sintética de otra tienda.','Política sintética de otra tienda.','active',now(),900004
);

DO $$
BEGIN
  BEGIN
    INSERT INTO merch_product(
      id,store_id,slug,name,description,category,policy_id,created_by,create_idempotency_key,create_request_sha256
    ) VALUES(
      '95000000-0000-4000-8000-000000000099','92000000-0000-4000-8000-000000000001','cross-store-policy',
      'Cross-store policy','This synthetic product must be rejected.','other',
      '93000000-0000-4000-8000-000000000002',900002,'synthetic-cross-policy',encode(digest('synthetic-cross-policy','sha256'),'hex')
    );
    RAISE EXCEPTION 'Cross-store product policy was accepted';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM = 'Cross-store product policy was accepted' THEN RAISE; END IF;
  END;
END $$;

INSERT INTO merch_shipping_zone(id,store_id,name,delivery_method,rate_minor,estimated_min_days,estimated_max_days)
VALUES('94000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001','Ecuador continental','national_shipping',500,2,7);

INSERT INTO merch_product(
  id,store_id,slug,name,description,category,status,availability_mode,policy_id,
  submitted_at,reviewed_by,reviewed_at,published_at,created_by,create_idempotency_key,create_request_sha256
) VALUES(
  '95000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001','camiseta-synthetic',
  'Camiseta Synthetic','Producto sintético para verificar el modelo de merch.','apparel','published','in_stock',
  '93000000-0000-4000-8000-000000000001',now(),900005,now(),now(),900002
  ,'synthetic-product-001',encode(digest('synthetic-product-request','sha256'),'hex')
);
INSERT INTO merch_product_variant(
  id,store_id,product_id,sku,name,option_values,price_minor,currency,weight_grams,stock_mode,stock_on_hand
) VALUES(
  '96000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001',
  '95000000-0000-4000-8000-000000000001','TEE-BLK-M','Negra / M','{"color":"Negro","size":"M"}',5000,'USD',250,'finite',5
);

INSERT INTO merch_cart(id,store_id,lookup_token_hash,currency)
VALUES('97000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001',encode(digest('synthetic-cart-token','sha256'),'hex'),'USD');
INSERT INTO merch_cart_item(cart_id,variant_id,quantity)
VALUES('97000000-0000-4000-8000-000000000001','96000000-0000-4000-8000-000000000001',4);

INSERT INTO merch_order(
  id,order_number,store_id,cart_id,customer_party_id,customer_email,customer_name,lookup_token_hash,currency,
  product_subtotal_minor,shipping_minor,tdf_commission_bps,tdf_commission_minor,seller_net_minor,total_minor,
  shipping_method,shipping_zone_snapshot,recipient_snapshot,policy_snapshot,commission_snapshot,
  create_idempotency_key,create_request_sha256
) VALUES(
  '98000000-0000-4000-8000-000000000001','TDF-MERCH-SYNTH001','92000000-0000-4000-8000-000000000001',
  '97000000-0000-4000-8000-000000000001',900006,'buyer.synthetic@example.test','Synthetic Buyer',
  encode(digest('synthetic-order-token','sha256'),'hex'),'USD',20000,500,1000,2000,18500,20500,
  'national_shipping','{"zoneId":"94000000-0000-4000-8000-000000000001","rateMinor":500}',
  '{"name":"Synthetic Buyer","countryCode":"EC","city":"Quito","addressLine1":"Synthetic 123"}',
  '{"policyId":"93000000-0000-4000-8000-000000000001","version":1}',
  '{"commissionBps":1000,"basis":"product_subtotal_after_discount"}',
  'synthetic-checkout-key-001',encode(digest('synthetic-request-001','sha256'),'hex')
);
INSERT INTO merch_order_line(
  id,order_id,line_number,product_id,variant_id,quantity,unit_price_minor,subtotal_minor,total_minor,
  product_snapshot,variant_snapshot,policy_snapshot
) VALUES(
  '99000000-0000-4000-8000-000000000001','98000000-0000-4000-8000-000000000001',1,
  '95000000-0000-4000-8000-000000000001','96000000-0000-4000-8000-000000000001',4,5000,20000,20000,
  '{"name":"Camiseta Synthetic","category":"apparel"}',
  '{"sku":"TEE-BLK-M","name":"Negra / M","options":{"color":"Negro","size":"M"}}',
  '{"policyId":"93000000-0000-4000-8000-000000000001","version":1}'
);

INSERT INTO commerce_checkout_session(
  id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,fee_minor,total_minor,
  customer_email,customer_party_id,lookup_token_hash,idempotency_key,expires_at
) VALUES(
  '9a000000-0000-4000-8000-000000000001','merch_order','98000000-0000-4000-8000-000000000001',
  'holding','sandbox','USD',20000,500,20500,'buyer.synthetic@example.test',900006,
  encode(digest('synthetic-checkout-token','sha256'),'hex'),'synthetic-checkout-key-001',now()+interval '20 minutes'
);
UPDATE merch_order SET checkout_id='9a000000-0000-4000-8000-000000000001' WHERE id='98000000-0000-4000-8000-000000000001';
INSERT INTO commerce_checkout_line_item(
  checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,
  subtotal_minor,total_minor,snapshot
) VALUES(
  '9a000000-0000-4000-8000-000000000001',1,'merch_variant','96000000-0000-4000-8000-000000000001','1',
  'Camiseta Synthetic — Negra / M',4,5000,20000,20000,
  '{"storeId":"92000000-0000-4000-8000-000000000001","sku":"TEE-BLK-M"}'
);
SELECT merch_reserve_stock(
  '98000000-0000-4000-8000-000000000001','9a000000-0000-4000-8000-000000000001',
  '[{"variantId":"96000000-0000-4000-8000-000000000001","quantity":4}]',
  (SELECT expires_at FROM commerce_checkout_session WHERE id='9a000000-0000-4000-8000-000000000001')
);
SQL

reserved=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT stock_reserved FROM merch_product_variant WHERE id='96000000-0000-4000-8000-000000000001';")
test "$reserved" = "4"

if psql_exec "$TDF_MERCH_DATABASE" -c "UPDATE commerce_checkout_session SET status='paid',paid_minor=total_minor WHERE id='9a000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "A browser-style checkout update marked merch paid without verified payment evidence" >&2
  exit 1
fi

psql_exec "$TDF_MERCH_DATABASE" <<'SQL' >/dev/null
INSERT INTO commerce_payment_attempt(
  id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key
) VALUES(
  '9b000000-0000-4000-8000-000000000001','9a000000-0000-4000-8000-000000000001',
  'bank_transfer','sandbox','manual_verify','succeeded',20500,'USD','synthetic-sandbox','synthetic-payment-001'
);
UPDATE commerce_checkout_session SET status='paid',paid_minor=total_minor,paid_at=now()
WHERE id='9a000000-0000-4000-8000-000000000001';
SQL

payment_inventory_state=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT order_record.payment_status || '|' || order_record.fulfillment_status || '|' || variant.stock_reserved || '|' || variant.stock_sold || '|' || reservation.status FROM merch_order order_record JOIN merch_inventory_reservation reservation ON reservation.order_id=order_record.id JOIN merch_product_variant variant ON variant.id=reservation.variant_id WHERE order_record.id='98000000-0000-4000-8000-000000000001';")
test "$payment_inventory_state" = "paid|pending|0|4|consumed"

psql_exec "$TDF_MERCH_DATABASE" <<'SQL' >/dev/null
INSERT INTO merch_order(
  id,order_number,store_id,customer_email,customer_name,lookup_token_hash,currency,product_subtotal_minor,
  tdf_commission_bps,tdf_commission_minor,seller_net_minor,total_minor,shipping_method,
  shipping_zone_snapshot,recipient_snapshot,policy_snapshot,commission_snapshot,
  create_idempotency_key,create_request_sha256
) VALUES
  ('98000000-0000-4000-8000-000000000002','TDF-MERCH-SYNTH002','92000000-0000-4000-8000-000000000001',
   'buyer.two@example.test','Synthetic Buyer Two',encode(digest('order-two','sha256'),'hex'),'USD',5000,1000,500,4500,5000,
   'coordinated_pickup','{}','{"name":"Synthetic Buyer Two","countryCode":"EC","city":"Quito","addressLine1":"Pickup"}',
   '{"policyId":"93000000-0000-4000-8000-000000000001","version":1}','{"commissionBps":1000}',
   'synthetic-checkout-key-002',encode(digest('synthetic-request-002','sha256'),'hex')),
  ('98000000-0000-4000-8000-000000000003','TDF-MERCH-SYNTH003','92000000-0000-4000-8000-000000000001',
   'buyer.three@example.test','Synthetic Buyer Three',encode(digest('order-three','sha256'),'hex'),'USD',5000,1000,500,4500,5000,
   'coordinated_pickup','{}','{"name":"Synthetic Buyer Three","countryCode":"EC","city":"Quito","addressLine1":"Pickup"}',
   '{"policyId":"93000000-0000-4000-8000-000000000001","version":1}','{"commissionBps":1000}',
   'synthetic-checkout-key-003',encode(digest('synthetic-request-003','sha256'),'hex'));
INSERT INTO commerce_checkout_session(
  id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,customer_email,
  lookup_token_hash,idempotency_key,expires_at
) VALUES
  ('9a000000-0000-4000-8000-000000000002','merch_order','98000000-0000-4000-8000-000000000002','holding','sandbox','USD',5000,5000,
   'buyer.two@example.test',encode(digest('checkout-two','sha256'),'hex'),'synthetic-checkout-key-002',now()+interval '20 minutes'),
  ('9a000000-0000-4000-8000-000000000003','merch_order','98000000-0000-4000-8000-000000000003','holding','sandbox','USD',5000,5000,
   'buyer.three@example.test',encode(digest('checkout-three','sha256'),'hex'),'synthetic-checkout-key-003',now()+interval '20 minutes');
UPDATE merch_order SET checkout_id='9a000000-0000-4000-8000-000000000002' WHERE id='98000000-0000-4000-8000-000000000002';
UPDATE merch_order SET checkout_id='9a000000-0000-4000-8000-000000000003' WHERE id='98000000-0000-4000-8000-000000000003';
SQL

psql_exec "$TDF_MERCH_DATABASE" -c "BEGIN; SELECT merch_reserve_stock('98000000-0000-4000-8000-000000000002','9a000000-0000-4000-8000-000000000002','[{\"variantId\":\"96000000-0000-4000-8000-000000000001\",\"quantity\":1}]',(SELECT expires_at FROM commerce_checkout_session WHERE id='9a000000-0000-4000-8000-000000000002')); SELECT pg_sleep(2); COMMIT;" >/dev/null &
first_reservation_pid=$!
sleep 1
if psql_exec "$TDF_MERCH_DATABASE" -c "SELECT merch_reserve_stock('98000000-0000-4000-8000-000000000003','9a000000-0000-4000-8000-000000000003','[{\"variantId\":\"96000000-0000-4000-8000-000000000001\",\"quantity\":1}]',(SELECT expires_at FROM commerce_checkout_session WHERE id='9a000000-0000-4000-8000-000000000003'));" >/dev/null 2>&1; then
  echo "Concurrent merch reservations oversold the final unit" >&2
  wait "$first_reservation_pid"
  exit 1
fi
wait "$first_reservation_pid"
concurrent_stock=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT stock_reserved || '|' || stock_sold FROM merch_product_variant WHERE id='96000000-0000-4000-8000-000000000001';")
test "$concurrent_stock" = "1|4"
released=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT merch_release_expired_reservations(now()+interval '1 hour');")
test "$released" = "1"
released_stock=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT stock_reserved || '|' || stock_sold FROM merch_product_variant WHERE id='96000000-0000-4000-8000-000000000001';")
test "$released_stock" = "0|4"
if psql_exec "$TDF_MERCH_DATABASE" -c "UPDATE merch_product_variant SET stock_on_hand=3 WHERE id='96000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Merch stock was reduced below already sold units" >&2
  exit 1
fi

commission=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT merch_calculate_commission_bps('92000000-0000-4000-8000-000000000001');")
test "$commission" = "1000"
psql_exec "$TDF_MERCH_DATABASE" -c "INSERT INTO merch_commission_policy(store_id,commission_bps,reason,approved_by) VALUES('92000000-0000-4000-8000-000000000001',0,'Synthetic pilot override',900005);" >/dev/null
pilot_commission=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT merch_calculate_commission_bps('92000000-0000-4000-8000-000000000001');")
test "$pilot_commission" = "0"

if psql_exec "$TDF_MERCH_DATABASE" -c "UPDATE merch_order_line SET unit_price_minor=1 WHERE id='99000000-0000-4000-8000-000000000001';" >/dev/null 2>&1; then
  echo "Immutable merch order line accepted a price mutation" >&2
  exit 1
fi
if psql_exec "$TDF_MERCH_DATABASE" -c "INSERT INTO merch_analytics_event(event_name,properties,consent_basis) VALUES('product_viewed','{\"email\":\"leak@example.test\"}','consented_analytics');" >/dev/null 2>&1; then
  echo "Merch analytics accepted forbidden personal data" >&2
  exit 1
fi
if apply_file "$TDF_MERCH_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts_rollback.sql" 2>/dev/null; then
  echo "Destructive merch rollback succeeded despite commercial evidence" >&2
  exit 1
fi

psql_exec "$TDF_MERCH_DATABASE" -c "UPDATE merch_order SET fulfillment_status='preparing' WHERE id='98000000-0000-4000-8000-000000000001';" >/dev/null
separated_state=$(psql_exec "$TDF_MERCH_DATABASE" -Atc "SELECT payment_status || '|' || fulfillment_status FROM merch_order WHERE id='98000000-0000-4000-8000-000000000001';")
test "$separated_state" = "paid|preparing"

psql_exec postgres -c "CREATE DATABASE $TDF_MERCH_ROLLBACK_DATABASE;" >/dev/null
prepare_dependencies "$TDF_MERCH_ROLLBACK_DATABASE"
apply_file "$TDF_MERCH_ROLLBACK_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"
apply_file "$TDF_MERCH_ROLLBACK_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts_rollback.sql"
remaining=$(psql_exec "$TDF_MERCH_ROLLBACK_DATABASE" -Atc "SELECT count(*) FROM information_schema.tables WHERE table_schema='public' AND table_name LIKE 'merch_%';")
test "$remaining" = "0"
remaining_functions=$(psql_exec "$TDF_MERCH_ROLLBACK_DATABASE" -Atc "SELECT count(*) FROM pg_proc function_record JOIN pg_namespace namespace_record ON namespace_record.oid=function_record.pronamespace WHERE namespace_record.nspname='public' AND function_record.proname LIKE 'merch_%';")
test "$remaining_functions" = "0"
apply_file "$TDF_MERCH_ROLLBACK_DATABASE" "$TDF_MERCH_ROOT/tdf-hq/sql/2026-09-07_artist_merch_storefronts.sql"

echo "Artist merch storefront migration passed rerun, claimed-profile eligibility, scoped ownership, cross-store policy isolation, immutable snapshots, payment evidence gating, independent fulfillment, concurrent no-oversell reservation and expiry, stock consumption and lower-bound protection, commission override, analytics privacy, guarded rollback, clean rollback, and reapply checks."
