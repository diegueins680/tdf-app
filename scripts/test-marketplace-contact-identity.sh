#!/bin/sh
set -eu
repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
test_database="tdf_marketplace_identity_test_$$"
export PGHOST="${PGHOST:-127.0.0.1}"
cleanup() { dropdb --if-exists "$test_database" >/dev/null; }
createdb "$test_database"
trap cleanup EXIT INT TERM
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/production-schema-20260814.sql" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" -f "$repo_root/scripts/__tests__/fixtures/catalog-production-source-fixture.sql" >/dev/null
node "$repo_root/scripts/render-production-migration-batch.mjs" | psql -X -v ON_ERROR_STOP=1 -d "$test_database" >/dev/null
psql -X -v ON_ERROR_STOP=1 -d "$test_database" <<'SQL'
INSERT INTO party(display_name,is_org,primary_email,created_at) VALUES ('Established synthetic account',FALSE,'manual-identity@example.test',NOW()),('Synthetic reviewer',FALSE,NULL,NOW());
DO $$ DECLARE
  fixture_index integer;
  order_key uuid;
  checkout_key uuid;
  attempt_key uuid;
BEGIN
  FOR fixture_index IN 1..4 LOOP
    order_key := ('50000000-0000-4000-8000-' || lpad(fixture_index::text,12,'0'))::uuid;
    checkout_key := ('60000000-0000-4000-8000-' || lpad(fixture_index::text,12,'0'))::uuid;
    attempt_key := ('70000000-0000-4000-8000-' || lpad(fixture_index::text,12,'0'))::uuid;
    INSERT INTO marketplace_order(id,buyer_name,buyer_email,total_usd_cents,currency,status,created_at,updated_at)
      VALUES (order_key,'Synthetic guest','manual-identity@example.test',1000,'USD','pending',now(),now());
    INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)
      VALUES (checkout_key,'marketplace_sale',order_key::text,'awaiting_payment','sandbox','USD',1000,1000,'manual-identity@example.test',md5(checkout_key::text)||md5(checkout_key::text),'manual-identity-checkout-'||fixture_index,now()+interval '15 minutes');
    INSERT INTO marketplace_sale_order_runtime(order_id,checkout_id,lookup_token_hash,create_idempotency_key,create_request_sha256,fulfillment_method,fulfillment_status,recipient_name,hold_expires_at)
      VALUES (order_key,checkout_key,md5(order_key::text)||md5(order_key::text),'manual-identity-order-'||fixture_index,repeat('a',64),'pickup','on_hold','Synthetic guest',now()+interval '15 minutes');
    IF fixture_index > 1 THEN
      INSERT INTO commerce_payment_attempt(id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,idempotency_key)
        VALUES (attempt_key,checkout_key,'bank_transfer','sandbox','manual_verify','requires_review',1000,'USD','tdf-manual-settlement','manual-identity-attempt-'||fixture_index);
      INSERT INTO commerce_manual_payment_evidence(checkout_id,payment_attempt_id,status) VALUES (checkout_key,attempt_key,'awaiting_evidence');
    END IF;
  END LOOP;
END $$;
SQL
export TDF_MARKETPLACE_IDENTITY_TEST_DB="host=$PGHOST dbname=$test_database"
cd "$repo_root/tdf-hq"
stack test tdf-hq:test:tdf-hq-test --fast --ghc-options=-O0 --jobs 1 --test-arguments='--match marketplace-contact-identity-postgresql'
