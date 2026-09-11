\set ON_ERROR_STOP on

-- Minimal canonical security registry needed to exercise the real Servant
-- bearer-token AuthHandler. These deterministic rows are synthetic and avoid
-- importing unrelated production catalog seed data into the runtime fixture.
INSERT INTO workflow_definition(id,code,name_es,name_en) VALUES
  ('8a000000-0000-4000-8000-000000000001','runtime-security','Seguridad sintética','Synthetic security');
INSERT INTO workflow_state(id,workflow_id,code,name_es,name_en) VALUES
  ('8a000000-0000-4000-8000-000000000002','8a000000-0000-4000-8000-000000000001','published','Publicado','Published');
INSERT INTO security_module(id,code,name_es,name_en) VALUES
  ('8a000000-0000-4000-8000-000000000003','admin','Administración','Administration');
INSERT INTO security_action(id,code,name_es,name_en) VALUES
  ('8a000000-0000-4000-8000-000000000004','access','Acceder','Access');
INSERT INTO security_permission(id,code,module_id,action_id,resource_scope,name_es,name_en) VALUES
  ('8a000000-0000-4000-8000-000000000005','admin.access',
   '8a000000-0000-4000-8000-000000000003','8a000000-0000-4000-8000-000000000004',
   'module','Acceder a administración','Access administration');
INSERT INTO security_role(id,code,name_es,name_en,system_role,workflow_state_id) VALUES
  ('8a000000-0000-4000-8000-000000000006','admin','Administrador sintético','Synthetic administrator',TRUE,
   '8a000000-0000-4000-8000-000000000002');
INSERT INTO role_permission(id,role_id,permission_id,approval_mode,active) VALUES
  ('8a000000-0000-4000-8000-000000000007','8a000000-0000-4000-8000-000000000006',
   '8a000000-0000-4000-8000-000000000005','bootstrap',TRUE);

INSERT INTO party(id,display_name,is_org,created_at) VALUES
  (900001,'Runtime Band',TRUE,now()),
  (900002,'Runtime Owner',FALSE,now()),
  (900003,'Runtime Collaborator',FALSE,now()),
  (900004,'Runtime Other Seller',FALSE,now()),
  (900005,'Runtime Strict Admin',FALSE,now()),
  (900006,'Runtime Pilot Applicant',FALSE,now()),
  (900007,'Runtime Applicant Band',TRUE,now()),
  (900008,'Runtime Independent Admin',FALSE,now());
SELECT setval(pg_get_serial_sequence('party','id'), 900100, TRUE);

INSERT INTO api_token(token,party_id,label,active) VALUES
  ('runtime-owner-token',900002,'Synthetic merch HTTP owner',TRUE),
  ('runtime-collaborator-token',900003,'Synthetic merch HTTP collaborator',TRUE),
  ('runtime-other-seller-token',900004,'Synthetic merch HTTP other seller',TRUE),
  ('runtime-admin-token',900005,'Synthetic merch HTTP strict administrator',TRUE),
  ('runtime-independent-admin-token',900008,'Synthetic independent settlement reviewer',TRUE),
  ('runtime-applicant-token',900006,'Synthetic merch HTTP pilot applicant',TRUE);

INSERT INTO party_security_role(party_id,role_id,approval_mode,active)
SELECT actor.id,role.id,'bootstrap',TRUE
FROM (VALUES (900005::bigint),(900008::bigint)) actor(id)
CROSS JOIN security_role role WHERE role.code='admin' AND role.active;

INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status) VALUES
  ('91000000-0000-4000-8000-000000000001',900001,'band','Runtime Band','runtime-band','published','public','allowed'),
  ('91000000-0000-4000-8000-000000000002',900004,'band','Runtime Other Seller','runtime-other-seller','published','public','allowed'),
  ('91000000-0000-4000-8000-000000000003',900007,'band','Runtime Applicant Band','runtime-applicant-band','published','public','allowed');
INSERT INTO directory_profile_manager(profile_id,account_party_id,active,can_manage,source_claim_id) VALUES
  ('91000000-0000-4000-8000-000000000001',900002,TRUE,TRUE,'91000000-0000-4000-8000-000000000091'),
  ('91000000-0000-4000-8000-000000000002',900004,TRUE,TRUE,'91000000-0000-4000-8000-000000000092'),
  ('91000000-0000-4000-8000-000000000003',900006,TRUE,TRUE,'91000000-0000-4000-8000-000000000093');

INSERT INTO merch_store(
  id,directory_profile_id,seller_party_id,primary_owner_party_id,slug,display_name,application_note,
  application_idempotency_key,application_request_sha256,application_status,operational_status,
  reviewed_by,reviewed_at,activated_at
) VALUES
  ('92000000-0000-4000-8000-000000000001','91000000-0000-4000-8000-000000000001',900001,900002,
   'runtime-band','Runtime Band','Synthetic runtime application for handler verification.',
   'runtime-store-application-001',encode(digest('runtime-store-request-001','sha256'),'hex'),'approved','active',900002,now(),now()),
  ('92000000-0000-4000-8000-000000000002','91000000-0000-4000-8000-000000000002',900004,900004,
   'runtime-other-seller','Runtime Other Seller','Synthetic runtime application for isolation verification.',
   'runtime-store-application-002',encode(digest('runtime-store-request-002','sha256'),'hex'),'approved','active',900004,now(),now());

INSERT INTO merch_store_member(
  store_id,party_id,member_role,invitation_status,can_catalog,can_stock,can_orders,can_fulfillment,
  can_finance,can_settings,invited_by,invitation_idempotency_key,invitation_request_sha256,accepted_at
) VALUES(
  '92000000-0000-4000-8000-000000000001',900003,'collaborator','accepted',FALSE,FALSE,TRUE,FALSE,
  FALSE,FALSE,900002,'runtime-member-001',encode(digest('runtime-member-request-001','sha256'),'hex'),now()
);

INSERT INTO merch_store_policy(id,store_id,version,shipping_policy,return_policy,status,effective_at,created_by)
VALUES(
  '93000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001',1,
  'Synthetic national shipping policy for runtime verification.',
  'Synthetic return policy for runtime verification and support.',
  'active',now(),900002
);
INSERT INTO merch_shipping_zone(
  id,store_id,name,country_code,subdivision_codes,delivery_method,rate_minor,
  free_shipping_min_minor,estimated_min_days,estimated_max_days,active
) VALUES(
  '93000000-0000-4000-8000-000000000002','92000000-0000-4000-8000-000000000001',
  'Synthetic Pichincha shipping','EC','{PICHINCHA}','national_shipping',500,20000,2,5,TRUE
);
INSERT INTO merch_product(
  id,store_id,slug,name,description,category,status,availability_mode,policy_id,
  submitted_at,reviewed_by,reviewed_at,published_at,created_by,create_idempotency_key,create_request_sha256
) VALUES(
  '95000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001','runtime-shirt',
  'Runtime Shirt','Synthetic product used only for isolated runtime tests.','apparel','published','in_stock',
  '93000000-0000-4000-8000-000000000001',now(),900002,now(),now(),900002,
  'runtime-product-001',encode(digest('runtime-product-request-001','sha256'),'hex')
);
INSERT INTO merch_product_variant(
  id,store_id,product_id,sku,name,option_values,price_minor,currency,weight_grams,stock_mode,stock_on_hand
) VALUES(
  '96000000-0000-4000-8000-000000000001','92000000-0000-4000-8000-000000000001',
  '95000000-0000-4000-8000-000000000001','RUNTIME-TEE-M','M','{"size":"M"}',5000,'USD',250,'finite',5
);
INSERT INTO merch_cart(id,store_id,lookup_token_hash,currency)
VALUES(
  '97000000-0000-4000-8000-000000000004','92000000-0000-4000-8000-000000000001',
  encode(digest('runtime-cart-token','sha256'),'hex'),'USD'
);
INSERT INTO merch_cart_item(cart_id,variant_id,quantity)
VALUES('97000000-0000-4000-8000-000000000004','96000000-0000-4000-8000-000000000001',2);

INSERT INTO merch_order(
  id,order_number,store_id,cart_id,customer_email,customer_name,lookup_token_hash,currency,
  product_subtotal_minor,tdf_commission_bps,tdf_commission_minor,seller_net_minor,total_minor,
  shipping_method,shipping_zone_snapshot,recipient_snapshot,policy_snapshot,commission_snapshot,
  create_idempotency_key,create_request_sha256
) VALUES(
  '98000000-0000-4000-8000-000000000004','TDF-MERCH-RUNTIME04','92000000-0000-4000-8000-000000000001',
  '97000000-0000-4000-8000-000000000004','runtime.buyer@example.test','Runtime Buyer',
  encode(digest('runtime-order-token','sha256'),'hex'),'USD',10000,1000,1000,9000,10000,
  'coordinated_pickup','{"deliveryMethod":"coordinated_pickup","rateMinor":0}',
  '{"name":"Runtime Buyer","countryCode":"EC","city":"Quito","addressLine1":"Synthetic address"}',
  '{"id":"93000000-0000-4000-8000-000000000001","version":1}',
  '{"commissionBps":1000,"basis":"product_subtotal_after_discount"}',
  'runtime-checkout-key-004',encode(digest('runtime-checkout-request-004','sha256'),'hex')
);
INSERT INTO merch_order(
  id,order_number,store_id,customer_email,customer_name,lookup_token_hash,currency,
  product_subtotal_minor,shipping_minor,tdf_commission_bps,tdf_commission_minor,seller_net_minor,total_minor,
  commercial_status,payment_status,fulfillment_status,settlement_status,shipping_method,
  shipping_zone_snapshot,recipient_snapshot,policy_snapshot,commission_snapshot,
  create_idempotency_key,create_request_sha256,confirmed_at,completed_at
) VALUES(
  '98000000-0000-4000-8000-000000000005','TDF-MERCH-RUNTIME05','92000000-0000-4000-8000-000000000001',
  'settlement.buyer@example.test','Synthetic Settlement Buyer',encode(digest('runtime-settlement-order-token','sha256'),'hex'),'USD',
  5000,500,1000,500,5000,5500,'completed','paid','delivered','ready','national_shipping',
  '{"deliveryMethod":"national_shipping","rateMinor":500}',
  '{"name":"Synthetic Settlement Buyer","countryCode":"EC","city":"Quito","addressLine1":"Synthetic settlement address"}',
  '{"id":"93000000-0000-4000-8000-000000000001","version":1}',
  '{"commissionBps":1000,"basis":"product_subtotal_after_discount"}',
  'runtime-settlement-order-005',encode(digest('runtime-settlement-order-request-005','sha256'),'hex'),now(),now()
);
INSERT INTO merch_order_line(
  id,order_id,line_number,product_id,variant_id,quantity,unit_price_minor,subtotal_minor,total_minor,
  product_snapshot,variant_snapshot,policy_snapshot
) VALUES(
  '99000000-0000-4000-8000-000000000004','98000000-0000-4000-8000-000000000004',1,
  '95000000-0000-4000-8000-000000000001','96000000-0000-4000-8000-000000000001',2,5000,10000,10000,
  '{"name":"Runtime Shirt"}','{"sku":"RUNTIME-TEE-M","name":"M"}',
  '{"id":"93000000-0000-4000-8000-000000000001","version":1}'
);
INSERT INTO merch_order_line(
  id,order_id,line_number,product_id,variant_id,quantity,unit_price_minor,subtotal_minor,total_minor,
  product_snapshot,variant_snapshot,policy_snapshot
) VALUES(
  '99000000-0000-4000-8000-000000000005','98000000-0000-4000-8000-000000000005',1,
  '95000000-0000-4000-8000-000000000001','96000000-0000-4000-8000-000000000001',1,5000,5000,5000,
  '{"name":"Runtime Shirt"}','{"sku":"RUNTIME-TEE-M","name":"M"}',
  '{"id":"93000000-0000-4000-8000-000000000001","version":1}'
);
INSERT INTO commerce_checkout_session(
  id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,total_minor,customer_email,
  lookup_token_hash,idempotency_key,expires_at
) VALUES(
  '9a000000-0000-4000-8000-000000000004','merch_order','98000000-0000-4000-8000-000000000004',
  'holding','sandbox','USD',10000,10000,'runtime.buyer@example.test',
  encode(digest('runtime-checkout-token','sha256'),'hex'),'runtime-checkout-key-004',now()+interval '20 minutes'
);
UPDATE merch_order SET checkout_id='9a000000-0000-4000-8000-000000000004'
WHERE id='98000000-0000-4000-8000-000000000004';
INSERT INTO commerce_checkout_line_item(
  checkout_id,line_number,product_type,product_id,product_version,description,quantity,unit_amount_minor,
  subtotal_minor,total_minor,snapshot
) VALUES(
  '9a000000-0000-4000-8000-000000000004',1,'merch_variant','96000000-0000-4000-8000-000000000001','1',
  'Runtime Shirt — M',2,5000,10000,10000,
  '{"storeId":"92000000-0000-4000-8000-000000000001","sku":"RUNTIME-TEE-M"}'
);
SELECT merch_reserve_stock(
  '98000000-0000-4000-8000-000000000004','9a000000-0000-4000-8000-000000000004',
  '[{"variantId":"96000000-0000-4000-8000-000000000001","quantity":2}]',
  (SELECT expires_at FROM commerce_checkout_session WHERE id='9a000000-0000-4000-8000-000000000004')
);

INSERT INTO commerce_checkout_session(
  id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,fee_minor,total_minor,
  paid_minor,customer_email,lookup_token_hash,idempotency_key,expires_at,paid_at
) VALUES(
  '9a000000-0000-4000-8000-000000000005','merch_order','98000000-0000-4000-8000-000000000005',
  'paid','sandbox','USD',5000,500,5500,5500,'settlement.buyer@example.test',
  encode(digest('runtime-settlement-checkout-token','sha256'),'hex'),
  'runtime-settlement-checkout-005',now()+interval '20 minutes',now()
);
UPDATE merch_order SET checkout_id='9a000000-0000-4000-8000-000000000005'
WHERE id='98000000-0000-4000-8000-000000000005';
INSERT INTO commerce_checkout_line_item(
  id,checkout_id,line_number,product_type,product_id,product_version,description,quantity,
  unit_amount_minor,subtotal_minor,total_minor,snapshot
) VALUES
  ('9b000000-0000-4000-8000-000000000005','9a000000-0000-4000-8000-000000000005',1,
   'merch_variant','96000000-0000-4000-8000-000000000001','1','Runtime Shirt — M',1,5000,5000,5000,
   '{"storeId":"92000000-0000-4000-8000-000000000001","sku":"RUNTIME-TEE-M"}'),
  ('9b000000-0000-4000-8000-000000000006','9a000000-0000-4000-8000-000000000005',2,
   'merch_shipping','93000000-0000-4000-8000-000000000002','1','Synthetic shipping',1,500,500,500,
   '{"storeId":"92000000-0000-4000-8000-000000000001","deliveryMethod":"national_shipping"}');
INSERT INTO commerce_payment_attempt(
  id,checkout_id,provider,environment,operation,status,amount_minor,currency,merchant_account_ref,
  idempotency_key,created_at,updated_at
) VALUES(
  '9c000000-0000-4000-8000-000000000005','9a000000-0000-4000-8000-000000000005',
  'paypal','sandbox','capture','succeeded',5500,'USD','runtime-merch-merchant',
  'runtime-settlement-payment-005',now(),now()
);

INSERT INTO merch_order_issue(
  id,order_id,opened_by_type,issue_type,public_message,idempotency_key,request_sha256
) VALUES
  ('94000000-0000-4000-8000-000000000001','98000000-0000-4000-8000-000000000004','buyer','shipping',
   'Synthetic operational issue for seller triage verification.','runtime-issue-shipping-001',encode(digest('runtime-issue-shipping-request-001','sha256'),'hex')),
  ('94000000-0000-4000-8000-000000000002','98000000-0000-4000-8000-000000000004','buyer','refund',
   'Synthetic financial issue that must remain under staff control.','runtime-issue-refund-001',encode(digest('runtime-issue-refund-request-001','sha256'),'hex'));

-- These flags are enabled only inside the disposable sandbox database used by
-- this test. The repository defaults and every staging/production row remain
-- disabled. Manual checkout creates a pending order but never records funds.
UPDATE revenue_feature_flag
SET enabled=TRUE,
    reason='Synthetic isolated HTTP runtime verification; no provider or funds involved',
    updated_at=now()
WHERE environment='sandbox'
  AND flag_key IN (
    'merch.storefronts',
    'merch.seller_applications',
    'merch.public_catalog',
    'merch.checkout',
    'merch.checkout.runtime_ready',
    'merch.checkout.manual',
    'merch.refunds',
    'merch.disputes'
  );
