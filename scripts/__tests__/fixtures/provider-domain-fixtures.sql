-- Test-only builders for the FULL schema. All policies, commercial identities,
-- line totals and runtime snapshots pass the production constraints unchanged.
\set ON_ERROR_STOP on
CREATE SCHEMA IF NOT EXISTS payment_test;
CREATE SEQUENCE IF NOT EXISTS payment_test.domo_slot;
WITH catalog AS (
  INSERT INTO service_catalog(name,kind,pricing_model,active) VALUES ('Synthetic provider service','Recording','Hourly',true) RETURNING id
)
UPDATE service_offering SET legacy_service_catalog_id=catalog.id
  FROM catalog WHERE code='recording';
CREATE OR REPLACE FUNCTION payment_test.ticket_order(quantity integer DEFAULT 1)
RETURNS bigint LANGUAGE plpgsql AS $$
DECLARE event_key bigint; tier_key bigint; order_key bigint;
BEGIN
  INSERT INTO social_event(organizer_party_id,title,event_type_id,workflow_state_id,start_time,end_time)
    VALUES ('1','Synthetic provider checkout',(SELECT id FROM event_type WHERE code='concert'),(SELECT s.id FROM workflow_state s JOIN workflow_definition d ON d.id=s.workflow_id WHERE d.code='social-event-lifecycle' AND s.code='on_sale'),now()+interval '1 day',now()+interval '2 days')
    RETURNING id INTO event_key;
  INSERT INTO event_ticket_tier(event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active)
    VALUES (event_key,'test','Synthetic ticket',CASE WHEN quantity=3 THEN 4172 ELSE 12515 END,'USD',20,quantity,true)
    RETURNING id INTO tier_key;
  INSERT INTO event_ticket_order(event_id,tier_id,buyer_party_id,buyer_name,buyer_email,quantity,amount_cents,currency,status,purchased_at)
    VALUES (event_key,tier_key,'1','Synthetic buyer','synthetic@example.invalid',quantity,12515,'USD','pending',now())
    RETURNING id INTO order_key;
  RETURN order_key;
END $$;

CREATE OR REPLACE FUNCTION payment_test.ticket_runtime(checkout_key uuid, order_key bigint,
  fee bigint DEFAULT 0, tax bigint DEFAULT 0)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE ticket_order event_ticket_order%ROWTYPE; policy_key uuid; unit_price bigint;
BEGIN
  SELECT * INTO STRICT ticket_order FROM event_ticket_order WHERE id=order_key;
  SELECT price_cents INTO STRICT unit_price FROM event_ticket_tier WHERE id=ticket_order.tier_id;
  INSERT INTO event_ticket_checkout_policy(event_id,policy_version,currency,
    buyer_fee_bps,organizer_fee_bps,tax_bps,terms_version,terms_summary,refund_policy,
    approval_status,active,approved_at,approved_by,transfer_deadline)
    VALUES (ticket_order.event_id,'synthetic-v1','USD',0,round(fee*10000.0/(12515-tax)),round(tax*10000.0/(12515-tax)),'synthetic-terms-v1',
      'Synthetic accepted terms','Synthetic refund policy','approved',true,now(),'test',now()+interval '1 hour')
    RETURNING id INTO policy_key;
  INSERT INTO event_ticket_checkout_runtime(order_id,event_id,tier_id,checkout_id,policy_id,policy_version,
    lookup_token_hash,create_idempotency_key,create_request_sha256,quantity,currency,unit_price_minor,
    gross_face_value_minor,discount_minor,net_face_value_minor,buyer_fee_bps,buyer_fee_minor,
    organizer_fee_bps,organizer_fee_minor,tax_bps,tax_minor,checkout_total_minor,
    organizer_payable_minor,platform_fee_minor,terms_version,terms_accepted_at,hold_expires_at)
  VALUES (order_key,ticket_order.event_id,ticket_order.tier_id,checkout_key,policy_key,'synthetic-v1',
    encode(digest(checkout_key::text,'sha256'),'hex'),checkout_key::text,repeat('a',64),ticket_order.quantity,
    'USD',unit_price,unit_price*ticket_order.quantity,unit_price*ticket_order.quantity-(12515-tax),
    12515-tax,0,0,round(fee*10000.0/(12515-tax)),fee,round(tax*10000.0/(12515-tax)),tax,12515,12515-fee-tax,fee,'synthetic-terms-v1',now(),now()+interval '30 minutes');
END $$;

CREATE OR REPLACE FUNCTION payment_test.checkout(kind text, fee bigint DEFAULT 0, tax bigint DEFAULT 0) RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE checkout_key uuid := gen_random_uuid(); subject_key bigint; course_key bigint;
  policy_key uuid; offering service_offering%ROWTYPE; service_key bigint;
  quote_key uuid := gen_random_uuid(); domo_key uuid := gen_random_uuid();
  due bigint := 12515; expires timestamptz := now()+interval '30 minutes';
  domo_start timestamptz := now()+(30+nextval('payment_test.domo_slot'))*interval '1 day';
BEGIN
  IF kind='event_ticket_order' THEN
    subject_key := payment_test.ticket_order();
  ELSIF kind='course_registration' THEN
    INSERT INTO course(slug,title,price_cents,currency,capacity)
      VALUES (checkout_key::text,'Synthetic provider course',12515,'USD',10) RETURNING id INTO course_key;
    INSERT INTO course_registration(course_slug,full_name,email,source,status)
      VALUES (checkout_key::text,'Synthetic buyer','synthetic@example.invalid','landing','pending_payment')
      RETURNING id INTO subject_key;
    INSERT INTO course_checkout_policy(course_id,policy_version,currency,price_minor,terms_version,
      terms_summary,cancellation_policy,approval_status,active,approved_at,approved_by)
      VALUES (course_key,'synthetic-v1','USD',12515,'synthetic-terms','Synthetic terms','Synthetic cancellation',
        'approved',true,now(),'test') RETURNING id INTO policy_key;
  ELSIF kind='service_booking' THEN
    SELECT * INTO STRICT offering FROM service_offering WHERE active AND deprecated_at IS NULL
      AND legacy_service_catalog_id IS NOT NULL ORDER BY id LIMIT 1;
    UPDATE service_booking_commerce_policy SET active=false WHERE service_offering_id=offering.id AND active;
    INSERT INTO service_booking_commerce_policy(service_offering_id,policy_version,currency,rate_minor,
      rate_unit_minutes,tax_bps,deposit_bps,min_duration_minutes,max_duration_minutes,duration_step_minutes,
      terms_version,terms_summary,approval_status,active,approved_at,approved_by)
      VALUES (offering.id,checkout_key::text,'USD',12515,60,0,10000,60,60,60,
        'synthetic-terms','Synthetic accepted terms','approved',true,now(),'test') RETURNING id INTO policy_key;
    INSERT INTO service_order(customer_id,catalog_id,service_offering_id,service_kind,title,status,
      price_quoted_cents,scheduled_start,scheduled_end,created_at)
      VALUES (1,offering.legacy_service_catalog_id,offering.id,'Recording','Synthetic service','deposit_due',
        12515,now()+interval '1 day',now()+interval '1 day 1 hour',now()) RETURNING id INTO service_key;
    INSERT INTO booking(title,service_order_id,party_id,service_offering_id,starts_at,ends_at,status,created_at)
      VALUES ('Synthetic service',service_key,1,offering.id,now()+interval '1 day',now()+interval '1 day 1 hour',
        'Tentative',now()) RETURNING id INTO subject_key;
  ELSIF kind='domo_event_quote' THEN
    due := 200704;
    UPDATE commerce_rate_card_review SET status='approved',reviewed_by=2,reviewed_at=now()
      WHERE product_version_id='81000000-0000-0000-0000-000000000001';
    UPDATE commerce_product_version SET status='active',approved_by=3,approved_at=now()
      WHERE id='81000000-0000-0000-0000-000000000001' AND status<>'active';
    INSERT INTO commerce_quote(id,domain_type,domain_subject_id,version,status,currency,
      subtotal_minor,tax_minor,total_minor,deposit_minor,expires_at)
      VALUES (quote_key,kind,domo_key::text,1,'sent','USD',448000,53760,501760,due,expires);
    INSERT INTO commerce_quote_line(quote_id,line_number,product_type,product_id,product_version,
      description,quantity,unit_amount_minor,subtotal_minor)
      VALUES (quote_key,1,'domo_rate_component','all','legacy_public_formula-v1','Synthetic quote',1,448000,448000);
  ELSE RAISE EXCEPTION 'Unknown fixture domain %',kind;
  END IF;
  INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,quote_id,status,environment,currency,
    subtotal_minor,discount_minor,tax_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)
    VALUES (checkout_key,kind,CASE WHEN kind='domo_event_quote' THEN 'domo-quote:'||domo_key::text
      ELSE subject_key::text END,CASE WHEN kind='domo_event_quote' THEN quote_key ELSE NULL END,
      CASE WHEN kind='domo_event_quote' THEN 'holding' ELSE 'awaiting_payment' END,'sandbox','USD',due,tax,tax,due,
      'synthetic@example.invalid',encode(digest(checkout_key::text,'sha256'),'hex'),checkout_key::text,expires);
  INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,product_version,
    description,quantity,unit_amount_minor,subtotal_minor,discount_minor,tax_minor,total_minor,snapshot)
    VALUES (checkout_key,1,kind,checkout_key::text,'synthetic-v1','Synthetic payment',1,due,due,tax,tax,due,'{}');
  IF kind='event_ticket_order' THEN
    PERFORM payment_test.ticket_runtime(checkout_key,subject_key,fee,tax);
  ELSIF kind='course_registration' THEN
    INSERT INTO course_registration_checkout_runtime(registration_id,course_id,checkout_id,policy_id,policy_version,
      lookup_token_hash,create_idempotency_key,create_request_sha256,payment_schedule,currency,price_minor,tax_bps,
      tax_minor,total_minor,due_now_minor,balance_minor,terms_version,terms_accepted_at,hold_expires_at)
      VALUES (subject_key,course_key,checkout_key,policy_key,'synthetic-v1',encode(digest(checkout_key::text,'sha256'),'hex'),
        checkout_key::text,repeat('a',64),'full','USD',due,0,0,due,due,0,'synthetic-terms',now(),expires);
  ELSIF kind='service_booking' THEN
    INSERT INTO service_booking_checkout_runtime(booking_id,service_order_id,checkout_id,service_offering_id,
      policy_id,policy_version,lookup_token_hash,create_idempotency_key,create_request_sha256,fulfillment_status,
      deposit_status,balance_status,starts_at,ends_at,timezone,duration_minutes,currency,rate_minor,rate_unit_minutes,
      tax_bps,deposit_bps,subtotal_minor,tax_minor,total_minor,deposit_minor,balance_minor,terms_version,
      terms_accepted_at,hold_expires_at)
      VALUES (subject_key,service_key,checkout_key,offering.id,policy_key,checkout_key::text,
        encode(digest(checkout_key::text,'sha256'),'hex'),checkout_key::text,repeat('a',64),'on_hold','awaiting_payment',
        'not_due',now()+interval '1 day',now()+interval '1 day 1 hour','America/Guayaquil',60,'USD',due,60,
        0,10000,due,0,due,due,0,'synthetic-terms',now(),expires);
  ELSE
    INSERT INTO domo_event_quote_runtime(id,quote_id,checkout_id,product_version_id,venue_key,lookup_token_hash,
      create_idempotency_key,create_request_sha256,customer_name,customer_email,event_type,guests,starts_at,ends_at,
      setup_starts_at,duration_hours,setup_hours,catering,production,transport,quote_status,fulfillment_status,currency,
      subtotal_minor,tax_minor,total_minor,deposit_minor,balance_minor,tax_basis_points,deposit_basis_points,
      rate_card_version,rate_card_rules_sha256,timezone,terms_version,hold_expires_at)
    SELECT domo_key,quote_key,checkout_key,id,'domo-del-pululahua',encode(digest(checkout_key::text,'sha256'),'hex'),
      checkout_key::text,repeat('a',64),'Synthetic customer','synthetic@example.invalid','wedding',80,
      domo_start,domo_start+interval '8 hours',domo_start-interval '2 hours',8,2,true,true,false,
      'sent','date_held','USD',448000,53760,501760,due,301056,1200,4000,'legacy_public_formula-v1',
      encode(digest(convert_to(pricing_rules::text,'UTF8'),'sha256'),'hex'),'America/Guayaquil',
      'domo-terms-legacy-draft-v1',expires FROM commerce_product_version
      WHERE id='81000000-0000-0000-0000-000000000001';
    UPDATE domo_event_quote_runtime SET quote_status='accepted',terms_accepted_at=now() WHERE id=domo_key;
    UPDATE commerce_quote SET status='accepted',accepted_at=now(),accepted_terms_version='domo-terms-legacy-draft-v1'
      WHERE id=quote_key;
    UPDATE domo_event_quote_runtime SET quote_status='deposit_due' WHERE id=domo_key;
    UPDATE commerce_checkout_session SET status='awaiting_payment' WHERE id=checkout_key;
  END IF;
  RETURN checkout_key;
END $$;
