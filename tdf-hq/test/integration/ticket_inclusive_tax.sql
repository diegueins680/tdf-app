BEGIN;
INSERT INTO social_event(id,title,start_time,end_time) VALUES(880,'Inclusive tax fixture',now()+interval '1 day',now()+interval '2 days');
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active)
VALUES(880,880,'INCLUSIVE','Inclusive',2000,'USD',100,0,true);
INSERT INTO event_ticket_checkout_policy(id,event_id,policy_version,currency,buyer_fee_bps,organizer_fee_bps,tax_bps,tax_included,terms_version,terms_summary,refund_policy,approval_status,active,approved_at,approved_by,max_tickets_per_order)
VALUES('88000000-0000-4000-8000-000000000001',880,'inclusive-v1','USD',0,0,1500,true,'v1','Inclusive test','Refund test','approved',true,now(),'fixture',4);

CREATE FUNCTION pg_temp.tax_order(oid bigint,q integer,tax bigint,mode boolean) RETURNS void LANGUAGE plpgsql AS $$
DECLARE cid uuid:=gen_random_uuid(); total bigint:=2000*q; h text:=encode(digest(oid::text,'sha256'),'hex');
BEGIN
 INSERT INTO event_ticket_order(id,event_id,tier_id,buyer_name,buyer_email,quantity,amount_cents,currency,status,original_amount_cents,payment_method)
 VALUES(oid,880,880,'Fixture','fixture@example.invalid',q,total,'USD','pending',total,'paypal');
 INSERT INTO commerce_checkout_session(id,domain_type,domain_order_id,status,environment,currency,subtotal_minor,tax_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at)
 VALUES(cid,'event_ticket_order',oid::text,'awaiting_payment','sandbox','USD',total,0,total,'fixture@example.invalid',h,'tax-checkout-idempotency-'||oid,now()+interval '10 minutes');
 INSERT INTO event_ticket_checkout_runtime(order_id,event_id,tier_id,checkout_id,policy_id,policy_version,lookup_token_hash,create_idempotency_key,create_request_sha256,quantity,currency,unit_price_minor,gross_face_value_minor,discount_minor,net_face_value_minor,buyer_fee_bps,buyer_fee_minor,organizer_fee_bps,organizer_fee_minor,tax_bps,tax_minor,tax_included,checkout_total_minor,organizer_payable_minor,platform_fee_minor,terms_version,terms_accepted_at,hold_expires_at)
 VALUES(oid,880,880,cid,'88000000-0000-4000-8000-000000000001','inclusive-v1',h,'tax-runtime-idempotency-'||oid,h,q,'USD',2000,total,0,total,0,0,0,0,1500,tax,mode,total,total-tax,0,'v1',now(),now()+interval '10 minutes');
END $$;
SELECT pg_temp.tax_order(881,1,261,true);
SELECT pg_temp.tax_order(882,2,522,true);
SELECT pg_temp.tax_order(883,3,783,true);
SELECT pg_temp.tax_order(884,4,1043,true);
DO $$ BEGIN
 IF (SELECT array_agg(checkout_total_minor ORDER BY order_id) FROM event_ticket_checkout_runtime WHERE event_id=880) <> ARRAY[2000,4000,6000,8000]::bigint[] THEN RAISE EXCEPTION 'Incorrect advertised totals'; END IF;
 IF EXISTS (SELECT 1 FROM event_ticket_checkout_runtime WHERE event_id=880 AND checkout_total_minor<>organizer_payable_minor+platform_fee_minor+tax_minor) THEN RAISE EXCEPTION 'Unbalanced tax and liability'; END IF;
 BEGIN
  PERFORM pg_temp.tax_order(885,4,1044,true);
  RAISE EXCEPTION 'Incorrect tax was accepted';
 EXCEPTION WHEN check_violation THEN
  IF SQLERRM NOT LIKE '%ticket_included_tax_arithmetic%' THEN RAISE; END IF;
 END;
 BEGIN
  PERFORM pg_temp.tax_order(886,1,0,false);
  RAISE EXCEPTION 'Old writer omitted purchased tax mode';
 EXCEPTION WHEN check_violation THEN
  IF SQLERRM NOT LIKE '%Ticket tax mode must match%' THEN RAISE; END IF;
 END;
 BEGIN
  UPDATE event_ticket_checkout_runtime SET tax_included=false WHERE order_id=881;
  RAISE EXCEPTION 'Purchased mode was mutable';
 EXCEPTION WHEN check_violation THEN
  IF SQLERRM NOT LIKE '%Purchased ticket tax mode is immutable%' THEN RAISE; END IF;
 END;
 BEGIN
  UPDATE event_ticket_checkout_policy SET tax_included=false WHERE event_id=880;
  RAISE EXCEPTION 'Approved policy tax mode was mutable';
 EXCEPTION WHEN raise_exception THEN
  IF SQLERRM NOT LIKE '%Published ticket policy is immutable%' THEN RAISE; END IF;
 END;
END $$;
ROLLBACK;
