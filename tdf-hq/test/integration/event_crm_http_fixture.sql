-- Synthetic pilot only. Never loaded by production migrations.
BEGIN;
UPDATE social_event SET title='Control financiero · ensayo privado' WHERE id=7001;
INSERT INTO party(id,display_name,is_org,primary_email,created_at) VALUES
  (7105,'Ventas de ensayo',false,'seller@example.invalid',now()),
  (7106,'Comprador de ensayo',false,'buyer@example.invalid',now()),
  (7107,'Verificador de ensayo',false,'verifier@example.invalid',now()),
  (7108,'Equipo móvil de ensayo',false,'mobile-team@example.invalid',now());
INSERT INTO user_credential(party_id,username,password_hash,active)
  SELECT id,'crm-synthetic-'||id,'not-a-real-password',true FROM party WHERE id BETWEEN 7101 AND 7108;
INSERT INTO api_token(party_id,token,active,label,issued_at_epoch,expires_at_epoch)
  SELECT id,encode(digest('event-crm-synthetic-token-'||id,'sha256'),'hex'),true,
    'password-login:crm-synthetic-'||id,floor(extract(epoch FROM now()))::bigint,floor(extract(epoch FROM now()))::bigint+3600
  FROM party WHERE id BETWEEN 7101 AND 7108 OR display_name='Catalog recovery fixture A';
INSERT INTO venue(id,name,city,country,timezone,created_at,updated_at)
  VALUES(7002,'Sede sintética de ensayo','Quito','Ecuador','America/Guayaquil',now(),now());
INSERT INTO social_event(id,organizer_party_id,title,venue_id,start_time,end_time,timezone,event_type_id,workflow_state_id,capacity)
  SELECT 7002,'7101','Evento sintético · borrador privado',7002,'2026-12-05T16:00:00Z','2026-12-06T03:00:00Z','America/Guayaquil',e.event_type_id,
    (SELECT s.id FROM workflow_state s JOIN workflow_definition w ON w.id=s.workflow_id WHERE w.code='social-event-lifecycle' AND s.code='planning' AND s.active LIMIT 1),1
  FROM social_event e WHERE id=7001;
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,sales_end)
  VALUES(7002,7002,'preventa','Preventa · ensayo',2000,'USD',1,0,false,'2026-12-02T04:59:59.999999Z');
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active,sales_start,sales_end)
  VALUES(7003,7002,'event-day','Día del evento · ensayo',3000,'USD',1,0,false,'2026-12-05T05:00:00Z','2026-12-06T04:59:59.999999Z');
INSERT INTO event_crm_control(event_id,enabled,transport_required,updated_by) VALUES(7001,true,false,7101),(7002,true,true,7101);
INSERT INTO event_crm_transport_slot(event_id,direction,departure_time) VALUES
  (7002,'outbound','11:00'),(7002,'outbound','12:00'),(7002,'return','17:00'),(7002,'return','18:00'),(7002,'return','22:00');
UPDATE event_crm_transport_slot SET departure_place=CASE WHEN direction='outbound' THEN 'Punto de partida de ensayo' ELSE 'Sede sintética de ensayo' END WHERE event_id=7002;

-- Model a transfer submitted while its reservation was live and reviewed after
-- expiry. Seed the historical timestamps on insert; the runtime keeps them immutable.
DO $$
DECLARE
  late_order bigint;
  late_checkout uuid;
  late_attempt uuid;
  hold_start timestamptz := now() - interval '20 minutes';
  hold_end timestamptz := now() - interval '10 minutes';
  buyer_email text := 'late-payment@example.invalid';
BEGIN
  INSERT INTO event_ticket_order(event_id,tier_id,buyer_name,buyer_email,quantity,amount_cents,
    currency,status,original_amount_cents,payment_method,purchased_at)
    VALUES(7001,7001,'Pago tardío sintético',buyer_email,1,2000,'USD','cancelled',2000,
      'bank_transfer',hold_start) RETURNING id INTO late_order;
  INSERT INTO commerce_checkout_session(domain_type,domain_order_id,status,environment,currency,
    subtotal_minor,tax_minor,total_minor,customer_email,lookup_token_hash,idempotency_key,expires_at,created_at)
    VALUES('event_ticket_order',late_order::text,'expired','sandbox','USD',2000,0,2000,buyer_email,
      encode(digest('crm-late-checkout','sha256'),'hex'),'crm-late-checkout',hold_end,hold_start)
    RETURNING id INTO late_checkout;
  INSERT INTO commerce_checkout_line_item(checkout_id,line_number,product_type,product_id,
    product_version,description,quantity,unit_amount_minor,subtotal_minor,total_minor,snapshot)
    VALUES(late_checkout,1,'event_ticket','7001','manual-review-v1','General',1,2000,2000,2000,'{}');
  INSERT INTO event_ticket_checkout_runtime(order_id,event_id,tier_id,checkout_id,policy_id,
    policy_version,lookup_token_hash,create_idempotency_key,create_request_sha256,fulfillment_status,
    quantity,currency,unit_price_minor,gross_face_value_minor,net_face_value_minor,buyer_fee_bps,
    buyer_fee_minor,organizer_fee_bps,organizer_fee_minor,tax_bps,tax_minor,checkout_total_minor,
    organizer_payable_minor,platform_fee_minor,terms_version,terms_accepted_at,hold_expires_at,created_at)
    SELECT late_order,7001,7001,late_checkout,id,policy_version,
      encode(digest('crm-late-runtime','sha256'),'hex'),'crm-late-runtime',
      encode(digest('crm-late-request','sha256'),'hex'),'expired',1,'USD',2000,2000,2000,0,
      0,0,0,0,0,2000,2000,0,terms_version,hold_start,hold_end,hold_start
    FROM event_ticket_checkout_policy WHERE event_id=7001 AND active;
  INSERT INTO commerce_payment_attempt(checkout_id,provider,environment,operation,status,amount_minor,
    currency,merchant_account_ref,idempotency_key)
    VALUES(late_checkout,'bank_transfer','sandbox','manual_verify','requires_review',2000,'USD',
      'tdf-manual-settlement','crm-late-attempt') RETURNING id INTO late_attempt;
  INSERT INTO commerce_manual_payment_evidence(checkout_id,payment_attempt_id,status)
    VALUES(late_checkout,late_attempt,'awaiting_evidence');
  UPDATE commerce_manual_payment_evidence SET status='submitted',customer_reference='TEST-LATE',
    currency='USD',submitted_amount_minor=2000,submitted_by=7104,
    submitted_at=hold_start+interval '1 minute',sale_occurred_at=hold_start+interval '1 minute'
    WHERE payment_attempt_id=late_attempt;
END $$;

INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason) VALUES
  ('commerce.event_tickets',true,'sandbox','Disposable local HTTP test only'),
  ('checkout.bank_transfer',true,'sandbox','Disposable local HTTP test only')
  ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true;
COMMIT;
