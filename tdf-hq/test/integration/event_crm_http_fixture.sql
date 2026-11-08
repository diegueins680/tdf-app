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
INSERT INTO revenue_feature_flag(flag_key,enabled,environment,reason) VALUES
  ('commerce.event_tickets',true,'sandbox','Disposable local HTTP test only'),
  ('checkout.bank_transfer',true,'sandbox','Disposable local HTTP test only')
  ON CONFLICT(flag_key,environment) DO UPDATE SET enabled=true;
COMMIT;
