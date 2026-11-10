CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE TABLE party(id bigserial PRIMARY KEY,display_name text NOT NULL,legal_name text,is_org boolean NOT NULL DEFAULT false,
  tax_id text,primary_email text,primary_phone text,whatsapp text,instagram text,emergency_contact text,notes text,
  stripe_customer_id text,country_code text,country_id uuid,created_at timestamptz NOT NULL DEFAULT now());
CREATE TABLE user_credential(id bigserial PRIMARY KEY,party_id bigint REFERENCES party(id),username text UNIQUE,password_hash text,active boolean NOT NULL DEFAULT true);
CREATE TABLE venue(id bigserial PRIMARY KEY,name text NOT NULL);
CREATE TABLE social_event(id bigserial PRIMARY KEY,organizer_party_id text,title text NOT NULL,venue_id bigint REFERENCES venue(id),
  start_time timestamptz NOT NULL,end_time timestamptz,timezone text,capacity integer);
CREATE TABLE promo_code(id bigserial PRIMARY KEY);
CREATE TABLE event_ticket_tier(id bigserial PRIMARY KEY,event_id bigint REFERENCES social_event(id),code text,name text,
  price_cents integer,currency text,quantity_total integer,quantity_sold integer NOT NULL DEFAULT 0,is_active boolean DEFAULT true,
  sales_start timestamptz,sales_end timestamptz,allow_transfers boolean DEFAULT true,
  refund_policy text DEFAULT 'full',refund_deadline timestamptz,updated_at timestamptz DEFAULT now());
CREATE TABLE event_ticket_order(id bigserial PRIMARY KEY,event_id bigint REFERENCES social_event(id),tier_id bigint REFERENCES event_ticket_tier(id),
  buyer_party_id text,buyer_name text,buyer_email text,quantity integer,amount_cents integer,currency text,status text,
  metadata text,checkout_idempotency_key text,purchased_at timestamptz DEFAULT now(),stripe_payment_intent_id text,
  promo_code_id bigint REFERENCES promo_code(id),original_amount_cents integer,payment_method text,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now());
CREATE TABLE event_ticket(id bigserial PRIMARY KEY,event_id bigint,tier_ref_id bigint,order_ref_id bigint,status text);
CREATE TABLE event_waitlist(id bigserial PRIMARY KEY,event_id bigint REFERENCES social_event(id),tier_id bigint,party_id text,email text NOT NULL,name text,
  quantity integer DEFAULT 1,status text DEFAULT 'active',priority integer DEFAULT 0,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now());
CREATE TABLE event_logistics_activity(id bigserial PRIMARY KEY,event_id bigint REFERENCES social_event(id),title text,
  notes text,activity_type text,status text,start_time timestamptz,end_time timestamptz,priority text,version integer DEFAULT 1,
  created_by_party_id text,created_at timestamptz DEFAULT now(),updated_at timestamptz DEFAULT now());
CREATE TABLE event_logistics_assignment(id bigserial PRIMARY KEY,activity_id bigint REFERENCES event_logistics_activity(id),party_id text,
  created_at timestamptz DEFAULT now());
INSERT INTO party(id,display_name,primary_email,primary_phone) VALUES
  (1,'Pilot owner','owner@example.test',NULL),(2,'Pilot coordinator','coordinator@example.test',NULL),
  (3,'Other account','other@example.test',NULL),(4,'Seller','seller@example.test',NULL),
  (5,'Customer A','customer@example.test','0991234567'),(6,'Customer B','conflict@example.test','+593991234567'),
  (7,'Verifier','verifier@example.test',NULL);
SELECT setval('party_id_seq',100);
INSERT INTO user_credential(party_id,username,password_hash) SELECT id,'pilot-'||id,'not-a-password' FROM party;
INSERT INTO venue(id,name) VALUES(1,'Sede sintética de ensayo');
INSERT INTO social_event(id,organizer_party_id,title,venue_id,start_time,timezone,capacity) VALUES
  (10,'1','Evento sintético · borrador privado',1,'2026-12-05T16:00:00Z','America/Guayaquil',30),
  (11,'3','Unrelated private event',1,'2026-12-05T16:00:00Z','America/Guayaquil',30);
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total) VALUES
  (1,10,'preventa','Preventa',2000,'USD',30),(2,11,'general','General',2000,'USD',30);
INSERT INTO event_logistics_activity(id,event_id,title,status,start_time) VALUES(1,10,'Coordinar equipo','planned','2026-12-05T15:00:00Z');
