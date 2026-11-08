BEGIN;

-- Additive, private, opt-in pilot. No event, account, ticket, sale or grant is seeded.
CREATE TABLE IF NOT EXISTS event_crm_control (
  event_id bigint PRIMARY KEY REFERENCES social_event(id),
  enabled boolean NOT NULL DEFAULT false,
  transport_required boolean NOT NULL DEFAULT false,
  -- Deliberately no activation command: joint checkout has not been integrated.
  combination_sales_enabled boolean NOT NULL DEFAULT false CHECK (NOT combination_sales_enabled),
  authorization_version bigint NOT NULL DEFAULT 0,
  updated_by bigint NOT NULL REFERENCES party(id),
  updated_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE TABLE IF NOT EXISTS event_crm_membership (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id bigint NOT NULL REFERENCES social_event(id),
  party_id bigint NOT NULL REFERENCES party(id),
  role text NOT NULL CHECK (role IN ('coordinator','sales','payment_verifier')),
  granted_by bigint NOT NULL REFERENCES party(id),
  granted_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  revoked_at timestamptz,
  revoked_by bigint REFERENCES party(id),
  CHECK ((revoked_at IS NULL) = (revoked_by IS NULL))
);
CREATE UNIQUE INDEX IF NOT EXISTS event_crm_active_membership
  ON event_crm_membership(event_id,party_id) WHERE revoked_at IS NULL;
CREATE TABLE IF NOT EXISTS event_crm_invitation (
  id uuid PRIMARY KEY,
  event_id bigint NOT NULL REFERENCES social_event(id),
  issued_by bigint NOT NULL REFERENCES party(id),
  recipient_party_id bigint REFERENCES party(id),
  recipient_email_hash text,
  token_hash text NOT NULL UNIQUE CHECK (token_hash ~ '^[0-9a-f]{64}$'),
  role text NOT NULL CHECK (role IN ('coordinator','sales','payment_verifier')),
  issued_at timestamptz NOT NULL,
  expires_at timestamptz NOT NULL,
  accepted_at timestamptz,
  accepted_by bigint REFERENCES party(id),
  membership_id uuid REFERENCES event_crm_membership(id),
  revoked_at timestamptz,
  revoked_by bigint REFERENCES party(id),
  CHECK (num_nonnulls(recipient_party_id,recipient_email_hash)=1),
  CHECK (recipient_email_hash IS NULL OR recipient_email_hash ~ '^[0-9a-f]{64}$'),
  CHECK (expires_at=issued_at+interval '7 days'),
  CHECK ((accepted_at IS NULL)=(accepted_by IS NULL)),
  CHECK ((accepted_at IS NULL)=(membership_id IS NULL)),
  CHECK ((revoked_at IS NULL)=(revoked_by IS NULL)),
  CHECK (accepted_at IS NULL OR revoked_at IS NULL)
);
CREATE TABLE IF NOT EXISTS event_crm_email_challenge (
  token_hash text PRIMARY KEY CHECK (token_hash ~ '^[0-9a-f]{64}$'),
  party_id bigint NOT NULL REFERENCES party(id),
  email_hash text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  expires_at timestamptz NOT NULL DEFAULT clock_timestamp()+interval '30 minutes',
  consumed_at timestamptz
);
CREATE TABLE IF NOT EXISTS event_crm_verified_email (
  party_id bigint NOT NULL REFERENCES party(id),
  email_hash text NOT NULL,
  verified_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY(party_id,email_hash)
);
CREATE TABLE IF NOT EXISTS event_crm_lead (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  event_id bigint NOT NULL REFERENCES social_event(id),
  contact_party_id bigint NOT NULL REFERENCES party(id),
  responsible_party_id bigint REFERENCES party(id),
  source text NOT NULL CHECK (length(source) BETWEEN 1 AND 160),
  contacted_at timestamptz,
  notes text NOT NULL DEFAULT '' CHECK (length(notes)<=4000),
  next_action text NOT NULL DEFAULT '' CHECK (length(next_action)<=500),
  follow_up_at timestamptz,
  stage text NOT NULL DEFAULT 'interested'
    CHECK (stage IN ('interested','contacted','reserved','paid','closed_lost')),
  requested_quantity integer NOT NULL DEFAULT 1 CHECK (requested_quantity BETWEEN 1 AND 100),
  order_id bigint UNIQUE REFERENCES event_ticket_order(id),
  reservation_expires_at timestamptz,
  outbound_time text CHECK (outbound_time ~ '^([01][0-9]|2[0-3]):[0-5][0-9]$'),
  return_time text CHECK (return_time ~ '^([01][0-9]|2[0-3]):[0-5][0-9]$'),
  created_by bigint NOT NULL REFERENCES party(id),
  updated_by bigint NOT NULL REFERENCES party(id),
  version bigint NOT NULL DEFAULT 1,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  updated_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  UNIQUE(event_id,contact_party_id),
  CHECK (stage<>'reserved' OR (order_id IS NOT NULL AND reservation_expires_at IS NOT NULL)),
  CHECK (stage<>'paid' OR order_id IS NOT NULL),
  CHECK ((outbound_time IS NULL)=(return_time IS NULL))
);
CREATE TABLE IF NOT EXISTS event_crm_audit (
  id bigserial PRIMARY KEY,
  event_id bigint NOT NULL REFERENCES social_event(id),
  actor_id bigint REFERENCES party(id),
  actor_type text NOT NULL DEFAULT 'user' CHECK (actor_type IN ('user','canonical_order')),
  operation text NOT NULL,
  subject_id text NOT NULL,
  from_stage text,
  to_stage text,
  occurred_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  CHECK ((actor_type='user')=(actor_id IS NOT NULL))
);
ALTER TABLE commerce_manual_payment_evidence ADD COLUMN IF NOT EXISTS sale_occurred_at timestamptz;
CREATE TABLE IF NOT EXISTS event_crm_import (
  id uuid PRIMARY KEY,
  event_id bigint NOT NULL REFERENCES social_event(id),
  actor_id bigint NOT NULL REFERENCES party(id),
  content_hash text NOT NULL,
  rows jsonb NOT NULL CHECK (jsonb_typeof(rows)='array' AND jsonb_array_length(rows)<=500),
  result jsonb,
  created_at timestamptz NOT NULL DEFAULT clock_timestamp(),
  confirmed_at timestamptz,
  UNIQUE(event_id,actor_id,content_hash)
);
CREATE TABLE IF NOT EXISTS event_crm_command_receipt (
  event_id bigint NOT NULL REFERENCES social_event(id),
  actor_id bigint NOT NULL REFERENCES party(id),
  command_id uuid NOT NULL,
  request_hash text NOT NULL,
  outcome jsonb NOT NULL,
  PRIMARY KEY(event_id,actor_id,command_id)
);
CREATE TABLE IF NOT EXISTS event_crm_transport_slot (
  event_id bigint NOT NULL REFERENCES social_event(id),
  direction text NOT NULL CHECK (direction IN ('outbound','return')),
  departure_time text NOT NULL CHECK (departure_time ~ '^([01][0-9]|2[0-3]):[0-5][0-9]$'),
  departure_place text NOT NULL DEFAULT '' CHECK (length(departure_place)<=160),
  capacity integer CHECK (capacity>0),
  confirmed_by bigint REFERENCES party(id),
  confirmed_at timestamptz,
  confirmation_reference text,
  PRIMARY KEY(event_id,direction,departure_time),
  CHECK ((confirmed_by IS NULL)=(confirmed_at IS NULL)),
  CHECK (confirmed_at IS NULL OR capacity IS NOT NULL)
);
CREATE TABLE IF NOT EXISTS event_crm_waitlist_link (
  lead_id uuid PRIMARY KEY REFERENCES event_crm_lead(id),
  waitlist_id bigint NOT NULL UNIQUE REFERENCES event_waitlist(id),
  outbound_time text NOT NULL,
  return_time text NOT NULL,
  created_by bigint NOT NULL REFERENCES party(id),
  created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);

CREATE OR REPLACE FUNCTION event_crm_email(raw text) RETURNS text
LANGUAGE sql IMMUTABLE AS $$ SELECT NULLIF(lower(btrim(raw)),'') $$;
CREATE OR REPLACE FUNCTION event_crm_timestamp(raw text) RETURNS timestamptz
LANGUAGE plpgsql IMMUTABLE STRICT AS $$
BEGIN
  IF raw !~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}(:[0-9]{2}([.][0-9]+)?)?(Z|[+-][0-9]{2}:[0-9]{2})$' THEN
    RAISE EXCEPTION 'Timestamp requires ISO format and timezone' USING ERRCODE='22023'; END IF;
  RETURN raw::timestamptz;
END $$;
CREATE OR REPLACE FUNCTION event_crm_phone(raw text) RETURNS text
LANGUAGE plpgsql IMMUTABLE AS $$
DECLARE n text;
BEGIN
  IF nullif(btrim(raw),'') IS NULL THEN RETURN NULL; END IF;
  IF raw !~ '^[+0-9 ().-]+$' OR length(raw)>32 THEN RETURN NULL; END IF;
  n:=regexp_replace(raw,'[^0-9]','','g');
  IF n ~ '^09[0-9]{8}$' THEN n:='593'||substr(n,2); END IF;
  IF n ~ '^00' THEN n:=substr(n,3); END IF;
  IF n !~ '^[1-9][0-9]{7,14}$' THEN RETURN NULL; END IF;
  RETURN '+'||n;
END $$;

-- All commands/read projections share the social_event row fence. A revocation
-- committed before a later operation is observed, including receipt replay/export.
CREATE OR REPLACE FUNCTION event_crm_role(e bigint,a bigint) RETURNS text
LANGUAGE plpgsql VOLATILE AS $$
DECLARE owner_id text; result text;
BEGIN
  SELECT organizer_party_id INTO owner_id FROM social_event WHERE id=e FOR UPDATE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  PERFORM authorization_version FROM event_crm_control WHERE event_id=e AND enabled FOR UPDATE;
  IF NOT FOUND THEN RETURN NULL; END IF;
  IF owner_id=a::text THEN RETURN 'owner'; END IF;
  SELECT role INTO result FROM event_crm_membership
    WHERE event_id=e AND party_id=a AND revoked_at IS NULL;
  RETURN result;
END $$;

CREATE OR REPLACE FUNCTION event_crm_candidates(email text,phone text) RETURNS bigint[]
LANGUAGE sql STABLE AS $$
  SELECT coalesce(array_agg(DISTINCT p.id ORDER BY p.id),'{}'::bigint[])
  FROM party p WHERE NOT EXISTS(SELECT 1 FROM identity_party_archive x WHERE x.party_id=p.id)
    AND ((email IS NOT NULL AND event_crm_email(p.primary_email)=email)
      OR (phone IS NOT NULL AND (event_crm_phone(p.primary_phone)=phone
        OR event_crm_phone(p.whatsapp)=phone)))
$$;

CREATE OR REPLACE FUNCTION event_crm_email_proof(a bigint,token_hash_input text,confirm boolean)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE email_fingerprint text; challenge event_crm_email_challenge%ROWTYPE;
BEGIN
  -- The account credential lock also fences concurrent requests for this account.
  PERFORM id FROM user_credential WHERE party_id=a AND active FOR UPDATE;
  IF NOT FOUND OR token_hash_input !~ '^[0-9a-f]{64}$' THEN RETURN '{"error":"invalid_request"}'; END IF;
  SELECT encode(digest(event_crm_email(primary_email),'sha256'),'hex') INTO email_fingerprint
    FROM party WHERE id=a FOR SHARE;
  IF email_fingerprint IS NULL THEN RETURN '{"error":"email_required"}'; END IF;
  IF NOT confirm THEN
    IF (SELECT count(*) FROM event_crm_email_challenge WHERE party_id=a
      AND created_at>clock_timestamp()-interval '1 hour')>=3 THEN RETURN '{"error":"rate_limited"}'; END IF;
    INSERT INTO event_crm_email_challenge(token_hash,party_id,email_hash)
      VALUES(token_hash_input,a,email_fingerprint);
    RETURN '{"requested":true}';
  END IF;
  SELECT * INTO challenge FROM event_crm_email_challenge WHERE token_hash=token_hash_input AND party_id=a FOR UPDATE;
  IF NOT FOUND OR challenge.consumed_at IS NOT NULL OR challenge.expires_at<=clock_timestamp()
    OR challenge.email_hash IS DISTINCT FROM email_fingerprint THEN RETURN '{"error":"verification_unavailable"}'; END IF;
  INSERT INTO event_crm_verified_email(party_id,email_hash) VALUES(a,email_fingerprint)
    ON CONFLICT(party_id,email_hash) DO UPDATE SET verified_at=clock_timestamp();
  UPDATE event_crm_email_challenge SET consumed_at=clock_timestamp() WHERE token_hash=token_hash_input;
  RETURN '{"verified":true}';
END $$;

CREATE OR REPLACE FUNCTION event_crm_prepare_row(e bigint,a bigint,row_data jsonb) RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE email text; phone text; ids bigint[]; selected bigint; derived text; ord event_ticket_order%ROWTYPE;
BEGIN
  IF jsonb_typeof(row_data)<>'object' OR length(coalesce(row_data->>'name','')) NOT BETWEEN 1 AND 160
    OR coalesce(row_data->>'quantity','1') !~ '^[0-9]{1,3}$'
    OR (coalesce(row_data->>'quantity','1'))::int NOT BETWEEN 1 AND 100
    OR length(coalesce(row_data->>'notes',''))>4000
    OR length(coalesce(row_data->>'nextAction',''))>500
    OR length(coalesce(row_data->>'source','manual')) NOT BETWEEN 1 AND 160
    OR coalesce(row_data->>'stage','interested') NOT IN ('interested','contacted','reserved','paid','closed_lost')
    OR EXISTS(SELECT 1 FROM jsonb_object_keys(row_data) k WHERE k NOT IN
      ('name','email','phone','contactId','responsibleId','source','contactedAt','notes','nextAction',
       'followUpAt','quantity','orderId','stage','outboundTime','returnTime','discard')) THEN
    RETURN jsonb_build_object('error','invalid_row');
  END IF;
  email:=event_crm_email(row_data->>'email'); phone:=event_crm_phone(row_data->>'phone');
  BEGIN
    PERFORM event_crm_timestamp(nullif(row_data->>'contactedAt',''));
    PERFORM event_crm_timestamp(nullif(row_data->>'followUpAt',''));
    PERFORM nullif(row_data->>'contactId','')::bigint;
    PERFORM nullif(row_data->>'responsibleId','')::bigint;
    PERFORM nullif(row_data->>'orderId','')::bigint;
  EXCEPTION WHEN invalid_parameter_value OR invalid_datetime_format OR datetime_field_overflow OR invalid_text_representation OR numeric_value_out_of_range THEN
    RETURN '{"error":"invalid_row_date_or_identifier"}';
  END;
  IF (email IS NOT NULL AND (length(email)>254 OR email !~ '^[^[:space:]@]+@[^[:space:]@]+[.][^[:space:]@]+$'))
    OR (nullif(row_data->>'phone','') IS NOT NULL AND phone IS NULL)
    OR (email IS NULL AND phone IS NULL) THEN RETURN '{"error":"contact_channel_required"}'; END IF;
  ids:=event_crm_candidates(email,phone);
  selected:=nullif(row_data->>'contactId','')::bigint;
  IF selected IS NOT NULL AND NOT(selected=ANY(ids)) THEN RETURN '{"error":"contact_conflict"}'; END IF;
  IF cardinality(ids)>1 AND selected IS NULL THEN
    RETURN jsonb_build_object('error','contact_conflict','candidateIds',ids);
  END IF;
  selected:=coalesce(selected,ids[1]);
  IF nullif(row_data->>'responsibleId','') IS NOT NULL AND
    NOT EXISTS(SELECT 1 FROM social_event WHERE id=e AND organizer_party_id=row_data->>'responsibleId') AND
    NOT EXISTS(SELECT 1 FROM event_crm_membership WHERE event_id=e AND party_id=(row_data->>'responsibleId')::bigint AND revoked_at IS NULL AND role IN ('coordinator','sales')) THEN
    RETURN '{"error":"assignee_unavailable"}';
  END IF;
  IF num_nonnulls(nullif(row_data->>'outboundTime',''),nullif(row_data->>'returnTime',''))=1
    OR (nullif(row_data->>'outboundTime','') IS NOT NULL AND NOT EXISTS(SELECT 1 FROM event_crm_transport_slot WHERE event_id=e AND direction='outbound' AND departure_time=row_data->>'outboundTime'))
    OR (nullif(row_data->>'returnTime','') IS NOT NULL AND NOT EXISTS(SELECT 1 FROM event_crm_transport_slot WHERE event_id=e AND direction='return' AND departure_time=row_data->>'returnTime')) THEN RETURN '{"error":"invalid_transport"}'; END IF;
  derived:=coalesce(row_data->>'stage','interested');
  IF nullif(row_data->>'orderId','') IS NOT NULL THEN
    SELECT * INTO ord FROM event_ticket_order WHERE id=(row_data->>'orderId')::bigint AND event_id=e;
    IF NOT FOUND OR ord.quantity<>coalesce(row_data->>'quantity','1')::int
      OR (ord.buyer_party_id IS NOT NULL AND (selected IS NULL OR ord.buyer_party_id IS DISTINCT FROM selected::text))
      OR (ord.buyer_party_id IS NULL AND (email IS NULL OR email IS DISTINCT FROM event_crm_email(ord.buyer_email)))
      THEN RETURN '{"error":"order_conflict"}'; END IF;
    IF ord.status='paid' AND EXISTS(
      SELECT 1 FROM event_ticket_checkout_runtime r JOIN commerce_checkout_session c ON c.id=r.checkout_id
      JOIN commerce_payment_attempt p ON p.checkout_id=c.id
      WHERE r.order_id=ord.id AND c.status IN ('paid','partially_refunded') AND p.status='succeeded'
        AND EXISTS(SELECT 1 FROM commerce_receipt receipt WHERE receipt.checkout_id=c.id
          AND receipt.kind='payment_receipt' AND receipt.voided_at IS NULL)) THEN derived:='paid';
    ELSIF ord.status='pending' AND EXISTS(SELECT 1 FROM event_ticket_checkout_runtime r WHERE r.order_id=ord.id
      AND r.fulfillment_status='seat_held' AND greatest(r.hold_expires_at,r.manual_hold_expires_at)>clock_timestamp()) THEN derived:='reserved';
    ELSIF ord.status IN ('refunded','cancelled') THEN derived:='closed_lost';
    ELSE derived:='contacted'; END IF;
  ELSIF derived IN ('paid','reserved') THEN
    -- A spreadsheet is a claim, never payment evidence or an inventory hold.
    derived:='contacted';
  END IF;
  RETURN jsonb_build_object('contactId',selected,'email',email,'phone',phone,'candidateIds',ids,
    'stage',derived,'claimedStage',coalesce(row_data->>'stage','interested'),
    'paymentReconciliationRequired',coalesce(row_data->>'stage','')='paid' AND derived<>'paid',
    'reservationRequired',coalesce(row_data->>'stage','')='reserved' AND derived<>'reserved');
END $$;

CREATE OR REPLACE FUNCTION event_crm_upsert_row(e bigint,a bigint,row_data jsonb) RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE preview jsonb; contact_id bigint; lead event_crm_lead%ROWTYPE; old_stage text; expiry timestamptz;
BEGIN
  -- Serialize normalization identities in a stable order; re-resolve after waits.
  PERFORM pg_advisory_xact_lock(hashtextextended('event-crm-contact:'||x,0)) FROM
    (SELECT x FROM unnest(ARRAY[event_crm_email(row_data->>'email'),event_crm_phone(row_data->>'phone')]) x WHERE x IS NOT NULL ORDER BY x) q;
  preview:=event_crm_prepare_row(e,a,row_data);
  IF preview ? 'error' THEN RETURN preview; END IF;
  contact_id:=(preview->>'contactId')::bigint;
  IF contact_id IS NULL THEN
    contact_id:=identity_create_contact(a,'event_crm_'||encode(digest(e::text||':'||coalesce(preview->>'email','')||':'||coalesce(preview->>'phone',''),'sha256'),'hex'),
      jsonb_build_object('display_name',btrim(row_data->>'name'),'is_org',false,
        'primary_email',preview->>'email','primary_phone',preview->>'phone'));
  END IF;
  SELECT * INTO lead FROM event_crm_lead WHERE event_id=e AND contact_party_id=contact_id FOR UPDATE;
  old_stage:=lead.stage;
  IF old_stage IS NOT NULL THEN
    -- Re-importing a list cannot overwrite later operational follow-up or sell twice.
    RETURN jsonb_build_object('leadId',lead.id,'contactId',contact_id,'existing',true);
  END IF;
  IF preview->>'stage'='reserved' THEN
    SELECT greatest(hold_expires_at,manual_hold_expires_at) INTO expiry FROM event_ticket_checkout_runtime WHERE order_id=(row_data->>'orderId')::bigint;
  END IF;
  INSERT INTO event_crm_lead(event_id,contact_party_id,responsible_party_id,source,contacted_at,notes,
    next_action,follow_up_at,stage,requested_quantity,order_id,reservation_expires_at,outbound_time,return_time,created_by,updated_by)
    VALUES(e,contact_id,nullif(row_data->>'responsibleId','')::bigint,coalesce(row_data->>'source','manual'),
      event_crm_timestamp(nullif(row_data->>'contactedAt','')),coalesce(row_data->>'notes',''),coalesce(row_data->>'nextAction',''),
      event_crm_timestamp(nullif(row_data->>'followUpAt','')),preview->>'stage',coalesce(row_data->>'quantity','1')::int,
      nullif(row_data->>'orderId','')::bigint,expiry,nullif(row_data->>'outboundTime',''),nullif(row_data->>'returnTime',''),a,a)
    RETURNING * INTO lead;
  INSERT INTO event_crm_audit(event_id,actor_id,operation,subject_id,from_stage,to_stage)
    VALUES(e,a,'lead.created',lead.id::text,NULL,lead.stage);
  RETURN jsonb_build_object('leadId',lead.id,'contactId',contact_id,'stage',lead.stage,'existing',false);
END $$;

CREATE OR REPLACE FUNCTION event_crm_snapshot(e bigint,a bigint,filters jsonb DEFAULT '{}') RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE role_code text; result jsonb;
BEGIN
  role_code:=event_crm_role(e,a);
  IF role_code IS NULL THEN RETURN '{"error":"not_found"}'; END IF;
  SELECT jsonb_build_object('event',jsonb_build_object('id',s.id,'title',s.title,'startTime',s.start_time,
    'organizerId',s.organizer_party_id,'venue',v.name,'timezone',s.timezone),
    'role',role_code,'leads',coalesce((SELECT jsonb_agg(to_jsonb(l)||jsonb_build_object('name',p.display_name,
      'email',p.primary_email,'phone',p.primary_phone,'orderStatus',o.status)
      ORDER BY l.follow_up_at NULLS LAST,l.id) FROM event_crm_lead l JOIN party p ON p.id=l.contact_party_id
      LEFT JOIN event_ticket_order o ON o.id=l.order_id
      WHERE l.event_id=e AND (role_code IN ('owner','coordinator') OR l.responsible_party_id=a)
        AND (nullif(filters->>'stage','') IS NULL OR l.stage=filters->>'stage')
        AND (nullif(filters->>'responsibleId','') IS NULL OR l.responsible_party_id=(filters->>'responsibleId')::bigint)
        AND (coalesce((filters->>'overdue')::boolean,false)=false OR l.follow_up_at<=clock_timestamp())), '[]'),
    'members',coalesce((SELECT jsonb_agg(jsonb_build_object('id',m.id,'partyId',m.party_id,'name',p.display_name,
      'role',m.role,'revokedAt',m.revoked_at)) FROM event_crm_membership m JOIN party p ON p.id=m.party_id
      WHERE m.event_id=e AND role_code IN ('owner','coordinator')),'[]'),
    'invitations',coalesce((SELECT jsonb_agg(to_jsonb(i)-'token_hash'-'recipient_email_hash') FROM event_crm_invitation i
      WHERE i.event_id=e AND (role_code='owner' OR (role_code='coordinator' AND i.role='sales'))),'[]'),
    'orders',coalesce((SELECT jsonb_agg(jsonb_build_object('id',o.id,'contactId',o.buyer_party_id,'quantity',o.quantity,
      'amountMinor',o.amount_cents,'currency',o.currency,'status',o.status,'paymentMethod',o.payment_method,
      'holdExpiresAt',greatest(r.hold_expires_at,r.manual_hold_expires_at)))
      FROM event_ticket_order o LEFT JOIN event_ticket_checkout_runtime r ON r.order_id=o.id
      WHERE o.event_id=e AND (role_code IN ('owner','coordinator','payment_verifier') OR EXISTS(SELECT 1 FROM event_crm_lead l WHERE l.order_id=o.id AND l.responsible_party_id=a))),'[]'),
    'manualPayments',coalesce((SELECT jsonb_agg(jsonb_build_object('orderId',r.order_id,'provider',p.provider,
      'amountMinor',p.amount_minor,'quantity',r.quantity,'reference',m.customer_reference,'occurredAt',m.sale_occurred_at,
      'submittedAt',m.submitted_at,'submittedBy',m.submitted_by,'status',m.status,'verifiedBy',m.reviewed_by,'verifiedAt',m.reviewed_at))
      FROM event_ticket_checkout_runtime r JOIN commerce_payment_attempt p ON p.checkout_id=r.checkout_id
      JOIN commerce_manual_payment_evidence m ON m.payment_attempt_id=p.id
      WHERE r.event_id=e AND role_code IN ('owner','coordinator','payment_verifier') AND p.provider IN ('cash','bank_transfer')),'[]'),
    'sales',coalesce((SELECT jsonb_agg(to_jsonb(totals)) FROM
      (SELECT c.currency,sum(c.paid_minor)::text AS gross_minor,sum(c.refunded_minor)::text AS refunded_minor,
         sum(c.paid_minor-c.refunded_minor)::text AS net_minor,count(DISTINCT o.id) AS paid_orders
       FROM event_ticket_order o JOIN event_ticket_checkout_runtime r ON r.order_id=o.id
       JOIN commerce_checkout_session c ON c.id=r.checkout_id
       WHERE o.event_id=e AND c.paid_minor>0 AND role_code IN ('owner','coordinator')
       GROUP BY c.currency) totals),'[]'),
    'activities',coalesce((SELECT jsonb_agg(jsonb_build_object('id',x.id,'title',x.title,'status',x.status,'startTime',x.start_time,'endTime',x.end_time,'notes',x.notes,'version',x.version))
      FROM event_logistics_activity x WHERE x.event_id=e AND role_code IN ('owner','coordinator')),'[]'),
    'inventory',coalesce((SELECT jsonb_agg(jsonb_build_object('tierId',t.id,'name',t.name,'total',t.quantity_total,
      'heldOrSold',t.quantity_sold,'remaining',greatest(0,t.quantity_total-t.quantity_sold))) FROM event_ticket_tier t WHERE t.event_id=e),'[]'),
    'waitlist',coalesce((SELECT jsonb_agg(jsonb_build_object('leadId',w.lead_id,'waitlistId',w.waitlist_id,
      'outboundTime',w.outbound_time,'returnTime',w.return_time,'status',q.status)) FROM event_crm_waitlist_link w
      JOIN event_crm_lead l ON l.id=w.lead_id JOIN event_waitlist q ON q.id=w.waitlist_id
      WHERE l.event_id=e AND (role_code IN ('owner','coordinator') OR l.responsible_party_id=a)),'[]'),
    'transport',coalesce((SELECT jsonb_agg(to_jsonb(t)) FROM event_crm_transport_slot t WHERE t.event_id=e),'[]'),
    'combinationSalesEnabled',false,
    'transportRequired',(SELECT transport_required FROM event_crm_control WHERE event_id=e),
    'audit',coalesce((SELECT jsonb_agg(to_jsonb(x)) FROM event_crm_audit x WHERE x.event_id=e AND role_code IN ('owner','coordinator')),'[]'))
    INTO result FROM social_event s LEFT JOIN venue v ON v.id=s.venue_id WHERE s.id=e;
  RETURN result;
END $$;

CREATE OR REPLACE FUNCTION event_crm_invitation_action(a bigint,token text,accept boolean) RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE inv event_crm_invitation%ROWTYPE; e bigint; member_id uuid; current_email_hash text; t timestamptz; issuer_role text;
BEGIN
  IF token !~ '^[a-f0-9]{64}$' THEN RETURN '{"error":"invitation_unavailable"}'; END IF;
  SELECT event_id INTO e FROM event_crm_invitation WHERE token_hash=token;
  PERFORM id FROM social_event WHERE id=e FOR UPDATE;
  SELECT * INTO inv FROM event_crm_invitation WHERE token_hash=token FOR UPDATE;
  t:=clock_timestamp();
  IF NOT FOUND OR inv.revoked_at IS NOT NULL OR inv.accepted_at IS NOT NULL OR inv.expires_at<=t
    OR NOT EXISTS(SELECT 1 FROM event_crm_control WHERE event_id=e AND enabled) THEN RETURN '{"error":"invitation_unavailable"}'; END IF;
  issuer_role:=event_crm_role(e,inv.issued_by);
  IF issuer_role IS NULL OR (inv.role='sales' AND issuer_role NOT IN ('owner','coordinator'))
    OR (inv.role<>'sales' AND issuer_role<>'owner') THEN RETURN '{"error":"invitation_unavailable"}'; END IF;
  SELECT encode(digest(event_crm_email(primary_email),'sha256'),'hex') INTO current_email_hash FROM party WHERE id=a;
  IF (inv.recipient_party_id IS NOT NULL AND inv.recipient_party_id<>a) OR
    (inv.recipient_email_hash IS NOT NULL AND (inv.recipient_email_hash IS DISTINCT FROM current_email_hash OR
      NOT EXISTS(SELECT 1 FROM event_crm_verified_email WHERE party_id=a AND email_hash=current_email_hash))) THEN
    RETURN '{"error":"recipient_verification_required"}';
  END IF;
  IF NOT EXISTS(SELECT 1 FROM user_credential WHERE party_id=a AND active) THEN RETURN '{"error":"recipient_verification_required"}'; END IF;
  IF NOT accept THEN RETURN (SELECT jsonb_build_object('eventId',e,'eventTitle',s.title,'startTime',s.start_time,
    'venue',v.name,'role',inv.role,'expiresAt',inv.expires_at,'permissions',CASE inv.role
      WHEN 'coordinator' THEN '["team.sales","logistics","crm.all","orders.read"]'::jsonb
      WHEN 'sales' THEN '["crm.assigned"]'::jsonb ELSE '["payments.verify","orders.read"]'::jsonb END)
    FROM social_event s LEFT JOIN venue v ON v.id=s.venue_id WHERE s.id=e); END IF;
  IF EXISTS(SELECT 1 FROM event_crm_membership WHERE event_id=e AND party_id=a AND revoked_at IS NULL) THEN RETURN '{"error":"membership_exists"}'; END IF;
  INSERT INTO event_crm_membership(event_id,party_id,role,granted_by) VALUES(e,a,inv.role,inv.issued_by) RETURNING id INTO member_id;
  UPDATE event_crm_invitation SET accepted_at=t,accepted_by=a,membership_id=member_id WHERE id=inv.id;
  INSERT INTO event_crm_audit(event_id,actor_id,operation,subject_id) VALUES(e,a,'invitation.accepted',inv.id::text);
  RETURN jsonb_build_object('eventId',e,'membershipId',member_id,'role',inv.role);
END $$;

CREATE OR REPLACE FUNCTION event_crm_command(e bigint,a bigint,cmd uuid,body jsonb,secret_hash text DEFAULT NULL) RETURNS jsonb
LANGUAGE plpgsql AS $$
DECLARE role_code text; op text; outcome jsonb; prior event_crm_command_receipt%ROWTYPE;
  inv event_crm_invitation%ROWTYPE; m event_crm_membership%ROWTYPE; lead event_crm_lead%ROWTYPE;
  preview jsonb; row_data jsonb; batch event_crm_import%ROWTYPE; prepared jsonb; results jsonb:='[]';
  t timestamptz; responsible bigint; old_stage text; requested text; target_order event_ticket_order%ROWTYPE; expiry timestamptz; duplicate_index bigint; wait_id bigint; contact_email text; target_activity_id bigint; activity_version integer; begin_at timestamptz; end_at timestamptz;
BEGIN
  role_code:=event_crm_role(e,a); t:=clock_timestamp();
  IF role_code IS NULL THEN RETURN '{"error":"not_found"}'; END IF;
  IF jsonb_typeof(body)<>'object' OR octet_length(body::text)>1048576 THEN RETURN '{"error":"invalid_request"}'; END IF;
  op:=body->>'operation';
  IF op NOT IN ('invitation.create','invitation.revoke','membership.revoke','lead.create','lead.update',
    'import.preview','import.confirm','task.assign','task.save','transport.confirm','recipient.lookup','waitlist.join') OR op IS NULL THEN RETURN '{"error":"invalid_request"}'; END IF;
  IF op IN ('invitation.create','invitation.revoke','membership.revoke','import.preview','import.confirm','task.assign','task.save','recipient.lookup') AND role_code NOT IN ('owner','coordinator') THEN RETURN '{"error":"forbidden"}'; END IF;
  IF op='transport.confirm' AND role_code<>'owner' THEN RETURN '{"error":"forbidden"}'; END IF;
  IF op='lead.create' AND role_code NOT IN ('owner','coordinator') THEN RETURN '{"error":"forbidden"}'; END IF;
  SELECT * INTO prior FROM event_crm_command_receipt WHERE event_id=e AND actor_id=a AND command_id=cmd;
  IF FOUND THEN
    IF prior.request_hash<>encode(digest(body::text,'sha256'),'hex') THEN RETURN '{"error":"idempotency_conflict"}'; END IF;
    RETURN prior.outcome||'{"replayed":true}';
  END IF;
  CASE op
  WHEN 'recipient.lookup' THEN
    IF length(coalesce(body->>'query','')) NOT BETWEEN 1 AND 254 AND coalesce(body->>'emailHash','') !~ '^[a-f0-9]{64}$' THEN RETURN '{"error":"invalid_request"}'; END IF;
    SELECT jsonb_build_object('candidates',coalesce(jsonb_agg(jsonb_build_object('partyId',p.id,'name',p.display_name,
      'hasAccount',EXISTS(SELECT 1 FROM user_credential u WHERE u.party_id=p.id AND u.active))),'[]')) INTO outcome
      FROM party p WHERE (p.id::text=body->>'query' OR encode(digest(event_crm_email(p.primary_email),'sha256'),'hex')=body->>'emailHash' OR lower(p.display_name)=lower(body->>'query'))
        AND NOT EXISTS(SELECT 1 FROM identity_party_archive x WHERE x.party_id=p.id);
  WHEN 'invitation.create' THEN
    IF body->>'role' NOT IN ('coordinator','sales','payment_verifier') OR
      (role_code='coordinator' AND body->>'role'<>'sales') THEN RETURN '{"error":"forbidden"}'; END IF;
    IF num_nonnulls(nullif(body->>'recipientPartyId',''),nullif(body->>'recipientEmailHash',''))<>1
      OR secret_hash IS NULL OR secret_hash !~ '^[0-9a-f]{64}$' THEN RETURN '{"error":"invalid_request"}'; END IF;
    IF nullif(body->>'recipientPartyId','') IS NOT NULL AND NOT EXISTS(SELECT 1 FROM user_credential
      WHERE party_id=(body->>'recipientPartyId')::bigint AND active) THEN RETURN '{"error":"recipient_account_missing"}'; END IF;
    IF nullif(body->>'recipientEmailHash','') IS NOT NULL AND body->>'recipientEmailHash' !~ '^[a-f0-9]{64}$' THEN RETURN '{"error":"invalid_request"}'; END IF;
    IF nullif(body->>'recipientEmailHash','') IS NOT NULL AND EXISTS(SELECT 1 FROM party p JOIN user_credential u ON u.party_id=p.id AND u.active
      WHERE encode(digest(event_crm_email(p.primary_email),'sha256'),'hex')=body->>'recipientEmailHash') THEN RETURN '{"error":"resolve_existing_account_first"}'; END IF;
    INSERT INTO event_crm_invitation(id,event_id,issued_by,recipient_party_id,recipient_email_hash,token_hash,role,issued_at,expires_at)
      VALUES(cmd,e,a,nullif(body->>'recipientPartyId','')::bigint,
        CASE WHEN nullif(body->>'recipientEmailHash','') IS NOT NULL THEN body->>'recipientEmailHash' END,
        secret_hash,body->>'role',t,t+interval '7 days') RETURNING * INTO inv;
    outcome:=jsonb_build_object('invitationId',inv.id,'expiresAt',inv.expires_at,'created',true);
  WHEN 'invitation.revoke' THEN
    SELECT * INTO inv FROM event_crm_invitation WHERE id=(body->>'invitationId')::uuid AND event_id=e FOR UPDATE;
    IF NOT FOUND OR (role_code='coordinator' AND inv.role<>'sales') THEN RETURN '{"error":"not_found"}'; END IF;
    IF inv.accepted_at IS NOT NULL THEN RETURN '{"error":"revoke_membership_separately"}'; END IF;
    UPDATE event_crm_invitation SET revoked_at=coalesce(revoked_at,t),revoked_by=coalesce(revoked_by,a) WHERE id=inv.id;
    outcome:=jsonb_build_object('invitationId',inv.id,'revoked',true);
  WHEN 'membership.revoke' THEN
    SELECT * INTO m FROM event_crm_membership WHERE id=(body->>'membershipId')::uuid AND event_id=e FOR UPDATE;
    IF NOT FOUND OR (role_code='coordinator' AND m.role<>'sales') THEN RETURN '{"error":"not_found"}'; END IF;
    UPDATE event_crm_membership SET revoked_at=coalesce(revoked_at,t),revoked_by=coalesce(revoked_by,a) WHERE id=m.id;
    outcome:=jsonb_build_object('membershipId',m.id,'revoked',true);
  WHEN 'lead.create' THEN
    outcome:=event_crm_upsert_row(e,a,body->'row');
  WHEN 'lead.update' THEN
    SELECT * INTO lead FROM event_crm_lead WHERE id=(body->>'leadId')::uuid AND event_id=e FOR UPDATE;
    IF NOT FOUND OR NOT(role_code IN ('owner','coordinator') OR (role_code='sales' AND lead.responsible_party_id=a)) THEN RETURN '{"error":"not_found"}'; END IF;
    IF lead.version IS DISTINCT FROM (body->>'expectedVersion')::bigint THEN RETURN '{"error":"version_conflict"}'; END IF;
    IF role_code='sales' AND (body ? 'responsibleId' OR body ? 'orderId') THEN RETURN '{"error":"forbidden"}'; END IF;
    responsible:=CASE WHEN body ? 'responsibleId' THEN nullif(body->>'responsibleId','')::bigint ELSE lead.responsible_party_id END;
    IF responsible IS NOT NULL AND NOT EXISTS(SELECT 1 FROM social_event WHERE id=e AND organizer_party_id=responsible::text)
      AND NOT EXISTS(SELECT 1 FROM event_crm_membership WHERE event_id=e AND party_id=responsible AND revoked_at IS NULL AND role IN ('coordinator','sales')) THEN RETURN '{"error":"assignee_unavailable"}'; END IF;
    requested:=coalesce(body->>'stage',lead.stage); expiry:=lead.reservation_expires_at; old_stage:=lead.stage;
    IF body ? 'orderId' THEN
      SELECT * INTO target_order FROM event_ticket_order WHERE id=(body->>'orderId')::bigint AND event_id=e FOR UPDATE;
      IF NOT FOUND OR (target_order.buyer_party_id IS NOT NULL AND target_order.buyer_party_id IS DISTINCT FROM lead.contact_party_id::text)
        THEN RETURN '{"error":"order_conflict"}'; END IF;
      -- The shared prepare function verifies authoritative payment/hold evidence.
      SELECT jsonb_build_object('name',p.display_name,'email',p.primary_email,'phone',p.primary_phone,
        'contactId',p.id,'quantity',target_order.quantity,'orderId',target_order.id,'stage',requested)
        INTO row_data FROM party p WHERE p.id=lead.contact_party_id;
      preview:=event_crm_prepare_row(e,a,row_data);
      IF preview ? 'error' THEN RETURN preview; END IF;
      requested:=preview->>'stage';
      IF requested='reserved' THEN SELECT greatest(hold_expires_at,manual_hold_expires_at) INTO expiry FROM event_ticket_checkout_runtime WHERE order_id=target_order.id; ELSE expiry:=NULL; END IF;
    ELSIF requested IN ('paid','reserved') AND requested IS DISTINCT FROM lead.stage THEN RETURN '{"error":"order_evidence_required"}'; END IF;
    IF requested NOT IN ('interested','contacted','reserved','paid','closed_lost') OR
      length(coalesce(body->>'notes',lead.notes))>4000 OR length(coalesce(body->>'nextAction',lead.next_action))>500 THEN RETURN '{"error":"invalid_request"}'; END IF;
    IF lead.stage='paid' AND requested<>'paid' THEN RETURN '{"error":"use_order_refund_or_cancel"}'; END IF;
    IF body ? 'outboundTime' OR body ? 'returnTime' THEN
      IF lead.order_id IS NOT NULL OR num_nonnulls(nullif(body->>'outboundTime',''),nullif(body->>'returnTime',''))=1
        OR (nullif(body->>'outboundTime','') IS NOT NULL AND NOT EXISTS(SELECT 1 FROM event_crm_transport_slot WHERE event_id=e AND direction='outbound' AND departure_time=body->>'outboundTime'))
        OR (nullif(body->>'returnTime','') IS NOT NULL AND NOT EXISTS(SELECT 1 FROM event_crm_transport_slot WHERE event_id=e AND direction='return' AND departure_time=body->>'returnTime')) THEN RETURN '{"error":"invalid_transport"}'; END IF;
    END IF;
    IF body ? 'quantity' AND (lead.order_id IS NOT NULL OR coalesce(body->>'quantity','') !~ '^[0-9]{1,3}$'
      OR (body->>'quantity')::int NOT BETWEEN 1 AND 100) THEN RETURN '{"error":"invalid_request"}'; END IF;
    UPDATE event_crm_lead SET responsible_party_id=responsible,stage=requested,
      notes=coalesce(body->>'notes',notes),next_action=coalesce(body->>'nextAction',next_action),
      follow_up_at=CASE WHEN body ? 'followUpAt' THEN event_crm_timestamp(nullif(body->>'followUpAt','')) ELSE follow_up_at END,
      contacted_at=CASE WHEN body ? 'contactedAt' THEN event_crm_timestamp(nullif(body->>'contactedAt','')) ELSE contacted_at END,
      order_id=coalesce(target_order.id,order_id),requested_quantity=coalesce(target_order.quantity,(body->>'quantity')::int,requested_quantity),
      outbound_time=CASE WHEN body ? 'outboundTime' THEN nullif(body->>'outboundTime','') ELSE outbound_time END,
      return_time=CASE WHEN body ? 'returnTime' THEN nullif(body->>'returnTime','') ELSE return_time END,
      reservation_expires_at=expiry,updated_by=a,updated_at=t,version=version+1 WHERE id=lead.id;
    -- Keep the canonical waitlist aligned with a lead edit; it never holds stock.
    UPDATE event_waitlist q SET quantity=l.requested_quantity,updated_at=t,
      status=CASE WHEN l.stage='closed_lost' OR l.order_id IS NOT NULL OR l.outbound_time IS NULL THEN 'removed' ELSE q.status END
      FROM event_crm_waitlist_link w JOIN event_crm_lead l ON l.id=w.lead_id
      WHERE l.id=lead.id AND q.id=w.waitlist_id AND q.status='active';
    UPDATE event_crm_waitlist_link w SET outbound_time=l.outbound_time,return_time=l.return_time
      FROM event_crm_lead l WHERE l.id=lead.id AND w.lead_id=l.id AND l.outbound_time IS NOT NULL AND l.return_time IS NOT NULL;
    INSERT INTO event_crm_audit(event_id,actor_id,operation,subject_id,from_stage,to_stage) VALUES(e,a,op,lead.id::text,old_stage,requested);
    outcome:=jsonb_build_object('leadId',lead.id,'version',lead.version+1);
  WHEN 'import.preview' THEN
    IF jsonb_typeof(body->'rows')<>'array' OR jsonb_array_length(body->'rows') NOT BETWEEN 1 AND 500 THEN RETURN '{"error":"invalid_request"}'; END IF;
    FOR row_data IN SELECT value FROM jsonb_array_elements(body->'rows') LOOP
      IF coalesce((row_data->>'discard')::boolean,false) THEN prepared:='{"discarded":true}';
      ELSE prepared:=event_crm_prepare_row(e,a,row_data); END IF;
      SELECT ordinality-1 INTO duplicate_index FROM jsonb_array_elements(results) WITH ORDINALITY r(value,ordinality)
        WHERE NOT(value ? 'error') AND NOT(prepared ? 'error') AND
          ((prepared->>'contactId' IS NOT NULL AND value->>'contactId'=prepared->>'contactId') OR
           (prepared->>'contactId' IS NULL AND ((prepared->>'email' IS NOT NULL AND value->>'email'=prepared->>'email')
             OR (prepared->>'phone' IS NOT NULL AND value->>'phone'=prepared->>'phone')))) LIMIT 1;
      IF duplicate_index IS NOT NULL THEN prepared:=prepared||jsonb_build_object('duplicateOf',duplicate_index); END IF;
      results:=results||jsonb_build_array(prepared);
    END LOOP;
    INSERT INTO event_crm_import(id,event_id,actor_id,content_hash,rows)
      VALUES(cmd,e,a,encode(digest((body->'rows')::text,'sha256'),'hex'),body->'rows')
      ON CONFLICT(event_id,actor_id,content_hash) DO NOTHING;
    SELECT * INTO batch FROM event_crm_import WHERE event_id=e AND actor_id=a AND content_hash=encode(digest((body->'rows')::text,'sha256'),'hex');
    outcome:=jsonb_build_object('importId',batch.id,'preview',results,'confirmed',batch.confirmed_at IS NOT NULL);
  WHEN 'import.confirm' THEN
    SELECT * INTO batch FROM event_crm_import WHERE id=(body->>'importId')::uuid AND event_id=e AND actor_id=a FOR UPDATE;
    IF NOT FOUND THEN RETURN '{"error":"not_found"}'; END IF;
    IF batch.confirmed_at IS NOT NULL THEN RETURN batch.result||'{"replayed":true}'; END IF;
    -- Validate every non-discarded row before ANY creation. A later conflict raises
    -- inside the subtransaction below, rolling back the complete batch.
    FOR row_data IN SELECT value FROM jsonb_array_elements(batch.rows) LOOP
      IF NOT coalesce((row_data->>'discard')::boolean,false) THEN
        preview:=event_crm_prepare_row(e,a,row_data); IF preview ? 'error' THEN RETURN '{"error":"import_conflict"}'; END IF;
      END IF;
    END LOOP;
    BEGIN
      FOR row_data IN SELECT value FROM jsonb_array_elements(batch.rows) LOOP
        IF coalesce((row_data->>'discard')::boolean,false) THEN preview:='{"discarded":true}';
        ELSE preview:=event_crm_upsert_row(e,a,row_data); END IF;
        IF preview ? 'error' THEN RAISE EXCEPTION 'import conflict' USING ERRCODE='P0001'; END IF;
        results:=results||jsonb_build_array(preview);
      END LOOP;
      outcome:=jsonb_build_object('importId',batch.id,'rows',results);
      UPDATE event_crm_import SET confirmed_at=t,result=outcome WHERE id=batch.id;
    EXCEPTION WHEN SQLSTATE 'P0001' OR unique_violation THEN RETURN '{"error":"import_conflict"}'; END;
  WHEN 'task.assign' THEN
    responsible:=(body->>'responsibleId')::bigint;
    IF NOT EXISTS(SELECT 1 FROM event_logistics_activity WHERE id=(body->>'activityId')::bigint AND event_id=e)
      OR (NOT EXISTS(SELECT 1 FROM event_crm_membership WHERE event_id=e AND party_id=responsible AND revoked_at IS NULL)
        AND NOT EXISTS(SELECT 1 FROM social_event WHERE id=e AND organizer_party_id=responsible::text))
      THEN RETURN '{"error":"not_found"}'; END IF;
    IF NOT EXISTS(SELECT 1 FROM event_logistics_assignment WHERE activity_id=(body->>'activityId')::bigint AND party_id=responsible::text) THEN
      INSERT INTO event_logistics_assignment(activity_id,party_id) VALUES((body->>'activityId')::bigint,responsible::text);
    END IF;
    outcome:=jsonb_build_object('assigned',true,'activityId',(body->>'activityId')::bigint,'responsibleId',responsible);
  WHEN 'task.save' THEN
    IF length(coalesce(body->>'title','')) NOT BETWEEN 1 AND 200 OR length(coalesce(body->>'notes',''))>4000
      OR nullif(body->>'startTime','') IS NULL THEN RETURN '{"error":"invalid_request"}'; END IF;
    BEGIN
      begin_at:=event_crm_timestamp(body->>'startTime');end_at:=event_crm_timestamp(nullif(body->>'endTime',''));
    EXCEPTION WHEN invalid_parameter_value OR invalid_datetime_format OR datetime_field_overflow THEN RETURN '{"error":"invalid_request"}'; END;
    IF end_at IS NOT NULL AND end_at<begin_at THEN RETURN '{"error":"invalid_request"}'; END IF;
    target_activity_id:=nullif(body->>'activityId','')::bigint;
    IF target_activity_id IS NULL THEN
      INSERT INTO event_logistics_activity(event_id,activity_type,title,notes,start_time,end_time,priority,status,version,created_by_party_id,created_at,updated_at)
        VALUES(e,'task',btrim(body->>'title'),nullif(body->>'notes',''),begin_at,end_at,'normal','planned',1,a::text,t,t) RETURNING id INTO target_activity_id;
      activity_version:=1;
    ELSE
      SELECT version INTO activity_version FROM event_logistics_activity WHERE id=target_activity_id AND event_id=e FOR UPDATE;
      IF NOT FOUND THEN RETURN '{"error":"not_found"}'; END IF;
      IF activity_version IS DISTINCT FROM (body->>'expectedVersion')::integer THEN RETURN '{"error":"version_conflict"}'; END IF;
      UPDATE event_logistics_activity SET title=btrim(body->>'title'),notes=nullif(body->>'notes',''),start_time=begin_at,end_time=end_at,
        version=version+1,updated_at=t WHERE id=target_activity_id AND event_id=e;
      activity_version:=activity_version+1;
    END IF;
    outcome:=jsonb_build_object('activityId',target_activity_id,'version',activity_version);
  WHEN 'transport.confirm' THEN
    IF body->>'direction' NOT IN ('outbound','return') OR (body->>'capacity')::int NOT BETWEEN 1 AND 1000
      OR length(coalesce(body->>'evidenceReference','')) NOT BETWEEN 3 AND 160 THEN RETURN '{"error":"invalid_request"}'; END IF;
    UPDATE event_crm_transport_slot SET capacity=(body->>'capacity')::int,confirmed_by=a,confirmed_at=t,confirmation_reference=body->>'evidenceReference'
      WHERE event_id=e AND direction=body->>'direction' AND departure_time=body->>'departureTime';
    IF NOT FOUND THEN RETURN '{"error":"not_found"}'; END IF;
    outcome:=jsonb_build_object('confirmed',true,'combinationSalesEnabled',false,'direction',body->>'direction','departureTime',body->>'departureTime','capacity',(body->>'capacity')::int);
  WHEN 'waitlist.join' THEN
    SELECT * INTO lead FROM event_crm_lead WHERE id=(body->>'leadId')::uuid AND event_id=e FOR UPDATE;
    IF NOT FOUND OR NOT(role_code IN ('owner','coordinator') OR (role_code='sales' AND lead.responsible_party_id=a))
      THEN RETURN '{"error":"not_found"}'; END IF;
    IF lead.outbound_time IS NULL OR lead.return_time IS NULL OR lead.stage IN ('paid','closed_lost') OR lead.order_id IS NOT NULL
      THEN RETURN '{"error":"invalid_transport"}'; END IF;
    SELECT event_crm_email(primary_email) INTO contact_email FROM party WHERE id=lead.contact_party_id;
    IF contact_email IS NULL THEN RETURN '{"error":"email_required_for_waitlist"}'; END IF;
    SELECT waitlist_id INTO wait_id FROM event_crm_waitlist_link WHERE lead_id=lead.id;
    IF wait_id IS NULL THEN
      SELECT id INTO wait_id FROM event_waitlist WHERE event_id=e AND party_id=lead.contact_party_id::text AND status='active' ORDER BY id LIMIT 1 FOR UPDATE;
      IF wait_id IS NULL THEN
        INSERT INTO event_waitlist(event_id,party_id,email,name,quantity,status,priority)
          SELECT e,p.id::text,contact_email,p.display_name,lead.requested_quantity,'active',0 FROM party p WHERE p.id=lead.contact_party_id RETURNING id INTO wait_id;
      END IF;
      INSERT INTO event_crm_waitlist_link(lead_id,waitlist_id,outbound_time,return_time,created_by)
        VALUES(lead.id,wait_id,lead.outbound_time,lead.return_time,a);
    END IF;
    UPDATE event_waitlist SET quantity=lead.requested_quantity,status='active',updated_at=t WHERE id=wait_id AND status IN ('active','removed');
    UPDATE event_crm_waitlist_link SET outbound_time=lead.outbound_time,return_time=lead.return_time WHERE lead_id=lead.id;
    outcome:=jsonb_build_object('waitlistId',wait_id,'reserved',false,'paid',false);
  ELSE RETURN '{"error":"invalid_request"}'; END CASE;
  IF outcome ? 'error' THEN RETURN outcome; END IF;
  INSERT INTO event_crm_command_receipt VALUES(e,a,cmd,encode(digest(body::text,'sha256'),'hex'),outcome);
  IF op<>'lead.update' THEN
    INSERT INTO event_crm_audit(event_id,actor_id,operation,subject_id) VALUES(e,a,op,
      coalesce(outcome->>'leadId',outcome->>'invitationId',outcome->>'membershipId',outcome->>'activityId',cmd::text));
  END IF;
  RETURN outcome;
END $$;

-- Refuse every order path for a transport bundle until joint reservation is
-- verified. This backstop includes legacy orders and the canonical checkout.
CREATE OR REPLACE FUNCTION event_crm_guard_bundle_order() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS(SELECT 1 FROM event_crm_control WHERE event_id=NEW.event_id AND transport_required
    AND NOT combination_sales_enabled) THEN RAISE EXCEPTION 'combination_sales_disabled' USING ERRCODE='55000'; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_crm_bundle_order_guard ON event_ticket_order;
CREATE TRIGGER event_crm_bundle_order_guard BEFORE INSERT ON event_ticket_order
  FOR EACH ROW EXECUTE FUNCTION event_crm_guard_bundle_order();

-- Automatic projection only: payment/refund authority stays in the canonical
-- commerce services. CRM stages cannot manufacture a sale or release inventory.
CREATE OR REPLACE FUNCTION event_crm_sync_order() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE l event_crm_lead%ROWTYPE; target text;
BEGIN
  SELECT * INTO l FROM event_crm_lead WHERE order_id=NEW.order_id FOR UPDATE;
  IF NOT FOUND THEN RETURN NEW; END IF;
  target:=CASE WHEN NEW.payment_status IN ('paid','partially_refunded') AND NEW.fulfillment_status IN ('issued','transferred','checked_in') THEN 'paid'
    WHEN NEW.payment_status='refunded' THEN 'closed_lost'
    WHEN NEW.fulfillment_status IN ('cancelled','refunded','expired') THEN 'closed_lost'
    WHEN NEW.fulfillment_status='seat_held' AND greatest(NEW.hold_expires_at,NEW.manual_hold_expires_at)>clock_timestamp() THEN 'reserved'
    ELSE 'contacted' END;
  IF target IS DISTINCT FROM l.stage THEN
    UPDATE event_crm_lead SET stage=target,reservation_expires_at=CASE WHEN target='reserved' THEN greatest(NEW.hold_expires_at,NEW.manual_hold_expires_at) END,
      version=version+1,updated_at=clock_timestamp() WHERE id=l.id;
    INSERT INTO event_crm_audit(event_id,actor_id,actor_type,operation,subject_id,from_stage,to_stage)
      VALUES(l.event_id,NULL,'canonical_order','order.projection',l.id::text,l.stage,target);
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_crm_order_projection ON event_ticket_checkout_runtime;
CREATE TRIGGER event_crm_order_projection AFTER UPDATE OF payment_status,fulfillment_status,hold_expires_at,manual_hold_expires_at
  ON event_ticket_checkout_runtime FOR EACH ROW EXECUTE FUNCTION event_crm_sync_order();

CREATE OR REPLACE FUNCTION event_crm_protect_verified_claim() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF OLD.status='approved' AND NEW.sale_occurred_at IS DISTINCT FROM OLD.sale_occurred_at THEN
    RAISE EXCEPTION 'Verified sale date is immutable' USING ERRCODE='23514'; END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_crm_verified_claim_guard ON commerce_manual_payment_evidence;
CREATE TRIGGER event_crm_verified_claim_guard BEFORE UPDATE ON commerce_manual_payment_evidence
  FOR EACH ROW EXECUTE FUNCTION event_crm_protect_verified_claim();

CREATE OR REPLACE FUNCTION event_crm_immutable_audit() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'Event CRM audit is append-only' USING ERRCODE='55000'; END $$;
DROP TRIGGER IF EXISTS event_crm_audit_immutable ON event_crm_audit;
CREATE TRIGGER event_crm_audit_immutable BEFORE UPDATE OR DELETE ON event_crm_audit
  FOR EACH ROW EXECUTE FUNCTION event_crm_immutable_audit();

-- Material authorization fence: snapshot-isolated readers abort rather than
-- disclosing a membership image from before a committed revocation.
CREATE OR REPLACE FUNCTION event_crm_fence_membership() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  UPDATE event_crm_control SET authorization_version=authorization_version+1
    WHERE event_id=CASE WHEN TG_OP='DELETE' THEN OLD.event_id ELSE NEW.event_id END;
  IF TG_OP='DELETE' THEN RETURN OLD; ELSE RETURN NEW; END IF;
END $$;
DROP TRIGGER IF EXISTS event_crm_membership_fence ON event_crm_membership;
CREATE TRIGGER event_crm_membership_fence BEFORE INSERT OR UPDATE OR DELETE ON event_crm_membership
  FOR EACH ROW EXECUTE FUNCTION event_crm_fence_membership();

-- Reuse the canonical archived-identity boundary for every new Party FK.
DO $$ DECLARE item record; BEGIN
  FOR item IN SELECT cl.oid::regclass AS target,string_agg(quote_literal(a.attname),',' ORDER BY a.attnum) AS columns
    FROM pg_class cl JOIN pg_attribute a ON a.attrelid=cl.oid AND a.attnum>0 AND NOT a.attisdropped
    WHERE cl.relnamespace='public'::regnamespace AND cl.relname LIKE 'event_crm_%' AND cl.relkind='r'
      AND EXISTS(SELECT 1 FROM pg_constraint f WHERE f.contype='f' AND f.conrelid=cl.oid AND f.confrelid='party'::regclass AND a.attnum=ANY(f.conkey))
    GROUP BY cl.oid LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',item.target);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',item.target,item.columns);
  END LOOP;
END $$;

-- SQL entry points are internal implementation, not public database RPCs.
REVOKE ALL ON event_crm_control,event_crm_membership,event_crm_invitation,
  event_crm_email_challenge,event_crm_verified_email,event_crm_lead,event_crm_audit,
  event_crm_import,event_crm_command_receipt,event_crm_transport_slot,event_crm_waitlist_link FROM PUBLIC;
REVOKE ALL ON FUNCTION event_crm_command(bigint,bigint,uuid,jsonb,text),
  event_crm_snapshot(bigint,bigint,jsonb),event_crm_invitation_action(bigint,text,boolean),
  event_crm_upsert_row(bigint,bigint,jsonb),event_crm_prepare_row(bigint,bigint,jsonb),
  event_crm_candidates(text,text),event_crm_role(bigint,bigint),
  event_crm_email_proof(bigint,text,boolean) FROM PUBLIC;
COMMIT;
