-- Event memory invitations reuse the canonical inbox; no email/push/marketing consent.
-- Applying this migration never enables a campaign or enrolls a recipient.
BEGIN;
SET LOCAL lock_timeout='5s';
CREATE TABLE IF NOT EXISTS event_memory_campaign (
 event_id bigint PRIMARY KEY REFERENCES social_event(id) ON DELETE CASCADE,
 enabled boolean NOT NULL DEFAULT false,
 changed_by bigint NOT NULL REFERENCES party(id),
 updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS event_memory_preference (
 party_id bigint PRIMARY KEY REFERENCES party(id),
 in_app boolean NOT NULL DEFAULT false,
 updated_at timestamptz NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS event_memory_delivery (
 id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
 event_id bigint NOT NULL REFERENCES social_event(id) ON DELETE CASCADE,
 party_id bigint NOT NULL REFERENCES party(id),
 step integer NOT NULL CHECK(step IN (1,2)),
 evidence text NOT NULL CHECK(evidence IN ('verified_attendance','professional','rsvp_intent')),
 notification_id bigint UNIQUE REFERENCES notification(id) ON DELETE SET NULL,
 delivered_at timestamptz NOT NULL DEFAULT now(),
 opened_at timestamptz,
 published_at timestamptz,
 UNIQUE(event_id,party_id,step)
);
CREATE INDEX IF NOT EXISTS event_memory_delivery_frequency ON event_memory_delivery(party_id,delivered_at DESC);
CREATE TABLE IF NOT EXISTS event_memory_migration_state (
 id integer PRIMARY KEY CHECK(id=1), notification_expression text NOT NULL,
 visibility_definition text NOT NULL, rsvp_view_definition text NOT NULL,
 installed_visibility_digest text, installed_view_digest text
);
DO $$ DECLARE previous text; BEGIN
 SELECT pg_get_expr(conbin,conrelid) INTO previous FROM pg_constraint
 WHERE conrelid='notification'::regclass AND conname='notification_notif_type_check' AND contype='c';
 IF previous IS NULL THEN RAISE EXCEPTION 'Notification type authority is missing'; END IF;
 IF NOT EXISTS(SELECT 1 FROM event_memory_migration_state) THEN
  INSERT INTO event_memory_migration_state(id,notification_expression,visibility_definition,rsvp_view_definition)
   VALUES(1,previous,pg_get_functiondef('interaction_notification_visible(bigint,bigint)'::regprocedure),pg_get_viewdef('directory_public_rsvp_event'::regclass,true));
 END IF;
 IF position('event.memory.invitation' in previous)=0 OR position('event.memory.reminder' in previous)=0 THEN
  ALTER TABLE notification DROP CONSTRAINT notification_notif_type_check;
  EXECUTE format('ALTER TABLE notification ADD CONSTRAINT notification_notif_type_check CHECK ((%s) OR notif_type IN (''event.memory.invitation'',''event.memory.reminder''))',previous);
 END IF;
END $$;

-- Completed public events remain readable communities. This grants neither
-- RSVP nor ticket purchase, and metadata/privacy/source suppression still apply.
CREATE TABLE IF NOT EXISTS event_memory_capability_backup (
 state_id uuid PRIMARY KEY REFERENCES workflow_state(id), prior_enabled boolean
);
INSERT INTO event_memory_capability_backup(state_id,prior_enabled)
 SELECT s.id,c.enabled FROM workflow_state s JOIN workflow_definition w ON w.id=s.workflow_id
 LEFT JOIN workflow_state_capability c ON c.state_id=s.id AND c.capability_code='public-listable'
 WHERE w.code='social-event-lifecycle' AND w.active AND s.active AND s.code='completed'
 ON CONFLICT DO NOTHING;
INSERT INTO workflow_state_capability(state_id,capability_code,enabled)
 SELECT state_id,'public-listable',true FROM event_memory_capability_backup
 ON CONFLICT(state_id,capability_code) DO UPDATE SET enabled=true,updated_at=now();

-- Do not infer an end from a start date, RSVP or elapsed duration. A completed
-- lifecycle AND confirmed end are required; cancellation never qualifies.
CREATE OR REPLACE FUNCTION event_memory_finished(event_key bigint) RETURNS boolean
LANGUAGE sql STABLE AS $$
 SELECT EXISTS(SELECT 1 FROM social_event e JOIN workflow_state s ON s.id=e.workflow_state_id
  JOIN workflow_definition w ON w.id=s.workflow_id AND w.active AND w.code='social-event-lifecycle'
  WHERE e.id=event_key AND s.active AND s.code='completed'
    AND e.end_time>e.start_time AND e.end_time<=now()-interval '1 hour'
    AND e.end_time>now()-interval '30 days'
    AND NOT EXISTS(SELECT 1 FROM external_event_ref r WHERE r.event_id=e.id AND lower(btrim(r.source_status))='suppressed'))
$$;

-- No name/email matching. Archive aliases are excluded: another account's
-- consent cannot be inferred from an identity merge. Only active login accounts
-- that can still read the parent event qualify.
CREATE OR REPLACE FUNCTION event_memory_candidates(event_key bigint)
RETURNS TABLE(party_id bigint,evidence text) LANGUAGE sql STABLE AS $$
 WITH signals AS (
  SELECT t.current_holder_party_id AS party,1 AS priority FROM event_ticket t
   JOIN event_ticket_order o ON o.id=t.order_ref_id AND o.event_id=t.event_id AND o.status='paid'
   JOIN event_ticket_admission_audit a ON a.ticket_id=t.id AND a.event_id=t.event_id
   WHERE t.event_id=event_key AND t.checked_in_at IS NOT NULL AND t.status='checked_in'
  UNION ALL SELECT organizer_party_id,2 FROM social_event WHERE id=event_key
  UNION ALL SELECT a.party_id,2 FROM event_artist ea JOIN social_artist_profile a ON a.id=ea.artist_id WHERE ea.event_id=event_key
  UNION ALL SELECT party_id,2 FROM event_logistics_member WHERE event_id=event_key
  UNION ALL SELECT party_id,3 FROM event_rsvp WHERE event_id=event_key AND status='accepted'
 ), identities AS (
  SELECT p.id,min(s.priority) AS priority FROM signals s JOIN party p ON p.id::text=s.party
  WHERE interaction_actor_live(p.id) AND interaction_event_access(event_key,p.id)
   AND NOT EXISTS(SELECT 1 FROM identity_party_archive a WHERE a.party_id=p.id)
  GROUP BY p.id
 ) SELECT id,CASE priority WHEN 1 THEN 'verified_attendance' WHEN 2 THEN 'professional' ELSE 'rsvp_intent' END FROM identities
$$;

CREATE OR REPLACE FUNCTION event_memory_preferences(actor bigint,desired boolean DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql AS $$
BEGIN
 IF NOT interaction_actor_live(actor) THEN RETURN jsonb_build_object('error','not_found'); END IF;
 -- Serialize opt-out, all event deliveries and publication for this recipient.
 PERFORM pg_advisory_xact_lock(hashtextextended('event-memory-recipient:'||actor,0));
 IF desired IS NOT NULL THEN
  INSERT INTO event_memory_preference(party_id,in_app) VALUES(actor,desired)
   ON CONFLICT(party_id) DO UPDATE SET in_app=excluded.in_app,updated_at=now();
 END IF;
 RETURN jsonb_build_object('inApp',coalesce((SELECT in_app FROM event_memory_preference WHERE party_id=actor),false),'supportsExtendedMoments',true);
END $$;

CREATE OR REPLACE FUNCTION event_memory_summary(event_key bigint,actor bigint,desired boolean DEFAULT NULL)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE enabled_value boolean;
BEGIN
 -- A logistics assignment is not campaign-management authority.
 IF NOT interaction_actor_live(actor) OR NOT EXISTS(SELECT 1 FROM social_event
  WHERE id=event_key AND organizer_party_id=actor::text) THEN RETURN jsonb_build_object('error','not_found'); END IF;
 PERFORM id FROM social_event WHERE id=event_key FOR SHARE;
 IF desired IS NOT NULL THEN
  INSERT INTO event_memory_campaign(event_id,enabled,changed_by) VALUES(event_key,desired,actor)
   ON CONFLICT(event_id) DO UPDATE SET enabled=excluded.enabled,changed_by=actor,updated_at=now();
 END IF;
 SELECT enabled INTO enabled_value FROM event_memory_campaign WHERE event_id=event_key;
 RETURN jsonb_build_object('enabled',coalesce(enabled_value,false),'eventFinished',event_memory_finished(event_key),
  'dispatcherEnabled',EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND enabled),
  'candidates',(SELECT count(*) FROM event_memory_candidates(event_key)),
  'consented',(SELECT count(*) FROM event_memory_candidates(event_key) c JOIN event_memory_preference p USING(party_id) WHERE p.in_app),
  'invitations',(SELECT count(*) FROM event_memory_delivery WHERE event_id=event_key AND step=1),
  'reminders',(SELECT count(*) FROM event_memory_delivery WHERE event_id=event_key AND step=2),
  'opened',(SELECT count(*) FROM event_memory_delivery WHERE event_id=event_key AND step=1 AND opened_at IS NOT NULL),
  'published',(SELECT count(*) FROM event_memory_delivery WHERE event_id=event_key AND step=1 AND published_at IS NOT NULL),
  'uniqueAuthors',(SELECT count(DISTINCT author_party_id) FROM event_moment WHERE event_id=event_key),
  'moments',(SELECT count(*) FROM event_moment WHERE event_id=event_key));
END $$;

CREATE OR REPLACE FUNCTION event_memory_open(event_key bigint,actor bigint,delivery_key uuid)
RETURNS jsonb LANGUAGE plpgsql AS $$
BEGIN
 IF NOT interaction_actor_live(actor) OR NOT interaction_event_access(event_key,actor) THEN RETURN jsonb_build_object('error','not_found'); END IF;
 UPDATE event_memory_delivery SET opened_at=coalesce(opened_at,now())
 WHERE id=delivery_key AND event_id=event_key AND party_id=actor;
 -- Identical response even for an unrelated invitation ID; never an access grant.
 RETURN jsonb_build_object('recorded',true);
END $$;

CREATE OR REPLACE FUNCTION event_memory_record_publication() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE author_id bigint;
BEGIN
 SELECT id INTO author_id FROM party WHERE id::text=NEW.author_party_id;
 IF author_id IS NOT NULL THEN
  PERFORM pg_advisory_xact_lock(hashtextextended('event-memory-recipient:'||author_id,0));
  UPDATE event_memory_delivery SET published_at=coalesce(published_at,now())
    WHERE event_id=NEW.event_id AND party_id=author_id;
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS event_memory_publication ON event_moment;
CREATE TRIGGER event_memory_publication AFTER INSERT ON event_moment
 FOR EACH ROW EXECUTE FUNCTION event_memory_record_publication();

-- Atomic, bounded internal delivery. The inbox insert IS delivery, never an
-- assertion about email/push, opened/read state, or human engagement.
CREATE OR REPLACE FUNCTION event_memory_dispatch_batch(batch_size integer DEFAULT 20)
RETURNS integer LANGUAGE plpgsql AS $$
DECLARE campaign record; candidate record; first_delivery event_memory_delivery%ROWTYPE;
 step_value integer; delivered integer:=0; delivery_key uuid; notice bigint;
BEGIN
 IF batch_size<1 OR batch_size>100 THEN RAISE EXCEPTION 'Invalid batch size'; END IF;
 PERFORM singleton FROM interaction_runtime WHERE singleton AND enabled FOR SHARE;
 IF NOT FOUND THEN RETURN 0; END IF;
 -- This lock also makes competing replica ticks skip, without a long lease.
 IF NOT pg_try_advisory_xact_lock(hashtextextended('event-memory-dispatch',0)) THEN RETURN 0; END IF;
 FOR campaign IN SELECT c.event_id FROM event_memory_campaign c
  WHERE c.enabled AND event_memory_finished(c.event_id) ORDER BY c.event_id FOR SHARE OF c LOOP
  -- Event updates/cancellation and organizer disable cannot race a committed send.
  PERFORM id FROM social_event WHERE id=campaign.event_id FOR SHARE;
  IF NOT event_memory_finished(campaign.event_id) THEN CONTINUE; END IF;
  FOR candidate IN SELECT * FROM event_memory_candidates(campaign.event_id) ORDER BY party_id LOOP
   PERFORM id FROM party WHERE id=candidate.party_id FOR SHARE;
   PERFORM pg_advisory_xact_lock(hashtextextended('event-memory-recipient:'||candidate.party_id,0));
   IF NOT EXISTS(SELECT 1 FROM event_memory_candidates(campaign.event_id) WHERE party_id=candidate.party_id)
    OR NOT EXISTS(SELECT 1 FROM event_memory_preference WHERE party_id=candidate.party_id AND in_app)
    OR NOT interaction_actor_live(candidate.party_id) OR NOT interaction_event_access(campaign.event_id,candidate.party_id)
    OR EXISTS(SELECT 1 FROM event_moment WHERE event_id=campaign.event_id AND author_party_id=candidate.party_id::text)
    OR EXISTS(SELECT 1 FROM event_memory_delivery WHERE party_id=candidate.party_id AND delivered_at>now()-interval '7 days') THEN CONTINUE; END IF;
   SELECT * INTO first_delivery FROM event_memory_delivery
    WHERE event_id=campaign.event_id AND party_id=candidate.party_id AND step=1;
   step_value:=1;
   IF FOUND THEN
    IF first_delivery.published_at IS NOT NULL OR first_delivery.delivered_at>now()-interval '7 days'
     OR EXISTS(SELECT 1 FROM event_memory_delivery WHERE event_id=campaign.event_id AND party_id=candidate.party_id AND step=2) THEN CONTINUE; END IF;
    step_value:=2;
   END IF;
   delivery_key:=gen_random_uuid();
   INSERT INTO event_memory_delivery(id,event_id,party_id,step,evidence)
    VALUES(delivery_key,campaign.event_id,candidate.party_id,step_value,candidate.evidence)
    ON CONFLICT(event_id,party_id,step) DO NOTHING;
   IF NOT FOUND THEN CONTINUE; END IF;
   INSERT INTO notification(recipient_party_id,notif_type,title,body,target_type,target_id,target_key,is_read)
    VALUES(candidate.party_id,CASE step_value WHEN 1 THEN 'event.memory.invitation' ELSE 'event.memory.reminder' END,
      '¡Compartamos los mejores momentos!',
      CASE candidate.evidence WHEN 'verified_attendance' THEN 'Gracias por asistir. Comparte tus recuerdos y descubre los momentos de la comunidad.'
       WHEN 'professional' THEN 'Comparte los recuerdos que quieras aportar a este evento y descubre a su comunidad.'
       ELSE 'El evento que te interesó ya terminó. Si participaste, comparte tus recuerdos con la comunidad.' END,
      'event_memories',campaign.event_id,delivery_key::text,false) RETURNING id INTO notice;
   UPDATE event_memory_delivery SET notification_id=notice WHERE id=delivery_key;
   delivered:=delivered+1;
   IF delivered>=batch_size THEN RETURN delivered; END IF;
  END LOOP;
 END LOOP;
 RETURN delivered;
END $$;

CREATE OR REPLACE FUNCTION interaction_notification_visible(notification_key bigint,recipient bigint)
RETURNS boolean LANGUAGE sql STABLE AS $$
  SELECT CASE WHEN EXISTS(SELECT 1 FROM notification WHERE id=notification_key
    AND (notif_type IN ('event.memory.invitation','event.memory.reminder') OR target_type='event_memories')) THEN
    EXISTS(SELECT 1 FROM event_memory_delivery d JOIN notification n ON n.id=d.notification_id
      JOIN event_memory_preference p ON p.party_id=d.party_id AND p.in_app
      WHERE n.id=notification_key AND n.recipient_party_id=recipient AND d.party_id=recipient
        AND interaction_actor_live(recipient) AND interaction_event_access(d.event_id,recipient)
        AND EXISTS(SELECT 1 FROM event_memory_candidates(d.event_id) c WHERE c.party_id=recipient))
  WHEN EXISTS (SELECT 1 FROM notification WHERE id=notification_key
    AND (notif_type='chat.message' OR target_type='chat_thread')) THEN
    EXISTS (SELECT 1 FROM chat_message_notification c
      JOIN notification n ON n.id=c.notification_id
      JOIN chat_message m ON m.id=c.message_id
      JOIN chat_thread t ON t.id=m.thread_id
      WHERE n.id=notification_key AND n.recipient_party_id=recipient
        AND recipient IN (t.dm_party_a,t.dm_party_b) AND m.sender_party_id<>recipient
        AND (NOT social_v2_dm_required(t.dm_party_a,t.dm_party_b)
          OR social_v2_dm_allowed(t.dm_party_a,t.dm_party_b)))
  ELSE
    NOT EXISTS(SELECT 1 FROM interaction_notification WHERE notification_id=notification_key)
    OR EXISTS(SELECT 1 FROM interaction_notification n JOIN interaction_notification_actor a USING(notification_id)
      WHERE n.notification_id=notification_key AND n.recipient_id=recipient
        AND CASE WHEN n.event_kind='moderation' THEN
          interaction_moderation_notice_allowed(n.target_id,recipient,a.actor_id,n.comment_id)
          ELSE interaction_notification_allowed(n.target_id,recipient,a.actor_id,n.event_kind) END
        AND (n.comment_id IS NULL OR EXISTS(SELECT 1 FROM interaction_comment c WHERE c.id=n.comment_id
          AND (c.state IN ('visible','deleted') OR n.event_kind='moderation')))
        AND (n.event_kind<>'reaction' OR EXISTS(SELECT 1 FROM interaction_reaction r
          WHERE r.target_id=n.target_id AND r.actor_id=a.actor_id)))
  END
$$;

CREATE OR REPLACE VIEW directory_public_rsvp_event AS
SELECT
  event.id,
  event.title,
  event.description,
  event.start_time,
  event.end_time,
  event.timezone,
  event.price_cents,
  event.currency_id,
  event.capacity,
  event.venue_id,
  venue.name AS venue_name,
  city.id AS city_id,
  coalesce(city.name_es,venue.city) AS city_name,
  country.alpha2 AS country_code,
  city.latitude AS public_latitude,
  city.longitude AS public_longitude,
  event.updated_at,
  CASE WHEN event.metadata IS JSON OBJECT
    AND strpos(event.metadata::jsonb->>'imageUrl',chr(92))=0
    AND (
      (
        event.metadata::jsonb->>'imageUrl' ~* '^https://[^[:space:][:cntrl:]]+$'
        AND split_part(event.metadata::jsonb->>'imageUrl','/',3) NOT LIKE '%@%'
      )
      OR event.metadata::jsonb->>'imageUrl' ~ '^/[^/[:space:][:cntrl:]][^[:space:][:cntrl:]]*$'
    )
    THEN event.metadata::jsonb->>'imageUrl'
    ELSE NULL
  END AS image_url,
  TRUE AS is_public,
  state.code AS workflow_state_code,
  EXISTS (
    SELECT 1 FROM workflow_state_capability rsvp_capability
    WHERE rsvp_capability.state_id=state.id
      AND rsvp_capability.capability_code='rsvp'
      AND rsvp_capability.enabled
  ) AS rsvp_eligible,
  (
    state.code<>'cancelled'
    AND (state.code='completed' OR EXISTS (
      SELECT 1 FROM workflow_state_capability share_capability
      WHERE share_capability.state_id=state.id
        AND share_capability.capability_code='rsvp'
        AND share_capability.enabled
    )
  )) AS public_share_eligible
FROM social_event event
JOIN workflow_state state ON state.id=event.workflow_state_id
LEFT JOIN venue ON venue.id=event.venue_id
LEFT JOIN city_reference city ON city.id=venue.city_id
LEFT JOIN country_reference country ON country.id=coalesce(venue.country_id,city.country_id)
WHERE state.active
  AND coalesce(
    CASE WHEN event.metadata IS JSON OBJECT
      AND event.metadata::jsonb->>'isPublic' IN ('true','false')
      THEN (event.metadata::jsonb->>'isPublic')::boolean
      ELSE FALSE
    END,
    FALSE
  )
  AND (
    state.code='cancelled'
    OR EXISTS (
      SELECT 1 FROM workflow_state_capability public_capability
      WHERE public_capability.state_id=state.id
        AND public_capability.capability_code='public-listable'
        AND public_capability.enabled
    )
  )
  AND NOT EXISTS (
    SELECT 1
    FROM external_event_ref reference
    WHERE reference.event_id=event.id
      AND lower(btrim(reference.source_status))='suppressed'
  );
UPDATE event_memory_migration_state SET
 installed_visibility_digest=md5(pg_get_functiondef('interaction_notification_visible(bigint,bigint)'::regprocedure)),
 installed_view_digest=md5(pg_get_viewdef('directory_public_rsvp_event'::regclass,true)) WHERE id=1;
COMMIT;
