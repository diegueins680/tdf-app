-- Additive canonical interactions. Install after the complete social authority
-- compatibility bundle. No public feature is enabled by this migration.
BEGIN;
SET LOCAL lock_timeout = '5s';

CREATE TABLE IF NOT EXISTS interaction_runtime (
  singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
  enabled boolean NOT NULL DEFAULT false,
  activated_once boolean NOT NULL DEFAULT false,
  updated_at timestamptz NOT NULL DEFAULT now()
);
INSERT INTO interaction_runtime(singleton) VALUES(true) ON CONFLICT DO NOTHING;

-- Registry entries require a code-reviewed resolver. They are not SQL fragments
-- or client-asserted ownership/visibility. Unknown entity kinds fail closed.
CREATE TABLE IF NOT EXISTS interaction_entity_kind (
  code text PRIMARY KEY CHECK (code ~ '^[a-z][a-z_]{0,47}$'),
  reactable boolean NOT NULL DEFAULT false,
  commentable boolean NOT NULL DEFAULT false,
  shareable boolean NOT NULL DEFAULT false,
  enabled boolean NOT NULL DEFAULT false
);
INSERT INTO interaction_entity_kind(code,reactable,commentable,shareable,enabled)
SELECT kind,true,true,true,true FROM unnest(ARRAY[
  'club_post','club_memory','event','event_moment','recording','recording_session',
  'record_release','artist_release','classified','directory_profile','artist_update'
]) kind ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS interaction_target (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_kind text NOT NULL REFERENCES interaction_entity_kind(code),
  entity_key text NOT NULL CHECK (length(entity_key) BETWEEN 1 AND 128),
  comment_policy text NOT NULL DEFAULT 'everyone'
    CHECK (comment_policy IN ('everyone','followers','mentioned','off')),
  version bigint NOT NULL DEFAULT 1 CHECK (version > 0),
  retired_at timestamptz,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE(entity_kind,entity_key)
);
CREATE TABLE IF NOT EXISTS interaction_target_mention (
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  PRIMARY KEY(target_id,party_id)
);

-- Preserve existing catalog identities and historical reactions; selectable
-- defaults are a projection, not a second reaction enumeration.
CREATE TABLE IF NOT EXISTS interaction_reaction_choice (
  reaction_type_id uuid PRIMARY KEY REFERENCES content_reaction_type(id),
  default_order smallint UNIQUE CHECK(default_order BETWEEN 0 AND 15)
);
INSERT INTO content_reaction_type(id,catalog_id,code,emoji,name_es,name_en,
  sort_order,active,workflow_state_id)
SELECT '50900000-0000-4000-8000-000000000006',catalog_id,'like','👍','Me gusta','Like',
  60,true,workflow_state_id FROM content_reaction_type WHERE code='fire'
ON CONFLICT(code) DO NOTHING;
INSERT INTO interaction_reaction_choice(reaction_type_id,default_order)
SELECT t.id,v.position FROM (VALUES('like',0),('heart',1),('fire',2),('clap',3)) v(code,position)
JOIN content_reaction_type t USING(code) ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS interaction_comment (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  author_id bigint REFERENCES party(id) ON DELETE RESTRICT,
  parent_id uuid,
  root_id uuid NOT NULL,
  depth integer NOT NULL DEFAULT 0 CHECK(depth BETWEEN 0 AND 1000),
  body text NOT NULL CHECK(length(body)<=4096),
  state text NOT NULL DEFAULT 'visible'
    CHECK(state IN ('visible','deleted','hidden','removed')),
  version bigint NOT NULL DEFAULT 1 CHECK(version>0),
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  edited_at timestamptz,
  legacy_kind text,
  legacy_key text,
  UNIQUE(id,target_id),
  UNIQUE(legacy_kind,legacy_key),
  FOREIGN KEY(parent_id,target_id) REFERENCES interaction_comment(id,target_id)
    ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY(root_id,target_id) REFERENCES interaction_comment(id,target_id)
    ON DELETE RESTRICT DEFERRABLE INITIALLY DEFERRED,
  CHECK ((parent_id IS NULL AND root_id=id AND depth=0)
      OR (parent_id IS NOT NULL AND parent_id<>id AND root_id<>id AND depth>0)),
  CHECK ((legacy_kind IS NULL)=(legacy_key IS NULL)),
  CHECK ((state IN ('visible','hidden') AND length(btrim(body))>0) OR (state IN ('deleted','removed') AND body=''))
);
CREATE INDEX IF NOT EXISTS interaction_comment_roots
  ON interaction_comment(target_id,created_at,id) WHERE parent_id IS NULL;
CREATE INDEX IF NOT EXISTS interaction_comment_replies
  ON interaction_comment(target_id,root_id,created_at,id) WHERE parent_id IS NOT NULL;
CREATE INDEX IF NOT EXISTS interaction_comment_parent ON interaction_comment(parent_id,id);
CREATE INDEX IF NOT EXISTS interaction_comment_author ON interaction_comment(author_id,created_at DESC);

CREATE OR REPLACE FUNCTION interaction_guard_comment_structure() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE p interaction_comment%ROWTYPE;
BEGIN
  IF TG_OP='UPDATE' AND (NEW.id,NEW.target_id,NEW.author_id,NEW.parent_id,NEW.root_id,NEW.depth,NEW.created_at)
    IS DISTINCT FROM (OLD.id,OLD.target_id,OLD.author_id,OLD.parent_id,OLD.root_id,OLD.depth,OLD.created_at)
  THEN RAISE EXCEPTION 'Immutable discussion identity' USING ERRCODE='23514'; END IF;
  IF TG_OP='INSERT' AND NEW.parent_id IS NOT NULL THEN
    SELECT * INTO p FROM interaction_comment WHERE id=NEW.parent_id AND target_id=NEW.target_id FOR SHARE;
    IF NOT FOUND OR NEW.root_id<>p.root_id OR NEW.depth<>p.depth+1 THEN
      RAISE EXCEPTION 'Invalid discussion parent' USING ERRCODE='23514';
    END IF;
  END IF;
  RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS interaction_comment_structure ON interaction_comment;
CREATE TRIGGER interaction_comment_structure BEFORE INSERT OR UPDATE ON interaction_comment
FOR EACH ROW EXECUTE FUNCTION interaction_guard_comment_structure();

-- Attachment identity is reserved independently of text/threads. Upload support
-- is deliberately not exposed until its existing scan/visibility rules are wired.
CREATE TABLE IF NOT EXISTS interaction_comment_attachment (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  comment_id uuid NOT NULL REFERENCES interaction_comment(id) ON DELETE RESTRICT,
  asset_id uuid NOT NULL,
  position smallint NOT NULL CHECK(position BETWEEN 0 AND 9),
  UNIQUE(comment_id,position)
);
CREATE TABLE IF NOT EXISTS interaction_comment_mention (
  comment_id uuid NOT NULL REFERENCES interaction_comment(id) ON DELETE RESTRICT,
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  start_offset integer NOT NULL CHECK(start_offset>=0),
  end_offset integer NOT NULL CHECK(end_offset>start_offset),
  PRIMARY KEY(comment_id,start_offset),
  UNIQUE(comment_id,party_id,start_offset)
);
CREATE INDEX IF NOT EXISTS interaction_mentions_recipient
  ON interaction_comment_mention(party_id,comment_id);

CREATE TABLE IF NOT EXISTS interaction_reaction (
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  actor_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  reaction_type_id uuid NOT NULL REFERENCES content_reaction_type(id),
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(target_id,actor_id)
);
CREATE INDEX IF NOT EXISTS interaction_reaction_page
  ON interaction_reaction(target_id,reaction_type_id,created_at,actor_id);
CREATE INDEX IF NOT EXISTS interaction_reaction_actor ON interaction_reaction(actor_id,target_id);
-- Counters are transactionally maintained by row triggers. Permission-filtered
-- display counts must subtract excluded actors; these totals are never authority.
CREATE TABLE IF NOT EXISTS interaction_reaction_total (
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  reaction_type_id uuid NOT NULL REFERENCES content_reaction_type(id),
  total bigint NOT NULL DEFAULT 0 CHECK(total>=0),
  PRIMARY KEY(target_id,reaction_type_id)
);
CREATE OR REPLACE FUNCTION interaction_count_reaction() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP IN ('DELETE','UPDATE') THEN
    UPDATE interaction_reaction_total SET total=total-1
      WHERE target_id=OLD.target_id AND reaction_type_id=OLD.reaction_type_id;
    IF NOT FOUND THEN RAISE EXCEPTION 'Missing reaction counter' USING ERRCODE='23514'; END IF;
  END IF;
  IF TG_OP IN ('INSERT','UPDATE') THEN
    INSERT INTO interaction_reaction_total(target_id,reaction_type_id,total)
      VALUES(NEW.target_id,NEW.reaction_type_id,1)
      ON CONFLICT(target_id,reaction_type_id) DO UPDATE SET total=interaction_reaction_total.total+1;
  END IF;
  RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS interaction_reaction_count ON interaction_reaction;
CREATE TRIGGER interaction_reaction_count AFTER INSERT OR UPDATE OF reaction_type_id OR DELETE
ON interaction_reaction FOR EACH ROW EXECUTE FUNCTION interaction_count_reaction();

CREATE TABLE IF NOT EXISTS interaction_subscription (
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  party_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  mode text NOT NULL DEFAULT 'participating' CHECK(mode IN ('all','participating','muted')),
  updated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(target_id,party_id)
);
CREATE TABLE IF NOT EXISTS interaction_notification_preference (
  party_id bigint PRIMARY KEY REFERENCES party(id) ON DELETE RESTRICT,
  reactions boolean NOT NULL DEFAULT true,
  comments boolean NOT NULL DEFAULT true,
  replies boolean NOT NULL DEFAULT true,
  mentions boolean NOT NULL DEFAULT true,
  updated_at timestamptz NOT NULL DEFAULT now()
);
-- A recipient-owned notification remains in the existing inbox. This provenance
-- relation supports current-policy revalidation and a stable exact destination.
CREATE TABLE IF NOT EXISTS interaction_notification (
  notification_id bigint PRIMARY KEY REFERENCES notification(id) ON DELETE CASCADE,
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  comment_id uuid REFERENCES interaction_comment(id) ON DELETE RESTRICT,
  recipient_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  event_kind text NOT NULL CHECK(event_kind IN ('reaction','comment','reply','mention','moderation')),
  dedupe_key text NOT NULL,
  UNIQUE(recipient_id,event_kind,dedupe_key),
  FOREIGN KEY(comment_id,target_id) REFERENCES interaction_comment(id,target_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS interaction_notification_comment_recipient
 ON interaction_notification(recipient_id,comment_id) WHERE event_kind IN ('comment','reply','mention');
CREATE OR REPLACE FUNCTION interaction_guard_notification() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF NOT EXISTS(SELECT 1 FROM notification n WHERE n.id=NEW.notification_id
   AND n.recipient_party_id=NEW.recipient_id
   AND n.target_type=CASE WHEN NEW.comment_id IS NULL THEN 'interaction_target' ELSE 'interaction_comment' END
   AND n.target_key=coalesce(NEW.comment_id,NEW.target_id)::text) THEN
   RAISE EXCEPTION 'Invalid notification provenance' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS interaction_notification_guard ON interaction_notification;
CREATE TRIGGER interaction_notification_guard BEFORE INSERT OR UPDATE ON interaction_notification
FOR EACH ROW EXECUTE FUNCTION interaction_guard_notification();
CREATE TABLE IF NOT EXISTS interaction_notification_actor (
  notification_id bigint NOT NULL REFERENCES interaction_notification(notification_id) ON DELETE CASCADE,
  actor_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  PRIMARY KEY(notification_id,actor_id)
);
CREATE TABLE IF NOT EXISTS interaction_report (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  comment_id uuid NOT NULL,
  reporter_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  reason text NOT NULL CHECK(length(btrim(reason)) BETWEEN 1 AND 1000),
  state text NOT NULL DEFAULT 'open' CHECK(state IN ('open','reviewed','dismissed')),
  created_at timestamptz NOT NULL DEFAULT now(),
  FOREIGN KEY(comment_id,target_id) REFERENCES interaction_comment(id,target_id),
  UNIQUE(comment_id,reporter_id)
);
CREATE INDEX IF NOT EXISTS interaction_report_queue ON interaction_report(state,created_at,id);
CREATE TABLE IF NOT EXISTS interaction_audit (
  id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  target_id uuid REFERENCES interaction_target(id) ON DELETE RESTRICT,
  comment_id uuid REFERENCES interaction_comment(id) ON DELETE RESTRICT,
  actor_id bigint REFERENCES party(id) ON DELETE RESTRICT,
  operation text NOT NULL,
  reason text,
  previous_state text,
  new_state text,
  created_at timestamptz NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS interaction_audit_target ON interaction_audit(target_id,created_at DESC,id);
CREATE TABLE IF NOT EXISTS interaction_request (
  actor_id bigint NOT NULL REFERENCES party(id) ON DELETE RESTRICT,
  request_key uuid NOT NULL,
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  payload_hash text NOT NULL,
  result jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(actor_id,request_key)
);
CREATE INDEX IF NOT EXISTS interaction_request_rate ON interaction_request(actor_id,created_at DESC);
CREATE TABLE IF NOT EXISTS interaction_legacy_mapping (
  legacy_kind text NOT NULL,
  legacy_id text NOT NULL,
  target_id uuid NOT NULL REFERENCES interaction_target(id) ON DELETE RESTRICT,
  comment_id uuid REFERENCES interaction_comment(id) ON DELETE RESTRICT,
  source_value jsonb NOT NULL,
  migrated_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY(legacy_kind,legacy_id)
);
CREATE UNIQUE INDEX IF NOT EXISTS interaction_legacy_comment_alias ON interaction_legacy_mapping(comment_id,legacy_kind) WHERE comment_id IS NOT NULL;
COMMIT;
