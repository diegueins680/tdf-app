-- Consolidation happens atomically at first activation, after all source writers
-- are fenced. Source identities remain available for old URLs and rollback
-- evidence. Only canonical rows accept engagement writes after activation.
BEGIN;
SET LOCAL lock_timeout='5s';
ALTER TABLE interaction_comment DROP CONSTRAINT IF EXISTS interaction_comment_body_check;
ALTER TABLE interaction_comment ADD CONSTRAINT interaction_comment_body_check CHECK(length(body)<=4096);
CREATE TABLE IF NOT EXISTS interaction_legacy_cutover (
 singleton boolean PRIMARY KEY DEFAULT true CHECK(singleton),
 completed_at timestamptz NOT NULL,
 source_counts jsonb NOT NULL,
 migrated_counts jsonb NOT NULL
);
CREATE OR REPLACE FUNCTION interaction_migrate_legacy() RETURNS void LANGUAGE plpgsql AS $$
DECLARE row_value record; target uuid; comment_key uuid; parent_key uuid; root_key uuid;
 parent_depth integer; source_counts jsonb; migrated_counts jsonb;
BEGIN
 IF EXISTS(SELECT 1 FROM interaction_legacy_cutover) THEN RETURN; END IF;
 LOCK TABLE fan_club_post,fan_club_post_reaction,fan_club_memory,fan_club_memory_reaction,
   event_moment,event_moment_comment,event_moment_reaction IN SHARE ROW EXCLUSIVE MODE;
 -- Fail the complete transaction on ambiguous identities; never discard a row
 -- by choosing an arbitrary winner or inventing an author account.
 IF EXISTS(SELECT 1 FROM fan_club_post p JOIN fan_club_post q ON q.id=p.parent_id WHERE p.club_id<>q.club_id)
   OR EXISTS(SELECT 1 FROM fan_club_post p LEFT JOIN fan_club_post q ON q.id=p.parent_id WHERE p.parent_id IS NOT NULL AND q.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_reaction r LEFT JOIN party p ON p.id::text=r.reactor_party_id WHERE p.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_comment c LEFT JOIN party p ON p.id::text=c.author_party_id WHERE c.author_party_id IS NOT NULL AND p.id IS NULL)
   OR EXISTS(SELECT 1 FROM event_moment_reaction GROUP BY moment_id,reactor_party_id HAVING count(*)>1)
 THEN RAISE EXCEPTION 'Legacy interactions require identity/parent reconciliation before activation'; END IF;
 WITH RECURSIVE reachable AS (
   SELECT id,0 AS depth FROM fan_club_post WHERE parent_id IS NULL
   UNION ALL SELECT p.id,r.depth+1 FROM fan_club_post p JOIN reachable r ON p.parent_id=r.id WHERE r.depth<1000
 ) SELECT jsonb_build_object('reachable',count(*),'source',(SELECT count(*) FROM fan_club_post)) INTO source_counts FROM reachable;
 IF source_counts->>'reachable'<>source_counts->>'source' THEN RAISE EXCEPTION 'Legacy reply cycle or depth exceeds supported structure'; END IF;
 SELECT jsonb_build_object('clubReplies',(SELECT count(*) FROM fan_club_post WHERE parent_id IS NOT NULL),
   'momentComments',(SELECT count(*) FROM event_moment_comment),
   'postReactions',(SELECT count(*) FROM fan_club_post_reaction),
   'memoryReactions',(SELECT count(*) FROM fan_club_memory_reaction),
   'momentReactions',(SELECT count(*) FROM event_moment_reaction)) INTO source_counts;
 INSERT INTO interaction_target(entity_kind,entity_key)
 SELECT 'club_post',id::text FROM fan_club_post
 UNION ALL SELECT 'club_memory',id::text FROM fan_club_memory
 UNION ALL SELECT 'event_moment',id::text FROM event_moment
 ON CONFLICT(entity_kind,entity_key) DO NOTHING;
 FOR row_value IN WITH RECURSIVE posts AS (
   SELECT p.*,p.id AS top_id,0 AS nesting FROM fan_club_post p WHERE parent_id IS NULL
   UNION ALL SELECT p.*,r.top_id,r.nesting+1 FROM fan_club_post p JOIN posts r ON p.parent_id=r.id
 ) SELECT * FROM posts WHERE nesting>0 ORDER BY nesting,id LOOP
   SELECT id INTO target FROM interaction_target WHERE entity_kind='club_post' AND entity_key=row_value.top_id::text;
   comment_key:=gen_random_uuid(); parent_key:=NULL; root_key:=comment_key; parent_depth:=-1;
   IF row_value.nesting>1 THEN
     SELECT c.id,c.root_id,c.depth INTO parent_key,root_key,parent_depth FROM interaction_legacy_mapping m
       JOIN interaction_comment c ON c.id=m.comment_id WHERE m.legacy_kind='club_reply' AND m.legacy_id=row_value.parent_id::text;
     IF parent_key IS NULL THEN RAISE EXCEPTION 'Legacy reply parent was not converted'; END IF;
   END IF;
   INSERT INTO interaction_comment(id,target_id,author_id,parent_id,root_id,depth,body,state,created_at,updated_at,edited_at,legacy_kind,legacy_key)
   VALUES(comment_key,target,row_value.fan_party_id,parent_key,root_key,parent_depth+1,row_value.content,
     CASE WHEN row_value.is_hidden THEN 'hidden' ELSE 'visible' END,row_value.created_at,
     coalesce(row_value.updated_at,row_value.created_at),row_value.updated_at,'club_reply',row_value.id::text);
   INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
   VALUES('club_reply',row_value.id::text,target,comment_key,jsonb_build_object('title',row_value.title,'mediaUrls',row_value.media_urls,
     'sourceHash',encode(digest(to_jsonb(row_value)::text,'sha256'),'hex')));
 END LOOP;
 FOR row_value IN SELECT c.*,p.id AS author_id FROM event_moment_comment c LEFT JOIN party p ON p.id::text=c.author_party_id ORDER BY c.id LOOP
   SELECT id INTO target FROM interaction_target WHERE entity_kind='event_moment' AND entity_key=row_value.moment_id::text;
   comment_key:=gen_random_uuid();
   INSERT INTO interaction_comment(id,target_id,author_id,root_id,body,created_at,updated_at,edited_at,legacy_kind,legacy_key)
   VALUES(comment_key,target,row_value.author_id,comment_key,row_value.body,row_value.created_at,row_value.updated_at,
     CASE WHEN row_value.updated_at>row_value.created_at THEN row_value.updated_at END,'moment_comment',row_value.id::text);
   INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,comment_id,source_value)
   VALUES('moment_comment',row_value.id::text,target,comment_key,jsonb_build_object('sourceHash',encode(digest(to_jsonb(row_value)::text,'sha256'),'hex')));
 END LOOP;
 INSERT INTO interaction_reaction(target_id,actor_id,reaction_type_id,created_at,updated_at)
 SELECT t.id,r.reactor_party_id,r.reaction_type_id,r.created_at,r.created_at FROM fan_club_post_reaction r
   JOIN interaction_target t ON t.entity_kind='club_post' AND t.entity_key=r.post_id::text
 UNION ALL SELECT t.id,r.reactor_party_id,r.reaction_type_id,r.created_at,r.created_at FROM fan_club_memory_reaction r
   JOIN interaction_target t ON t.entity_kind='club_memory' AND t.entity_key=r.memory_id::text
 UNION ALL SELECT t.id,p.id,c.id,r.created_at,r.created_at FROM event_moment_reaction r
   JOIN party p ON p.id::text=r.reactor_party_id
   JOIN interaction_target t ON t.entity_kind='event_moment' AND t.entity_key=r.moment_id::text
   JOIN reaction_type old ON old.id=r.reaction_type_id
   JOIN content_reaction_type c ON c.code=CASE old.code WHEN 'love' THEN 'heart' WHEN 'applause' THEN 'clap' ELSE old.code END;
 INSERT INTO interaction_legacy_mapping(legacy_kind,legacy_id,target_id,source_value)
 SELECT 'post_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM fan_club_post_reaction r JOIN interaction_target t ON t.entity_kind='club_post' AND t.entity_key=r.post_id::text
 UNION ALL SELECT 'memory_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM fan_club_memory_reaction r JOIN interaction_target t ON t.entity_kind='club_memory' AND t.entity_key=r.memory_id::text
 UNION ALL SELECT 'moment_reaction',r.id::text,t.id,jsonb_build_object('actorId',r.reactor_party_id,'reactionTypeId',r.reaction_type_id)
 FROM event_moment_reaction r JOIN interaction_target t ON t.entity_kind='event_moment' AND t.entity_key=r.moment_id::text;
 SELECT jsonb_build_object('clubReplies',(SELECT count(*) FROM interaction_comment WHERE legacy_kind='club_reply'),
   'momentComments',(SELECT count(*) FROM interaction_comment WHERE legacy_kind='moment_comment'),
   'postReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_post'),
   'memoryReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='club_memory'),
   'momentReactions',(SELECT count(*) FROM interaction_reaction r JOIN interaction_target t ON t.id=r.target_id WHERE t.entity_kind='event_moment')) INTO migrated_counts;
 IF source_counts<>migrated_counts THEN RAISE EXCEPTION 'Legacy interaction reconciliation mismatch'; END IF;
 INSERT INTO interaction_legacy_cutover(singleton,completed_at,source_counts,migrated_counts) VALUES(true,now(),source_counts,migrated_counts);
END $$;
CREATE OR REPLACE FUNCTION interaction_activate() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_OP='DELETE' THEN RAISE EXCEPTION 'Interaction activation history cannot be deleted'; END IF;
 IF OLD.activated_once AND NOT NEW.activated_once THEN RAISE EXCEPTION 'Interaction activation history cannot be reset'; END IF;
 IF NEW.activated_once AND NOT OLD.activated_once AND NOT NEW.enabled THEN
   RAISE EXCEPTION 'First activation must run the guarded migration';
 END IF;
 IF NEW.enabled AND NOT OLD.activated_once THEN
   PERFORM interaction_migrate_legacy(); NEW.activated_once:=true;
 END IF;
 NEW.updated_at:=now(); RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS interaction_runtime_activation ON interaction_runtime;
CREATE TRIGGER interaction_runtime_activation BEFORE UPDATE OR DELETE ON interaction_runtime FOR EACH ROW EXECUTE FUNCTION interaction_activate();
CREATE OR REPLACE FUNCTION interaction_legacy_write_fence() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF EXISTS(SELECT 1 FROM interaction_runtime WHERE singleton AND activated_once) AND pg_trigger_depth()<=1 THEN
   IF TG_TABLE_NAME<>'fan_club_post' THEN
     RAISE EXCEPTION 'Use canonical interaction commands after activation' USING ERRCODE='55000';
   END IF;
   IF (TG_OP<>'INSERT' AND OLD.parent_id IS NOT NULL) OR (TG_OP<>'DELETE' AND NEW.parent_id IS NOT NULL) THEN
     RAISE EXCEPTION 'Use canonical interaction commands after activation' USING ERRCODE='55000';
   END IF;
 END IF;
 RETURN CASE WHEN TG_OP='DELETE' THEN OLD ELSE NEW END;
END $$;
DO $$ DECLARE relation text; BEGIN
 FOREACH relation IN ARRAY ARRAY['fan_club_post','fan_club_post_reaction','fan_club_memory_reaction','event_moment_reaction','event_moment_comment'] LOOP
   EXECUTE format('DROP TRIGGER IF EXISTS interaction_legacy_fence ON %I',relation);
   EXECUTE format('CREATE TRIGGER interaction_legacy_fence BEFORE INSERT OR UPDATE OR DELETE ON %I FOR EACH ROW EXECUTE FUNCTION interaction_legacy_write_fence()',relation);
 END LOOP;
END $$;
-- Author/admin erasure also scrubs retained source text and media. The audit
-- keeps identities/state transitions, never a copy of the deleted body.
CREATE OR REPLACE FUNCTION interaction_erase_legacy() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF NEW.state IN ('deleted','removed') AND OLD.state NOT IN ('deleted','removed') THEN
   UPDATE interaction_legacy_mapping SET source_value=source_value-'title'-'mediaUrls' WHERE comment_id=NEW.id;
   IF NEW.legacy_kind='club_reply' THEN UPDATE fan_club_post SET content='',title=NULL,media_urls=NULL,is_hidden=true WHERE id::text=NEW.legacy_key;
   ELSIF NEW.legacy_kind='moment_comment' THEN UPDATE event_moment_comment SET body='',author_name='' WHERE id::text=NEW.legacy_key;
   END IF;
 END IF;
 RETURN NULL;
END $$;
DROP TRIGGER IF EXISTS interaction_legacy_erasure ON interaction_comment;
CREATE TRIGGER interaction_legacy_erasure AFTER UPDATE OF state ON interaction_comment FOR EACH ROW EXECUTE FUNCTION interaction_erase_legacy();
CREATE OR REPLACE FUNCTION interaction_audit_immutable() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN RAISE EXCEPTION 'Interaction audit is append only' USING ERRCODE='55000'; END $$;
DROP TRIGGER IF EXISTS interaction_audit_immutable ON interaction_audit;
CREATE TRIGGER interaction_audit_immutable BEFORE UPDATE OR DELETE ON interaction_audit FOR EACH ROW EXECUTE FUNCTION interaction_audit_immutable();
COMMIT;
