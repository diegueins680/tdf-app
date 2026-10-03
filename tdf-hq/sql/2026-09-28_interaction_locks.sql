BEGIN;
-- Domain rows are locked before rechecking visibility. This composes with normal
-- UPDATE/DELETE, so a concurrent unpublish cannot race an accepted interaction.
CREATE OR REPLACE FUNCTION interaction_lock_source(target uuid) RETURNS void LANGUAGE plpgsql AS $$
DECLARE t interaction_target%ROWTYPE; table_name text; parent_event bigint; state_key uuid; key_type text;
BEGIN
 SELECT * INTO t FROM interaction_target WHERE id=target;
 table_name:=CASE t.entity_kind WHEN 'club_post' THEN 'fan_club_post' WHEN 'club_memory' THEN 'fan_club_memory'
   WHEN 'event' THEN 'social_event' WHEN 'event_moment' THEN 'event_moment'
   WHEN 'recording' THEN 'recording' WHEN 'recording_session' THEN 'recording_session'
   WHEN 'record_release' THEN 'record_release' WHEN 'artist_release' THEN 'artist_release'
   WHEN 'artist_update' THEN 'social_sync_post' WHEN 'classified' THEN 'classified'
   WHEN 'directory_profile' THEN 'directory_profile' END;
 IF table_name IS NULL THEN RETURN; END IF;
 key_type:=CASE WHEN t.entity_kind IN ('recording','recording_session','record_release','classified','directory_profile') THEN 'uuid' ELSE 'bigint' END;
 EXECUTE format('SELECT id FROM %I WHERE id=$1::%s FOR SHARE',table_name,key_type) USING t.entity_key;
 IF t.entity_kind IN ('recording','recording_session','record_release') THEN
   EXECUTE format('SELECT workflow_state_id FROM %I WHERE id=$1::uuid',table_name) INTO state_key USING t.entity_key;
   PERFORM id FROM workflow_state WHERE id=state_key FOR SHARE;
 END IF;
 IF t.entity_kind IN ('artist_release','artist_update') THEN
   PERFORM a.id FROM artist_profile a WHERE a.artist_party_id IN (
     SELECT artist_party_id FROM artist_release WHERE id::text=t.entity_key AND t.entity_kind='artist_release'
     UNION SELECT artist_party_id FROM social_sync_post WHERE id::text=t.entity_key AND t.entity_kind='artist_update') FOR SHARE;
 END IF;
 IF t.entity_kind IN ('club_post','club_memory') THEN
   PERFORM c.id FROM fan_club c WHERE c.id IN (
     SELECT club_id FROM fan_club_post WHERE id::text=t.entity_key AND t.entity_kind='club_post'
     UNION SELECT p.club_id FROM fan_club_memory m JOIN fan_club_member_profile p ON p.id=m.member_profile_id
       WHERE m.id::text=t.entity_key AND t.entity_kind='club_memory') ORDER BY c.id FOR SHARE;
 END IF;
 IF t.entity_kind='event' THEN SELECT id INTO parent_event FROM social_event WHERE id::text=t.entity_key; END IF;
 IF t.entity_kind='event_moment' THEN
   SELECT event_id INTO parent_event FROM event_moment WHERE id::text=t.entity_key;
   PERFORM id FROM social_event WHERE id=parent_event FOR SHARE;
 END IF;
 IF parent_event IS NOT NULL THEN
   PERFORM id FROM event_logistics_member WHERE event_id=parent_event ORDER BY id FOR SHARE;
   PERFORM id FROM workflow_state WHERE id IN (SELECT workflow_state_id FROM social_event WHERE id=parent_event) FOR SHARE;
   PERFORM state_id FROM workflow_state_capability WHERE state_id IN (SELECT workflow_state_id FROM social_event WHERE id=parent_event) FOR SHARE;
   PERFORM id FROM external_event_ref WHERE event_id=parent_event ORDER BY id FOR SHARE;
 END IF;
 IF t.entity_kind IN ('classified','directory_profile') THEN
   PERFORM p.id FROM directory_profile p WHERE p.id::text=t.entity_key OR p.id IN (
     SELECT author_profile_id FROM classified WHERE id::text=t.entity_key) ORDER BY p.id FOR SHARE;
   -- Directory block commands have normal row locks only. Pair-independent
   -- insertion is fenced by locking the referenced profile on both paths.
 END IF;
END $$;
CREATE OR REPLACE FUNCTION interaction_lock_permissions(target uuid,actor bigint) RETURNS void LANGUAGE plpgsql AS $$
DECLARE context jsonb; owner_id bigint; club_id_value bigint;
BEGIN
 context:=interaction_target_context(target,actor); owner_id:=(context->>'ownerId')::bigint;
 SELECT p.club_id INTO club_id_value FROM interaction_target t JOIN fan_club_post p ON p.id::text=t.entity_key
   WHERE t.id=target AND t.entity_kind='club_post';
 IF club_id_value IS NULL THEN
   SELECT p.club_id INTO club_id_value FROM interaction_target t JOIN fan_club_memory m ON m.id::text=t.entity_key
     JOIN fan_club_member_profile p ON p.id=m.member_profile_id WHERE t.id=target AND t.entity_kind='club_memory';
 END IF;
 PERFORM id FROM fan_follow WHERE fan_party_id=actor AND (artist_party_id=owner_id OR artist_party_id IN (
   SELECT artist_party_id FROM fan_club WHERE id=club_id_value)) ORDER BY id FOR SHARE;
 PERFORM id FROM party_follow WHERE follower_party_id=actor AND following_party_id=owner_id ORDER BY id FOR SHARE;
 PERFORM id FROM fan_club_officer WHERE club_id=club_id_value AND fan_party_id=actor ORDER BY id FOR SHARE;
 PERFORM profile_id FROM directory_profile_manager WHERE account_party_id=actor ORDER BY profile_id FOR SHARE;
 PERFORM id FROM party_security_role WHERE party_id=actor ORDER BY id FOR SHARE;
END $$;
-- Existing directory block writers participate in the same account fence. A
-- plain FK lock does not serialize visibility revocation with a discussion write.
CREATE OR REPLACE FUNCTION interaction_directory_block_fence() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE profile_keys uuid[];
BEGIN
 profile_keys:='{}'::uuid[];
 IF TG_OP<>'INSERT' THEN profile_keys:=profile_keys||ARRAY[OLD.blocker_profile_id,OLD.blocked_profile_id]; END IF;
 IF TG_OP<>'DELETE' THEN profile_keys:=profile_keys||ARRAY[NEW.blocker_profile_id,NEW.blocked_profile_id]; END IF;
 PERFORM id FROM party WHERE id IN (SELECT subject_party_id FROM directory_profile WHERE id=ANY(profile_keys)) ORDER BY id FOR UPDATE;
 RETURN CASE WHEN TG_OP='DELETE' THEN OLD ELSE NEW END;
END $$;
DROP TRIGGER IF EXISTS interaction_directory_block_lock ON directory_profile_block;
CREATE TRIGGER interaction_directory_block_lock BEFORE INSERT OR UPDATE OR DELETE ON directory_profile_block
FOR EACH ROW EXECUTE FUNCTION interaction_directory_block_fence();
COMMIT;
