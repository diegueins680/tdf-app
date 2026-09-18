SELECT jsonb_build_object(
 'parties',(SELECT coalesce(jsonb_agg(to_jsonb(p)),'[]') FROM party p),
 'archived_party_ids',(SELECT coalesce(jsonb_agg(party_id),'[]') FROM identity_party_archive),
 'review_cases',(SELECT coalesce(jsonb_agg(jsonb_build_object('member_ids',member_ids,'status',status)),'[]') FROM identity_reconciliation_case),
 'reconciliation',jsonb_build_object(
   'confirmed_groups',(SELECT count(*) FROM identity_reconciliation_case WHERE status IN ('confirmed','applied','reverted') AND reviewed_by IS NOT NULL),
   'awaiting_review',(SELECT count(*) FROM identity_reconciliation_case WHERE status IN ('review','confirmed','reverted')),
   'merges_completed',(SELECT count(*) FROM identity_merge_history),
   'merges_rolled_back',(SELECT count(*) FROM identity_merge_history WHERE reverted_at IS NOT NULL),
   'active_links',(SELECT count(*) FROM identity_complementary_link WHERE revoked_at IS NULL)),
 'credentials',(SELECT jsonb_agg(jsonb_build_object('id',id,'party_id',party_id,'active',active)) FROM user_credential),
 'artists',(SELECT jsonb_agg(jsonb_build_object('party_id',artist_party_id,'spotify_id',spotify_artist_id,'youtube_id',youtube_channel_id)) FROM artist_profile),
 'sources',(SELECT jsonb_agg(to_jsonb(r)) FROM artist_inventory_reference r),
 'profiles',(SELECT jsonb_agg(jsonb_build_object('id',id,'party_id',subject_party_id,'kind',profile_kind,'status',profile_status,'canonical_id',canonical_profile_id)) FROM directory_profile),
 'sync',(SELECT jsonb_agg(jsonb_build_object('party_id',party_id,'platform',platform,'external_id',external_user_id)) FROM social_sync_account),
 'softReferences',(SELECT jsonb_agg(jsonb_build_object('table',c.table_name,'column',c.column_name,'type',c.data_type)) FROM information_schema.columns c WHERE c.table_schema='public' AND c.table_name IN (SELECT table_name FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE') AND (c.column_name ~ '(party|user|customer|buyer|owner|actor|created_by|updated_by|reviewer|approver|reviewed_by|approved_by)' OR c.data_type IN ('json','jsonb') OR EXISTS(SELECT 1 FROM identity_known_party_reference r WHERE r.table_name=c.table_name AND r.column_name=c.column_name)) AND NOT EXISTS (SELECT 1 FROM pg_constraint fk JOIN pg_attribute a ON a.attrelid=fk.conrelid AND a.attnum=ANY(fk.conkey) WHERE fk.contype='f' AND fk.confrelid='party'::regclass AND fk.conrelid=to_regclass('public.'||c.table_name) AND a.attname=c.column_name)));
