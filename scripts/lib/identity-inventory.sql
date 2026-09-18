SELECT jsonb_build_object(
 'parties',(SELECT jsonb_agg(to_jsonb(p)) FROM party p),
 'credentials',(SELECT jsonb_agg(jsonb_build_object('id',id,'party_id',party_id,'active',active)) FROM user_credential),
 'artists',(SELECT jsonb_agg(jsonb_build_object('party_id',artist_party_id,'spotify_id',spotify_artist_id,'youtube_id',youtube_channel_id)) FROM artist_profile),
 'sources',(SELECT jsonb_agg(to_jsonb(r)) FROM artist_inventory_reference r),
 'profiles',(SELECT jsonb_agg(jsonb_build_object('id',id,'party_id',subject_party_id,'kind',profile_kind,'status',profile_status,'canonical_id',canonical_profile_id)) FROM directory_profile),
 'sync',(SELECT jsonb_agg(jsonb_build_object('party_id',party_id,'platform',platform,'external_id',external_user_id)) FROM social_sync_account),
 'softReferences',(SELECT jsonb_agg(jsonb_build_object('table',c.table_name,'column',c.column_name,'type',c.data_type)) FROM information_schema.columns c WHERE c.table_schema='public' AND c.table_name IN (SELECT table_name FROM information_schema.tables WHERE table_schema='public' AND table_type='BASE TABLE') AND (c.column_name ~ '(party|user|customer|buyer|owner|actor|created_by|updated_by)' OR c.data_type IN ('json','jsonb')) AND NOT EXISTS (SELECT 1 FROM pg_constraint fk JOIN pg_attribute a ON a.attrelid=fk.conrelid AND a.attnum=ANY(fk.conkey) WHERE fk.contype='f' AND fk.confrelid='party'::regclass AND fk.conrelid=to_regclass('public.'||c.table_name) AND a.attname=c.column_name)));
