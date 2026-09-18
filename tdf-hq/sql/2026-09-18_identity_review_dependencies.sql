-- Forward-only repair: the initial migration has already been exercised in staging.
-- Legacy catalog reviewer/approver columns are Party identifiers but have no FKs.
-- Preserve audit history by blocking merges, and reject new archived-party references.
-- Model-declared references cover legacy columns without guessing ownership from names.
BEGIN;
-- BEGIN MODEL PARTY REFERENCES
CREATE OR REPLACE VIEW identity_known_party_reference AS
SELECT * FROM (VALUES
  ('api_token', 'party_id'),
  ('appearance_mode_option', 'approved_by'),
  ('appearance_mode_option', 'created_by'),
  ('appearance_mode_option', 'updated_by'),
  ('artist_enrichment_run', 'requested_artist_id'),
  ('artist_enrichment_suggestion', 'artist_party_id'),
  ('artist_enrichment_suggestion', 'decided_by'),
  ('artist_field_change', 'artist_party_id'),
  ('artist_identity_candidate', 'artist_party_id'),
  ('artist_identity_candidate', 'decided_by'),
  ('artist_inventory_reference', 'artist_party_id'),
  ('artist_media_asset', 'artist_party_id'),
  ('artist_profile_enrichment', 'artist_party_id'),
  ('artist_profile_genre_membership', 'artist_party_id'),
  ('artist_profile', 'artist_party_id'),
  ('artist_promo_slot', 'artist_party_id'),
  ('artist_release', 'artist_party_id'),
  ('artist_research_source', 'artist_party_id'),
  ('artist_tip', 'tipper_party_id'),
  ('attendance', 'party_id'),
  ('audit_log', 'actor_id'),
  ('authored_content', 'approved_by'),
  ('authored_content', 'created_by'),
  ('band_member', 'party_id'),
  ('band', 'party_id'),
  ('booking_type', 'approved_by'),
  ('booking_type', 'created_by'),
  ('booking_type', 'updated_by'),
  ('booking', 'created_by'),
  ('booking', 'engineer_party_id'),
  ('booking', 'party_id'),
  ('campaign_delivery', 'party_id'),
  ('campaign_enrollment', 'party_id'),
  ('catalog_audit_event', 'actor_id'),
  ('catalog_audit_event', 'approver_id'),
  ('catalog_audit_event', 'reviewer_id'),
  ('catalog_backfill_run', 'started_by'),
  ('catalog_dependency_rule', 'approved_by'),
  ('catalog_dependency_rule', 'created_by'),
  ('catalog_import_job', 'requested_by'),
  ('catalog_import_job', 'reviewed_by'),
  ('catalog_import_review_entry', 'resolved_by'),
  ('catalog_merge_operation', 'approved_by'),
  ('catalog_merge_operation', 'requested_by'),
  ('catalog_merge_operation', 'reversed_by'),
  ('catalog_migration_mapping', 'reviewed_by'),
  ('catalog_revision', 'approved_by'),
  ('catalog_revision', 'created_by'),
  ('catalog_revision', 'reviewed_by'),
  ('catalog_scoped_default', 'created_by'),
  ('catalog_slug_alias', 'created_by'),
  ('chat_message', 'sender_party_id'),
  ('chat_thread', 'dm_party_a'),
  ('chat_thread', 'dm_party_b'),
  ('class_package_purchase', 'commissioned_teacher_id'),
  ('class_package_purchase', 'seller_id'),
  ('class_package_purchase', 'student_id'),
  ('class_session', 'student_id'),
  ('class_session', 'teacher_id'),
  ('cms_content', 'created_by'),
  ('commission', 'teacher_id'),
  ('content_type', 'approved_by'),
  ('content_type', 'created_by'),
  ('course_registration_follow_up', 'created_by'),
  ('course_registration_follow_up', 'party_id'),
  ('course_registration_receipt', 'party_id'),
  ('course_registration_receipt', 'uploaded_by'),
  ('course_registration', 'party_id'),
  ('creator_badge', 'party_id'),
  ('currency_conversion_audit', 'user_id'),
  ('editorial_collection', 'approved_by'),
  ('editorial_collection', 'created_by'),
  ('editorial_collection', 'updated_by'),
  ('engagement_event', 'actor_party_id'),
  ('engagement_event', 'target_artist_id'),
  ('event_type', 'approved_by'),
  ('event_type', 'created_by'),
  ('event_type', 'updated_by'),
  ('fan_club_candidacy', 'fan_party_id'),
  ('fan_club_event', 'created_by_party_id'),
  ('fan_club_inbox_message', 'fan_party_id'),
  ('fan_club_inbox_message', 'officer_party_id'),
  ('fan_club_member_profile', 'party_id'),
  ('fan_club_memory_reaction', 'reactor_party_id'),
  ('fan_club_memory_report', 'reporter_id'),
  ('fan_club_officer', 'fan_party_id'),
  ('fan_club_post_reaction', 'reactor_party_id'),
  ('fan_club_post', 'fan_party_id'),
  ('fan_club_vote', 'fan_party_id'),
  ('fan_club', 'artist_party_id'),
  ('fan_follow', 'artist_party_id'),
  ('fan_follow', 'fan_party_id'),
  ('fan_profile_genre_membership', 'fan_party_id'),
  ('fan_profile', 'fan_party_id'),
  ('feature_access_request_history', 'actor_party_id'),
  ('feature_access_requests', 'requester_party_id'),
  ('feature_access_requests', 'reviewer_party_id'),
  ('feature_navigation_preferences', 'party_id'),
  ('feedback_category', 'approved_by'),
  ('feedback_category', 'created_by'),
  ('feedback_category', 'updated_by'),
  ('feedback_severity', 'approved_by'),
  ('feedback_severity', 'created_by'),
  ('feedback_severity', 'updated_by'),
  ('feedback', 'created_by'),
  ('genre', 'approved_by'),
  ('genre', 'created_by'),
  ('genre', 'updated_by'),
  ('google_calendar_config', 'owner_id'),
  ('instrument', 'approved_by'),
  ('instrument', 'created_by'),
  ('instrument', 'updated_by'),
  ('intern_audit_notification_outbox', 'recipient_party_id'),
  ('intern_audit_plan', 'completion_approved_by'),
  ('intern_audit_plan', 'created_by'),
  ('intern_audit_plan', 'proposed_assignee'),
  ('intern_daily_summary', 'author_party_id'),
  ('intern_final_summary', 'approved_by'),
  ('intern_final_summary', 'author_party_id'),
  ('intern_permission_request', 'party_id'),
  ('intern_permission_request', 'reviewed_by'),
  ('intern_profile', 'party_id'),
  ('intern_project', 'created_by'),
  ('intern_task', 'assigned_to'),
  ('intern_task', 'created_by'),
  ('intern_task', 'proposed_assignee'),
  ('intern_test_execution', 'executor_party_id'),
  ('intern_time_entry', 'party_id'),
  ('intern_todo', 'owner_party_id'),
  ('internal_feedback_comment', 'author_party_id'),
  ('internal_feedback_evidence', 'uploaded_by'),
  ('internal_feedback_history', 'actor_party_id'),
  ('internal_feedback_report', 'assigned_to'),
  ('internal_feedback_report', 'reporter_party_id'),
  ('internal_feedback_retest', 'tester_party_id'),
  ('invoice', 'customer_id'),
  ('label_project_note', 'created_by'),
  ('label_project_note', 'updated_by'),
  ('label_track', 'owner_party_id'),
  ('lead_interest', 'party_id'),
  ('live_session_intake', 'created_by'),
  ('live_session_musician', 'party_id'),
  ('navigation_item', 'approved_by'),
  ('navigation_item', 'created_by'),
  ('notification', 'recipient_party_id'),
  ('package_purchase', 'buyer_id'),
  ('party_follow', 'follower_party_id'),
  ('party_follow', 'following_party_id'),
  ('party_radio_presence', 'party_id'),
  ('party_security_role', 'approved_by'),
  ('party_security_role', 'granted_by'),
  ('party_security_role', 'party_id'),
  ('payment_split', 'payer_id'),
  ('payment', 'created_by'),
  ('payment', 'party_id'),
  ('proposal', 'client_party_id'),
  ('radio_auto_stop_option', 'approved_by'),
  ('radio_auto_stop_option', 'created_by'),
  ('radio_auto_stop_option', 'updated_by'),
  ('receipt', 'buyer_party_id'),
  ('record_release', 'approved_by'),
  ('record_release', 'created_by'),
  ('record_release', 'updated_by'),
  ('recording_session', 'approved_by'),
  ('recording_session', 'created_by'),
  ('recording_session', 'updated_by'),
  ('recording', 'approved_by'),
  ('recording', 'created_by'),
  ('recording', 'updated_by'),
  ('role_permission', 'approved_by'),
  ('role_permission', 'granted_by'),
  ('security_audit_event', 'actor_id'),
  ('security_audit_event', 'approver_id'),
  ('security_audit_event', 'party_id'),
  ('security_audit_event', 'reviewer_id'),
  ('security_grant_revision', 'approved_by'),
  ('security_grant_revision', 'created_by'),
  ('security_grant_revision', 'party_id'),
  ('security_grant_revision', 'reviewed_by'),
  ('security_role_assignment_policy', 'approved_by'),
  ('security_role_assignment_policy', 'created_by'),
  ('security_role_assignment_policy', 'updated_by'),
  ('security_role', 'approved_by'),
  ('security_role', 'created_by'),
  ('security_role', 'updated_by'),
  ('service_ad', 'provider_party_id'),
  ('service_category', 'approved_by'),
  ('service_category', 'created_by'),
  ('service_category', 'updated_by'),
  ('service_escrow', 'patron_party_id'),
  ('service_escrow', 'provider_party_id'),
  ('service_offering', 'approved_by'),
  ('service_offering', 'created_by'),
  ('service_offering', 'updated_by'),
  ('service_order', 'artist_id'),
  ('service_order', 'customer_id'),
  ('service_pricing_model', 'approved_by'),
  ('service_pricing_model', 'created_by'),
  ('service_pricing_model', 'updated_by'),
  ('service_resource_selection_mode', 'approved_by'),
  ('service_resource_selection_mode', 'created_by'),
  ('service_resource_selection_mode', 'updated_by'),
  ('service_status_change', 'changed_by'),
  ('social_discovery_review', 'reviewed_by_party_id'),
  ('social_sync_account', 'party_id'),
  ('social_sync_post', 'artist_party_id'),
  ('teacher_availability', 'teacher_id'),
  ('teacher_student', 'student_id'),
  ('teacher_student', 'teacher_id'),
  ('teacher_subject', 'teacher_id'),
  ('trial_assignment', 'teacher_id'),
  ('trial_request', 'assigned_teacher_id'),
  ('trial_request', 'party_id'),
  ('trial_throttle', 'party_id'),
  ('user_credential', 'party_id'),
  ('user_experiment_assignment', 'party_id'),
  ('user_locale_preferences', 'user_id'),
  ('user_onboarding_progress', 'party_id'),
  ('whats_app_message', 'actor_party_id'),
  ('whats_app_message', 'party_id'),
  ('workflow_migration_mapping', 'reviewed_by'),
  ('workflow_transition', 'created_by')
) AS reference(table_name,column_name);
REVOKE ALL ON identity_known_party_reference FROM PUBLIC;
-- END MODEL PARTY REFERENCES
CREATE OR REPLACE FUNCTION identity_party_dependencies(candidate bigint)
RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE item record; n bigint; result jsonb:='[]'::jsonb;
BEGIN
  FOR item IN
    SELECT DISTINCT ns.nspname AS schema_name,cl.relname AS table_name,a.attname AS column_name
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    JOIN pg_namespace ns ON ns.oid=cl.relnamespace
    WHERE ns.nspname='public' AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f'
           AND fk.confrelid='party'::regclass AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$'
        OR EXISTS(SELECT 1 FROM identity_known_party_reference r WHERE r.table_name=cl.relname AND r.column_name=a.attname)
        OR (a.attname IN ('user_id','owner_user_id','claimant_user_id','actor_id','created_by','updated_by','reviewer_id','approver_id','reviewed_by','approved_by')
            AND cl.relname<>'party'))
    ORDER BY 1,2,3
  LOOP
    EXECUTE format('SELECT count(*) FROM %I.%I WHERE %I::text=$1',item.schema_name,item.table_name,item.column_name)
      INTO n USING candidate::text;
    IF n>0 THEN result:=result||jsonb_build_array(jsonb_build_object('table',item.table_name,'column',item.column_name,'count',n)); END IF;
  END LOOP;
  RETURN result;
END $$;

DO $$
DECLARE item record;
BEGIN
  FOR item IN
    SELECT cl.oid::regclass table_name,string_agg(quote_literal(a.attname),',' ORDER BY a.attnum) columns
    FROM pg_attribute a JOIN pg_class cl ON cl.oid=a.attrelid
    WHERE cl.relnamespace='public'::regnamespace AND cl.relkind IN ('r','p') AND a.attnum>0 AND NOT a.attisdropped
      AND cl.relname NOT IN ('identity_contact_request','identity_reconciliation_case','identity_merge_history','identity_party_archive')
      AND (EXISTS(SELECT 1 FROM pg_constraint fk WHERE fk.contype='f' AND fk.confrelid='party'::regclass
           AND fk.conrelid=cl.oid AND a.attnum=ANY(fk.conkey))
        OR a.attname ~ '(^|_)(party_id|party_ref)$'
        OR EXISTS(SELECT 1 FROM identity_known_party_reference r WHERE r.table_name=cl.relname AND r.column_name=a.attname))
    GROUP BY cl.oid ORDER BY cl.oid
  LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %s',item.table_name);
    EXECUTE format('CREATE TRIGGER identity_archive_reference_guard BEFORE INSERT OR UPDATE ON %s FOR EACH ROW EXECUTE FUNCTION identity_reject_archived_reference(%s)',item.table_name,item.columns);
  END LOOP;
END $$;
REVOKE ALL ON FUNCTION identity_party_dependencies(bigint) FROM PUBLIC;
COMMIT;
