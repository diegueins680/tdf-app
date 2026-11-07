-- A populated agreement store contains immutable legal/audit evidence. Keep its
-- guards and require a forward repair; never remove protection to allow edits.
BEGIN;
SET LOCAL lock_timeout = '10s';
DO $$
DECLARE name text; populated boolean; scope text := current_schema();
BEGIN
  FOREACH name IN ARRAY ARRAY['event_agreement','event_agreement_acceptance','event_agreement_version','event_workspace_action','event_workspace_command','event_workspace_plan'] LOOP
    EXECUTE format('LOCK TABLE %I.%I IN ACCESS EXCLUSIVE MODE',scope,name);
  END LOOP;
  FOREACH name IN ARRAY ARRAY['event_agreement','event_agreement_acceptance','event_agreement_version','event_workspace_action','event_workspace_command','event_workspace_plan'] LOOP
    EXECUTE format('SELECT EXISTS (SELECT 1 FROM %I.%I)',scope,name) INTO populated;
    IF populated THEN
      RAISE EXCEPTION 'Event agreement evidence exists; use a forward identity repair' USING ERRCODE='55000';
    END IF;
  END LOOP;
  FOREACH name IN ARRAY ARRAY['event_agreement','event_agreement_acceptance','event_agreement_version','event_workspace_action','event_workspace_command','event_workspace_plan'] LOOP
    EXECUTE format('DROP TRIGGER IF EXISTS identity_archive_reference_guard ON %I.%I',scope,name);
  END LOOP;
END $$;
-- Model registry, canonical functions, tables, FKs and data are unchanged.
COMMIT;
