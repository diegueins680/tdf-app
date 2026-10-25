BEGIN;
SET LOCAL lock_timeout='5s';
DO $$ DECLARE authority event_memory_moderation_authority%ROWTYPE; helper_signature text; BEGIN
 IF EXISTS(SELECT 1 FROM interaction_report WHERE comment_id IS NULL)
  OR EXISTS(SELECT 1 FROM interaction_audit WHERE operation LIKE 'source.%')
  OR EXISTS(SELECT 1 FROM event_moment WHERE source_moderation_state<>'visible') THEN
  RAISE EXCEPTION 'Retain source moderation authority after evidence; use a forward repair';
 END IF;
 FOR authority IN SELECT * FROM event_memory_moderation_authority LOOP
  IF md5(pg_get_functiondef(authority.signature::regprocedure))<>authority.installed_digest THEN
   RAISE EXCEPTION 'A successor changed source moderation authority; use a forward repair';
  END IF;
  helper_signature:=replace(authority.signature,split_part(authority.signature,'(',1)||'(',split_part(authority.signature,'(',1)||'_before_event_source(');
  IF md5(pg_get_functiondef(to_regprocedure(helper_signature))) IS DISTINCT FROM authority.helper_digest THEN
   RAISE EXCEPTION 'A successor changed the captured source authority; use a forward repair';
  END IF;
  EXECUTE authority.definition;
 END LOOP;
END $$;
-- Retain columns, reports, audit and captured definitions. All source states are
-- visible by the guard above; older functions reject the new commands.
COMMIT;
