-- Reuse the existing scoped-task test seeds and canonical history snapshot.
-- The reduced fixture omitted this real-schema column; completion updates it.
ALTER TABLE event_logistics_activity ADD COLUMN updated_at TIMESTAMPTZ NOT NULL DEFAULT now();
CREATE SCHEMA completion_test;
CREATE FUNCTION completion_test.command(task BIGINT, actor BIGINT, key INTEGER, expected BIGINT,
  reason TEXT DEFAULT 'synthetic completion', correlation TEXT DEFAULT 'completion-test')
RETURNS JSONB LANGUAGE sql AS $$
  SELECT event_operation_complete_task(10,task,actor,raci_command_test.key(key),expected,reason,correlation)
$$;
CREATE FUNCTION completion_test.rows() RETURNS JSONB LANGUAGE sql AS $$
  SELECT jsonb_build_object('canonical',raci_command_test.rows(),
    'dependencies',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_logistics_dependency t),
    'policies',(SELECT jsonb_agg(to_jsonb(t) ORDER BY activity_id) FROM event_operation_task_policy t),
    'overrides',(SELECT jsonb_agg(to_jsonb(t) ORDER BY id) FROM event_operation_task_override t))
$$;
SELECT raci_command_test.seed(n) FROM generate_series(400,405) n;
SELECT raci_command_test.check_that(NOT has_function_privilege('raci_command_untrusted',
  'event_operation_complete_task(bigint,bigint,bigint,uuid,bigint,text,text)','EXECUTE'),
  'completion is not executable by PUBLIC');
