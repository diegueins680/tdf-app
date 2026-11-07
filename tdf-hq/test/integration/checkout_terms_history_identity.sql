-- LEGAL-CONSENT-001: synthetic full-schema cases; all fixture writes roll back.
BEGIN;
SET LOCAL statement_timeout = '15s';
DO $$
DECLARE course_key bigint; event_key bigint; subject record; policy_key uuid;
  insert_sql text; count_before bigint; current_scope text; rejection text; mutation text;
BEGIN
  INSERT INTO course(slug,title,price_cents,currency,capacity)
    VALUES('terms-history-'||gen_random_uuid()::text,'Synthetic history course',100,'USD',2)
    RETURNING id INTO course_key;
  INSERT INTO social_event(title,start_time,end_time,created_at,updated_at,event_type_id,workflow_state_id)
    VALUES('Synthetic history event',now()+interval '30 days',now()+interval '31 days',now(),now(),
      (SELECT id FROM event_type ORDER BY id LIMIT 1),
      (SELECT s.id FROM workflow_state s JOIN workflow_definition d ON d.id=s.workflow_id
        WHERE d.code='social-event-lifecycle' AND s.code='on_sale'))
    RETURNING id INTO event_key;
  FOR subject IN SELECT * FROM (VALUES
      ('course','course_checkout_policy','course_id','cancellation_policy',course_key,',price_minor',',100'),
      ('event','event_ticket_checkout_policy','event_id','refund_policy',event_key,'','')
    ) AS subjects(kind,table_name,scope_column,policy_column,scope_key,extra_columns,extra_values)
  LOOP
    current_scope := current_setting('tdf.terms_history_scope',true);
    CONTINUE WHEN coalesce(current_scope,'')<>'' AND current_scope<>subject.kind;
    insert_sql := format('INSERT INTO %I(%I,policy_version,currency,terms_version,terms_summary,%I%s) VALUES($1,$2,''USD'',$3,$4,''Original policy''%s) RETURNING id',
      subject.table_name,subject.scope_column,subject.policy_column,subject.extra_columns,subject.extra_values);
    EXECUTE insert_sql INTO policy_key USING subject.scope_key,'history-first','history-v1','Original terms';
    EXECUTE format('DELETE FROM %I WHERE id=$1',subject.table_name) USING policy_key;
    EXECUTE format('SELECT count(*) FROM %I WHERE %I=$1',subject.table_name||'_history',subject.scope_column)
      INTO count_before USING subject.scope_key;
    IF count_before<2 THEN RAISE EXCEPTION 'Historical snapshots did not survive deletion'; END IF;
    -- Every mutation is tested independently by the red/guard-removal runners.
    -- All attempted writes are in subtransactions inside this rolled-back fixture.
    FOREACH mutation IN ARRAY ARRAY['DELETE','UPDATE','TRUNCATE'] LOOP
      CONTINUE WHEN coalesce(current_setting('tdf.terms_history_mutation',true),'')<>''
        AND current_setting('tdf.terms_history_mutation',true)<>mutation;
      BEGIN
        IF mutation='DELETE' THEN
          EXECUTE format('DELETE FROM %I WHERE %I=$1',subject.table_name||'_history',subject.scope_column)
            USING subject.scope_key;
        ELSIF mutation='UPDATE' THEN
          EXECUTE format('UPDATE %I SET snapshot=jsonb_set(snapshot,''{terms_summary}'',to_jsonb(''Changed history''::text)) WHERE %I=$1',
            subject.table_name||'_history',subject.scope_column) USING subject.scope_key;
        ELSE
          EXECUTE format('TRUNCATE TABLE %I',subject.table_name||'_history');
        END IF;
        RAISE EXCEPTION '% history % accepted',subject.kind,mutation;
      EXCEPTION WHEN integrity_constraint_violation THEN
        GET STACKED DIAGNOSTICS rejection = MESSAGE_TEXT;
        IF position('history is append-only' IN rejection)=0 THEN RAISE; END IF;
      END;
      RAISE NOTICE 'PASS: % history % refused',subject.kind,mutation;
    END LOOP;
    EXECUTE format('SELECT count(*) FROM %I WHERE %I=$1',subject.table_name||'_history',subject.scope_column)
      INTO current_scope USING subject.scope_key;
    IF current_scope::bigint<>count_before THEN RAISE EXCEPTION 'Historical snapshots changed'; END IF;
    BEGIN
      EXECUTE insert_sql INTO policy_key USING subject.scope_key,'history-conflict','history-v1','Changed terms';
      RAISE EXCEPTION '% historical reuse accepted after last policy deletion',subject.kind;
    EXCEPTION WHEN integrity_constraint_violation THEN
      GET STACKED DIAGNOSTICS rejection = MESSAGE_TEXT;
      IF position('different historical terms' IN rejection)=0 THEN RAISE; END IF;
    END;
    -- Identical text may reuse the version; this does not prohibit price-only policies.
    EXECUTE insert_sql INTO policy_key USING subject.scope_key,'history-identical','history-v1','Original terms';
    BEGIN
      EXECUTE format('UPDATE %I SET terms_summary=''Changed terms'' WHERE id=$1',subject.table_name) USING policy_key;
      RAISE EXCEPTION '% historical identity changed through the last remaining row',subject.kind;
    EXCEPTION WHEN integrity_constraint_violation THEN
      GET STACKED DIAGNOSTICS rejection = MESSAGE_TEXT;
      IF position('different historical terms' IN rejection)=0 THEN RAISE; END IF;
    END;
    -- A new version permits different text without rewriting earlier snapshots.
    EXECUTE insert_sql INTO policy_key USING subject.scope_key,'history-new','history-v2','Changed terms';
    RAISE NOTICE 'PASS: % deletion/reinsert refusal, identical replay, self-update refusal, new version',subject.kind;
  END LOOP;
END $$;
ROLLBACK;
