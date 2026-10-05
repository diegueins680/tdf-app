-- Limit synchronous manual projection to its admitted event. The worker wrapper
-- preserves the existing ordered batch behavior. Historical migrations are intact.
BEGIN;
CREATE OR REPLACE FUNCTION operations_process_outbox_batch(
  p_limit INTEGER,
  p_worker TEXT,
  p_event_id UUID
) RETURNS TABLE(processed INTEGER, failed INTEGER, dead_lettered INTEGER)
LANGUAGE plpgsql AS $$
DECLARE
  queued RECORD;
  work_id UUID;
  priority_value TEXT;
  ack_minutes INTEGER;
  mitigation_minutes INTEGER;
  resolution_minutes INTEGER;
  ack_due TIMESTAMPTZ;
  mitigation_due TIMESTAMPTZ;
  resolution_due TIMESTAMPTZ;
  terminal_event BOOLEAN;
  processed_count INTEGER := 0;
  failed_count INTEGER := 0;
  dead_count INTEGER := 0;
BEGIN
  FOR queued IN
    SELECT o.*, e.event_type, e.branch_id, e.source_system, e.source_channel,
      e.correlation_key, e.provider_event_id, e.occurred_at, e.continuous_sla, e.payload
    FROM operations_outbox o
    JOIN operations_domain_event e ON e.id = o.event_id
    WHERE (p_event_id IS NULL OR o.event_id = p_event_id)
      AND o.status IN ('pending', 'processing')
      AND o.next_attempt_at <= now()
      AND (o.locked_at IS NULL OR o.locked_at < now() - interval '5 minutes')
      AND NOT EXISTS (
        SELECT 1 FROM operations_outbox earlier
        WHERE earlier.organization_id = o.organization_id
          AND earlier.aggregate_type = o.aggregate_type
          AND earlier.aggregate_id = o.aggregate_id
          AND earlier.aggregate_sequence < o.aggregate_sequence
          AND earlier.status <> 'processed'
      )
    ORDER BY o.created_at, o.id
    FOR UPDATE OF o SKIP LOCKED
    LIMIT LEAST(GREATEST(p_limit, 1), 500)
  LOOP
    BEGIN
      UPDATE operations_outbox
      SET status = 'processing', locked_at = now(), locked_by = p_worker
      WHERE id = queued.id;

      IF queued.event_type IN ('manual.created','manual.uncorrelated_created') AND
         (queued.correlation_key <> 'manual:' || queued.event_id::text
          OR NOT (queued.payload ? '__manualActorPartyId')
          OR queued.payload->'metadata' IS DISTINCT FROM '{}'::jsonb) THEN
        RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Unbound manual event requires review';
      END IF;

      priority_value := CASE queued.payload->>'priority'
        WHEN 'urgent' THEN 'urgent'
        WHEN 'high' THEN 'high'
        WHEN 'low' THEN 'low'
        ELSE 'normal'
      END;
      ack_minutes := CASE priority_value WHEN 'urgent' THEN 15 WHEN 'high' THEN 60 WHEN 'normal' THEN 240 ELSE 480 END;
      mitigation_minutes := CASE priority_value WHEN 'urgent' THEN 60 ELSE ack_minutes END;
      resolution_minutes := CASE priority_value WHEN 'urgent' THEN 240 WHEN 'high' THEN 480 WHEN 'normal' THEN 1440 ELSE 2400 END;
      terminal_event := COALESCE((queued.payload->'metadata'->>'terminal')::boolean, FALSE);

      IF queued.continuous_sla OR priority_value = 'urgent' THEN
        ack_due := queued.occurred_at + make_interval(mins => ack_minutes);
        mitigation_due := queued.occurred_at + make_interval(mins => mitigation_minutes);
        resolution_due := queued.occurred_at + make_interval(mins => resolution_minutes);
      ELSE
        ack_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, ack_minutes);
        mitigation_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, mitigation_minutes);
        resolution_due := operations_business_deadline(queued.organization_id, queued.branch_id, queued.occurred_at, resolution_minutes);
      END IF;

      INSERT INTO operations_work_item (
        organization_id, branch_id, source_system, source_channel, entity_type, entity_id,
        uncorrelated, correlation_key, external_provider_event_id,
        title_es, title_en, description_es, description_en,
        status, priority, recommended_priority, severity,
        created_at, updated_at, due_at, resolved_at, metadata
      ) VALUES (
        queued.organization_id, queued.branch_id, queued.source_system, queued.source_channel,
        queued.aggregate_type,
        CASE WHEN queued.aggregate_type = 'uncorrelated_inbound' THEN NULL ELSE queued.aggregate_id END,
        queued.aggregate_type = 'uncorrelated_inbound', queued.correlation_key, queued.provider_event_id,
        COALESCE(queued.payload->>'titleEs', queued.event_type),
        COALESCE(queued.payload->>'titleEn', queued.event_type),
        COALESCE(queued.payload->>'descriptionEs', queued.event_type),
        COALESCE(queued.payload->>'descriptionEn', queued.event_type),
        CASE WHEN terminal_event THEN 'resolved' ELSE 'new' END, priority_value, priority_value,
        CASE priority_value WHEN 'urgent' THEN 'error' WHEN 'high' THEN 'warning' ELSE 'info' END,
        queued.occurred_at, now(), resolution_due,
        CASE WHEN terminal_event THEN queued.occurred_at ELSE NULL END,
        COALESCE(queued.payload->'metadata', '{}'::jsonb)
      )
      ON CONFLICT (organization_id, correlation_key) DO UPDATE SET
        entity_type = CASE WHEN queued.event_type = 'communication.whatsapp.received' AND queued.source_channel = 'whatsapp' AND operations_work_item.source_channel = 'whatsapp' AND queued.correlation_key LIKE 'whatsapp:%' AND operations_work_item.entity_type = 'uncorrelated_inbound' AND EXCLUDED.entity_type = 'party' THEN EXCLUDED.entity_type ELSE operations_work_item.entity_type END,
        entity_id = CASE WHEN queued.event_type = 'communication.whatsapp.received' AND queued.source_channel = 'whatsapp' AND operations_work_item.source_channel = 'whatsapp' AND queued.correlation_key LIKE 'whatsapp:%' AND operations_work_item.entity_type = 'uncorrelated_inbound' AND EXCLUDED.entity_type = 'party' THEN EXCLUDED.entity_id ELSE operations_work_item.entity_id END,
        uncorrelated = CASE WHEN queued.event_type = 'communication.whatsapp.received' AND queued.source_channel = 'whatsapp' AND operations_work_item.source_channel = 'whatsapp' AND queued.correlation_key LIKE 'whatsapp:%' AND operations_work_item.entity_type = 'uncorrelated_inbound' AND EXCLUDED.entity_type = 'party' THEN false ELSE operations_work_item.uncorrelated END,
        title_es = EXCLUDED.title_es,
        title_en = EXCLUDED.title_en,
        description_es = EXCLUDED.description_es,
        description_en = EXCLUDED.description_en,
        source_channel = EXCLUDED.source_channel,
        external_provider_event_id = COALESCE(EXCLUDED.external_provider_event_id, operations_work_item.external_provider_event_id),
        recommended_priority = EXCLUDED.recommended_priority,
        priority = CASE
          WHEN operations_work_item.priority_override_reason IS NOT NULL THEN operations_work_item.priority
          WHEN array_position(ARRAY['urgent','high','normal','low'], EXCLUDED.priority) <
               array_position(ARRAY['urgent','high','normal','low'], operations_work_item.priority)
            THEN EXCLUDED.priority
          ELSE operations_work_item.priority
        END,
        status = CASE
          WHEN terminal_event THEN 'resolved'
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN 'new'
          ELSE operations_work_item.status END,
        resolved_at = CASE
          WHEN terminal_event THEN queued.occurred_at
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN NULL
          ELSE operations_work_item.resolved_at END,
        archived_at = CASE
          WHEN terminal_event THEN NULL
          WHEN operations_work_item.status IN ('resolved', 'archived') THEN NULL
          ELSE operations_work_item.archived_at END,
        due_at = CASE WHEN operations_work_item.status IN ('resolved', 'archived') THEN EXCLUDED.due_at ELSE operations_work_item.due_at END,
        metadata = operations_work_item.metadata || EXCLUDED.metadata,
        updated_at = now(),
        version = operations_work_item.version + 1
      WHERE operations_work_item.branch_id IS NOT DISTINCT FROM EXCLUDED.branch_id
        AND (operations_work_item.entity_type = EXCLUDED.entity_type
          OR (queued.event_type = 'communication.whatsapp.received' AND queued.source_channel = 'whatsapp' AND operations_work_item.source_channel = 'whatsapp' AND queued.correlation_key LIKE 'whatsapp:%'
            AND operations_work_item.entity_type IN ('uncorrelated_inbound','party')
            AND EXCLUDED.entity_type IN ('uncorrelated_inbound','party')))
      RETURNING id INTO work_id;
      IF work_id IS NULL THEN
        RAISE EXCEPTION USING ERRCODE='23514', MESSAGE='Projection key belongs to a different scope or domain';
      END IF;

      INSERT INTO operations_work_item_event (
        organization_id, work_item_id, domain_event_id, event_type,
        body_es, body_en, metadata, occurred_at
      ) VALUES (
        queued.organization_id, work_id, queued.event_id, queued.event_type,
        COALESCE(queued.payload->>'descriptionEs', queued.event_type),
        COALESCE(queued.payload->>'descriptionEn', queued.event_type),
        COALESCE(queued.payload->'metadata', '{}'::jsonb), queued.occurred_at
      ) ON CONFLICT (domain_event_id) DO NOTHING;

      INSERT INTO operations_sla_timer (
        organization_id, work_item_id, phase, starts_at, due_at, continuous_elapsed
      ) VALUES
        (queued.organization_id, work_id, 'acknowledge', queued.occurred_at, ack_due, queued.continuous_sla OR priority_value = 'urgent'),
        (queued.organization_id, work_id, 'mitigate', queued.occurred_at, mitigation_due, queued.continuous_sla OR priority_value = 'urgent'),
        (queued.organization_id, work_id, 'resolve', queued.occurred_at, resolution_due, queued.continuous_sla OR priority_value = 'urgent')
      ON CONFLICT (work_item_id, phase) DO NOTHING;

      INSERT INTO operations_stream_event (
        organization_id, branch_id, event_type, work_item_id, payload
      ) VALUES (
        queued.organization_id, queued.branch_id, 'work_item.updated', work_id,
        jsonb_build_object('workItemId', work_id, 'domainEventId', queued.event_id)
      );

      INSERT INTO operations_admin_audit (
        organization_id, branch_id, acting_role, source_client, action,
        target_entity_type, target_entity_id, new_value, request_id, correlation_id
      ) VALUES (
        queued.organization_id, queued.branch_id, 'system', p_worker, 'project_domain_event',
        'operations_work_item', work_id::text,
        jsonb_build_object('domainEventId', queued.event_id), queued.id::text, queued.correlation_key
      );

      UPDATE operations_outbox
      SET status = 'processed', processed_at = now(), locked_at = NULL, locked_by = NULL,
          last_error = NULL
      WHERE id = queued.id;
      processed_count := processed_count + 1;
    EXCEPTION WHEN OTHERS THEN
      failed_count := failed_count + 1;
      UPDATE operations_outbox
      SET attempt_count = attempt_count + 1,
          status = CASE WHEN attempt_count + 1 >= 8 THEN 'dead_letter' ELSE 'pending' END,
          next_attempt_at = now() +
            make_interval(secs => LEAST(3600, (2 ^ LEAST(attempt_count + 1, 10))::integer)) +
            make_interval(secs => floor(random() * 15)::integer),
          last_error = SQLSTATE || ': projection failed',
          locked_at = NULL,
          locked_by = NULL
      WHERE id = queued.id;

      IF (SELECT status = 'dead_letter' FROM operations_outbox WHERE id = queued.id) THEN
        dead_count := dead_count + 1;
        INSERT INTO operations_integration_failure (
          organization_id, branch_id, provider, direction, source_record_type,
          source_record_id, failure_code, redacted_summary, retryable, status,
          attempt_count, last_attempt_at
        ) VALUES (
          queued.organization_id, queued.branch_id, 'internal_outbox', 'internal',
          queued.aggregate_type, queued.aggregate_id, SQLSTATE, 'Projection failed; private diagnostics withheld',
          TRUE, 'dead_letter', 8, now()
        );
      END IF;
    END;
  END LOOP;
  RETURN QUERY SELECT processed_count, failed_count, dead_count;
END;
$$;

CREATE OR REPLACE FUNCTION operations_process_outbox_batch(
  p_limit INTEGER DEFAULT 100,
  p_worker TEXT DEFAULT 'operations-worker'
) RETURNS TABLE(processed INTEGER, failed INTEGER, dead_lettered INTEGER)
LANGUAGE sql AS $$
  SELECT * FROM operations_process_outbox_batch(p_limit, p_worker, NULL::uuid)
$$;
COMMIT;
