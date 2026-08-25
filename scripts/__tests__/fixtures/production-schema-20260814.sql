-- Schema-only, no-row production baseline captured 2026-08-14 for migration
-- compatibility tests. The dump was created with --no-owner --no-privileges.
--
-- PostgreSQL database dump
--

-- Dumped from database version 17.2 (Ubuntu 17.2-1.pgdg24.04+1)
-- Dumped by pg_dump version 17.7 (Ubuntu 17.7-3.pgdg24.04+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


--
-- Name: vector; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS vector WITH SCHEMA public;


--
-- Name: EXTENSION vector; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION vector IS 'vector data type and ivfflat and hnsw access methods';


--
-- Name: operations_artist_profile_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_artist_profile_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM operations_record_event(
    'artist.registration_requires_review', 'artist_profile', NEW.id::text,
    'artist_profile:' || NEW.id::text, 'web', 'normal',
    'Perfil de artista requiere revisión', 'Artist profile needs review',
    'Revise identidad, permisos y publicación del perfil.',
    'Review identity, permissions, and profile publication.',
    jsonb_build_object('artistPartyId', NEW.artist_party_id, 'terminal', false),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_backfill_batch(text, integer, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_backfill_batch(p_run_key text, p_batch_size integer DEFAULT 500, p_dry_run boolean DEFAULT true) RETURNS TABLE(run_id uuid, eligible bigint, inserted bigint, remaining bigint, run_status text)
    LANGUAGE plpgsql
    AS $$
DECLARE
  org_id UUID := '00000000-0000-4000-8000-000000000001'::uuid;
  current_run_id UUID;
  v_eligible_count BIGINT := 0;
  v_inserted_count BIGINT := 0;
  v_remaining_count BIGINT := 0;
  source RECORD;
BEGIN
  IF btrim(COALESCE(p_run_key, '')) = '' THEN
    RAISE EXCEPTION 'run key is required' USING ERRCODE = '22023';
  END IF;
  IF p_batch_size < 1 OR p_batch_size > 5000 THEN
    RAISE EXCEPTION 'batch size must be between 1 and 5000' USING ERRCODE = '22023';
  END IF;

  INSERT INTO operations_backfill_run (
    organization_id, source_name, run_key, status, dry_run, heartbeat_at
  ) VALUES (org_id, 'operations-v1', p_run_key, 'running', p_dry_run, now())
  ON CONFLICT (organization_id, source_name, run_key, dry_run) DO UPDATE SET
    status = 'running', heartbeat_at = now(), finished_at = NULL
  RETURNING id INTO current_run_id;

  SELECT count(*) INTO v_eligible_count FROM (
    SELECT 'course_registration:' || id::text AS correlation_key FROM course_registration
      WHERE lower(status) IN ('new', 'pending', 'pending_payment', 'awaiting_confirmation', 'waitlisted')
    UNION ALL
    SELECT 'booking:' || id::text FROM booking
      WHERE status::text = 'Tentative' AND ends_at >= now() - interval '1 day'
    UNION ALL
    SELECT 'invoice:' || id::text FROM invoice
      WHERE status::text IN ('Sent', 'PartiallyPaid')
    UNION ALL
    SELECT 'package_purchase:' || id::text FROM package_purchase
      WHERE lower(status) = 'active' AND
        (remaining_units <= 2 OR (expires_at IS NOT NULL AND expires_at <= now() + interval '30 days'))
    UNION ALL
    SELECT 'marketplace_order:' || id::text FROM marketplace_order
      WHERE lower(status) IN ('pending', 'stripe_pending', 'paypal_pending', 'datafast_init', 'payment_failed', 'disputed')
    UNION ALL
    SELECT 'maintenance_ticket:' || id::text FROM maintenance_ticket
      WHERE lower(status) NOT IN ('closed', 'completed', 'resolved')
    UNION ALL
    SELECT 'service_order:' || id::text FROM service_order
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
    UNION ALL
    SELECT 'lead_interest:' || id::text FROM lead_interest
      WHERE lower(status) IN ('open', 'new', 'contacted', 'qualified')
    UNION ALL
    SELECT 'trial_request:' || id::text FROM trial_request
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
    UNION ALL
    SELECT 'proposal:' || id::text FROM proposal
      WHERE lower(status) NOT IN ('accepted', 'rejected', 'expired', 'cancelled')
    UNION ALL
    SELECT 'stock_item:' || id::text FROM stock_item
      WHERE reorder_point IS NOT NULL AND on_hand <= reorder_point
    UNION ALL
    SELECT 'feature_access_request:' || id::text FROM feature_access_requests
      WHERE lower(status) IN ('pending', 'open')
    UNION ALL
    SELECT 'intern_project:' || id::text FROM intern_project
      WHERE lower(status) NOT IN ('completed', 'cancelled', 'archived')
    UNION ALL
    SELECT 'social_event:' || id::text FROM social_event
      WHERE end_time >= now()
  ) candidates
  WHERE NOT EXISTS (
    SELECT 1 FROM operations_domain_event event
    WHERE event.organization_id = org_id
      AND event.correlation_key = candidates.correlation_key
      AND event.payload->'metadata'->>'backfillVersion' = 'operations-v1'
  );

  IF NOT p_dry_run THEN
    FOR source IN
      SELECT entity_type, entity_id, correlation_key, occurred_at,
        priority, title_es, title_en, metadata
      FROM (
        SELECT 'course_registration'::text AS entity_type, id::text AS entity_id,
          'course_registration:' || id::text AS correlation_key, created_at AS occurred_at,
          'high'::text AS priority, 'Inscripción existente requiere atención'::text AS title_es,
          'Existing registration needs attention'::text AS title_en,
          jsonb_build_object('courseSlug', course_slug, 'registrationStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false) AS metadata
        FROM course_registration
        WHERE lower(status) IN ('new', 'pending', 'pending_payment', 'awaiting_confirmation', 'waitlisted')
        UNION ALL
        SELECT 'booking', id::text, 'booking:' || id::text, created_at,
          CASE WHEN starts_at <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Reserva existente requiere revisión', 'Existing reservation needs review',
          jsonb_build_object('bookingStatus', status::text, 'startsAt', starts_at, 'endsAt', ends_at,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM booking WHERE status::text = 'Tentative' AND ends_at >= now() - interval '1 day'
        UNION ALL
        SELECT 'invoice', id::text, 'invoice:' || id::text, created_at,
          CASE WHEN due_date < current_date THEN 'high' ELSE 'normal' END,
          CASE WHEN due_date < current_date THEN 'Factura vencida existente' ELSE 'Factura emitida requiere seguimiento' END,
          CASE WHEN due_date < current_date THEN 'Existing overdue invoice' ELSE 'Issued invoice needs follow-up' END,
          jsonb_build_object('invoiceStatus', status::text, 'amountMinor', total_cents, 'currency', currency,
            'dueDate', due_date, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM invoice WHERE status::text IN ('Sent', 'PartiallyPaid')
        UNION ALL
        SELECT 'package_purchase', id::text, 'package_purchase:' || id::text, purchased_at,
          CASE WHEN remaining_units <= 0 OR expires_at <= now() + interval '7 days' THEN 'high' ELSE 'normal' END,
          'Paquete existente próximo a agotarse o vencer', 'Existing package nearing depletion or expiry',
          jsonb_build_object('remainingUnits', remaining_units, 'expiresAt', expires_at,
            'buyerPartyId', buyer_id, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM package_purchase WHERE lower(status) = 'active' AND
          (remaining_units <= 2 OR (expires_at IS NOT NULL AND expires_at <= now() + interval '30 days'))
        UNION ALL
        SELECT 'marketplace_order', id::text, 'marketplace_order:' || id::text, created_at,
          CASE WHEN lower(status) IN ('payment_failed', 'disputed') THEN 'urgent' ELSE 'high' END,
          'Pedido existente requiere atención', 'Existing marketplace order needs attention',
          jsonb_build_object('orderStatus', status, 'amountMinor', total_usd_cents, 'currency', currency,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM marketplace_order WHERE lower(status) IN
          ('pending', 'stripe_pending', 'paypal_pending', 'datafast_init', 'payment_failed', 'disputed')
        UNION ALL
        SELECT 'maintenance_ticket', id::text, 'maintenance_ticket:' || id::text, opened_at,
          CASE WHEN lower(status) IN ('blocked', 'unsafe') THEN 'urgent' ELSE 'high' END,
          'Mantenimiento existente requiere atención', 'Existing maintenance needs attention',
          jsonb_build_object('assetId', asset_id, 'maintenanceStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM maintenance_ticket WHERE lower(status) NOT IN ('closed', 'completed', 'resolved')
        UNION ALL
        SELECT 'service_order', id::text, 'service_order:' || id::text, created_at,
          CASE WHEN scheduled_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Solicitud de servicio existente requiere seguimiento', 'Existing service request needs follow-up',
          jsonb_build_object('serviceKind', service_kind::text, 'orderStatus', status,
            'amountMinor', price_quoted_cents, 'startsAt', scheduled_start,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM service_order WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
        UNION ALL
        SELECT 'lead_interest', id::text, 'lead_interest:' || id::text, created_at,
          'high', 'Lead existente requiere seguimiento', 'Existing lead needs follow-up',
          jsonb_build_object('partyId', party_id, 'interestType', interest_type, 'leadStatus', status,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM lead_interest WHERE lower(status) IN ('open', 'new', 'contacted', 'qualified')
        UNION ALL
        SELECT 'trial_request', id::text, 'trial_request:' || id::text, created_at,
          CASE WHEN pref1_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
          'Clase de prueba existente requiere coordinación', 'Existing trial lesson needs coordination',
          jsonb_build_object('partyId', party_id, 'subjectId', subject_id, 'startsAt', pref1_start,
            'trialStatus', status, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM trial_request WHERE lower(status) NOT IN ('completed', 'cancelled', 'rejected')
        UNION ALL
        SELECT 'proposal', id::text, 'proposal:' || id::text, created_at,
          CASE WHEN lower(status) = 'sent' THEN 'high' ELSE 'normal' END,
          'Cotización existente requiere seguimiento', 'Existing quote needs follow-up',
          jsonb_build_object('clientPartyId', client_party_id, 'proposalStatus', status,
            'serviceKind', service_kind, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM proposal WHERE lower(status) NOT IN ('accepted', 'rejected', 'expired', 'cancelled')
        UNION ALL
        SELECT 'stock_item', id::text, 'stock_item:' || id::text, now(),
          CASE WHEN on_hand <= 0 THEN 'high' ELSE 'normal' END,
          'Inventario existente requiere reposición', 'Existing inventory needs replenishment',
          jsonb_build_object('onHand', on_hand, 'reorderPoint', reorder_point,
            'timestampBasis', 'backfill_run', 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM stock_item WHERE reorder_point IS NOT NULL AND on_hand <= reorder_point
        UNION ALL
        SELECT 'feature_access_request', id::text, 'feature_access_request:' || id::text, requested_at,
          'normal', 'Solicitud de acceso existente requiere revisión', 'Existing access request needs review',
          jsonb_build_object('requesterPartyId', requester_party_id, 'featureId', feature_id,
            'requestedAction', action, 'backfillVersion', 'operations-v1', 'terminal', false)
        FROM feature_access_requests WHERE lower(status) IN ('pending', 'open')
        UNION ALL
        SELECT 'intern_project', id::text, 'intern_project:' || id::text, created_at,
          CASE WHEN due_at IS NOT NULL AND due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
          'Proyecto existente requiere seguimiento', 'Existing project needs follow-up',
          jsonb_build_object('projectStatus', status, 'dueAt', due_at,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM intern_project WHERE lower(status) NOT IN ('completed', 'cancelled', 'archived')
        UNION ALL
        SELECT 'social_event', id::text, 'social_event:' || id::text, created_at,
          CASE WHEN start_time <= now() + interval '24 hours' THEN 'urgent' ELSE 'normal' END,
          'Evento existente requiere coordinación', 'Existing event needs coordination',
          jsonb_build_object('startsAt', start_time, 'endsAt', end_time, 'venueId', venue_id,
            'backfillVersion', 'operations-v1', 'terminal', false)
        FROM social_event WHERE end_time >= now()
      ) eligible_source
      WHERE NOT EXISTS (
        SELECT 1 FROM operations_domain_event event
        WHERE event.organization_id = org_id
          AND event.correlation_key = eligible_source.correlation_key
          AND event.payload->'metadata'->>'backfillVersion' = 'operations-v1'
      )
      ORDER BY occurred_at, entity_type, entity_id
      LIMIT p_batch_size
    LOOP
      PERFORM operations_record_event(
        'backfill.' || source.entity_type || '.attention_required', source.entity_type,
        source.entity_id, source.correlation_key, 'backfill', source.priority,
        source.title_es, source.title_en,
        'Registro operativo pendiente detectado por backfill; revise el registro fuente.',
        'Pending operational record detected by backfill; review the source record.',
        source.metadata, source.occurred_at, NULL, source.priority = 'urgent'
      );
      v_inserted_count := v_inserted_count + 1;
    END LOOP;
  END IF;

  v_remaining_count := CASE WHEN p_dry_run THEN v_eligible_count ELSE GREATEST(v_eligible_count - v_inserted_count, 0) END;
  UPDATE operations_backfill_run SET
    scanned_count = operations_backfill_run.scanned_count + v_eligible_count,
    eligible_count = operations_backfill_run.eligible_count + v_eligible_count,
    inserted_count = operations_backfill_run.inserted_count + v_inserted_count,
    skipped_count = operations_backfill_run.skipped_count + GREATEST(v_eligible_count - v_inserted_count, 0),
    cursor_value = jsonb_build_object('remaining', v_remaining_count, 'batchSize', p_batch_size)::text,
    heartbeat_at = now(),
    status = CASE WHEN p_dry_run OR v_remaining_count = 0 THEN 'completed' ELSE 'running' END,
    finished_at = CASE WHEN p_dry_run OR v_remaining_count = 0 THEN now() ELSE NULL END
  WHERE id = current_run_id;

  RETURN QUERY SELECT current_run_id, v_eligible_count, v_inserted_count, v_remaining_count,
    CASE WHEN p_dry_run OR v_remaining_count = 0 THEN 'completed'::text ELSE 'running'::text END;
END;
$$;


--
-- Name: operations_booking_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_booking_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.starts_at IS NOT DISTINCT FROM OLD.starts_at
    AND NEW.ends_at IS NOT DISTINCT FROM OLD.ends_at THEN
    RETURN NEW;
  END IF;
  event_name := CASE WHEN TG_OP = 'INSERT' THEN 'booking.created' ELSE 'booking.modified' END;
  priority_name := CASE WHEN NEW.starts_at <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END;
  PERFORM operations_record_event(
    event_name, 'booking', NEW.id::text, 'booking:' || NEW.id::text,
    'web', priority_name,
    'Reserva requiere revisión', 'Reservation needs review',
    'Revise horario, recursos y conflictos antes de confirmar.',
    'Review schedule, resources, and conflicts before confirming.',
    jsonb_build_object('bookingStatus', NEW.status::text, 'startsAt', NEW.starts_at, 'endsAt', NEW.ends_at,
      'terminal', NEW.status::text IN ('Completed', 'Cancelled', 'NoShow')),
    COALESCE(NEW.created_at, now()), NULL, NEW.starts_at <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_business_deadline(uuid, uuid, timestamp with time zone, integer); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_business_deadline(p_organization_id uuid, p_branch_id uuid, p_started_at timestamp with time zone, p_business_minutes integer) RETURNS timestamp with time zone
    LANGUAGE plpgsql STABLE
    AS $$
DECLARE
  tz TEXT;
  cursor_at TIMESTAMPTZ := p_started_at;
  local_day DATE;
  day_open TIME;
  day_close TIME;
  open_at TIMESTAMPTZ;
  close_at TIMESTAMPTZ;
  available_minutes INTEGER;
  remaining_minutes INTEGER := GREATEST(p_business_minutes, 0);
  guard_days INTEGER := 0;
BEGIN
  SELECT COALESCE(b.timezone, o.default_timezone) INTO tz
  FROM operations_organization o
  LEFT JOIN operations_branch b ON b.id = p_branch_id AND b.organization_id = o.id
  WHERE o.id = p_organization_id;
  tz := COALESCE(tz, 'America/Guayaquil');

  WHILE remaining_minutes > 0 LOOP
    guard_days := guard_days + 1;
    IF guard_days > 740 THEN
      RAISE EXCEPTION 'business calendar has no available hours';
    END IF;
    local_day := (cursor_at AT TIME ZONE tz)::date;

    SELECT h.opens_at, h.closes_at INTO day_open, day_close
    FROM operations_business_hours h
    WHERE h.organization_id = p_organization_id
      AND (h.branch_id = p_branch_id OR (p_branch_id IS NULL AND h.branch_id IS NULL))
      AND h.iso_weekday = EXTRACT(ISODOW FROM local_day)::smallint
      AND NOT EXISTS (
        SELECT 1 FROM operations_holiday holiday
        WHERE holiday.organization_id = p_organization_id
          AND (holiday.branch_id = p_branch_id OR holiday.branch_id IS NULL)
          AND holiday.holiday_date = local_day
      )
    ORDER BY (h.branch_id IS NOT NULL) DESC
    LIMIT 1;

    IF day_open IS NULL THEN
      cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
      CONTINUE;
    END IF;

    open_at := ((local_day + day_open)::timestamp AT TIME ZONE tz);
    close_at := ((local_day + day_close)::timestamp AT TIME ZONE tz);
    cursor_at := GREATEST(cursor_at, open_at);
    IF cursor_at >= close_at THEN
      cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
      CONTINUE;
    END IF;

    available_minutes := floor(EXTRACT(EPOCH FROM (close_at - cursor_at)) / 60)::integer;
    IF remaining_minutes <= available_minutes THEN
      RETURN cursor_at + make_interval(mins => remaining_minutes);
    END IF;
    remaining_minutes := remaining_minutes - available_minutes;
    cursor_at := ((local_day + 1)::timestamp AT TIME ZONE tz);
  END LOOP;
  RETURN cursor_at;
END;
$$;


--
-- Name: operations_course_registration_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_course_registration_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN
    RETURN NEW;
  END IF;
  event_name := CASE WHEN TG_OP = 'INSERT' THEN 'course_registration.created'
    ELSE 'course_registration.' || lower(NEW.status) END;
  priority_name := CASE WHEN lower(NEW.status) IN ('pending', 'new', 'awaiting_confirmation') THEN 'high' ELSE 'normal' END;
  PERFORM operations_record_event(
    event_name, 'course_registration', NEW.id::text,
    'course_registration:' || NEW.id::text, NEW.source, priority_name,
    'Inscripción de curso requiere atención', 'Course registration needs attention',
    'Revise la inscripción y ejecute la acción empresarial correspondiente.',
    'Review the registration and perform the corresponding business action.',
    jsonb_build_object('courseSlug', NEW.course_slug, 'registrationStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('cancelled', 'rejected')),
    COALESCE(NEW.updated_at, NEW.created_at), NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_enqueue_domain_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_enqueue_domain_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  next_sequence BIGINT;
BEGIN
  INSERT INTO operations_aggregate_sequence (
    organization_id, aggregate_type, aggregate_id, last_sequence
  ) VALUES (
    NEW.organization_id, NEW.aggregate_type, NEW.aggregate_id, 1
  )
  ON CONFLICT (organization_id, aggregate_type, aggregate_id) DO UPDATE
  SET last_sequence = operations_aggregate_sequence.last_sequence + 1,
      updated_at = now()
  RETURNING last_sequence INTO next_sequence;

  INSERT INTO operations_outbox (
    organization_id, event_id, aggregate_type, aggregate_id, aggregate_sequence
  ) VALUES (
    NEW.organization_id, NEW.id, NEW.aggregate_type, NEW.aggregate_id, next_sequence
  ) ON CONFLICT (event_id) DO NOTHING;
  RETURN NEW;
END;
$$;


--
-- Name: operations_feature_access_request_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_feature_access_request_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) NOT IN ('pending', 'open');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'user.access_request.closed' ELSE 'user.access_request.review_required' END,
    'feature_access_request', NEW.id::text, 'feature_access_request:' || NEW.id::text,
    'web', CASE WHEN NEW.expires_at IS NOT NULL AND NEW.expires_at <= now() + interval '1 day' THEN 'high' ELSE 'normal' END,
    'Solicitud de acceso requiere revisión', 'Access request needs review',
    'Revise el alcance y aplique mínimo privilegio.', 'Review scope and apply least privilege.',
    jsonb_build_object('requesterPartyId', NEW.requester_party_id, 'featureId', NEW.feature_id,
      'requestedAction', NEW.action, 'requestStatus', NEW.status, 'terminal', terminal),
    COALESCE(NEW.updated_at, NEW.requested_at), NULL, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_integration_failure_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_integration_failure_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM operations_record_event(
    'integration.failure', 'integration_failure', NEW.id::text,
    'integration_failure:' || NEW.id::text, 'internal',
    CASE WHEN NEW.status = 'dead_letter' THEN 'urgent' ELSE 'high' END,
    'Fallo de integración requiere atención', 'Integration failure needs attention',
    NEW.redacted_summary, NEW.redacted_summary,
    jsonb_build_object('provider', NEW.provider, 'failureCode', NEW.failure_code,
      'retryable', NEW.retryable, 'terminal', false),
    NEW.created_at, NULL, NEW.status = 'dead_letter'
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_intern_project_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_intern_project_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) IN ('completed', 'cancelled', 'archived');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'project.closed' ELSE 'project.action_required' END,
    'intern_project', NEW.id::text, 'intern_project:' || NEW.id::text, 'internal',
    CASE WHEN NEW.due_at IS NOT NULL AND NEW.due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
    'Proyecto requiere seguimiento', 'Project needs follow-up',
    'Revise estado, responsables y fecha objetivo.', 'Review status, owners, and target date.',
    jsonb_build_object('projectStatus', NEW.status, 'dueAt', NEW.due_at, 'terminal', terminal),
    NEW.updated_at, NULL, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_intern_task_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_intern_task_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.assigned_to IS NOT DISTINCT FROM OLD.assigned_to
    AND NEW.due_at IS NOT DISTINCT FROM OLD.due_at THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'project_task.created' ELSE 'project_task.' || lower(NEW.status) END,
    'intern_task', NEW.id::text, 'intern_task:' || NEW.id::text, 'internal',
    CASE WHEN NEW.due_at IS NOT NULL AND NEW.due_at <= current_date + 1 THEN 'high' ELSE 'normal' END,
    'Tarea de proyecto requiere acción', 'Project task needs action',
    COALESCE(NEW.description, NEW.title), COALESCE(NEW.description, NEW.title),
    jsonb_build_object('projectId', NEW.project_id, 'assignedPartyId', NEW.assigned_to,
      'dueAt', NEW.due_at, 'taskStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('done', 'completed', 'cancelled')),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_invoice_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_invoice_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  event_name TEXT;
  priority_name TEXT;
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.due_date IS NOT DISTINCT FROM OLD.due_date THEN
    RETURN NEW;
  END IF;
  event_name := CASE
    WHEN NEW.status::text = 'Draft' THEN 'invoice.created'
    WHEN NEW.due_date < current_date AND NEW.status::text NOT IN ('Paid', 'CancelledI') THEN 'invoice.overdue'
    ELSE 'invoice.' || lower(NEW.status::text)
  END;
  priority_name := CASE WHEN event_name = 'invoice.overdue' THEN 'high' ELSE 'normal' END;
  PERFORM operations_record_event(
    event_name, 'invoice', NEW.id::text, 'invoice:' || NEW.id::text,
    'web', priority_name,
    CASE WHEN event_name = 'invoice.overdue' THEN 'Factura vencida' ELSE 'Factura requiere seguimiento' END,
    CASE WHEN event_name = 'invoice.overdue' THEN 'Overdue invoice' ELSE 'Invoice needs follow-up' END,
    'Revise el estado legal, de entrega y de pago sin reescribir documentos emitidos.',
    'Review legal, delivery, and payment status without rewriting issued documents.',
    jsonb_build_object('invoiceStatus', NEW.status::text, 'amountMinor', NEW.total_cents, 'currency', NEW.currency, 'dueDate', NEW.due_date,
      'terminal', NEW.status::text IN ('Paid', 'CancelledI')),
    COALESCE(NEW.created_at, now()), NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_lead_interest_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_lead_interest_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'lead.created' ELSE 'lead.' || lower(NEW.status) END,
    'lead_interest', NEW.id::text, 'lead_interest:' || NEW.id::text, NEW.source,
    CASE WHEN lower(NEW.status) IN ('open', 'new') THEN 'high' ELSE 'normal' END,
    'Lead requiere seguimiento', 'Lead needs follow-up',
    COALESCE(NEW.details, 'Contacte al lead y registre el siguiente paso.'),
    COALESCE(NEW.details, 'Contact the lead and record the next step.'),
    jsonb_build_object('partyId', NEW.party_id, 'interestType', NEW.interest_type,
      'leadStatus', NEW.status, 'terminal', lower(NEW.status) IN ('won', 'lost', 'closed', 'cancelled')),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_maintenance_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_maintenance_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'maintenance.opened' ELSE 'maintenance.' || lower(NEW.status) END,
    'maintenance_ticket', NEW.id::text, 'maintenance_ticket:' || NEW.id::text,
    'internal', CASE WHEN lower(NEW.status) IN ('blocked', 'unsafe') THEN 'urgent' ELSE 'high' END,
    'Mantenimiento requiere atención', 'Maintenance needs attention',
    NEW.summary, NEW.summary,
    jsonb_build_object('assetId', NEW.asset_id, 'maintenanceStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('closed', 'completed', 'resolved')),
    NEW.opened_at, NULL, lower(NEW.status) IN ('blocked', 'unsafe')
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_marketplace_order_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_marketplace_order_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'marketplace_order.created' ELSE 'marketplace_order.' || lower(NEW.status) END,
    'marketplace_order', NEW.id::text, 'marketplace_order:' || NEW.id::text,
    'marketplace', CASE WHEN lower(NEW.status) IN ('payment_failed', 'disputed') THEN 'urgent' ELSE 'high' END,
    'Pedido de marketplace requiere atención', 'Marketplace order needs attention',
    'Revise pago, proveedor y cumplimiento del pedido.', 'Review payment, provider, and order fulfillment.',
    jsonb_build_object('orderStatus', NEW.status, 'amountMinor', NEW.total_usd_cents, 'currency', NEW.currency,
      'terminal', lower(NEW.status) IN ('fulfilled', 'completed', 'cancelled', 'refunded')),
    COALESCE(NEW.updated_at, NEW.created_at), NULL, lower(NEW.status) IN ('payment_failed', 'disputed')
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_package_purchase_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_package_purchase_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.remaining_units IS NOT DISTINCT FROM OLD.remaining_units
    AND NEW.expires_at IS NOT DISTINCT FROM OLD.expires_at THEN RETURN NEW; END IF;
  IF lower(NEW.status) = 'active' AND
      (NEW.remaining_units <= 2 OR (NEW.expires_at IS NOT NULL AND NEW.expires_at <= now() + interval '30 days')) THEN
    PERFORM operations_record_event(
      'package.depletion_or_expiry_warning', 'package_purchase', NEW.id::text,
      'package_purchase:' || NEW.id::text, 'internal',
      CASE WHEN NEW.remaining_units <= 0 OR NEW.expires_at <= now() + interval '7 days' THEN 'high' ELSE 'normal' END,
      'Paquete próximo a agotarse o vencer', 'Package nearing depletion or expiry',
      'Contacte al cliente y defina renovación o uso pendiente.',
      'Contact the customer and arrange renewal or remaining use.',
      jsonb_build_object('remainingUnits', NEW.remaining_units, 'expiresAt', NEW.expires_at,
        'buyerPartyId', NEW.buyer_id, 'terminal', false),
      NEW.purchased_at, NULL, FALSE
    );
  END IF;
  RETURN NEW;
END;
$$;


--
-- Name: operations_payment_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_payment_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM operations_record_event(
    'payment.recorded', 'payment', NEW.id::text, 'payment:' || NEW.id::text,
    'web', CASE WHEN NEW.method::text = 'BankTransferM' THEN 'high' ELSE 'normal' END,
    CASE WHEN NEW.method::text = 'BankTransferM' THEN 'Transferencia requiere verificación' ELSE 'Pago registrado' END,
    CASE WHEN NEW.method::text = 'BankTransferM' THEN 'Transfer requires verification' ELSE 'Payment recorded' END,
    'Verifique y concilie el pago con la factura o pedido correspondiente.',
    'Verify and reconcile the payment with the corresponding invoice or order.',
    jsonb_build_object('paymentMethod', NEW.method::text, 'amountMinor', NEW.amount_cents, 'currency', NEW.currency, 'invoiceId', NEW.invoice_id),
    COALESCE(NEW.created_at, NEW.received_at), NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_process_outbox_batch(integer, text); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_process_outbox_batch(p_limit integer DEFAULT 100, p_worker text DEFAULT 'operations-worker'::text) RETURNS TABLE(processed integer, failed integer, dead_lettered integer)
    LANGUAGE plpgsql
    AS $$
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
    WHERE o.status IN ('pending', 'processing')
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
      RETURNING id INTO work_id;

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
          last_error = left(SQLSTATE || ': ' || SQLERRM, 1000),
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
          queued.aggregate_type, queued.aggregate_id, SQLSTATE, left(SQLERRM, 500),
          TRUE, 'dead_letter', 8, now()
        );
      END IF;
    END;
  END LOOP;
  RETURN QUERY SELECT processed_count, failed_count, dead_count;
END;
$$;


--
-- Name: operations_proposal_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_proposal_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE terminal BOOLEAN := lower(NEW.status) IN ('accepted', 'rejected', 'expired', 'cancelled');
BEGIN
  PERFORM operations_record_event(
    CASE WHEN terminal THEN 'proposal.closed' ELSE 'proposal.review_required' END,
    'proposal', NEW.id::text, 'proposal:' || NEW.id::text, 'web',
    CASE WHEN lower(NEW.status) = 'sent' THEN 'high' ELSE 'normal' END,
    'Cotización requiere seguimiento', 'Quote needs follow-up',
    'Revise la cotización, el cliente y el siguiente paso.', 'Review the quote, customer, and next step.',
    jsonb_build_object('clientPartyId', NEW.client_party_id, 'proposalStatus', NEW.status,
      'serviceKind', NEW.service_kind, 'terminal', terminal),
    NEW.updated_at, NULL, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_record_event(text, text, text, text, text, text, text, text, text, text, jsonb, timestamp with time zone, text, boolean); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_record_event(p_event_type text, p_aggregate_type text, p_aggregate_id text, p_correlation_key text, p_source_channel text, p_priority text, p_title_es text, p_title_en text, p_description_es text, p_description_en text, p_metadata jsonb DEFAULT '{}'::jsonb, p_occurred_at timestamp with time zone DEFAULT now(), p_provider_event_id text DEFAULT NULL::text, p_continuous_sla boolean DEFAULT false) RETURNS uuid
    LANGUAGE plpgsql
    AS $$
DECLARE
  org_id UUID := '00000000-0000-4000-8000-000000000001'::uuid;
  branch_id UUID := '00000000-0000-4000-8000-000000000002'::uuid;
  event_id UUID;
  dedup_key TEXT;
  event_source_system TEXT;
BEGIN
  event_source_system := CASE
    WHEN p_provider_event_id IS NULL THEN 'tdf-hq'
    ELSE p_source_channel
  END;
  dedup_key := encode(digest(
    concat_ws('|', p_event_type, p_aggregate_type, p_aggregate_id, p_correlation_key,
      p_occurred_at::text, COALESCE(p_provider_event_id, ''), COALESCE(p_metadata, '{}'::jsonb)::text),
    'sha256'), 'hex');

  INSERT INTO operations_domain_event (
    organization_id, branch_id, event_type, aggregate_type, aggregate_id,
    source_system, source_channel, correlation_key, deduplication_key,
    provider_event_id, occurred_at, continuous_sla, payload
  ) VALUES (
    org_id, branch_id, p_event_type, p_aggregate_type, p_aggregate_id,
    event_source_system, p_source_channel, p_correlation_key, dedup_key,
    p_provider_event_id, p_occurred_at, p_continuous_sla,
    jsonb_strip_nulls(jsonb_build_object(
      'priority', p_priority,
      'titleEs', p_title_es,
      'titleEn', p_title_en,
      'descriptionEs', p_description_es,
      'descriptionEn', p_description_en,
      'metadata', COALESCE(p_metadata, '{}'::jsonb)
    ))
  )
  ON CONFLICT DO NOTHING
  RETURNING id INTO event_id;
  IF event_id IS NULL THEN
    SELECT id INTO event_id
    FROM operations_domain_event AS existing_event
    WHERE existing_event.organization_id = org_id
      AND (
        existing_event.deduplication_key = dedup_key
        OR (p_provider_event_id IS NOT NULL
          AND existing_event.source_system = event_source_system
          AND existing_event.provider_event_id = p_provider_event_id)
      )
    ORDER BY recorded_at
    LIMIT 1;
  END IF;
  IF event_id IS NULL THEN
    RAISE EXCEPTION 'operations event conflict could not be resolved'
      USING ERRCODE = '40001';
  END IF;
  RETURN event_id;
END;
$$;


--
-- Name: operations_registration_receipt_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_registration_receipt_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM operations_record_event(
    'bank_transfer_receipt.uploaded', 'course_registration', NEW.registration_id::text,
    'course_registration:' || NEW.registration_id::text, 'web', 'high',
    'Comprobante de transferencia cargado', 'Bank-transfer receipt uploaded',
    'Verifique el comprobante antes de aplicar el pago.',
    'Verify the receipt before applying the payment.',
    jsonb_build_object('receiptId', NEW.id, 'mimeType', NEW.mime_type),
    NEW.created_at, NULL, FALSE
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_reject_mutation(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_reject_mutation() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  RAISE EXCEPTION '% is append-only', TG_TABLE_NAME USING ERRCODE = '55000';
END;
$$;


--
-- Name: operations_service_order_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_service_order_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.scheduled_start IS NOT DISTINCT FROM OLD.scheduled_start
    AND NEW.scheduled_end IS NOT DISTINCT FROM OLD.scheduled_end THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'service_inquiry.created' ELSE 'service_order.' || lower(NEW.status) END,
    'service_order', NEW.id::text, 'service_order:' || NEW.id::text,
    'web', CASE WHEN NEW.scheduled_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
    'Solicitud de servicio requiere seguimiento', 'Service request needs follow-up',
    COALESCE(NEW.description, NEW.title, 'Revise alcance, cotización, agenda y pago.'),
    COALESCE(NEW.description, NEW.title, 'Review scope, quote, schedule, and payment.'),
    jsonb_build_object('serviceKind', NEW.service_kind::text, 'orderStatus', NEW.status,
      'amountMinor', NEW.price_quoted_cents, 'startsAt', NEW.scheduled_start,
      'terminal', lower(NEW.status) IN ('completed', 'cancelled', 'rejected')),
    NEW.created_at, NULL, NEW.scheduled_start <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_social_event_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_social_event_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NEW.end_time < now() THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'event.production_review_required', 'social_event', NEW.id::text,
    'social_event:' || NEW.id::text, 'web',
    CASE WHEN NEW.start_time <= now() + interval '24 hours' THEN 'urgent' ELSE 'normal' END,
    'Evento requiere coordinación de producción', 'Event needs production coordination',
    'Revise venue, capacidad, tareas y responsables.', 'Review venue, capacity, tasks, and owners.',
    jsonb_build_object('startsAt', NEW.start_time, 'endsAt', NEW.end_time,
      'venueId', NEW.venue_id, 'terminal', false), NEW.updated_at, NULL,
    NEW.start_time <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_social_inbound_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_social_inbound_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE provider_name TEXT := CASE WHEN TG_TABLE_NAME = 'instagram_message' THEN 'instagram' ELSE 'facebook' END;
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'communication.' || provider_name || '.received', 'uncorrelated_inbound', NEW.sender_id,
    provider_name || ':' || NEW.sender_id, provider_name, 'high',
    'Mensaje social requiere respuesta', 'Social message needs a response',
    'Correlacione la identidad si es posible y responda por el canal autorizado.',
    'Correlate the identity when possible and respond through the approved channel.',
    jsonb_build_object('provider', provider_name, 'replyStatus', NEW.reply_status,
      'uncorrelatedIdentity', true, 'terminal', false),
    NEW.created_at, NEW.external_id, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_stock_item_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_stock_item_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE warning BOOLEAN := NEW.reorder_point IS NOT NULL AND NEW.on_hand <= NEW.reorder_point;
BEGIN
  PERFORM operations_record_event(
    CASE WHEN warning THEN 'inventory.reorder_required' ELSE 'inventory.stock_restored' END,
    'stock_item', NEW.id::text, 'stock_item:' || NEW.id::text, 'internal',
    CASE WHEN warning AND NEW.on_hand <= 0 THEN 'high' ELSE 'normal' END,
    CASE WHEN warning THEN 'Inventario requiere reposición' ELSE 'Nivel de inventario restablecido' END,
    CASE WHEN warning THEN 'Inventory requires replenishment' ELSE 'Inventory level restored' END,
    'Revise existencias y necesidades operativas.', 'Review stock and operational requirements.',
    jsonb_build_object('onHand', NEW.on_hand, 'reorderPoint', NEW.reorder_point,
      'terminal', NOT warning), now(), NULL, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_sync_scope_member_from_role(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_sync_scope_member_from_role() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NEW.active = TRUE AND NEW.role::text IN (
    'Admin', 'Manager', 'StudioManager', 'Accounting', 'Reception', 'Teacher',
    'Engineer', 'LiveSessionsProducer', 'Producer', 'AandR', 'Maintenance', 'ReadOnly'
  ) THEN
    INSERT INTO operations_scope_member (organization_id, branch_id, party_id)
    VALUES (
      '00000000-0000-4000-8000-000000000001',
      '00000000-0000-4000-8000-000000000002',
      NEW.party_id
    ) ON CONFLICT (organization_id, branch_id, party_id) DO UPDATE SET
      active = TRUE, updated_at = now();
  END IF;
  RETURN NEW;
END;
$$;


--
-- Name: operations_tick_sla(timestamp with time zone); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_tick_sla(p_now timestamp with time zone DEFAULT now()) RETURNS TABLE(reminders_created integer, breached_created integer)
    LANGUAGE plpgsql
    AS $$
DECLARE
  reminder_count INTEGER := 0;
  breach_count INTEGER := 0;
BEGIN
  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, threshold, 'responsible'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  CROSS JOIN (VALUES (50), (80)) AS thresholds(threshold)
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL
    AND timer.paused_at IS NULL
    AND p_now >= timer.starts_at +
      ((timer.due_at - timer.starts_at) * (thresholds.threshold::numeric / 100))
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;
  GET DIAGNOSTICS reminder_count = ROW_COUNT;

  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, 100, 'manager'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL AND timer.paused_at IS NULL
    AND p_now >= timer.due_at
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;

  INSERT INTO operations_sla_reminder (
    organization_id, work_item_id, timer_id, threshold_percent, target_role
  )
  SELECT timer.organization_id, timer.work_item_id, timer.id, 150, 'admin'
  FROM operations_sla_timer timer
  JOIN operations_work_item item ON item.id = timer.work_item_id
  WHERE item.status NOT IN ('resolved', 'archived')
    AND timer.completed_at IS NULL AND timer.paused_at IS NULL
    AND p_now >= timer.starts_at + ((timer.due_at - timer.starts_at) * 1.5)
  ON CONFLICT (timer_id, threshold_percent, target_role) DO NOTHING;
  GET DIAGNOSTICS breach_count = ROW_COUNT;

  UPDATE operations_work_item item
  SET sla_breached_at = COALESCE(item.sla_breached_at, p_now),
      updated_at = p_now,
      version = version + 1
  WHERE item.sla_breached_at IS NULL
    AND EXISTS (
      SELECT 1 FROM operations_sla_reminder reminder
      WHERE reminder.work_item_id = item.id AND reminder.threshold_percent = 150
    );

  UPDATE operations_sla_timer timer
  SET breached_at = COALESCE(timer.breached_at, p_now)
  WHERE timer.breached_at IS NULL
    AND EXISTS (
      SELECT 1 FROM operations_sla_reminder reminder
      WHERE reminder.timer_id = timer.id AND reminder.threshold_percent = 150
    );

  RETURN QUERY SELECT reminder_count, breach_count;
END;
$$;


--
-- Name: operations_trial_request_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_trial_request_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF TG_OP = 'UPDATE' AND NEW.status IS NOT DISTINCT FROM OLD.status
    AND NEW.assigned_teacher_id IS NOT DISTINCT FROM OLD.assigned_teacher_id THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    CASE WHEN TG_OP = 'INSERT' THEN 'trial_request.created' ELSE 'trial_request.' || lower(NEW.status) END,
    'trial_request', NEW.id::text, 'trial_request:' || NEW.id::text, 'web',
    CASE WHEN NEW.pref1_start <= now() + interval '24 hours' THEN 'urgent' ELSE 'high' END,
    'Clase de prueba requiere coordinación', 'Trial lesson needs coordination',
    'Asigne profesor, horario y sala antes de confirmar.',
    'Assign a teacher, schedule, and room before confirming.',
    jsonb_build_object('partyId', NEW.party_id, 'subjectId', NEW.subject_id,
      'startsAt', NEW.pref1_start, 'trialStatus', NEW.status,
      'terminal', lower(NEW.status) IN ('completed', 'cancelled', 'rejected')),
    NEW.created_at, NULL, NEW.pref1_start <= now() + interval '24 hours'
  );
  RETURN NEW;
END;
$$;


--
-- Name: operations_validate_entity_reference(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_validate_entity_reference() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
  exists_value BOOLEAN;
BEGIN
  IF NEW.uncorrelated THEN RETURN NEW; END IF;
  CASE NEW.entity_type
    WHEN 'course_registration' THEN SELECT EXISTS(SELECT 1 FROM course_registration WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'booking' THEN SELECT EXISTS(SELECT 1 FROM booking WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'invoice' THEN SELECT EXISTS(SELECT 1 FROM invoice WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'payment' THEN SELECT EXISTS(SELECT 1 FROM payment WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'package_purchase' THEN SELECT EXISTS(SELECT 1 FROM package_purchase WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'party' THEN SELECT EXISTS(SELECT 1 FROM party WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'marketplace_order' THEN SELECT EXISTS(SELECT 1 FROM marketplace_order WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'maintenance_ticket' THEN SELECT EXISTS(SELECT 1 FROM maintenance_ticket WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'service_order' THEN SELECT EXISTS(SELECT 1 FROM service_order WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'lead_interest' THEN SELECT EXISTS(SELECT 1 FROM lead_interest WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'trial_request' THEN SELECT EXISTS(SELECT 1 FROM trial_request WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'artist_profile' THEN SELECT EXISTS(SELECT 1 FROM artist_profile WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'intern_task' THEN SELECT EXISTS(SELECT 1 FROM intern_task WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'integration_failure' THEN SELECT EXISTS(SELECT 1 FROM operations_integration_failure WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'feature_access_request' THEN SELECT EXISTS(SELECT 1 FROM feature_access_requests WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'proposal' THEN SELECT EXISTS(SELECT 1 FROM proposal WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'stock_item' THEN SELECT EXISTS(SELECT 1 FROM stock_item WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'intern_project' THEN SELECT EXISTS(SELECT 1 FROM intern_project WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'social_event' THEN SELECT EXISTS(SELECT 1 FROM social_event WHERE id::text = NEW.entity_id) INTO exists_value;
    WHEN 'manual' THEN
      SELECT EXISTS(
        SELECT 1 FROM operations_domain_event
        WHERE aggregate_type = 'manual' AND aggregate_id = NEW.entity_id
      ) INTO exists_value;
    ELSE RAISE EXCEPTION 'unsupported operations entity_type %', NEW.entity_type USING ERRCODE = '23514';
  END CASE;
  IF NOT exists_value THEN
    RAISE EXCEPTION 'operations work item references missing %.%', NEW.entity_type, NEW.entity_id USING ERRCODE = '23503';
  END IF;
  RETURN NEW;
END;
$$;


--
-- Name: operations_whatsapp_inbound_event(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.operations_whatsapp_inbound_event() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF lower(NEW.direction) NOT IN ('inbound', 'received') THEN RETURN NEW; END IF;
  PERFORM operations_record_event(
    'communication.whatsapp.received',
    CASE WHEN NEW.party_id IS NULL THEN 'uncorrelated_inbound' ELSE 'party' END,
    COALESCE(NEW.party_id::text, NEW.sender_id),
    'whatsapp:' || NEW.sender_id, 'whatsapp', 'high',
    'Mensaje de WhatsApp requiere respuesta', 'WhatsApp message needs a response',
    'Revise la conversación y responda por un canal autorizado.',
    'Review the conversation and respond through an approved channel.',
    jsonb_build_object('partyId', NEW.party_id, 'replyStatus', NEW.reply_status,
      'uncorrelatedIdentity', NEW.party_id IS NULL, 'terminal', false),
    NEW.created_at, NEW.external_id, false
  );
  RETURN NEW;
END;
$$;


--
-- Name: trigger_set_timestamp(); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION public.trigger_set_timestamp() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$;


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: academy_lesson; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.academy_lesson (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    microcourse_id uuid NOT NULL,
    day bigint NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL
);


--
-- Name: academy_microcourse; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.academy_microcourse (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug character varying NOT NULL,
    title character varying NOT NULL,
    summary character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: academy_progress; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.academy_progress (
    user_id uuid NOT NULL,
    lesson_id uuid NOT NULL,
    completed_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: academy_user; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.academy_user (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    email character varying NOT NULL,
    role character varying NOT NULL,
    platform character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ad_conversation_example; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ad_conversation_example (
    id bigint NOT NULL,
    ad_id bigint NOT NULL,
    user_message character varying NOT NULL,
    assistant_message character varying NOT NULL,
    tags text[],
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ad_conversation_example_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ad_conversation_example_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ad_conversation_example_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ad_conversation_example_id_seq OWNED BY public.ad_conversation_example.id;


--
-- Name: ad_creative; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ad_creative (
    id bigint NOT NULL,
    campaign_id bigint,
    name character varying NOT NULL,
    channel character varying,
    audience character varying,
    landing_url character varying,
    cta character varying,
    status character varying DEFAULT 'active'::character varying NOT NULL,
    notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    external_id character varying
);


--
-- Name: ad_creative_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ad_creative_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ad_creative_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ad_creative_id_seq OWNED BY public.ad_creative.id;


--
-- Name: api_token; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.api_token (
    id bigint NOT NULL,
    token character varying NOT NULL,
    party_id bigint NOT NULL,
    label character varying,
    active boolean NOT NULL
);


--
-- Name: api_token_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.api_token_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: api_token_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.api_token_id_seq OWNED BY public.api_token.id;


--
-- Name: artist_enrichment_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_enrichment_run (
    id bigint NOT NULL,
    run_key text NOT NULL,
    mode text NOT NULL,
    scope text NOT NULL,
    requested_artist_id bigint,
    status text NOT NULL,
    phase text NOT NULL,
    checkpoint text,
    counters text,
    error_summary text,
    started_at timestamp with time zone NOT NULL,
    heartbeat_at timestamp with time zone NOT NULL,
    finished_at timestamp with time zone,
    CONSTRAINT ck_artist_enrichment_run_mode CHECK ((mode = ANY (ARRAY['dry_run'::text, 'production'::text]))),
    CONSTRAINT ck_artist_enrichment_run_status CHECK ((status = ANY (ARRAY['running'::text, 'completed'::text, 'failed'::text, 'cancelled'::text, 'blocked'::text])))
);


--
-- Name: artist_enrichment_run_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_enrichment_run_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_enrichment_run_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_enrichment_run_id_seq OWNED BY public.artist_enrichment_run.id;


--
-- Name: artist_enrichment_suggestion; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_enrichment_suggestion (
    id bigint NOT NULL,
    artist_party_id bigint,
    inventory_reference_id bigint,
    field_name text NOT NULL,
    current_value text,
    proposed_value text,
    confidence double precision NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    auto_publish boolean DEFAULT false NOT NULL,
    evidence text NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    decided_at timestamp with time zone,
    decided_by bigint,
    decision_note text,
    CONSTRAINT ck_artist_enrichment_suggestion_confidence CHECK (((confidence >= (0)::double precision) AND (confidence <= (1)::double precision))),
    CONSTRAINT ck_artist_enrichment_suggestion_status CHECK ((status = ANY (ARRAY['pending'::text, 'approved'::text, 'rejected'::text, 'superseded'::text, 'auto_applied'::text])))
);


--
-- Name: artist_enrichment_suggestion_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_enrichment_suggestion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_enrichment_suggestion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_enrichment_suggestion_id_seq OWNED BY public.artist_enrichment_suggestion.id;


--
-- Name: artist_field_change; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_field_change (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    suggestion_id bigint,
    field_name text NOT NULL,
    previous_value text,
    new_value text,
    evidence text NOT NULL,
    confidence double precision NOT NULL,
    actor text NOT NULL,
    changed_at timestamp with time zone NOT NULL,
    idempotency_key text NOT NULL,
    CONSTRAINT ck_artist_field_change_confidence CHECK (((confidence >= (0)::double precision) AND (confidence <= (1)::double precision)))
);


--
-- Name: artist_field_change_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_field_change_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_field_change_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_field_change_id_seq OWNED BY public.artist_field_change.id;


--
-- Name: artist_follow; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_follow (
    artist_id bigint NOT NULL,
    follower_party_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: artist_genre; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_genre (
    artist_id bigint NOT NULL,
    genre character varying NOT NULL
);


--
-- Name: artist_identity_candidate; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_identity_candidate (
    id bigint NOT NULL,
    inventory_reference_id bigint NOT NULL,
    artist_party_id bigint,
    provider text NOT NULL,
    external_id text,
    candidate_url text,
    evidence text NOT NULL,
    confidence double precision NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    decided_at timestamp with time zone,
    decided_by bigint,
    decision_note text,
    CONSTRAINT ck_artist_identity_candidate_confidence CHECK (((confidence >= (0)::double precision) AND (confidence <= (1)::double precision))),
    CONSTRAINT ck_artist_identity_candidate_status CHECK ((status = ANY (ARRAY['pending'::text, 'approved'::text, 'rejected'::text, 'superseded'::text])))
);


--
-- Name: artist_identity_candidate_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_identity_candidate_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_identity_candidate_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_identity_candidate_id_seq OWNED BY public.artist_identity_candidate.id;


--
-- Name: artist_inventory_reference; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_inventory_reference (
    id bigint NOT NULL,
    idempotency_key text NOT NULL,
    source_type text NOT NULL,
    source_record_id text NOT NULL,
    original_name text NOT NULL,
    normalized_name text NOT NULL,
    artist_party_id bigint,
    social_artist_id bigint,
    aliases text,
    evidence text,
    confidence double precision,
    disposition text DEFAULT 'discovered'::text NOT NULL,
    first_seen_at timestamp with time zone NOT NULL,
    last_seen_at timestamp with time zone NOT NULL,
    CONSTRAINT ck_artist_inventory_reference_confidence CHECK (((confidence IS NULL) OR ((confidence >= (0)::double precision) AND (confidence <= (1)::double precision))))
);


--
-- Name: artist_inventory_reference_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_inventory_reference_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_inventory_reference_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_inventory_reference_id_seq OWNED BY public.artist_inventory_reference.id;


--
-- Name: artist_media_asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_media_asset (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    asset_kind text NOT NULL,
    source_url text NOT NULL,
    source_attribution text NOT NULL,
    retrieved_at timestamp with time zone NOT NULL,
    source_content_hash text NOT NULL,
    source_width integer NOT NULL,
    source_height integer NOT NULL,
    source_mime_type text NOT NULL,
    source_byte_size bigint NOT NULL,
    content_hash text NOT NULL,
    width integer NOT NULL,
    height integer NOT NULL,
    mime_type text NOT NULL,
    byte_size bigint NOT NULL,
    rights_status text NOT NULL,
    drive_file_id text NOT NULL,
    public_url text NOT NULL,
    parent_asset_id bigint,
    focal_point text,
    idempotency_key text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT ck_artist_media_asset_dimensions CHECK (((width > 0) AND (height > 0))),
    CONSTRAINT ck_artist_media_asset_hash CHECK ((content_hash ~ '^[0-9a-f]{64}([0-9a-f]{64})?$'::text)),
    CONSTRAINT ck_artist_media_asset_mime CHECK ((mime_type = ANY (ARRAY['image/avif'::text, 'image/webp'::text]))),
    CONSTRAINT ck_artist_media_asset_rights CHECK ((rights_status = ANY (ARRAY['authorized'::text, 'licensed'::text]))),
    CONSTRAINT ck_artist_media_asset_size CHECK ((byte_size > 0)),
    CONSTRAINT ck_artist_media_asset_source_dimensions CHECK (((source_width > 0) AND (source_height > 0))),
    CONSTRAINT ck_artist_media_asset_source_hash CHECK ((source_content_hash ~ '^[0-9a-f]{64}([0-9a-f]{64})?$'::text)),
    CONSTRAINT ck_artist_media_asset_source_mime CHECK ((source_mime_type = ANY (ARRAY['image/jpeg'::text, 'image/png'::text, 'image/avif'::text, 'image/webp'::text]))),
    CONSTRAINT ck_artist_media_asset_source_size CHECK ((source_byte_size > 0))
);


--
-- Name: artist_media_asset_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_media_asset_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_media_asset_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_media_asset_id_seq OWNED BY public.artist_media_asset.id;


--
-- Name: artist_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_profile (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    slug character varying,
    bio character varying,
    city character varying,
    hero_image_url character varying,
    spotify_artist_id character varying,
    spotify_url character varying,
    youtube_channel_id character varying,
    youtube_url character varying,
    website_url character varying,
    featured_video_url character varying,
    genres character varying,
    highlights character varying,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone,
    stripe_account_id text,
    country_code text
);


--
-- Name: artist_profile_enrichment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_profile_enrichment (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    official_name text,
    country text,
    instagram_url text,
    social_links text,
    discography text,
    achievements text,
    hero_original_url text,
    hero_square_url text,
    hero_landscape_url text,
    hero_responsive_urls text,
    hero_focal_point text,
    last_verified_at timestamp with time zone,
    confidence double precision,
    review_status text DEFAULT 'unverified'::text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT ck_artist_profile_enrichment_confidence CHECK (((confidence IS NULL) OR ((confidence >= (0)::double precision) AND (confidence <= (1)::double precision)))),
    CONSTRAINT ck_artist_profile_enrichment_status CHECK ((review_status = ANY (ARRAY['unverified'::text, 'pending'::text, 'verified'::text, 'rejected'::text, 'ambiguous'::text])))
);


--
-- Name: artist_profile_enrichment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_profile_enrichment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_profile_enrichment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_profile_enrichment_id_seq OWNED BY public.artist_profile_enrichment.id;


--
-- Name: artist_profile_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_profile_id_seq OWNED BY public.artist_profile.id;


--
-- Name: artist_promo_slot; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_promo_slot (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    day date NOT NULL,
    start_time time without time zone NOT NULL,
    medium text NOT NULL,
    program text NOT NULL,
    interviewer_host text NOT NULL,
    band_members text NOT NULL,
    status text,
    notes text,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: artist_promo_slot_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_promo_slot_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_promo_slot_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_promo_slot_id_seq OWNED BY public.artist_promo_slot.id;


--
-- Name: artist_release; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_release (
    id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    title character varying NOT NULL,
    release_date date,
    description character varying,
    cover_image_url character varying,
    spotify_url character varying,
    youtube_url character varying,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: artist_release_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_release_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_release_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_release_id_seq OWNED BY public.artist_release.id;


--
-- Name: artist_research_source; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_research_source (
    id bigint NOT NULL,
    artist_party_id bigint,
    inventory_reference_id bigint,
    source_url text NOT NULL,
    source_type text NOT NULL,
    retrieved_at timestamp with time zone NOT NULL,
    supported_fields text NOT NULL,
    attribution text,
    content_hash text,
    idempotency_key text NOT NULL,
    CONSTRAINT ck_artist_research_source_owner CHECK (((artist_party_id IS NOT NULL) OR (inventory_reference_id IS NOT NULL)))
);


--
-- Name: artist_research_source_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_research_source_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_research_source_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_research_source_id_seq OWNED BY public.artist_research_source.id;


--
-- Name: artist_tip; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.artist_tip (
    id bigint NOT NULL,
    artist_profile_id bigint NOT NULL,
    tipper_party_id bigint,
    tipper_email text,
    tipper_name text,
    amount_cents integer NOT NULL,
    currency text NOT NULL,
    platform_fee_cents integer NOT NULL,
    stripe_payment_intent_id text,
    status text DEFAULT 'pending'::text NOT NULL,
    message text,
    created_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    CONSTRAINT artist_tip_amount_cents_check CHECK ((amount_cents > 0)),
    CONSTRAINT artist_tip_platform_fee_cents_check CHECK ((platform_fee_cents >= 0)),
    CONSTRAINT artist_tip_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'paid'::text, 'failed'::text, 'refunded'::text])))
);


--
-- Name: artist_tip_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.artist_tip_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: artist_tip_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.artist_tip_id_seq OWNED BY public.artist_tip.id;


--
-- Name: asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.asset (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying NOT NULL,
    category character varying NOT NULL,
    brand character varying,
    model character varying,
    serial_number character varying,
    purchase_date date,
    purchase_price_usd_cents bigint,
    condition character varying DEFAULT 'Good'::character varying NOT NULL,
    status character varying DEFAULT 'Active'::character varying NOT NULL,
    location_id uuid,
    owner character varying DEFAULT 'TDF'::character varying NOT NULL,
    qr_code character varying,
    photo_url character varying,
    notes character varying,
    warranty_expires date,
    maintenance_policy character varying DEFAULT 'None'::character varying NOT NULL,
    next_maintenance_due date
);


--
-- Name: asset_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.asset_audit (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_id uuid NOT NULL,
    at timestamp with time zone DEFAULT now() NOT NULL,
    event character varying NOT NULL,
    detail character varying
);


--
-- Name: asset_checkout; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.asset_checkout (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_id uuid NOT NULL,
    target_kind character varying NOT NULL,
    target_session_id uuid,
    target_party_ref character varying,
    target_room_id uuid,
    checked_out_by_ref character varying NOT NULL,
    checked_out_at timestamp with time zone DEFAULT now() NOT NULL,
    due_at timestamp with time zone,
    condition_out character varying,
    photo_drive_file_id character varying,
    returned_at timestamp with time zone,
    condition_in character varying,
    notes character varying,
    disposition character varying DEFAULT 'Loan'::character varying NOT NULL,
    terms_and_conditions character varying,
    holder_email character varying,
    holder_phone character varying,
    payment_type character varying,
    payment_installments bigint,
    payment_reference character varying,
    payment_amount_cents bigint,
    payment_currency character varying,
    payment_outstanding_cents bigint,
    photo_out_url character varying,
    photo_in_url character varying
);


--
-- Name: asset_kit_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.asset_kit_member (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    kit_id uuid NOT NULL,
    member_id uuid NOT NULL,
    qty bigint DEFAULT 1 NOT NULL
);


--
-- Name: attendance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attendance (
    id bigint NOT NULL,
    booking_id bigint NOT NULL,
    party_id bigint NOT NULL,
    status character varying NOT NULL,
    notes character varying
);


--
-- Name: attendance_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.attendance_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: attendance_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.attendance_id_seq OWNED BY public.attendance.id;


--
-- Name: audit_log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.audit_log (
    id bigint NOT NULL,
    actor_id bigint,
    entity character varying NOT NULL,
    entity_id character varying NOT NULL,
    action character varying NOT NULL,
    diff character varying,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: audit_log_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.audit_log_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: audit_log_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.audit_log_id_seq OWNED BY public.audit_log.id;


--
-- Name: band; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.band (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    party_id bigint NOT NULL,
    name character varying NOT NULL,
    label_artist boolean DEFAULT false NOT NULL,
    primary_genre character varying,
    home_city character varying,
    photo_url character varying,
    contract_flags character varying
);


--
-- Name: band_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.band_member (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    band_id uuid NOT NULL,
    party_id bigint NOT NULL,
    role_in_band character varying
);


--
-- Name: booking; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.booking (
    id bigint NOT NULL,
    title character varying NOT NULL,
    service_order_id bigint,
    party_id bigint,
    service_type character varying,
    starts_at timestamp with time zone NOT NULL,
    ends_at timestamp with time zone NOT NULL,
    status character varying NOT NULL,
    created_by bigint,
    notes character varying,
    created_at timestamp with time zone NOT NULL,
    engineer_party_id bigint,
    engineer_name character varying
);


--
-- Name: booking_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.booking_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: booking_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.booking_id_seq OWNED BY public.booking.id;


--
-- Name: booking_resource; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.booking_resource (
    id bigint NOT NULL,
    booking_id bigint NOT NULL,
    resource_id bigint NOT NULL,
    role character varying NOT NULL
);


--
-- Name: booking_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.booking_resource_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: booking_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.booking_resource_id_seq OWNED BY public.booking_resource.id;


--
-- Name: campaign; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.campaign (
    id bigint NOT NULL,
    name character varying NOT NULL,
    objective character varying,
    platform character varying,
    status character varying DEFAULT 'active'::character varying NOT NULL,
    budget_cents bigint,
    start_date date,
    end_date date,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: campaign_automation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.campaign_automation (
    id bigint NOT NULL,
    campaign_id bigint NOT NULL,
    template_key text NOT NULL,
    status text DEFAULT 'draft'::text NOT NULL,
    start_at timestamp with time zone NOT NULL,
    daily_limit integer DEFAULT 20 NOT NULL,
    last_run_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT campaign_automation_daily_limit_check CHECK (((daily_limit >= 1) AND (daily_limit <= 100))),
    CONSTRAINT campaign_automation_status_check CHECK ((status = ANY (ARRAY['draft'::text, 'active'::text, 'paused'::text, 'completed'::text])))
);


--
-- Name: campaign_automation_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.campaign_automation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_automation_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.campaign_automation_id_seq OWNED BY public.campaign_automation.id;


--
-- Name: campaign_automation_step; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.campaign_automation_step (
    id bigint NOT NULL,
    automation_id bigint NOT NULL,
    "position" integer NOT NULL,
    delay_days integer NOT NULL,
    channel text DEFAULT 'whatsapp'::text NOT NULL,
    provider_template_name text NOT NULL,
    language_code text DEFAULT 'es'::text NOT NULL,
    body text NOT NULL,
    cta_path text NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT campaign_automation_step_channel_check CHECK ((channel = 'whatsapp'::text)),
    CONSTRAINT campaign_automation_step_delay_check CHECK ((delay_days >= 0)),
    CONSTRAINT campaign_automation_step_position_check CHECK (("position" > 0))
);


--
-- Name: campaign_automation_step_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.campaign_automation_step_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_automation_step_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.campaign_automation_step_id_seq OWNED BY public.campaign_automation_step.id;


--
-- Name: campaign_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.campaign_delivery (
    id bigint NOT NULL,
    automation_id bigint NOT NULL,
    enrollment_id bigint NOT NULL,
    step_id bigint NOT NULL,
    party_id bigint NOT NULL,
    channel text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    scheduled_at timestamp with time zone NOT NULL,
    attempted_at timestamp with time zone,
    sent_at timestamp with time zone,
    provider_message_id text,
    error text,
    body_snapshot text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT campaign_delivery_channel_check CHECK ((channel = 'whatsapp'::text)),
    CONSTRAINT campaign_delivery_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'sent'::text, 'failed'::text])))
);


--
-- Name: campaign_delivery_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.campaign_delivery_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_delivery_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.campaign_delivery_id_seq OWNED BY public.campaign_delivery.id;


--
-- Name: campaign_enrollment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.campaign_enrollment (
    id bigint NOT NULL,
    automation_id bigint NOT NULL,
    party_id bigint NOT NULL,
    status text DEFAULT 'scheduled'::text NOT NULL,
    next_step_position integer DEFAULT 1 NOT NULL,
    next_run_at timestamp with time zone NOT NULL,
    last_sent_at timestamp with time zone,
    stopped_at timestamp with time zone,
    stop_reason text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT campaign_enrollment_next_step_check CHECK ((next_step_position > 0)),
    CONSTRAINT campaign_enrollment_status_check CHECK ((status = ANY (ARRAY['scheduled'::text, 'completed'::text, 'stopped'::text, 'replied'::text, 'converted'::text])))
);


--
-- Name: campaign_enrollment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.campaign_enrollment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_enrollment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.campaign_enrollment_id_seq OWNED BY public.campaign_enrollment.id;


--
-- Name: campaign_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.campaign_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: campaign_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.campaign_id_seq OWNED BY public.campaign.id;


--
-- Name: catalog_asset; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_asset (
    id integer NOT NULL,
    asset_type text NOT NULL,
    uri text NOT NULL,
    logical_name text NOT NULL,
    mime_type text NOT NULL,
    size_bytes bigint NOT NULL,
    sha256 text NOT NULL,
    metadata_json jsonb,
    CONSTRAINT catalog_asset_asset_type_check CHECK ((asset_type = ANY (ARRAY['AudioFile'::text, 'ImageFile'::text, 'DocumentFile'::text, 'VideoFile'::text])))
);


--
-- Name: catalog_asset_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_asset_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_asset_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_asset_id_seq OWNED BY public.catalog_asset.id;


--
-- Name: catalog_credit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_credit (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    entity_type text NOT NULL,
    party_id integer NOT NULL,
    role text NOT NULL,
    credit_text text,
    sequence integer,
    CONSTRAINT catalog_credit_entity_type_check CHECK ((entity_type = ANY (ARRAY['Release'::text, 'Resource'::text]))),
    CONSTRAINT catalog_credit_role_check CHECK ((role = ANY (ARRAY['MainArtist'::text, 'FeaturedArtist'::text, 'Producer'::text, 'Engineer'::text, 'Mixer'::text, 'MasteringEngineer'::text, 'Composer'::text, 'Lyricist'::text, 'Arranger'::text, 'Performer'::text, 'StudioMusician'::text])))
);


--
-- Name: catalog_credit_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_credit_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_credit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_credit_id_seq OWNED BY public.catalog_credit.id;


--
-- Name: catalog_deal; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_deal (
    id integer NOT NULL,
    release_id integer,
    resource_id integer,
    model text NOT NULL,
    start_date timestamp with time zone NOT NULL,
    end_date timestamp with time zone,
    takedown_date timestamp with time zone,
    partner_name text NOT NULL,
    CONSTRAINT catalog_deal_model_check CHECK ((model = ANY (ARRAY['ExclusiveLicense'::text, 'DistributionAgreement'::text, 'AdministrationDeal'::text, 'PressAndDistribution'::text])))
);


--
-- Name: catalog_deal_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_deal_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_deal_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_deal_id_seq OWNED BY public.catalog_deal.id;


--
-- Name: catalog_deal_territory; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_deal_territory (
    id integer NOT NULL,
    deal_id integer NOT NULL,
    territory_code text NOT NULL,
    is_included boolean DEFAULT true NOT NULL
);


--
-- Name: catalog_deal_territory_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_deal_territory_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_deal_territory_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_deal_territory_id_seq OWNED BY public.catalog_deal_territory.id;


--
-- Name: catalog_identifier; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_identifier (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    entity_type text NOT NULL,
    scheme text NOT NULL,
    value text NOT NULL,
    namespace text,
    CONSTRAINT catalog_identifier_entity_type_check CHECK ((entity_type = ANY (ARRAY['Release'::text, 'Resource'::text, 'Party'::text]))),
    CONSTRAINT catalog_identifier_scheme_check CHECK ((scheme = ANY (ARRAY['ISRC'::text, 'UPC'::text, 'EAN'::text, 'GRid'::text, 'IPI'::text, 'ISNI'::text, 'DPID'::text, 'Proprietary'::text])))
);


--
-- Name: catalog_identifier_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_identifier_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_identifier_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_identifier_id_seq OWNED BY public.catalog_identifier.id;


--
-- Name: catalog_release; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_release (
    id integer NOT NULL,
    title text NOT NULL,
    sub_title text,
    release_type text NOT NULL,
    release_date timestamp with time zone,
    original_release_date timestamp with time zone,
    label_name text,
    status text DEFAULT 'Draft'::text NOT NULL,
    copyright_line text,
    phonographic_copyright_line text,
    genre text,
    cover_art_asset_id integer,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT catalog_release_release_type_check CHECK ((release_type = ANY (ARRAY['Album'::text, 'Single'::text, 'EP'::text, 'Compilation'::text, 'LiveAlbum'::text, 'RemixAlbum'::text, 'Soundtrack'::text, 'SpokenWord'::text]))),
    CONSTRAINT catalog_release_status_check CHECK ((status = ANY (ARRAY['Draft'::text, 'Active'::text, 'Takedown'::text])))
);


--
-- Name: TABLE catalog_release; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.catalog_release IS 'Canonical releases independent of DDEX format';


--
-- Name: catalog_release_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_release_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_release_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_release_id_seq OWNED BY public.catalog_release.id;


--
-- Name: catalog_release_resource; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_release_resource (
    id integer NOT NULL,
    release_id integer NOT NULL,
    resource_id integer NOT NULL,
    disc_number integer DEFAULT 1 NOT NULL,
    sequence integer NOT NULL,
    is_primary boolean DEFAULT true NOT NULL
);


--
-- Name: catalog_release_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_release_resource_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_release_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_release_resource_id_seq OWNED BY public.catalog_release_resource.id;


--
-- Name: catalog_resource; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_resource (
    id integer NOT NULL,
    resource_type text NOT NULL,
    title text NOT NULL,
    version text,
    duration_ms integer,
    language_code text,
    is_explicit boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT catalog_resource_resource_type_check CHECK ((resource_type = ANY (ARRAY['SoundRecording'::text, 'MusicVideo'::text, 'Image'::text, 'Text'::text, 'Software'::text])))
);


--
-- Name: catalog_resource_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_resource_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_resource_id_seq OWNED BY public.catalog_resource.id;


--
-- Name: catalog_source_link; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.catalog_source_link (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    entity_type text NOT NULL,
    ddex_document_id integer,
    ddex_xpath_reference text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: catalog_source_link_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.catalog_source_link_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: catalog_source_link_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.catalog_source_link_id_seq OWNED BY public.catalog_source_link.id;


--
-- Name: chat_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_message (
    id bigint NOT NULL,
    thread_id bigint NOT NULL,
    sender_party_id bigint NOT NULL,
    body character varying NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: chat_message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.chat_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: chat_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.chat_message_id_seq OWNED BY public.chat_message.id;


--
-- Name: chat_thread; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.chat_thread (
    id bigint NOT NULL,
    dm_party_a bigint NOT NULL,
    dm_party_b bigint NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: chat_thread_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.chat_thread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: chat_thread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.chat_thread_id_seq OWNED BY public.chat_thread.id;


--
-- Name: class_package_purchase; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.class_package_purchase (
    id bigint NOT NULL,
    student_id bigint NOT NULL,
    package_id bigint NOT NULL,
    price_cents bigint NOT NULL,
    discount_cents bigint DEFAULT 0 NOT NULL,
    tax_cents bigint DEFAULT 0 NOT NULL,
    total_paid_cents bigint DEFAULT 0 NOT NULL,
    purchased_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    seller_id bigint,
    commissioned_teacher_id bigint,
    trial_request_id bigint,
    status character varying DEFAULT 'Open'::character varying NOT NULL
);


--
-- Name: class_package_purchase_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.class_package_purchase_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: class_package_purchase_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.class_package_purchase_id_seq OWNED BY public.class_package_purchase.id;


--
-- Name: class_session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.class_session (
    id bigint NOT NULL,
    student_id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    subject_id bigint NOT NULL,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone NOT NULL,
    room_id bigint NOT NULL,
    booking_id bigint,
    attended boolean DEFAULT false NOT NULL,
    purchase_id bigint,
    consumed_minutes bigint DEFAULT 0 NOT NULL,
    notes character varying
);


--
-- Name: class_session_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.class_session_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: class_session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.class_session_id_seq OWNED BY public.class_session.id;


--
-- Name: cms_content; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cms_content (
    id bigint NOT NULL,
    slug text NOT NULL,
    locale text NOT NULL,
    version bigint NOT NULL,
    status text NOT NULL,
    title text,
    payload jsonb,
    created_by bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_at timestamp with time zone
);


--
-- Name: cms_content_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.cms_content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: cms_content_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.cms_content_id_seq OWNED BY public.cms_content.id;


--
-- Name: cohort; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug character varying NOT NULL,
    title character varying NOT NULL,
    starts_at timestamp with time zone NOT NULL,
    ends_at timestamp with time zone NOT NULL,
    seat_cap bigint NOT NULL
);


--
-- Name: cohort_enrollment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.cohort_enrollment (
    cohort_id uuid NOT NULL,
    user_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: commission; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.commission (
    id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    purchase_id bigint NOT NULL,
    basis_cents bigint NOT NULL,
    percent double precision NOT NULL,
    amount_cents bigint NOT NULL,
    recognized_at timestamp with time zone NOT NULL,
    status character varying DEFAULT 'Accrued'::character varying NOT NULL,
    paid_at timestamp with time zone
);


--
-- Name: commission_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.commission_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: commission_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.commission_id_seq OWNED BY public.commission.id;


--
-- Name: country; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.country (
    id bigint NOT NULL,
    code character varying NOT NULL,
    name character varying NOT NULL
);


--
-- Name: country_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.country_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: country_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.country_id_seq OWNED BY public.country.id;


--
-- Name: course; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course (
    id bigint NOT NULL,
    slug character varying NOT NULL,
    title character varying NOT NULL,
    subtitle character varying,
    format character varying,
    duration character varying,
    price_cents bigint NOT NULL,
    currency character varying NOT NULL,
    capacity bigint NOT NULL,
    session_start_hour bigint,
    session_duration_hours bigint,
    location_label character varying,
    location_map_url character varying,
    whatsapp_cta_url character varying,
    landing_url character varying,
    daws text[],
    includes text[],
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    instructor_name character varying,
    instructor_bio character varying,
    instructor_avatar_url character varying,
    stripe_subscription_price_id text
);


--
-- Name: course_email_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_email_event (
    id bigint NOT NULL,
    course_slug character varying NOT NULL,
    registration_id bigint,
    recipient_email character varying NOT NULL,
    recipient_name character varying,
    event_type character varying NOT NULL,
    status character varying NOT NULL,
    message character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: course_email_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_email_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_email_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_email_event_id_seq OWNED BY public.course_email_event.id;


--
-- Name: course_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_id_seq OWNED BY public.course.id;


--
-- Name: course_registration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_registration (
    id bigint NOT NULL,
    course_slug character varying NOT NULL,
    full_name character varying,
    email character varying,
    phone_e164 character varying,
    source character varying NOT NULL,
    status character varying NOT NULL,
    how_heard character varying,
    utm_source character varying,
    utm_medium character varying,
    utm_campaign character varying,
    utm_content character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    party_id bigint,
    admin_notes character varying,
    stripe_subscription_id text,
    subscription_status text,
    stripe_payment_intent_id text
);


--
-- Name: course_registration_follow_up; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_registration_follow_up (
    id bigint NOT NULL,
    registration_id bigint NOT NULL,
    party_id bigint,
    entry_type character varying NOT NULL,
    subject character varying,
    notes character varying NOT NULL,
    attachment_url character varying,
    attachment_name character varying,
    next_follow_up_at timestamp with time zone,
    created_by bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: course_registration_follow_up_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_registration_follow_up_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_registration_follow_up_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_registration_follow_up_id_seq OWNED BY public.course_registration_follow_up.id;


--
-- Name: course_registration_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_registration_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_registration_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_registration_id_seq OWNED BY public.course_registration.id;


--
-- Name: course_registration_receipt; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_registration_receipt (
    id bigint NOT NULL,
    registration_id bigint NOT NULL,
    party_id bigint,
    file_url character varying NOT NULL,
    file_name character varying,
    mime_type character varying,
    notes character varying,
    uploaded_by bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: course_registration_receipt_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_registration_receipt_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_registration_receipt_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_registration_receipt_id_seq OWNED BY public.course_registration_receipt.id;


--
-- Name: course_session_model; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_session_model (
    id bigint NOT NULL,
    course_id bigint NOT NULL,
    label character varying NOT NULL,
    date date NOT NULL,
    "order" bigint
);


--
-- Name: course_session_model_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_session_model_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_session_model_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_session_model_id_seq OWNED BY public.course_session_model.id;


--
-- Name: course_syllabus_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.course_syllabus_item (
    id bigint NOT NULL,
    course_id bigint NOT NULL,
    title character varying NOT NULL,
    topics character varying NOT NULL,
    "order" bigint
);


--
-- Name: course_syllabus_item_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.course_syllabus_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: course_syllabus_item_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.course_syllabus_item_id_seq OWNED BY public.course_syllabus_item.id;


--
-- Name: currency_conversion_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.currency_conversion_audit (
    id bigint NOT NULL,
    user_id bigint,
    source_currency text NOT NULL,
    target_currency text NOT NULL,
    source_minor_units bigint NOT NULL,
    target_minor_units bigint NOT NULL,
    exchange_rate numeric(24,12) NOT NULL,
    rate_source text NOT NULL,
    rate_observed_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT currency_conversion_positive_rate CHECK ((exchange_rate > (0)::numeric)),
    CONSTRAINT currency_conversion_source_code CHECK ((source_currency ~ '^[A-Z]{3}$'::text)),
    CONSTRAINT currency_conversion_target_code CHECK ((target_currency ~ '^[A-Z]{3}$'::text))
);


--
-- Name: currency_conversion_audit_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.currency_conversion_audit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: currency_conversion_audit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.currency_conversion_audit_id_seq OWNED BY public.currency_conversion_audit.id;


--
-- Name: ddex_document; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_document (
    id integer NOT NULL,
    file_name text NOT NULL,
    private_uri text NOT NULL,
    sha256 text NOT NULL,
    size_bytes bigint NOT NULL,
    family text NOT NULL,
    version text NOT NULL,
    namespace text,
    message_type text,
    status text DEFAULT 'received'::text NOT NULL,
    uploaded_by integer NOT NULL,
    message_id text,
    sender_id text,
    recipient_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ddex_document_status_check CHECK ((status = ANY (ARRAY['received'::text, 'quarantined'::text, 'queued'::text, 'validating'::text, 'invalid'::text, 'valid'::text, 'mapping_required'::text, 'ready_to_import'::text, 'importing'::text, 'imported'::text, 'import_failed'::text, 'superseded'::text])))
);


--
-- Name: TABLE ddex_document; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.ddex_document IS 'Immutable record of received DDEX messages';


--
-- Name: ddex_document_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_document_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_document_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_document_id_seq OWNED BY public.ddex_document.id;


--
-- Name: ddex_export; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_export (
    id integer NOT NULL,
    release_id integer NOT NULL,
    partner_id integer,
    ern_version text NOT NULL,
    profile_name text,
    xml_checksum text NOT NULL,
    private_uri text NOT NULL,
    validation_result text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ddex_export_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_export_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_export_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_export_id_seq OWNED BY public.ddex_export.id;


--
-- Name: ddex_import_change; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_import_change (
    id integer NOT NULL,
    import_run_id integer NOT NULL,
    entity_type text NOT NULL,
    entity_id integer,
    operation text NOT NULL,
    previous_state jsonb,
    new_state jsonb,
    CONSTRAINT ddex_import_change_operation_check CHECK ((operation = ANY (ARRAY['Create'::text, 'Update'::text, 'Skip'::text])))
);


--
-- Name: ddex_import_change_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_import_change_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_import_change_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_import_change_id_seq OWNED BY public.ddex_import_change.id;


--
-- Name: ddex_import_plan; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_import_plan (
    id integer NOT NULL,
    document_id integer NOT NULL,
    status text DEFAULT 'draft'::text NOT NULL,
    snapshot_json jsonb NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ddex_import_plan_status_check CHECK ((status = ANY (ARRAY['draft'::text, 'resolved'::text, 'committed'::text, 'abandoned'::text])))
);


--
-- Name: ddex_import_plan_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_import_plan_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_import_plan_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_import_plan_id_seq OWNED BY public.ddex_import_plan.id;


--
-- Name: ddex_import_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_import_run (
    id integer NOT NULL,
    plan_id integer NOT NULL,
    actor_id integer NOT NULL,
    status text NOT NULL,
    started_at timestamp with time zone DEFAULT now() NOT NULL,
    finished_at timestamp with time zone,
    error_message text,
    CONSTRAINT ddex_import_run_status_check CHECK ((status = ANY (ARRAY['Pending'::text, 'Running'::text, 'Success'::text, 'Failed'::text, 'RolledBack'::text])))
);


--
-- Name: ddex_import_run_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_import_run_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_import_run_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_import_run_id_seq OWNED BY public.ddex_import_run.id;


--
-- Name: ddex_job; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_job (
    id integer NOT NULL,
    job_type text NOT NULL,
    entity_id integer NOT NULL,
    status text DEFAULT 'Pending'::text NOT NULL,
    attempts integer DEFAULT 0 NOT NULL,
    leased_until timestamp with time zone,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT ddex_job_job_type_check CHECK ((job_type = ANY (ARRAY['Validate'::text, 'Import'::text, 'Export'::text, 'Cleanup'::text]))),
    CONSTRAINT ddex_job_status_check CHECK ((status = ANY (ARRAY['Pending'::text, 'Processing'::text, 'Completed'::text, 'Failed'::text, 'Retry'::text])))
);


--
-- Name: TABLE ddex_job; Type: COMMENT; Schema: public; Owner: -
--

COMMENT ON TABLE public.ddex_job IS 'Queue for background processing without external broker';


--
-- Name: ddex_job_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_job_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_job_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_job_id_seq OWNED BY public.ddex_job.id;


--
-- Name: ddex_message_header; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_message_header (
    id integer NOT NULL,
    document_id integer NOT NULL,
    message_id text NOT NULL,
    thread_id text,
    sender_dpid text,
    recipient_dpid text,
    created_date timestamp with time zone,
    control_type text
);


--
-- Name: ddex_message_header_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_message_header_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_message_header_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_message_header_id_seq OWNED BY public.ddex_message_header.id;


--
-- Name: ddex_partner; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_partner (
    id integer NOT NULL,
    name text NOT NULL,
    dpid text,
    allowed_versions text[] DEFAULT '{4.3.2}'::text[] NOT NULL,
    rules_json jsonb,
    naming_convention text,
    is_active boolean DEFAULT true NOT NULL
);


--
-- Name: ddex_partner_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_partner_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_partner_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_partner_id_seq OWNED BY public.ddex_partner.id;


--
-- Name: ddex_validation_issue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_validation_issue (
    id integer NOT NULL,
    validation_run_id integer NOT NULL,
    severity text NOT NULL,
    layer text NOT NULL,
    code text,
    line_number integer,
    column_number integer,
    xpath_ref text,
    message text NOT NULL,
    suggestion text,
    CONSTRAINT ddex_validation_issue_layer_check CHECK ((layer = ANY (ARRAY['XML'::text, 'XSD'::text, 'AVS'::text, 'Business'::text]))),
    CONSTRAINT ddex_validation_issue_severity_check CHECK ((severity = ANY (ARRAY['Error'::text, 'Warning'::text, 'Info'::text])))
);


--
-- Name: ddex_validation_issue_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_validation_issue_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_validation_issue_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_validation_issue_id_seq OWNED BY public.ddex_validation_issue.id;


--
-- Name: ddex_validation_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ddex_validation_run (
    id integer NOT NULL,
    document_id integer NOT NULL,
    validator_version text,
    schema_version text,
    started_at timestamp with time zone DEFAULT now() NOT NULL,
    finished_at timestamp with time zone,
    result text,
    error_count integer DEFAULT 0,
    warning_count integer DEFAULT 0,
    CONSTRAINT ddex_validation_run_result_check CHECK ((result = ANY (ARRAY['Success'::text, 'Failure'::text, 'Warning'::text])))
);


--
-- Name: ddex_validation_run_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ddex_validation_run_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ddex_validation_run_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ddex_validation_run_id_seq OWNED BY public.ddex_validation_run.id;


--
-- Name: dropdown_option; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.dropdown_option (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    category character varying NOT NULL,
    value character varying NOT NULL,
    label character varying,
    active boolean DEFAULT true NOT NULL,
    sort_order bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_artist; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_artist (
    event_id bigint NOT NULL,
    artist_id bigint NOT NULL,
    role character varying
);


--
-- Name: event_budget_line; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_budget_line (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    code character varying NOT NULL,
    name character varying NOT NULL,
    line_type character varying NOT NULL,
    category character varying NOT NULL,
    planned_cents bigint NOT NULL,
    notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_budget_line_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_budget_line_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_budget_line_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_budget_line_id_seq OWNED BY public.event_budget_line.id;


--
-- Name: event_city; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_city (
    id bigint NOT NULL,
    name text NOT NULL,
    normalized_name text NOT NULL,
    country_code text NOT NULL,
    time_zone text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_city_country_code_check CHECK ((country_code ~ '^[A-Z]{2}$'::text))
);


--
-- Name: event_city_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_city_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_city_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_city_id_seq OWNED BY public.event_city.id;


--
-- Name: event_city_subscription; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_city_subscription (
    id bigint NOT NULL,
    party_id text NOT NULL,
    city_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_city_subscription_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_city_subscription_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_city_subscription_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_city_subscription_id_seq OWNED BY public.event_city_subscription.id;


--
-- Name: event_discovery_source; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_discovery_source (
    id bigint NOT NULL,
    source_key text NOT NULL,
    name text NOT NULL,
    source_type text NOT NULL,
    feed_url text,
    city_id bigint,
    enabled boolean DEFAULT true NOT NULL,
    priority integer DEFAULT 100 NOT NULL,
    configuration text,
    etag text,
    last_modified text,
    consecutive_failures integer DEFAULT 0 NOT NULL,
    last_success_at timestamp with time zone,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_discovery_source_priority_check CHECK (((priority >= 0) AND (priority <= 10000))),
    CONSTRAINT event_discovery_source_type_check CHECK ((source_type = ANY (ARRAY['ticketmaster'::text, 'buenplan'::text, 'ical'::text, 'json'::text])))
);


--
-- Name: event_discovery_source_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_discovery_source_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_discovery_source_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_discovery_source_id_seq OWNED BY public.event_discovery_source.id;


--
-- Name: event_finance_entry; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_finance_entry (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    budget_line_id bigint,
    direction character varying NOT NULL,
    source character varying NOT NULL,
    category character varying NOT NULL,
    concept character varying NOT NULL,
    amount_cents bigint NOT NULL,
    currency character varying NOT NULL,
    status character varying NOT NULL,
    external_ref character varying,
    notes character varying,
    metadata character varying,
    occurred_at timestamp with time zone NOT NULL,
    recorded_by_party_id character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_finance_entry_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_finance_entry_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_finance_entry_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_finance_entry_id_seq OWNED BY public.event_finance_entry.id;


--
-- Name: event_invitation; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_invitation (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    from_party_id character varying,
    to_party_id character varying,
    status character varying,
    message character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_invitation_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_invitation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_invitation_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_invitation_id_seq OWNED BY public.event_invitation.id;


--
-- Name: event_logistics_activity; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_activity (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    activity_type text NOT NULL,
    title text NOT NULL,
    notes text,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone,
    place_id bigint,
    origin_place_id bigint,
    destination_place_id bigint,
    travel_mode text,
    buffer_minutes integer,
    priority text NOT NULL,
    status text NOT NULL,
    version integer DEFAULT 1 NOT NULL,
    created_by_party_id text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_activity_buffer_check CHECK (((buffer_minutes IS NULL) OR ((buffer_minutes >= 0) AND (buffer_minutes <= 1440)))),
    CONSTRAINT event_logistics_activity_mode_check CHECK (((travel_mode IS NULL) OR (travel_mode = ANY (ARRAY['drive'::text, 'walk'::text, 'bicycle'::text, 'two_wheeler'::text, 'transit'::text])))),
    CONSTRAINT event_logistics_activity_priority_check CHECK ((priority = ANY (ARRAY['low'::text, 'normal'::text, 'high'::text, 'critical'::text]))),
    CONSTRAINT event_logistics_activity_status_check CHECK ((status = ANY (ARRAY['planned'::text, 'confirmed'::text, 'in_progress'::text, 'completed'::text, 'cancelled'::text]))),
    CONSTRAINT event_logistics_activity_type_check CHECK ((activity_type = ANY (ARRAY['task'::text, 'milestone'::text, 'wait'::text, 'travel'::text]))),
    CONSTRAINT event_logistics_activity_version_check CHECK ((version > 0))
);


--
-- Name: event_logistics_activity_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_activity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_activity_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_activity_id_seq OWNED BY public.event_logistics_activity.id;


--
-- Name: event_logistics_alert_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_alert_delivery (
    id bigint NOT NULL,
    activity_id bigint NOT NULL,
    activity_version integer NOT NULL,
    checkpoint text NOT NULL,
    recipient_party_id text NOT NULL,
    channel text NOT NULL,
    delivered_at timestamp with time zone NOT NULL
);


--
-- Name: event_logistics_alert_delivery_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_alert_delivery_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_alert_delivery_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_alert_delivery_id_seq OWNED BY public.event_logistics_alert_delivery.id;


--
-- Name: event_logistics_assignment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_assignment (
    id bigint NOT NULL,
    activity_id bigint NOT NULL,
    party_id text,
    external_name text,
    external_phone text,
    external_email text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_assignment_identity_check CHECK (((party_id IS NOT NULL) <> (external_name IS NOT NULL)))
);


--
-- Name: event_logistics_assignment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_assignment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_assignment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_assignment_id_seq OWNED BY public.event_logistics_assignment.id;


--
-- Name: event_logistics_dependency; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_dependency (
    id bigint NOT NULL,
    activity_id bigint NOT NULL,
    depends_on_activity_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_dependency_not_self CHECK ((activity_id <> depends_on_activity_id))
);


--
-- Name: event_logistics_dependency_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_dependency_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_dependency_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_dependency_id_seq OWNED BY public.event_logistics_dependency.id;


--
-- Name: event_logistics_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_member (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    party_id text NOT NULL,
    member_role text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_member_role_check CHECK ((member_role = ANY (ARRAY['viewer'::text, 'editor'::text])))
);


--
-- Name: event_logistics_member_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_member_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_member_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_member_id_seq OWNED BY public.event_logistics_member.id;


--
-- Name: event_logistics_place; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_place (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    venue_id bigint,
    label text NOT NULL,
    place_type text NOT NULL,
    address text,
    google_place_id text,
    latitude double precision NOT NULL,
    longitude double precision NOT NULL,
    instructions text,
    contact_name text,
    contact_phone text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_place_latitude_check CHECK (((latitude >= ('-90'::integer)::double precision) AND (latitude <= (90)::double precision))),
    CONSTRAINT event_logistics_place_longitude_check CHECK (((longitude >= ('-180'::integer)::double precision) AND (longitude <= (180)::double precision))),
    CONSTRAINT event_logistics_place_type_check CHECK ((place_type = ANY (ARRAY['venue'::text, 'hotel'::text, 'airport'::text, 'pickup'::text, 'custom'::text])))
);


--
-- Name: event_logistics_place_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_place_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_place_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_place_id_seq OWNED BY public.event_logistics_place.id;


--
-- Name: event_logistics_plan; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_logistics_plan (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    timezone text DEFAULT 'America/Guayaquil'::text NOT NULL,
    default_travel_mode text DEFAULT 'drive'::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_logistics_plan_mode_check CHECK ((default_travel_mode = ANY (ARRAY['drive'::text, 'walk'::text, 'bicycle'::text, 'two_wheeler'::text, 'transit'::text])))
);


--
-- Name: event_logistics_plan_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_logistics_plan_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_logistics_plan_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_logistics_plan_id_seq OWNED BY public.event_logistics_plan.id;


--
-- Name: event_moment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_moment (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    author_party_id character varying,
    author_name character varying NOT NULL,
    caption character varying,
    media_url character varying NOT NULL,
    media_type character varying NOT NULL,
    media_width bigint,
    media_height bigint,
    media_duration_ms bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_moment_comment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_moment_comment (
    id bigint NOT NULL,
    moment_id bigint NOT NULL,
    author_party_id character varying,
    author_name character varying NOT NULL,
    body character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_moment_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_moment_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_moment_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_moment_comment_id_seq OWNED BY public.event_moment_comment.id;


--
-- Name: event_moment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_moment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_moment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_moment_id_seq OWNED BY public.event_moment.id;


--
-- Name: event_moment_reaction; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_moment_reaction (
    moment_id bigint NOT NULL,
    reaction character varying NOT NULL,
    reactor_party_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_route_verification; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_route_verification (
    id bigint NOT NULL,
    activity_id bigint NOT NULL,
    activity_version integer NOT NULL,
    provider text NOT NULL,
    travel_mode text NOT NULL,
    departure_time timestamp with time zone NOT NULL,
    duration_seconds integer,
    static_duration_seconds integer,
    distance_meters integer,
    buffer_seconds integer NOT NULL,
    allocated_seconds integer NOT NULL,
    verdict text NOT NULL,
    encoded_polyline text,
    error_message text,
    checkpoint text,
    verified_at timestamp with time zone NOT NULL,
    CONSTRAINT event_route_verification_checkpoint_check CHECK (((checkpoint IS NULL) OR (checkpoint = ANY (ARRAY['24h'::text, '2h'::text])))),
    CONSTRAINT event_route_verification_verdict_check CHECK ((verdict = ANY (ARRAY['feasible'::text, 'tight'::text, 'infeasible'::text, 'provisional'::text, 'unavailable'::text, 'stale'::text])))
);


--
-- Name: event_route_verification_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_route_verification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_route_verification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_route_verification_id_seq OWNED BY public.event_route_verification.id;


--
-- Name: event_rsvp; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_rsvp (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    party_id character varying NOT NULL,
    status character varying NOT NULL,
    metadata character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_rsvp_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_rsvp_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_rsvp_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_rsvp_id_seq OWNED BY public.event_rsvp.id;


--
-- Name: event_ticket; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_ticket (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    tier_ref_id bigint NOT NULL,
    order_ref_id bigint NOT NULL,
    holder_name character varying,
    holder_email character varying,
    code character varying NOT NULL,
    status character varying NOT NULL,
    checked_in_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    current_holder_party_id character varying,
    current_holder_email character varying,
    current_holder_name character varying,
    original_holder_party_id character varying,
    transfer_history character varying
);


--
-- Name: event_ticket_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_ticket_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_ticket_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_ticket_id_seq OWNED BY public.event_ticket.id;


--
-- Name: event_ticket_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_ticket_order (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    tier_id bigint NOT NULL,
    buyer_party_id character varying,
    buyer_name character varying,
    buyer_email character varying,
    quantity bigint NOT NULL,
    amount_cents bigint NOT NULL,
    currency character varying NOT NULL,
    status character varying NOT NULL,
    metadata character varying,
    purchased_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    stripe_payment_intent_id character varying,
    promo_code_id bigint,
    original_amount_cents bigint,
    payment_method character varying,
    checkout_idempotency_key character varying
);


--
-- Name: event_ticket_order_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_ticket_order_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_ticket_order_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_ticket_order_id_seq OWNED BY public.event_ticket_order.id;


--
-- Name: event_ticket_tier; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_ticket_tier (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    code character varying NOT NULL,
    name character varying NOT NULL,
    description character varying,
    price_cents bigint NOT NULL,
    currency character varying NOT NULL,
    quantity_total bigint NOT NULL,
    quantity_sold bigint NOT NULL,
    sales_start timestamp with time zone,
    sales_end timestamp with time zone,
    is_active boolean NOT NULL,
    "position" bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    enable_waitlist boolean DEFAULT false NOT NULL,
    allow_transfers boolean DEFAULT true NOT NULL,
    refund_policy character varying DEFAULT 'full'::character varying NOT NULL,
    refund_deadline timestamp with time zone
);


--
-- Name: event_ticket_tier_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_ticket_tier_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_ticket_tier_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_ticket_tier_id_seq OWNED BY public.event_ticket_tier.id;


--
-- Name: event_waitlist; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.event_waitlist (
    id bigint NOT NULL,
    event_id bigint NOT NULL,
    tier_id bigint,
    party_id character varying,
    email character varying NOT NULL,
    name character varying,
    quantity bigint DEFAULT 1 NOT NULL,
    status character varying DEFAULT 'active'::character varying NOT NULL,
    priority bigint DEFAULT 0 NOT NULL,
    notified_at timestamp with time zone,
    expires_at timestamp with time zone,
    converted_order_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: event_waitlist_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.event_waitlist_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_waitlist_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.event_waitlist_id_seq OWNED BY public.event_waitlist.id;


--
-- Name: external_artist_ref; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.external_artist_ref (
    id bigint NOT NULL,
    provider text NOT NULL,
    external_id text NOT NULL,
    artist_id bigint NOT NULL,
    last_seen_at timestamp with time zone NOT NULL
);


--
-- Name: external_artist_ref_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.external_artist_ref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: external_artist_ref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.external_artist_ref_id_seq OWNED BY public.external_artist_ref.id;


--
-- Name: external_calendar_mapping; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.external_calendar_mapping (
    id bigint NOT NULL,
    resource_id bigint NOT NULL,
    google_calendar_id character varying NOT NULL,
    direction character varying NOT NULL
);


--
-- Name: external_calendar_mapping_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.external_calendar_mapping_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: external_calendar_mapping_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.external_calendar_mapping_id_seq OWNED BY public.external_calendar_mapping.id;


--
-- Name: external_event_discovery_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.external_event_discovery_run (
    id bigint NOT NULL,
    provider text NOT NULL,
    run_date date NOT NULL,
    status text NOT NULL,
    cities_count integer DEFAULT 0 NOT NULL,
    events_seen integer DEFAULT 0 NOT NULL,
    events_created integer DEFAULT 0 NOT NULL,
    events_updated integer DEFAULT 0 NOT NULL,
    venues_created integer DEFAULT 0 NOT NULL,
    artists_created integer DEFAULT 0 NOT NULL,
    error_message text,
    started_at timestamp with time zone NOT NULL,
    finished_at timestamp with time zone,
    scheduled_for timestamp with time zone
);


--
-- Name: external_event_discovery_run_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.external_event_discovery_run_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: external_event_discovery_run_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.external_event_discovery_run_id_seq OWNED BY public.external_event_discovery_run.id;


--
-- Name: external_event_ref; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.external_event_ref (
    id bigint NOT NULL,
    provider text NOT NULL,
    external_id text NOT NULL,
    event_id bigint NOT NULL,
    city text NOT NULL,
    source_url text,
    last_seen_at timestamp with time zone NOT NULL,
    country_code text,
    price_cents integer,
    currency text,
    missing_runs integer DEFAULT 0 NOT NULL,
    source_status text DEFAULT 'active'::text NOT NULL
);


--
-- Name: external_event_ref_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.external_event_ref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: external_event_ref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.external_event_ref_id_seq OWNED BY public.external_event_ref.id;


--
-- Name: external_venue_ref; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.external_venue_ref (
    id bigint NOT NULL,
    provider text NOT NULL,
    external_id text NOT NULL,
    venue_id bigint NOT NULL,
    last_seen_at timestamp with time zone NOT NULL
);


--
-- Name: external_venue_ref_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.external_venue_ref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: external_venue_ref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.external_venue_ref_id_seq OWNED BY public.external_venue_ref.id;


--
-- Name: facebook_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.facebook_message (
    id bigint NOT NULL,
    external_id character varying NOT NULL,
    sender_id character varying NOT NULL,
    sender_name character varying,
    text character varying,
    direction character varying NOT NULL,
    ad_external_id character varying,
    ad_name character varying,
    campaign_external_id character varying,
    campaign_name character varying,
    metadata character varying,
    replied_at timestamp with time zone,
    reply_text character varying,
    reply_error character varying,
    created_at timestamp with time zone NOT NULL,
    reply_status character varying DEFAULT 'pending'::character varying NOT NULL,
    hold_reason character varying,
    hold_required_fields character varying,
    last_attempt_at timestamp with time zone,
    attempt_count bigint DEFAULT 0 NOT NULL,
    deleted_at timestamp with time zone
);


--
-- Name: facebook_message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.facebook_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: facebook_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.facebook_message_id_seq OWNED BY public.facebook_message.id;


--
-- Name: fan_club; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club (
    id integer NOT NULL,
    artist_party_id bigint NOT NULL,
    name character varying NOT NULL,
    description character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_candidacy; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_candidacy (
    id integer NOT NULL,
    election_id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    role character varying NOT NULL,
    manifesto character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_candidacy_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_candidacy_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_candidacy_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_candidacy_id_seq OWNED BY public.fan_club_candidacy.id;


--
-- Name: fan_club_election; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_election (
    id integer NOT NULL,
    club_id bigint NOT NULL,
    year integer NOT NULL,
    candidacy_starts_at timestamp with time zone,
    candidacy_ends_at timestamp with time zone,
    voting_starts_at timestamp with time zone,
    voting_ends_at timestamp with time zone,
    status character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_election_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_election_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_election_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_election_id_seq OWNED BY public.fan_club_election.id;


--
-- Name: fan_club_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_event (
    id integer NOT NULL,
    club_id bigint NOT NULL,
    title character varying NOT NULL,
    description character varying,
    starts_at timestamp with time zone,
    ends_at timestamp with time zone,
    location character varying,
    is_artist_concert boolean DEFAULT false NOT NULL,
    created_by_party_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_event_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_event_id_seq OWNED BY public.fan_club_event.id;


--
-- Name: fan_club_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_id_seq OWNED BY public.fan_club.id;


--
-- Name: fan_club_member_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_member_profile (
    id integer NOT NULL,
    party_id bigint NOT NULL,
    club_id bigint NOT NULL,
    handle character varying,
    bio character varying,
    avatar_url character varying,
    joined_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_member_profile_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_member_profile_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_member_profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_member_profile_id_seq OWNED BY public.fan_club_member_profile.id;


--
-- Name: fan_club_memory; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_memory (
    id integer NOT NULL,
    member_profile_id bigint NOT NULL,
    title character varying NOT NULL,
    description character varying,
    media_urls character varying,
    is_hidden boolean DEFAULT false NOT NULL,
    is_deleted boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_memory_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_memory_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_memory_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_memory_id_seq OWNED BY public.fan_club_memory.id;


--
-- Name: fan_club_memory_report; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_memory_report (
    id integer NOT NULL,
    reporter_id bigint NOT NULL,
    memory_id bigint NOT NULL,
    reason character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_memory_report_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_memory_report_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_memory_report_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_memory_report_id_seq OWNED BY public.fan_club_memory_report.id;


--
-- Name: fan_club_officer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_officer (
    id integer NOT NULL,
    club_id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    role character varying NOT NULL,
    elected_at timestamp with time zone,
    term_ends_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_officer_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_officer_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_officer_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_officer_id_seq OWNED BY public.fan_club_officer.id;


--
-- Name: fan_club_post; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_post (
    id integer NOT NULL,
    club_id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    parent_id bigint,
    title character varying,
    content character varying NOT NULL,
    is_pinned boolean DEFAULT false NOT NULL,
    is_hidden boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone
);


--
-- Name: fan_club_post_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_post_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_post_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_post_id_seq OWNED BY public.fan_club_post.id;


--
-- Name: fan_club_vote; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_club_vote (
    id integer NOT NULL,
    election_id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    candidacy_id bigint NOT NULL,
    role character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: fan_club_vote_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_club_vote_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_club_vote_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_club_vote_id_seq OWNED BY public.fan_club_vote.id;


--
-- Name: fan_follow; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_follow (
    id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    artist_party_id bigint NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: fan_follow_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_follow_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_follow_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_follow_id_seq OWNED BY public.fan_follow.id;


--
-- Name: fan_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.fan_profile (
    id bigint NOT NULL,
    fan_party_id bigint NOT NULL,
    display_name character varying,
    avatar_url character varying,
    favorite_genres character varying,
    bio character varying,
    city character varying,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone
);


--
-- Name: fan_profile_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.fan_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fan_profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.fan_profile_id_seq OWNED BY public.fan_profile.id;


--
-- Name: feature_access_request_history; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feature_access_request_history (
    id bigint NOT NULL,
    request_id bigint NOT NULL,
    actor_party_id bigint,
    transition text NOT NULL,
    from_status text,
    to_status text NOT NULL,
    note text,
    created_at timestamp with time zone NOT NULL,
    CONSTRAINT feature_access_request_history_note_length_check CHECK (((note IS NULL) OR (char_length(note) <= 2000)))
);


--
-- Name: feature_access_request_history_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.feature_access_request_history_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: feature_access_request_history_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.feature_access_request_history_id_seq OWNED BY public.feature_access_request_history.id;


--
-- Name: feature_access_requests; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feature_access_requests (
    id bigint NOT NULL,
    requester_party_id bigint NOT NULL,
    feature_id text NOT NULL,
    action text NOT NULL,
    role_context text NOT NULL,
    module_context text NOT NULL,
    justification text,
    status text DEFAULT 'pending'::text NOT NULL,
    reviewer_group text NOT NULL,
    reviewer_party_id bigint,
    reviewer_notes text,
    requested_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    decided_at timestamp with time zone,
    cancelled_at timestamp with time zone,
    expires_at timestamp with time zone,
    CONSTRAINT feature_access_requests_action_check CHECK ((action = ANY (ARRAY['discover'::text, 'view'::text, 'create'::text, 'edit'::text, 'delete'::text, 'archive'::text, 'deactivate'::text, 'import'::text, 'export'::text, 'submit'::text, 'validate'::text, 'approve'::text, 'reject'::text, 'assign'::text, 'publish'::text, 'report'::text, 'administer'::text]))),
    CONSTRAINT feature_access_requests_justification_length_check CHECK (((justification IS NULL) OR (char_length(justification) <= 2000))),
    CONSTRAINT feature_access_requests_reviewer_notes_length_check CHECK (((reviewer_notes IS NULL) OR (char_length(reviewer_notes) <= 2000))),
    CONSTRAINT feature_access_requests_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'approved'::text, 'rejected'::text, 'cancelled'::text, 'expired'::text])))
);


--
-- Name: feature_access_requests_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.feature_access_requests_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: feature_access_requests_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.feature_access_requests_id_seq OWNED BY public.feature_access_requests.id;


--
-- Name: feature_navigation_preferences; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feature_navigation_preferences (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    feature_id text NOT NULL,
    favorite boolean DEFAULT false NOT NULL,
    pinned boolean DEFAULT false NOT NULL,
    pin_order integer,
    last_visited_at timestamp with time zone,
    use_count integer DEFAULT 0 NOT NULL,
    updated_at timestamp with time zone NOT NULL,
    CONSTRAINT feature_navigation_preferences_feature_id_check CHECK ((((length(feature_id) >= 1) AND (length(feature_id) <= 160)) AND (feature_id !~ '[[:cntrl:]]'::text))),
    CONSTRAINT feature_navigation_preferences_pin_order_check CHECK (((pinned AND ((pin_order >= 0) AND (pin_order <= 1000))) OR ((NOT pinned) AND (pin_order IS NULL)))),
    CONSTRAINT feature_navigation_preferences_use_count_check CHECK ((use_count >= 0))
);


--
-- Name: feature_navigation_preferences_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.feature_navigation_preferences_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: feature_navigation_preferences_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.feature_navigation_preferences_id_seq OWNED BY public.feature_navigation_preferences.id;


--
-- Name: feedback; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.feedback (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title character varying NOT NULL,
    description character varying NOT NULL,
    category character varying,
    severity character varying,
    contact_email character varying,
    attachment character varying,
    consent boolean DEFAULT false NOT NULL,
    created_by bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: input_list; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.input_list (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    session_id uuid NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: input_list_template; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.input_list_template (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying NOT NULL,
    genre character varying,
    channel_count bigint,
    notes character varying
);


--
-- Name: input_list_template_row; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.input_list_template_row (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    template_id uuid NOT NULL,
    channel_number bigint NOT NULL,
    track_name character varying,
    instrument character varying,
    mic_id uuid,
    stand_id uuid,
    cable_id uuid,
    preamp_id uuid,
    insert_outboard_id uuid,
    converter_channel character varying,
    phantom boolean,
    polarity boolean,
    hpf boolean,
    pad boolean,
    notes character varying
);


--
-- Name: input_list_version; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.input_list_version (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    input_list_id uuid NOT NULL,
    version bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by_ref character varying,
    notes character varying
);


--
-- Name: input_row; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.input_row (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    version_id uuid NOT NULL,
    channel_number bigint NOT NULL,
    track_name character varying,
    instrument character varying,
    mic_id uuid,
    stand_id uuid,
    cable_id uuid,
    preamp_id uuid,
    insert_outboard_id uuid,
    converter_channel character varying,
    phantom boolean,
    polarity boolean,
    hpf boolean,
    pad boolean,
    notes character varying
);


--
-- Name: instagram_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.instagram_message (
    id bigint NOT NULL,
    external_id character varying NOT NULL,
    sender_id character varying NOT NULL,
    sender_name character varying,
    text character varying,
    direction character varying NOT NULL,
    created_at timestamp with time zone NOT NULL,
    ad_external_id character varying,
    ad_name character varying,
    campaign_external_id character varying,
    campaign_name character varying,
    metadata character varying,
    replied_at timestamp with time zone,
    reply_text character varying,
    reply_error character varying,
    reply_status character varying DEFAULT 'pending'::character varying NOT NULL,
    hold_reason character varying,
    hold_required_fields character varying,
    last_attempt_at timestamp with time zone,
    attempt_count bigint DEFAULT 0 NOT NULL,
    deleted_at timestamp with time zone
);


--
-- Name: instagram_message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.instagram_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: instagram_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.instagram_message_id_seq OWNED BY public.instagram_message.id;


--
-- Name: intern_permission_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_permission_request (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    party_id bigint NOT NULL,
    category character varying NOT NULL,
    reason character varying,
    start_at date NOT NULL,
    end_at date,
    status character varying DEFAULT 'pending'::character varying NOT NULL,
    reviewed_by bigint,
    reviewed_at timestamp with time zone,
    decision_notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: intern_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_profile (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    party_id bigint NOT NULL,
    start_at date,
    end_at date,
    required_hours bigint,
    skills character varying,
    areas character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: intern_project; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_project (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title character varying NOT NULL,
    description character varying,
    status character varying DEFAULT 'active'::character varying NOT NULL,
    start_at date,
    due_at date,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: intern_task; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_task (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    project_id uuid NOT NULL,
    title character varying NOT NULL,
    description character varying,
    status character varying DEFAULT 'todo'::character varying NOT NULL,
    progress bigint DEFAULT 0 NOT NULL,
    assigned_to bigint,
    due_at date,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: intern_time_entry; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_time_entry (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    party_id bigint NOT NULL,
    clock_in timestamp with time zone NOT NULL,
    clock_out timestamp with time zone,
    notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: intern_todo; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.intern_todo (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    owner_party_id bigint NOT NULL,
    text character varying NOT NULL,
    done boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: invoice; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.invoice (
    id bigint NOT NULL,
    customer_id bigint NOT NULL,
    issue_date date NOT NULL,
    due_date date NOT NULL,
    number character varying,
    status character varying NOT NULL,
    currency character varying NOT NULL,
    subtotal_cents bigint NOT NULL,
    tax_cents bigint NOT NULL,
    total_cents bigint NOT NULL,
    sri_document_id character varying,
    notes character varying,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: invoice_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.invoice_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: invoice_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.invoice_id_seq OWNED BY public.invoice.id;


--
-- Name: invoice_line; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.invoice_line (
    id bigint NOT NULL,
    invoice_id bigint NOT NULL,
    service_order_id bigint,
    package_purchase_id bigint,
    description character varying NOT NULL,
    quantity bigint NOT NULL,
    unit_cents bigint NOT NULL,
    tax_bps bigint NOT NULL,
    total_cents bigint NOT NULL
);


--
-- Name: invoice_line_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.invoice_line_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: invoice_line_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.invoice_line_id_seq OWNED BY public.invoice_line.id;


--
-- Name: label_track; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.label_track (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title character varying NOT NULL,
    note character varying,
    status character varying DEFAULT 'open'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    owner_party_id bigint
);


--
-- Name: lead_interest; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.lead_interest (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    interest_type character varying NOT NULL,
    subject_id bigint,
    details character varying,
    source character varying NOT NULL,
    drive_link character varying,
    status character varying DEFAULT 'Open'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: lead_interest_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.lead_interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: lead_interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.lead_interest_id_seq OWNED BY public.lead_interest.id;


--
-- Name: live_session_intake; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.live_session_intake (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    band_name character varying NOT NULL,
    contact_email character varying,
    contact_phone character varying,
    session_date date,
    rider_path character varying,
    created_by bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    availability character varying,
    accepted_terms boolean DEFAULT false NOT NULL,
    terms_version character varying,
    band_description character varying,
    primary_genre character varying,
    input_list character varying
);


--
-- Name: live_session_musician; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.live_session_musician (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    intake_id uuid NOT NULL,
    party_id bigint NOT NULL,
    name character varying NOT NULL,
    email character varying,
    instrument character varying,
    role character varying,
    notes character varying,
    is_existing boolean DEFAULT false NOT NULL
);


--
-- Name: live_session_song; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.live_session_song (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    intake_id uuid NOT NULL,
    title character varying NOT NULL,
    bpm bigint,
    song_key character varying,
    lyrics character varying,
    sort_order bigint DEFAULT 0 NOT NULL
);


--
-- Name: maintenance_attachment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.maintenance_attachment (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    ticket_id uuid NOT NULL,
    drive_file_id character varying NOT NULL,
    label character varying
);


--
-- Name: maintenance_ticket; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.maintenance_ticket (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_id uuid NOT NULL,
    status character varying NOT NULL,
    opened_at timestamp with time zone DEFAULT now() NOT NULL,
    closed_at timestamp with time zone,
    vendor_party_ref character varying,
    summary character varying NOT NULL,
    details character varying
);


--
-- Name: marketplace_cart; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.marketplace_cart (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: marketplace_cart_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.marketplace_cart_item (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    cart_id uuid NOT NULL,
    listing_id uuid NOT NULL,
    quantity bigint DEFAULT 1 NOT NULL
);


--
-- Name: marketplace_listing; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.marketplace_listing (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    asset_id uuid NOT NULL,
    title character varying NOT NULL,
    price_usd_cents bigint NOT NULL,
    markup_pct bigint DEFAULT 25 NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    purpose character varying DEFAULT 'sale'::character varying NOT NULL
);


--
-- Name: marketplace_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.marketplace_order (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    cart_id uuid,
    buyer_name character varying NOT NULL,
    buyer_email character varying NOT NULL,
    buyer_phone character varying,
    total_usd_cents bigint NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    status character varying DEFAULT 'pending'::character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    payment_provider character varying,
    paypal_order_id character varying,
    paypal_payer_email character varying,
    datafast_checkout_id character varying,
    datafast_resource_path character varying,
    datafast_payment_id character varying,
    datafast_result_code character varying,
    datafast_result_description character varying,
    datafast_payment_brand character varying,
    datafast_auth_code character varying,
    datafast_acquirer_code character varying,
    paid_at timestamp with time zone,
    stripe_payment_intent_id text,
    stripe_idempotency_key text
);


--
-- Name: marketplace_order_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.marketplace_order_item (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    order_id uuid NOT NULL,
    listing_id uuid NOT NULL,
    quantity bigint NOT NULL,
    unit_price_usd_cents bigint NOT NULL,
    subtotal_usd_cents bigint NOT NULL
);


--
-- Name: notification; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.notification (
    id bigint NOT NULL,
    recipient_party_id bigint NOT NULL,
    notif_type text NOT NULL,
    title text NOT NULL,
    body text NOT NULL,
    target_type text,
    target_id bigint,
    is_read boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT notification_notif_type_check CHECK ((notif_type = ANY (ARRAY['reaction_received'::text, 'post_trending'::text, 'weekly_top'::text, 'artist_liked'::text])))
);


--
-- Name: notification_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.notification_id_seq OWNED BY public.notification.id;


--
-- Name: operations_admin_audit; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_admin_audit (
    id bigint NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    actor_party_id bigint,
    acting_role text NOT NULL,
    source_client text NOT NULL,
    action text NOT NULL,
    target_entity_type text NOT NULL,
    target_entity_id text NOT NULL,
    previous_value jsonb,
    new_value jsonb,
    request_id text NOT NULL,
    correlation_id text NOT NULL,
    approval_request_id uuid,
    reason text,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: operations_admin_audit_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.operations_admin_audit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: operations_admin_audit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.operations_admin_audit_id_seq OWNED BY public.operations_admin_audit.id;


--
-- Name: operations_aggregate_sequence; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_aggregate_sequence (
    organization_id uuid NOT NULL,
    aggregate_type text NOT NULL,
    aggregate_id text NOT NULL,
    last_sequence bigint NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_aggregate_sequence_last_sequence_check CHECK ((last_sequence > 0))
);


--
-- Name: operations_approval_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_approval_request (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    work_item_id uuid,
    action_type text NOT NULL,
    target_entity_type text NOT NULL,
    target_entity_id text NOT NULL,
    amount_minor bigint,
    currency character(3),
    requester_party_id bigint NOT NULL,
    requester_role text NOT NULL,
    request_reason text NOT NULL,
    requested_at timestamp with time zone DEFAULT now() NOT NULL,
    approver_party_id bigint,
    approver_role text,
    decision text DEFAULT 'pending'::text NOT NULL,
    decision_reason text,
    decided_at timestamp with time zone,
    expires_at timestamp with time zone,
    execution_status text DEFAULT 'not_started'::text NOT NULL,
    idempotency_key text NOT NULL,
    CONSTRAINT operations_approval_request_check CHECK (((approver_party_id IS NULL) OR (approver_party_id <> requester_party_id))),
    CONSTRAINT operations_approval_request_check1 CHECK ((((decision = 'pending'::text) AND (approver_party_id IS NULL) AND (decided_at IS NULL)) OR (decision <> 'pending'::text))),
    CONSTRAINT operations_approval_request_decision_check CHECK ((decision = ANY (ARRAY['pending'::text, 'approved'::text, 'rejected'::text, 'expired'::text, 'cancelled'::text]))),
    CONSTRAINT operations_approval_request_execution_status_check CHECK ((execution_status = ANY (ARRAY['not_started'::text, 'pending'::text, 'completed'::text, 'failed'::text])))
);


--
-- Name: operations_backfill_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_backfill_run (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    source_name text NOT NULL,
    run_key text DEFAULT 'default'::text NOT NULL,
    status text DEFAULT 'running'::text NOT NULL,
    dry_run boolean DEFAULT true NOT NULL,
    cursor_value text,
    scanned_count bigint DEFAULT 0 NOT NULL,
    eligible_count bigint DEFAULT 0 NOT NULL,
    inserted_count bigint DEFAULT 0 NOT NULL,
    skipped_count bigint DEFAULT 0 NOT NULL,
    error_count bigint DEFAULT 0 NOT NULL,
    started_at timestamp with time zone DEFAULT now() NOT NULL,
    heartbeat_at timestamp with time zone DEFAULT now() NOT NULL,
    finished_at timestamp with time zone,
    CONSTRAINT operations_backfill_run_status_check CHECK ((status = ANY (ARRAY['running'::text, 'completed'::text, 'failed'::text, 'cancelled'::text])))
);


--
-- Name: operations_branch; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_branch (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    slug text NOT NULL,
    display_name text NOT NULL,
    timezone text DEFAULT 'America/Guayaquil'::text NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: operations_business_hours; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_business_hours (
    organization_id uuid NOT NULL,
    branch_id uuid NOT NULL,
    iso_weekday smallint NOT NULL,
    opens_at time without time zone NOT NULL,
    closes_at time without time zone NOT NULL,
    CONSTRAINT operations_business_hours_check CHECK ((opens_at < closes_at)),
    CONSTRAINT operations_business_hours_iso_weekday_check CHECK (((iso_weekday >= 1) AND (iso_weekday <= 7)))
);


--
-- Name: operations_domain_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_domain_event (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    event_type text NOT NULL,
    aggregate_type text NOT NULL,
    aggregate_id text NOT NULL,
    source_system text NOT NULL,
    source_channel text NOT NULL,
    correlation_key text NOT NULL,
    deduplication_key text NOT NULL,
    provider_event_id text,
    occurred_at timestamp with time zone NOT NULL,
    recorded_at timestamp with time zone DEFAULT now() NOT NULL,
    continuous_sla boolean DEFAULT false NOT NULL,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT operations_domain_event_correlation_key_check CHECK (((length(correlation_key) >= 1) AND (length(correlation_key) <= 320))),
    CONSTRAINT operations_domain_event_event_type_check CHECK (((length(event_type) >= 1) AND (length(event_type) <= 160))),
    CONSTRAINT operations_domain_event_payload_check CHECK ((jsonb_typeof(payload) = 'object'::text))
);


--
-- Name: operations_holiday; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_holiday (
    organization_id uuid NOT NULL,
    branch_id uuid NOT NULL,
    holiday_date date NOT NULL,
    label text NOT NULL
);


--
-- Name: operations_inbound_receipt; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_inbound_receipt (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    provider text NOT NULL,
    provider_event_id text NOT NULL,
    signature_verified boolean NOT NULL,
    received_at timestamp with time zone DEFAULT now() NOT NULL,
    occurred_at timestamp with time zone,
    payload_digest text NOT NULL,
    replay_window_valid boolean NOT NULL,
    correlation_status text NOT NULL,
    party_id bigint,
    entity_type text,
    entity_id text,
    redacted_metadata jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT operations_inbound_receipt_correlation_status_check CHECK ((correlation_status = ANY (ARRAY['correlated'::text, 'uncertain'::text, 'uncorrelated'::text, 'rejected'::text])))
);


--
-- Name: operations_integration_failure; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_integration_failure (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    provider text NOT NULL,
    direction text NOT NULL,
    source_record_type text NOT NULL,
    source_record_id text NOT NULL,
    failure_code text NOT NULL,
    redacted_summary text NOT NULL,
    retryable boolean NOT NULL,
    status text DEFAULT 'open'::text NOT NULL,
    attempt_count integer DEFAULT 0 NOT NULL,
    last_attempt_at timestamp with time zone,
    next_attempt_at timestamp with time zone,
    resolved_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_integration_failure_direction_check CHECK ((direction = ANY (ARRAY['inbound'::text, 'outbound'::text, 'internal'::text]))),
    CONSTRAINT operations_integration_failure_status_check CHECK ((status = ANY (ARRAY['open'::text, 'retrying'::text, 'resolved'::text, 'dead_letter'::text])))
);


--
-- Name: operations_mention; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_mention (
    note_id uuid NOT NULL,
    mentioned_party_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: operations_note; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_note (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    work_item_id uuid NOT NULL,
    author_party_id bigint NOT NULL,
    body text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    edited_at timestamp with time zone,
    CONSTRAINT operations_note_body_check CHECK (((length(btrim(body)) >= 1) AND (length(btrim(body)) <= 5000)))
);


--
-- Name: operations_organization; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_organization (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    slug text NOT NULL,
    display_name text NOT NULL,
    default_timezone text DEFAULT 'America/Guayaquil'::text NOT NULL,
    default_currency character(3) DEFAULT 'USD'::bpchar NOT NULL,
    operations_enabled boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_organization_default_currency_check CHECK ((default_currency ~ '^[A-Z]{3}$'::text)),
    CONSTRAINT operations_organization_slug_check CHECK ((slug ~ '^[a-z0-9][a-z0-9-]{1,62}$'::text))
);


--
-- Name: operations_outbound_delivery; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_outbound_delivery (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    work_item_id uuid,
    channel text NOT NULL,
    provider text NOT NULL,
    template_key text,
    recipient_ref text NOT NULL,
    consent_basis text,
    idempotency_key text NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempt_count integer DEFAULT 0 NOT NULL,
    next_attempt_at timestamp with time zone DEFAULT now() NOT NULL,
    provider_message_id text,
    last_error_code text,
    last_error_redacted text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    delivered_at timestamp with time zone,
    CONSTRAINT operations_outbound_delivery_attempt_count_check CHECK ((attempt_count >= 0)),
    CONSTRAINT operations_outbound_delivery_channel_check CHECK ((channel = ANY (ARRAY['email'::text, 'push'::text, 'sms'::text, 'whatsapp'::text]))),
    CONSTRAINT operations_outbound_delivery_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'sending'::text, 'delivered'::text, 'failed'::text, 'dead_letter'::text])))
);


--
-- Name: operations_outbox; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_outbox (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    event_id uuid NOT NULL,
    aggregate_type text NOT NULL,
    aggregate_id text NOT NULL,
    aggregate_sequence bigint NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    attempt_count integer DEFAULT 0 NOT NULL,
    next_attempt_at timestamp with time zone DEFAULT now() NOT NULL,
    locked_at timestamp with time zone,
    locked_by text,
    processed_at timestamp with time zone,
    last_error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_outbox_attempt_count_check CHECK ((attempt_count >= 0)),
    CONSTRAINT operations_outbox_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'processing'::text, 'processed'::text, 'dead_letter'::text])))
);


--
-- Name: operations_provider_config; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_provider_config (
    organization_id uuid NOT NULL,
    provider text NOT NULL,
    country_code character(2) NOT NULL,
    currency character(3) NOT NULL,
    enabled boolean DEFAULT false NOT NULL,
    sandbox boolean DEFAULT true NOT NULL,
    configuration jsonb DEFAULT '{}'::jsonb NOT NULL,
    updated_by bigint,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_provider_config_configuration_check CHECK ((configuration ?& ARRAY[]::text[])),
    CONSTRAINT operations_provider_config_configuration_check1 CHECK ((NOT (configuration ?| ARRAY['secret'::text, 'token'::text, 'password'::text, 'privateKey'::text, 'certificate'::text])))
);


--
-- Name: operations_push_subscription; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_push_subscription (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    party_id bigint NOT NULL,
    platform text NOT NULL,
    device_token_digest text NOT NULL,
    encrypted_device_token bytea NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_push_subscription_platform_check CHECK ((platform = ANY (ARRAY['ios'::text, 'android'::text, 'web'::text])))
);


--
-- Name: operations_saved_view; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_saved_view (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    owner_party_id bigint,
    name text NOT NULL,
    shared boolean DEFAULT false NOT NULL,
    filters jsonb DEFAULT '{}'::jsonb NOT NULL,
    columns jsonb DEFAULT '[]'::jsonb NOT NULL,
    widgets jsonb DEFAULT '[]'::jsonb NOT NULL,
    subscribed_event_types jsonb DEFAULT '[]'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: operations_scope_member; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_scope_member (
    organization_id uuid NOT NULL,
    branch_id uuid NOT NULL,
    party_id bigint NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_scope_member_branch_id_check CHECK ((branch_id IS NOT NULL))
);


--
-- Name: operations_sla_reminder; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_sla_reminder (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    work_item_id uuid NOT NULL,
    timer_id uuid NOT NULL,
    threshold_percent smallint NOT NULL,
    target_role text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    acknowledged_at timestamp with time zone,
    CONSTRAINT operations_sla_reminder_threshold_percent_check CHECK ((threshold_percent = ANY (ARRAY[50, 80, 100, 150])))
);


--
-- Name: operations_sla_timer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_sla_timer (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    work_item_id uuid NOT NULL,
    phase text NOT NULL,
    starts_at timestamp with time zone NOT NULL,
    due_at timestamp with time zone NOT NULL,
    continuous_elapsed boolean DEFAULT false NOT NULL,
    paused_at timestamp with time zone,
    paused_seconds bigint DEFAULT 0 NOT NULL,
    completed_at timestamp with time zone,
    breached_at timestamp with time zone,
    CONSTRAINT operations_sla_timer_paused_seconds_check CHECK ((paused_seconds >= 0)),
    CONSTRAINT operations_sla_timer_phase_check CHECK ((phase = ANY (ARRAY['acknowledge'::text, 'mitigate'::text, 'resolve'::text])))
);


--
-- Name: operations_stream_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_stream_event (
    id bigint NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    event_type text NOT NULL,
    work_item_id uuid,
    visible_to_party_id bigint,
    payload jsonb DEFAULT '{}'::jsonb NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: operations_stream_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.operations_stream_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: operations_stream_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.operations_stream_event_id_seq OWNED BY public.operations_stream_event.id;


--
-- Name: operations_work_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_work_item (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    organization_id uuid NOT NULL,
    branch_id uuid,
    source_system text NOT NULL,
    source_channel text NOT NULL,
    entity_type text NOT NULL,
    entity_id text,
    uncorrelated boolean DEFAULT false NOT NULL,
    correlation_key text NOT NULL,
    external_provider_event_id text,
    title_es text NOT NULL,
    title_en text NOT NULL,
    description_es text NOT NULL,
    description_en text NOT NULL,
    status text DEFAULT 'new'::text NOT NULL,
    priority text DEFAULT 'normal'::text NOT NULL,
    recommended_priority text DEFAULT 'normal'::text NOT NULL,
    priority_override_reason text,
    severity text DEFAULT 'info'::text NOT NULL,
    first_seen_by bigint,
    first_seen_at timestamp with time zone,
    assignee_party_id bigint,
    responsible_team text,
    customer_party_id bigint,
    service_key text,
    amount_minor bigint,
    currency character(3),
    payment_state text,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    due_at timestamp with time zone,
    snoozed_until timestamp with time zone,
    waiting_started_at timestamp with time zone,
    waiting_reason text,
    waiting_external_dependency boolean DEFAULT false NOT NULL,
    resume_at timestamp with time zone,
    resolved_at timestamp with time zone,
    archived_at timestamp with time zone,
    sla_breached_at timestamp with time zone,
    version bigint DEFAULT 1 NOT NULL,
    metadata jsonb DEFAULT '{}'::jsonb NOT NULL,
    CONSTRAINT operations_work_item_check CHECK (((uncorrelated AND (entity_id IS NULL)) OR ((NOT uncorrelated) AND (entity_id IS NOT NULL)))),
    CONSTRAINT operations_work_item_check1 CHECK ((((status = 'waiting'::text) AND (waiting_reason IS NOT NULL)) OR (status <> 'waiting'::text))),
    CONSTRAINT operations_work_item_check2 CHECK ((((first_seen_at IS NULL) AND (first_seen_by IS NULL)) OR ((first_seen_at IS NOT NULL) AND (first_seen_by IS NOT NULL)))),
    CONSTRAINT operations_work_item_currency_check CHECK (((currency IS NULL) OR (currency ~ '^[A-Z]{3}$'::text))),
    CONSTRAINT operations_work_item_metadata_check CHECK ((jsonb_typeof(metadata) = 'object'::text)),
    CONSTRAINT operations_work_item_priority_check CHECK ((priority = ANY (ARRAY['urgent'::text, 'high'::text, 'normal'::text, 'low'::text]))),
    CONSTRAINT operations_work_item_recommended_priority_check CHECK ((recommended_priority = ANY (ARRAY['urgent'::text, 'high'::text, 'normal'::text, 'low'::text]))),
    CONSTRAINT operations_work_item_severity_check CHECK ((severity = ANY (ARRAY['critical'::text, 'error'::text, 'warning'::text, 'info'::text]))),
    CONSTRAINT operations_work_item_status_check CHECK ((status = ANY (ARRAY['new'::text, 'seen'::text, 'assigned'::text, 'in_progress'::text, 'waiting'::text, 'resolved'::text, 'archived'::text])))
);


--
-- Name: operations_work_item_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.operations_work_item_event (
    id bigint NOT NULL,
    organization_id uuid NOT NULL,
    work_item_id uuid NOT NULL,
    domain_event_id uuid,
    event_type text NOT NULL,
    actor_party_id bigint,
    actor_role text,
    body_es text NOT NULL,
    body_en text NOT NULL,
    metadata jsonb DEFAULT '{}'::jsonb NOT NULL,
    occurred_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT operations_work_item_event_metadata_check CHECK ((jsonb_typeof(metadata) = 'object'::text))
);


--
-- Name: operations_work_item_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.operations_work_item_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: operations_work_item_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.operations_work_item_event_id_seq OWNED BY public.operations_work_item_event.id;


--
-- Name: package_catalog; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_catalog (
    id bigint NOT NULL,
    subject_id bigint NOT NULL,
    name character varying NOT NULL,
    hours_qty bigint NOT NULL,
    price_cents bigint NOT NULL,
    expires_days bigint NOT NULL,
    refund_policy character varying NOT NULL,
    active boolean DEFAULT true NOT NULL
);


--
-- Name: package_catalog_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.package_catalog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_catalog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.package_catalog_id_seq OWNED BY public.package_catalog.id;


--
-- Name: package_ledger; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_ledger (
    id bigint NOT NULL,
    purchase_id bigint NOT NULL,
    booking_id bigint,
    delta_units bigint NOT NULL,
    notes character varying,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: package_ledger_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.package_ledger_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_ledger_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.package_ledger_id_seq OWNED BY public.package_ledger.id;


--
-- Name: package_product; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_product (
    id bigint NOT NULL,
    name character varying NOT NULL,
    service_kind character varying NOT NULL,
    units_kind character varying NOT NULL,
    units_qty bigint NOT NULL,
    price_cents bigint NOT NULL,
    expires_days bigint,
    transferable boolean NOT NULL,
    refund_policy character varying NOT NULL,
    active boolean NOT NULL
);


--
-- Name: package_product_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.package_product_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_product_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.package_product_id_seq OWNED BY public.package_product.id;


--
-- Name: package_purchase; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.package_purchase (
    id bigint NOT NULL,
    buyer_id bigint NOT NULL,
    product_id bigint NOT NULL,
    purchased_at timestamp with time zone NOT NULL,
    price_cents bigint NOT NULL,
    expires_at timestamp with time zone,
    remaining_units bigint NOT NULL,
    status character varying NOT NULL
);


--
-- Name: package_purchase_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.package_purchase_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: package_purchase_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.package_purchase_id_seq OWNED BY public.package_purchase.id;


--
-- Name: party; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.party (
    id bigint NOT NULL,
    legal_name character varying,
    display_name character varying NOT NULL,
    is_org boolean NOT NULL,
    tax_id character varying,
    primary_email character varying,
    primary_phone character varying,
    whatsapp character varying,
    instagram character varying,
    emergency_contact character varying,
    notes character varying,
    created_at timestamp with time zone NOT NULL,
    stripe_customer_id text,
    country_code text
);


--
-- Name: party_follow; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.party_follow (
    id bigint NOT NULL,
    follower_party_id bigint NOT NULL,
    following_party_id bigint NOT NULL,
    via_nfc boolean NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: party_follow_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.party_follow_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: party_follow_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.party_follow_id_seq OWNED BY public.party_follow.id;


--
-- Name: party_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.party_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: party_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.party_id_seq OWNED BY public.party.id;


--
-- Name: party_radio_presence; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.party_radio_presence (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    stream_url character varying NOT NULL,
    station_name character varying,
    station_id character varying,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: party_radio_presence_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.party_radio_presence_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: party_radio_presence_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.party_radio_presence_id_seq OWNED BY public.party_radio_presence.id;


--
-- Name: party_role; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.party_role (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    role character varying NOT NULL,
    active boolean NOT NULL
);


--
-- Name: party_role_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.party_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: party_role_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.party_role_id_seq OWNED BY public.party_role.id;


--
-- Name: payment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment (
    id bigint NOT NULL,
    invoice_id bigint,
    method character varying NOT NULL,
    amount_cents bigint NOT NULL,
    received_at timestamp with time zone NOT NULL,
    reference character varying,
    created_by bigint,
    order_id bigint,
    party_id bigint NOT NULL,
    concept character varying,
    period character varying,
    attachment character varying,
    created_at timestamp with time zone,
    currency text DEFAULT 'USD'::text NOT NULL
);


--
-- Name: payment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.payment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: payment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.payment_id_seq OWNED BY public.payment.id;


--
-- Name: payment_split; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.payment_split (
    id bigint NOT NULL,
    payment_id bigint NOT NULL,
    payer_id bigint NOT NULL,
    amount_cents bigint NOT NULL
);


--
-- Name: payment_split_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.payment_split_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: payment_split_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.payment_split_id_seq OWNED BY public.payment_split.id;


--
-- Name: pipeline_card; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.pipeline_card (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    service_kind character varying NOT NULL,
    title character varying NOT NULL,
    artist character varying,
    stage character varying NOT NULL,
    sort_order bigint DEFAULT 0 NOT NULL,
    notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: promo_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.promo_code (
    id bigint NOT NULL,
    event_id bigint,
    code character varying NOT NULL,
    description character varying,
    discount_type character varying NOT NULL,
    discount_value bigint NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    max_redemptions bigint,
    current_redemptions bigint DEFAULT 0 NOT NULL,
    valid_from timestamp with time zone,
    valid_until timestamp with time zone,
    tier_ids character varying,
    min_purchase_amount_cents bigint,
    is_active boolean DEFAULT true NOT NULL,
    created_by_party_id character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: promo_code_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.promo_code_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: promo_code_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.promo_code_id_seq OWNED BY public.promo_code.id;


--
-- Name: promo_code_redemption; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.promo_code_redemption (
    id bigint NOT NULL,
    promo_code_id bigint NOT NULL,
    order_id bigint NOT NULL,
    discount_amount_cents bigint NOT NULL,
    redeemed_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: promo_code_redemption_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.promo_code_redemption_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: promo_code_redemption_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.promo_code_redemption_id_seq OWNED BY public.promo_code_redemption.id;


--
-- Name: proposal; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.proposal (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title character varying NOT NULL,
    service_kind character varying,
    client_party_id bigint,
    contact_name character varying,
    contact_email character varying,
    contact_phone character varying,
    pipeline_card_id uuid,
    status character varying DEFAULT 'draft'::character varying NOT NULL,
    notes character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    last_generated_at timestamp with time zone,
    sent_at timestamp with time zone
);


--
-- Name: proposal_version; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.proposal_version (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    proposal_id uuid NOT NULL,
    version bigint NOT NULL,
    latex character varying NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by_ref character varying,
    notes character varying
);


--
-- Name: radio_stream; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.radio_stream (
    id bigint NOT NULL,
    stream_url character varying NOT NULL,
    name character varying,
    country character varying,
    genre character varying,
    is_active boolean NOT NULL,
    last_checked_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: radio_stream_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.radio_stream_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: radio_stream_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.radio_stream_id_seq OWNED BY public.radio_stream.id;


--
-- Name: rag_chunk; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.rag_chunk (
    id bigint NOT NULL,
    source text NOT NULL,
    source_id text,
    chunk_index integer NOT NULL,
    content text NOT NULL,
    metadata jsonb,
    embedding public.vector(1536) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: rag_chunk_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.rag_chunk_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: rag_chunk_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.rag_chunk_id_seq OWNED BY public.rag_chunk.id;


--
-- Name: receipt; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.receipt (
    id bigint NOT NULL,
    invoice_id bigint NOT NULL,
    number character varying NOT NULL,
    issue_date date NOT NULL,
    issued_at timestamp with time zone NOT NULL,
    buyer_party_id bigint,
    buyer_name character varying NOT NULL,
    buyer_email character varying,
    currency character varying NOT NULL,
    subtotal_cents bigint NOT NULL,
    tax_cents bigint NOT NULL,
    total_cents bigint NOT NULL,
    notes character varying,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: receipt_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.receipt_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: receipt_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.receipt_id_seq OWNED BY public.receipt.id;


--
-- Name: receipt_line; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.receipt_line (
    id bigint NOT NULL,
    receipt_id bigint NOT NULL,
    description character varying NOT NULL,
    quantity bigint NOT NULL,
    unit_cents bigint NOT NULL,
    tax_bps bigint,
    total_cents bigint NOT NULL
);


--
-- Name: receipt_line_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.receipt_line_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: receipt_line_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.receipt_line_id_seq OWNED BY public.receipt_line.id;


--
-- Name: referral_claim; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.referral_claim (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    code_id character varying NOT NULL,
    claimant_user_id uuid,
    email character varying NOT NULL,
    claimed_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: referral_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.referral_code (
    id character varying NOT NULL,
    owner_user_id uuid,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: resource; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.resource (
    id bigint NOT NULL,
    name character varying NOT NULL,
    slug character varying NOT NULL,
    resource_type character varying NOT NULL,
    capacity bigint,
    active boolean NOT NULL
);


--
-- Name: resource_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.resource_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: resource_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.resource_id_seq OWNED BY public.resource.id;


--
-- Name: room; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying NOT NULL,
    is_bookable boolean DEFAULT true NOT NULL,
    capacity bigint,
    channel_count bigint,
    default_sample_rate bigint,
    patchbay_notes character varying
);


--
-- Name: room_default_gear; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room_default_gear (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    room_id uuid NOT NULL,
    asset_id uuid NOT NULL
);


--
-- Name: room_feature; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.room_feature (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    room_id uuid NOT NULL,
    key character varying NOT NULL,
    value character varying NOT NULL
);


--
-- Name: service_ad; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_ad (
    id bigint NOT NULL,
    provider_party_id bigint NOT NULL,
    service_catalog_id bigint,
    role_tag character varying NOT NULL,
    headline character varying NOT NULL,
    description character varying,
    fee_cents bigint NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    slot_minutes bigint DEFAULT 60 NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: service_ad_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_ad_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_ad_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_ad_id_seq OWNED BY public.service_ad.id;


--
-- Name: service_ad_slot; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_ad_slot (
    id bigint NOT NULL,
    ad_id bigint NOT NULL,
    starts_at timestamp with time zone NOT NULL,
    ends_at timestamp with time zone NOT NULL,
    status character varying DEFAULT 'open'::character varying NOT NULL,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: service_ad_slot_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_ad_slot_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_ad_slot_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_ad_slot_id_seq OWNED BY public.service_ad_slot.id;


--
-- Name: service_catalog; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_catalog (
    id bigint NOT NULL,
    name character varying NOT NULL,
    kind character varying NOT NULL,
    pricing_model character varying NOT NULL,
    default_rate_cents bigint,
    tax_bps bigint,
    active boolean NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    billing_unit character varying
);


--
-- Name: service_catalog_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_catalog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_catalog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_catalog_id_seq OWNED BY public.service_catalog.id;


--
-- Name: service_escrow; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_escrow (
    id bigint NOT NULL,
    booking_id bigint NOT NULL,
    service_order_id bigint NOT NULL,
    ad_id bigint NOT NULL,
    patron_party_id bigint NOT NULL,
    provider_party_id bigint NOT NULL,
    amount_cents bigint NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    status character varying NOT NULL,
    held_payment_id bigint,
    released_payment_id bigint,
    held_at timestamp with time zone NOT NULL,
    released_at timestamp with time zone
);


--
-- Name: service_escrow_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_escrow_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_escrow_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_escrow_id_seq OWNED BY public.service_escrow.id;


--
-- Name: service_order; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_order (
    id bigint NOT NULL,
    customer_id bigint NOT NULL,
    artist_id bigint,
    catalog_id bigint NOT NULL,
    service_kind character varying NOT NULL,
    title character varying,
    description character varying,
    status character varying NOT NULL,
    price_quoted_cents bigint,
    quote_sent_at timestamp with time zone,
    scheduled_start timestamp with time zone,
    scheduled_end timestamp with time zone,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: service_order_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_order_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_order_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_order_id_seq OWNED BY public.service_order.id;


--
-- Name: service_status_change; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.service_status_change (
    id bigint NOT NULL,
    service_order_id bigint NOT NULL,
    status character varying NOT NULL,
    notes character varying,
    changed_by bigint,
    created_at timestamp with time zone NOT NULL
);


--
-- Name: service_status_change_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.service_status_change_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: service_status_change_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.service_status_change_id_seq OWNED BY public.service_status_change.id;


--
-- Name: session; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    booking_ref character varying,
    band_id uuid,
    client_party_ref character varying,
    service character varying NOT NULL,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone NOT NULL,
    engineer_ref character varying NOT NULL,
    assistant_ref character varying,
    status character varying DEFAULT 'InPrep'::character varying NOT NULL,
    sample_rate bigint,
    bit_depth bigint,
    daw character varying,
    session_folder_drive_id character varying,
    notes character varying
);


--
-- Name: session_deliverable; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session_deliverable (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    session_id uuid NOT NULL,
    kind character varying NOT NULL,
    name character varying NOT NULL,
    drive_file_id character varying,
    external_url character varying,
    delivered_at timestamp with time zone,
    approved_at timestamp with time zone,
    notes character varying
);


--
-- Name: session_invoice; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session_invoice (
    id bigint NOT NULL,
    session_id uuid NOT NULL,
    invoice_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: session_invoice_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.session_invoice_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: session_invoice_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.session_invoice_id_seq OWNED BY public.session_invoice.id;


--
-- Name: session_room; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.session_room (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    session_id uuid NOT NULL,
    room_id uuid NOT NULL
);


--
-- Name: social_artist_profile; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_artist_profile (
    id bigint NOT NULL,
    party_id character varying,
    name character varying NOT NULL,
    bio character varying,
    avatar_url character varying,
    genres text[],
    social_links character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    country_code text
);


--
-- Name: social_artist_profile_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_artist_profile_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_artist_profile_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_artist_profile_id_seq OWNED BY public.social_artist_profile.id;


--
-- Name: social_discovery_review; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_discovery_review (
    id bigint NOT NULL,
    social_sync_post_id bigint NOT NULL,
    status text DEFAULT 'pending'::text NOT NULL,
    review_notes text,
    reviewed_by_party_id bigint,
    reviewed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT social_discovery_review_review_notes_check CHECK ((char_length(review_notes) <= 2000)),
    CONSTRAINT social_discovery_review_status_check CHECK ((status = ANY (ARRAY['pending'::text, 'approved'::text, 'dismissed'::text])))
);


--
-- Name: social_discovery_review_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_discovery_review_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_discovery_review_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_discovery_review_id_seq OWNED BY public.social_discovery_review.id;


--
-- Name: social_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_event (
    id bigint NOT NULL,
    organizer_party_id character varying,
    title character varying NOT NULL,
    description character varying,
    venue_id bigint,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    price_cents bigint,
    capacity bigint,
    metadata character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    timezone text
);


--
-- Name: social_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_event_id_seq OWNED BY public.social_event.id;


--
-- Name: social_sync_account; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_sync_account (
    id bigint NOT NULL,
    party_id bigint,
    artist_profile_id bigint,
    platform character varying NOT NULL,
    external_user_id character varying NOT NULL,
    handle character varying,
    access_token character varying,
    token_expires_at timestamp with time zone,
    status character varying NOT NULL,
    last_synced_at timestamp with time zone,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone
);


--
-- Name: social_sync_account_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_sync_account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_sync_account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_sync_account_id_seq OWNED BY public.social_sync_account.id;


--
-- Name: social_sync_post; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_sync_post (
    id bigint NOT NULL,
    account_id bigint,
    platform character varying NOT NULL,
    external_post_id character varying NOT NULL,
    artist_party_id bigint,
    artist_profile_id bigint,
    caption character varying,
    permalink character varying,
    media_urls character varying,
    posted_at timestamp with time zone,
    fetched_at timestamp with time zone NOT NULL,
    tags character varying,
    summary character varying,
    ingest_source character varying NOT NULL,
    like_count bigint,
    comment_count bigint,
    share_count bigint,
    view_count bigint,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


--
-- Name: social_sync_post_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_sync_post_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_sync_post_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_sync_post_id_seq OWNED BY public.social_sync_post.id;


--
-- Name: social_sync_run; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.social_sync_run (
    id bigint NOT NULL,
    platform character varying NOT NULL,
    ingest_source character varying NOT NULL,
    started_at timestamp with time zone NOT NULL,
    ended_at timestamp with time zone,
    status character varying NOT NULL,
    new_posts bigint NOT NULL,
    updated_posts bigint NOT NULL,
    error_message character varying
);


--
-- Name: social_sync_run_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.social_sync_run_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: social_sync_run_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.social_sync_run_id_seq OWNED BY public.social_sync_run.id;


--
-- Name: stock_item; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.stock_item (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name character varying NOT NULL,
    sku character varying NOT NULL,
    unit character varying DEFAULT 'Pcs'::character varying NOT NULL,
    bin_location character varying,
    on_hand bigint DEFAULT 0 NOT NULL,
    reorder_point bigint,
    vendor_party_ref character varying,
    notes character varying
);


--
-- Name: stock_movement; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.stock_movement (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    stock_item_id uuid NOT NULL,
    change_qty bigint NOT NULL,
    reason character varying DEFAULT 'OtherMove'::character varying NOT NULL,
    at timestamp with time zone DEFAULT now() NOT NULL,
    ref_checkout_id uuid,
    ref_session_id uuid,
    notes character varying
);


--
-- Name: stripe_payment_intent; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.stripe_payment_intent (
    id bigint NOT NULL,
    order_id bigint NOT NULL,
    stripe_payment_intent_id character varying NOT NULL,
    stripe_client_secret character varying NOT NULL,
    amount_cents bigint NOT NULL,
    currency character varying DEFAULT 'USD'::character varying NOT NULL,
    status character varying NOT NULL,
    metadata character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: stripe_payment_intent_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.stripe_payment_intent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: stripe_payment_intent_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.stripe_payment_intent_id_seq OWNED BY public.stripe_payment_intent.id;


--
-- Name: stripe_webhook_event; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.stripe_webhook_event (
    id bigint NOT NULL,
    stripe_event_id character varying NOT NULL,
    event_type character varying NOT NULL,
    payload character varying NOT NULL,
    processed_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: stripe_webhook_event_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.stripe_webhook_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: stripe_webhook_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.stripe_webhook_event_id_seq OWNED BY public.stripe_webhook_event.id;


--
-- Name: studio_brain_entry; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.studio_brain_entry (
    id bigint NOT NULL,
    title character varying NOT NULL,
    body character varying NOT NULL,
    category character varying,
    tags text[],
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: studio_brain_entry_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.studio_brain_entry_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: studio_brain_entry_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.studio_brain_entry_id_seq OWNED BY public.studio_brain_entry.id;


--
-- Name: subject; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.subject (
    id bigint NOT NULL,
    name character varying NOT NULL,
    active boolean DEFAULT true NOT NULL
);


--
-- Name: subject_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.subject_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: subject_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.subject_id_seq OWNED BY public.subject.id;


--
-- Name: subject_room_preference; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.subject_room_preference (
    id bigint NOT NULL,
    subject_id bigint NOT NULL,
    room_id bigint NOT NULL,
    priority bigint DEFAULT 1 NOT NULL
);


--
-- Name: subject_room_preference_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.subject_room_preference_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: subject_room_preference_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.subject_room_preference_id_seq OWNED BY public.subject_room_preference.id;


--
-- Name: supported_currencies; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.supported_currencies (
    id bigint NOT NULL,
    currency_code text NOT NULL,
    symbol text NOT NULL,
    decimal_places integer NOT NULL,
    decimal_separator text NOT NULL,
    thousands_separator text NOT NULL,
    enabled boolean DEFAULT true NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT supported_currency_code_iso4217 CHECK ((currency_code ~ '^[A-Z]{3}$'::text)),
    CONSTRAINT supported_currency_decimal_places CHECK (((decimal_places >= 0) AND (decimal_places <= 3)))
);


--
-- Name: supported_currencies_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.supported_currencies_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: supported_currencies_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.supported_currencies_id_seq OWNED BY public.supported_currencies.id;


--
-- Name: tdf_release_lease; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tdf_release_lease (
    singleton boolean DEFAULT true NOT NULL,
    source_commit text NOT NULL,
    owner_token uuid NOT NULL,
    acquired_at timestamp with time zone NOT NULL,
    heartbeat_at timestamp with time zone NOT NULL,
    CONSTRAINT tdf_release_lease_singleton_check CHECK (singleton),
    CONSTRAINT tdf_release_lease_source_commit_check CHECK ((source_commit ~ '^[0-9a-f]{40}$'::text))
);


--
-- Name: tdf_schema_migration; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.tdf_schema_migration (
    migration_id text NOT NULL,
    checksum text NOT NULL,
    source_commit text NOT NULL,
    applied_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT tdf_schema_migration_checksum_check CHECK ((checksum ~ '^[0-9a-f]{64}$'::text)),
    CONSTRAINT tdf_schema_migration_source_commit_check CHECK ((source_commit ~ '^[0-9a-f]{40}$'::text))
);


--
-- Name: teacher_availability; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.teacher_availability (
    id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    subject_id bigint NOT NULL,
    room_id bigint NOT NULL,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone NOT NULL,
    notes character varying,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: teacher_availability_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.teacher_availability_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: teacher_availability_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.teacher_availability_id_seq OWNED BY public.teacher_availability.id;


--
-- Name: teacher_student; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.teacher_student (
    id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    student_id bigint NOT NULL,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: teacher_student_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.teacher_student_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: teacher_student_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.teacher_student_id_seq OWNED BY public.teacher_student.id;


--
-- Name: teacher_subject; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.teacher_subject (
    id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    subject_id bigint NOT NULL,
    level_min bigint,
    level_max bigint
);


--
-- Name: teacher_subject_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.teacher_subject_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: teacher_subject_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.teacher_subject_id_seq OWNED BY public.teacher_subject.id;


--
-- Name: ticket_qr_code; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ticket_qr_code (
    id bigint NOT NULL,
    ticket_id bigint NOT NULL,
    qr_data character varying NOT NULL,
    qr_image_url character varying,
    generated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ticket_qr_code_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ticket_qr_code_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ticket_qr_code_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ticket_qr_code_id_seq OWNED BY public.ticket_qr_code.id;


--
-- Name: ticket_refund_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ticket_refund_request (
    id bigint NOT NULL,
    order_id bigint NOT NULL,
    requested_by_party_id character varying,
    reason character varying,
    amount_cents bigint NOT NULL,
    status character varying DEFAULT 'pending'::character varying NOT NULL,
    approved_by_party_id character varying,
    approved_at timestamp with time zone,
    rejection_reason character varying,
    stripe_refund_id character varying,
    processed_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ticket_refund_request_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ticket_refund_request_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ticket_refund_request_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ticket_refund_request_id_seq OWNED BY public.ticket_refund_request.id;


--
-- Name: ticket_transfer; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.ticket_transfer (
    id bigint NOT NULL,
    ticket_id bigint NOT NULL,
    from_party_id character varying,
    to_party_id character varying,
    to_email character varying,
    to_name character varying,
    status character varying DEFAULT 'pending'::character varying NOT NULL,
    transfer_code character varying NOT NULL,
    message character varying,
    expires_at timestamp with time zone,
    accepted_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: ticket_transfer_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.ticket_transfer_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: ticket_transfer_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.ticket_transfer_id_seq OWNED BY public.ticket_transfer.id;


--
-- Name: trial_assignment; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.trial_assignment (
    id bigint NOT NULL,
    request_id bigint NOT NULL,
    teacher_id bigint NOT NULL,
    start_at timestamp with time zone NOT NULL,
    end_at timestamp with time zone NOT NULL,
    room_id bigint NOT NULL,
    booking_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: trial_assignment_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.trial_assignment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: trial_assignment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.trial_assignment_id_seq OWNED BY public.trial_assignment.id;


--
-- Name: trial_request; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.trial_request (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    subject_id bigint NOT NULL,
    pref1_start timestamp with time zone NOT NULL,
    pref1_end timestamp with time zone NOT NULL,
    pref2_start timestamp with time zone,
    pref2_end timestamp with time zone,
    pref3_start timestamp with time zone,
    pref3_end timestamp with time zone,
    notes character varying,
    status character varying NOT NULL,
    assigned_teacher_id bigint,
    assigned_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


--
-- Name: trial_request_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.trial_request_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: trial_request_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.trial_request_id_seq OWNED BY public.trial_request.id;


--
-- Name: trial_throttle; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.trial_throttle (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    day date NOT NULL,
    count bigint NOT NULL
);


--
-- Name: trial_throttle_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.trial_throttle_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: trial_throttle_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.trial_throttle_id_seq OWNED BY public.trial_throttle.id;


--
-- Name: user_credential; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_credential (
    id bigint NOT NULL,
    party_id bigint NOT NULL,
    username character varying NOT NULL,
    password_hash character varying NOT NULL,
    active boolean NOT NULL
);


--
-- Name: user_credential_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.user_credential_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_credential_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.user_credential_id_seq OWNED BY public.user_credential.id;


--
-- Name: user_locale_preferences; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.user_locale_preferences (
    id bigint NOT NULL,
    user_id bigint NOT NULL,
    locale text NOT NULL,
    currency text NOT NULL,
    timezone text NOT NULL,
    country_code text,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT user_locale_preferences_country_check CHECK (((country_code IS NULL) OR (country_code ~ '^[A-Z]{2}$'::text))),
    CONSTRAINT user_locale_preferences_locale_check CHECK ((locale ~ '^[a-z]{2}(-[A-Z]{2})?$'::text))
);


--
-- Name: user_locale_preferences_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.user_locale_preferences_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_locale_preferences_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.user_locale_preferences_id_seq OWNED BY public.user_locale_preferences.id;


--
-- Name: venue; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.venue (
    id bigint NOT NULL,
    name character varying NOT NULL,
    address character varying,
    city character varying,
    country character varying,
    latitude double precision,
    longitude double precision,
    capacity bigint,
    contact character varying,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    country_code text,
    timezone text
);


--
-- Name: venue_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.venue_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: venue_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.venue_id_seq OWNED BY public.venue.id;


--
-- Name: whats_app_consent; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.whats_app_consent (
    id bigint NOT NULL,
    phone_e164 character varying NOT NULL,
    display_name character varying,
    consent boolean DEFAULT false NOT NULL,
    source character varying,
    note character varying,
    consented_at timestamp with time zone,
    revoked_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


--
-- Name: whats_app_consent_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.whats_app_consent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: whats_app_consent_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.whats_app_consent_id_seq OWNED BY public.whats_app_consent.id;


--
-- Name: whats_app_message; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.whats_app_message (
    id bigint NOT NULL,
    external_id character varying NOT NULL,
    sender_id character varying NOT NULL,
    sender_name character varying,
    text character varying,
    direction character varying NOT NULL,
    ad_external_id character varying,
    ad_name character varying,
    campaign_external_id character varying,
    campaign_name character varying,
    metadata character varying,
    replied_at timestamp with time zone,
    reply_text character varying,
    reply_error character varying,
    created_at timestamp with time zone NOT NULL,
    reply_status character varying DEFAULT 'pending'::character varying NOT NULL,
    hold_reason character varying,
    hold_required_fields character varying,
    last_attempt_at timestamp with time zone,
    attempt_count bigint DEFAULT 0 NOT NULL,
    party_id bigint,
    actor_party_id bigint,
    phone_e164 character varying,
    contact_email character varying,
    delivery_status character varying DEFAULT 'pending'::character varying NOT NULL,
    delivery_updated_at timestamp with time zone,
    delivery_error character varying,
    transport_payload character varying,
    status_payload character varying,
    source character varying,
    resend_of_message_id bigint
);


--
-- Name: whats_app_message_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.whats_app_message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: whats_app_message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.whats_app_message_id_seq OWNED BY public.whats_app_message.id;


--
-- Name: ad_conversation_example id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_conversation_example ALTER COLUMN id SET DEFAULT nextval('public.ad_conversation_example_id_seq'::regclass);


--
-- Name: ad_creative id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_creative ALTER COLUMN id SET DEFAULT nextval('public.ad_creative_id_seq'::regclass);


--
-- Name: api_token id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.api_token ALTER COLUMN id SET DEFAULT nextval('public.api_token_id_seq'::regclass);


--
-- Name: artist_enrichment_run id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_run ALTER COLUMN id SET DEFAULT nextval('public.artist_enrichment_run_id_seq'::regclass);


--
-- Name: artist_enrichment_suggestion id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion ALTER COLUMN id SET DEFAULT nextval('public.artist_enrichment_suggestion_id_seq'::regclass);


--
-- Name: artist_field_change id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_field_change ALTER COLUMN id SET DEFAULT nextval('public.artist_field_change_id_seq'::regclass);


--
-- Name: artist_identity_candidate id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate ALTER COLUMN id SET DEFAULT nextval('public.artist_identity_candidate_id_seq'::regclass);


--
-- Name: artist_inventory_reference id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_inventory_reference ALTER COLUMN id SET DEFAULT nextval('public.artist_inventory_reference_id_seq'::regclass);


--
-- Name: artist_media_asset id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset ALTER COLUMN id SET DEFAULT nextval('public.artist_media_asset_id_seq'::regclass);


--
-- Name: artist_profile id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile ALTER COLUMN id SET DEFAULT nextval('public.artist_profile_id_seq'::regclass);


--
-- Name: artist_profile_enrichment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile_enrichment ALTER COLUMN id SET DEFAULT nextval('public.artist_profile_enrichment_id_seq'::regclass);


--
-- Name: artist_promo_slot id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_promo_slot ALTER COLUMN id SET DEFAULT nextval('public.artist_promo_slot_id_seq'::regclass);


--
-- Name: artist_release id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_release ALTER COLUMN id SET DEFAULT nextval('public.artist_release_id_seq'::regclass);


--
-- Name: artist_research_source id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_research_source ALTER COLUMN id SET DEFAULT nextval('public.artist_research_source_id_seq'::regclass);


--
-- Name: artist_tip id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_tip ALTER COLUMN id SET DEFAULT nextval('public.artist_tip_id_seq'::regclass);


--
-- Name: attendance id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance ALTER COLUMN id SET DEFAULT nextval('public.attendance_id_seq'::regclass);


--
-- Name: audit_log id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.audit_log ALTER COLUMN id SET DEFAULT nextval('public.audit_log_id_seq'::regclass);


--
-- Name: booking id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking ALTER COLUMN id SET DEFAULT nextval('public.booking_id_seq'::regclass);


--
-- Name: booking_resource id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking_resource ALTER COLUMN id SET DEFAULT nextval('public.booking_resource_id_seq'::regclass);


--
-- Name: campaign id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign ALTER COLUMN id SET DEFAULT nextval('public.campaign_id_seq'::regclass);


--
-- Name: campaign_automation id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation ALTER COLUMN id SET DEFAULT nextval('public.campaign_automation_id_seq'::regclass);


--
-- Name: campaign_automation_step id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation_step ALTER COLUMN id SET DEFAULT nextval('public.campaign_automation_step_id_seq'::regclass);


--
-- Name: campaign_delivery id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery ALTER COLUMN id SET DEFAULT nextval('public.campaign_delivery_id_seq'::regclass);


--
-- Name: campaign_enrollment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_enrollment ALTER COLUMN id SET DEFAULT nextval('public.campaign_enrollment_id_seq'::regclass);


--
-- Name: catalog_asset id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_asset ALTER COLUMN id SET DEFAULT nextval('public.catalog_asset_id_seq'::regclass);


--
-- Name: catalog_credit id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_credit ALTER COLUMN id SET DEFAULT nextval('public.catalog_credit_id_seq'::regclass);


--
-- Name: catalog_deal id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal ALTER COLUMN id SET DEFAULT nextval('public.catalog_deal_id_seq'::regclass);


--
-- Name: catalog_deal_territory id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal_territory ALTER COLUMN id SET DEFAULT nextval('public.catalog_deal_territory_id_seq'::regclass);


--
-- Name: catalog_identifier id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_identifier ALTER COLUMN id SET DEFAULT nextval('public.catalog_identifier_id_seq'::regclass);


--
-- Name: catalog_release id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release ALTER COLUMN id SET DEFAULT nextval('public.catalog_release_id_seq'::regclass);


--
-- Name: catalog_release_resource id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release_resource ALTER COLUMN id SET DEFAULT nextval('public.catalog_release_resource_id_seq'::regclass);


--
-- Name: catalog_resource id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_resource ALTER COLUMN id SET DEFAULT nextval('public.catalog_resource_id_seq'::regclass);


--
-- Name: catalog_source_link id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_source_link ALTER COLUMN id SET DEFAULT nextval('public.catalog_source_link_id_seq'::regclass);


--
-- Name: chat_message id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_message ALTER COLUMN id SET DEFAULT nextval('public.chat_message_id_seq'::regclass);


--
-- Name: chat_thread id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_thread ALTER COLUMN id SET DEFAULT nextval('public.chat_thread_id_seq'::regclass);


--
-- Name: class_package_purchase id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_package_purchase ALTER COLUMN id SET DEFAULT nextval('public.class_package_purchase_id_seq'::regclass);


--
-- Name: class_session id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_session ALTER COLUMN id SET DEFAULT nextval('public.class_session_id_seq'::regclass);


--
-- Name: cms_content id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cms_content ALTER COLUMN id SET DEFAULT nextval('public.cms_content_id_seq'::regclass);


--
-- Name: commission id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.commission ALTER COLUMN id SET DEFAULT nextval('public.commission_id_seq'::regclass);


--
-- Name: country id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.country ALTER COLUMN id SET DEFAULT nextval('public.country_id_seq'::regclass);


--
-- Name: course id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course ALTER COLUMN id SET DEFAULT nextval('public.course_id_seq'::regclass);


--
-- Name: course_email_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_email_event ALTER COLUMN id SET DEFAULT nextval('public.course_email_event_id_seq'::regclass);


--
-- Name: course_registration id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration ALTER COLUMN id SET DEFAULT nextval('public.course_registration_id_seq'::regclass);


--
-- Name: course_registration_follow_up id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up ALTER COLUMN id SET DEFAULT nextval('public.course_registration_follow_up_id_seq'::regclass);


--
-- Name: course_registration_receipt id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_receipt ALTER COLUMN id SET DEFAULT nextval('public.course_registration_receipt_id_seq'::regclass);


--
-- Name: course_session_model id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_session_model ALTER COLUMN id SET DEFAULT nextval('public.course_session_model_id_seq'::regclass);


--
-- Name: course_syllabus_item id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_syllabus_item ALTER COLUMN id SET DEFAULT nextval('public.course_syllabus_item_id_seq'::regclass);


--
-- Name: currency_conversion_audit id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.currency_conversion_audit ALTER COLUMN id SET DEFAULT nextval('public.currency_conversion_audit_id_seq'::regclass);


--
-- Name: ddex_document id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_document ALTER COLUMN id SET DEFAULT nextval('public.ddex_document_id_seq'::regclass);


--
-- Name: ddex_export id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_export ALTER COLUMN id SET DEFAULT nextval('public.ddex_export_id_seq'::regclass);


--
-- Name: ddex_import_change id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_change ALTER COLUMN id SET DEFAULT nextval('public.ddex_import_change_id_seq'::regclass);


--
-- Name: ddex_import_plan id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_plan ALTER COLUMN id SET DEFAULT nextval('public.ddex_import_plan_id_seq'::regclass);


--
-- Name: ddex_import_run id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_run ALTER COLUMN id SET DEFAULT nextval('public.ddex_import_run_id_seq'::regclass);


--
-- Name: ddex_job id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_job ALTER COLUMN id SET DEFAULT nextval('public.ddex_job_id_seq'::regclass);


--
-- Name: ddex_message_header id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_message_header ALTER COLUMN id SET DEFAULT nextval('public.ddex_message_header_id_seq'::regclass);


--
-- Name: ddex_partner id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_partner ALTER COLUMN id SET DEFAULT nextval('public.ddex_partner_id_seq'::regclass);


--
-- Name: ddex_validation_issue id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_issue ALTER COLUMN id SET DEFAULT nextval('public.ddex_validation_issue_id_seq'::regclass);


--
-- Name: ddex_validation_run id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_run ALTER COLUMN id SET DEFAULT nextval('public.ddex_validation_run_id_seq'::regclass);


--
-- Name: event_budget_line id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_budget_line ALTER COLUMN id SET DEFAULT nextval('public.event_budget_line_id_seq'::regclass);


--
-- Name: event_city id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city ALTER COLUMN id SET DEFAULT nextval('public.event_city_id_seq'::regclass);


--
-- Name: event_city_subscription id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city_subscription ALTER COLUMN id SET DEFAULT nextval('public.event_city_subscription_id_seq'::regclass);


--
-- Name: event_discovery_source id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_discovery_source ALTER COLUMN id SET DEFAULT nextval('public.event_discovery_source_id_seq'::regclass);


--
-- Name: event_finance_entry id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_finance_entry ALTER COLUMN id SET DEFAULT nextval('public.event_finance_entry_id_seq'::regclass);


--
-- Name: event_invitation id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_invitation ALTER COLUMN id SET DEFAULT nextval('public.event_invitation_id_seq'::regclass);


--
-- Name: event_logistics_activity id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_activity_id_seq'::regclass);


--
-- Name: event_logistics_alert_delivery id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_alert_delivery ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_alert_delivery_id_seq'::regclass);


--
-- Name: event_logistics_assignment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_assignment ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_assignment_id_seq'::regclass);


--
-- Name: event_logistics_dependency id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_dependency ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_dependency_id_seq'::regclass);


--
-- Name: event_logistics_member id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_member ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_member_id_seq'::regclass);


--
-- Name: event_logistics_place id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_place ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_place_id_seq'::regclass);


--
-- Name: event_logistics_plan id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_plan ALTER COLUMN id SET DEFAULT nextval('public.event_logistics_plan_id_seq'::regclass);


--
-- Name: event_moment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment ALTER COLUMN id SET DEFAULT nextval('public.event_moment_id_seq'::regclass);


--
-- Name: event_moment_comment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment_comment ALTER COLUMN id SET DEFAULT nextval('public.event_moment_comment_id_seq'::regclass);


--
-- Name: event_route_verification id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_route_verification ALTER COLUMN id SET DEFAULT nextval('public.event_route_verification_id_seq'::regclass);


--
-- Name: event_rsvp id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_rsvp ALTER COLUMN id SET DEFAULT nextval('public.event_rsvp_id_seq'::regclass);


--
-- Name: event_ticket id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket ALTER COLUMN id SET DEFAULT nextval('public.event_ticket_id_seq'::regclass);


--
-- Name: event_ticket_order id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order ALTER COLUMN id SET DEFAULT nextval('public.event_ticket_order_id_seq'::regclass);


--
-- Name: event_ticket_tier id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_tier ALTER COLUMN id SET DEFAULT nextval('public.event_ticket_tier_id_seq'::regclass);


--
-- Name: event_waitlist id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_waitlist ALTER COLUMN id SET DEFAULT nextval('public.event_waitlist_id_seq'::regclass);


--
-- Name: external_artist_ref id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_artist_ref ALTER COLUMN id SET DEFAULT nextval('public.external_artist_ref_id_seq'::regclass);


--
-- Name: external_calendar_mapping id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_calendar_mapping ALTER COLUMN id SET DEFAULT nextval('public.external_calendar_mapping_id_seq'::regclass);


--
-- Name: external_event_discovery_run id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_discovery_run ALTER COLUMN id SET DEFAULT nextval('public.external_event_discovery_run_id_seq'::regclass);


--
-- Name: external_event_ref id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_ref ALTER COLUMN id SET DEFAULT nextval('public.external_event_ref_id_seq'::regclass);


--
-- Name: external_venue_ref id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_venue_ref ALTER COLUMN id SET DEFAULT nextval('public.external_venue_ref_id_seq'::regclass);


--
-- Name: facebook_message id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.facebook_message ALTER COLUMN id SET DEFAULT nextval('public.facebook_message_id_seq'::regclass);


--
-- Name: fan_club id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club ALTER COLUMN id SET DEFAULT nextval('public.fan_club_id_seq'::regclass);


--
-- Name: fan_club_candidacy id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_candidacy ALTER COLUMN id SET DEFAULT nextval('public.fan_club_candidacy_id_seq'::regclass);


--
-- Name: fan_club_election id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_election ALTER COLUMN id SET DEFAULT nextval('public.fan_club_election_id_seq'::regclass);


--
-- Name: fan_club_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_event ALTER COLUMN id SET DEFAULT nextval('public.fan_club_event_id_seq'::regclass);


--
-- Name: fan_club_member_profile id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_member_profile ALTER COLUMN id SET DEFAULT nextval('public.fan_club_member_profile_id_seq'::regclass);


--
-- Name: fan_club_memory id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory ALTER COLUMN id SET DEFAULT nextval('public.fan_club_memory_id_seq'::regclass);


--
-- Name: fan_club_memory_report id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory_report ALTER COLUMN id SET DEFAULT nextval('public.fan_club_memory_report_id_seq'::regclass);


--
-- Name: fan_club_officer id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_officer ALTER COLUMN id SET DEFAULT nextval('public.fan_club_officer_id_seq'::regclass);


--
-- Name: fan_club_post id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_post ALTER COLUMN id SET DEFAULT nextval('public.fan_club_post_id_seq'::regclass);


--
-- Name: fan_club_vote id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote ALTER COLUMN id SET DEFAULT nextval('public.fan_club_vote_id_seq'::regclass);


--
-- Name: fan_follow id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_follow ALTER COLUMN id SET DEFAULT nextval('public.fan_follow_id_seq'::regclass);


--
-- Name: fan_profile id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_profile ALTER COLUMN id SET DEFAULT nextval('public.fan_profile_id_seq'::regclass);


--
-- Name: feature_access_request_history id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_request_history ALTER COLUMN id SET DEFAULT nextval('public.feature_access_request_history_id_seq'::regclass);


--
-- Name: feature_access_requests id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_requests ALTER COLUMN id SET DEFAULT nextval('public.feature_access_requests_id_seq'::regclass);


--
-- Name: feature_navigation_preferences id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_navigation_preferences ALTER COLUMN id SET DEFAULT nextval('public.feature_navigation_preferences_id_seq'::regclass);


--
-- Name: instagram_message id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.instagram_message ALTER COLUMN id SET DEFAULT nextval('public.instagram_message_id_seq'::regclass);


--
-- Name: invoice id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice ALTER COLUMN id SET DEFAULT nextval('public.invoice_id_seq'::regclass);


--
-- Name: invoice_line id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice_line ALTER COLUMN id SET DEFAULT nextval('public.invoice_line_id_seq'::regclass);


--
-- Name: lead_interest id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.lead_interest ALTER COLUMN id SET DEFAULT nextval('public.lead_interest_id_seq'::regclass);


--
-- Name: notification id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notification ALTER COLUMN id SET DEFAULT nextval('public.notification_id_seq'::regclass);


--
-- Name: operations_admin_audit id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit ALTER COLUMN id SET DEFAULT nextval('public.operations_admin_audit_id_seq'::regclass);


--
-- Name: operations_stream_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event ALTER COLUMN id SET DEFAULT nextval('public.operations_stream_event_id_seq'::regclass);


--
-- Name: operations_work_item_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event ALTER COLUMN id SET DEFAULT nextval('public.operations_work_item_event_id_seq'::regclass);


--
-- Name: package_catalog id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_catalog ALTER COLUMN id SET DEFAULT nextval('public.package_catalog_id_seq'::regclass);


--
-- Name: package_ledger id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_ledger ALTER COLUMN id SET DEFAULT nextval('public.package_ledger_id_seq'::regclass);


--
-- Name: package_product id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_product ALTER COLUMN id SET DEFAULT nextval('public.package_product_id_seq'::regclass);


--
-- Name: package_purchase id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_purchase ALTER COLUMN id SET DEFAULT nextval('public.package_purchase_id_seq'::regclass);


--
-- Name: party id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party ALTER COLUMN id SET DEFAULT nextval('public.party_id_seq'::regclass);


--
-- Name: party_follow id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_follow ALTER COLUMN id SET DEFAULT nextval('public.party_follow_id_seq'::regclass);


--
-- Name: party_radio_presence id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_radio_presence ALTER COLUMN id SET DEFAULT nextval('public.party_radio_presence_id_seq'::regclass);


--
-- Name: party_role id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_role ALTER COLUMN id SET DEFAULT nextval('public.party_role_id_seq'::regclass);


--
-- Name: payment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment ALTER COLUMN id SET DEFAULT nextval('public.payment_id_seq'::regclass);


--
-- Name: payment_split id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_split ALTER COLUMN id SET DEFAULT nextval('public.payment_split_id_seq'::regclass);


--
-- Name: promo_code id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code ALTER COLUMN id SET DEFAULT nextval('public.promo_code_id_seq'::regclass);


--
-- Name: promo_code_redemption id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code_redemption ALTER COLUMN id SET DEFAULT nextval('public.promo_code_redemption_id_seq'::regclass);


--
-- Name: radio_stream id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.radio_stream ALTER COLUMN id SET DEFAULT nextval('public.radio_stream_id_seq'::regclass);


--
-- Name: rag_chunk id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rag_chunk ALTER COLUMN id SET DEFAULT nextval('public.rag_chunk_id_seq'::regclass);


--
-- Name: receipt id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt ALTER COLUMN id SET DEFAULT nextval('public.receipt_id_seq'::regclass);


--
-- Name: receipt_line id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt_line ALTER COLUMN id SET DEFAULT nextval('public.receipt_line_id_seq'::regclass);


--
-- Name: resource id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.resource ALTER COLUMN id SET DEFAULT nextval('public.resource_id_seq'::regclass);


--
-- Name: service_ad id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad ALTER COLUMN id SET DEFAULT nextval('public.service_ad_id_seq'::regclass);


--
-- Name: service_ad_slot id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad_slot ALTER COLUMN id SET DEFAULT nextval('public.service_ad_slot_id_seq'::regclass);


--
-- Name: service_catalog id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_catalog ALTER COLUMN id SET DEFAULT nextval('public.service_catalog_id_seq'::regclass);


--
-- Name: service_escrow id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow ALTER COLUMN id SET DEFAULT nextval('public.service_escrow_id_seq'::regclass);


--
-- Name: service_order id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_order ALTER COLUMN id SET DEFAULT nextval('public.service_order_id_seq'::regclass);


--
-- Name: service_status_change id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_status_change ALTER COLUMN id SET DEFAULT nextval('public.service_status_change_id_seq'::regclass);


--
-- Name: session_invoice id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_invoice ALTER COLUMN id SET DEFAULT nextval('public.session_invoice_id_seq'::regclass);


--
-- Name: social_artist_profile id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_artist_profile ALTER COLUMN id SET DEFAULT nextval('public.social_artist_profile_id_seq'::regclass);


--
-- Name: social_discovery_review id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_discovery_review ALTER COLUMN id SET DEFAULT nextval('public.social_discovery_review_id_seq'::regclass);


--
-- Name: social_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_event ALTER COLUMN id SET DEFAULT nextval('public.social_event_id_seq'::regclass);


--
-- Name: social_sync_account id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_account ALTER COLUMN id SET DEFAULT nextval('public.social_sync_account_id_seq'::regclass);


--
-- Name: social_sync_post id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post ALTER COLUMN id SET DEFAULT nextval('public.social_sync_post_id_seq'::regclass);


--
-- Name: social_sync_run id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_run ALTER COLUMN id SET DEFAULT nextval('public.social_sync_run_id_seq'::regclass);


--
-- Name: stripe_payment_intent id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_payment_intent ALTER COLUMN id SET DEFAULT nextval('public.stripe_payment_intent_id_seq'::regclass);


--
-- Name: stripe_webhook_event id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_webhook_event ALTER COLUMN id SET DEFAULT nextval('public.stripe_webhook_event_id_seq'::regclass);


--
-- Name: studio_brain_entry id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.studio_brain_entry ALTER COLUMN id SET DEFAULT nextval('public.studio_brain_entry_id_seq'::regclass);


--
-- Name: subject id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject ALTER COLUMN id SET DEFAULT nextval('public.subject_id_seq'::regclass);


--
-- Name: subject_room_preference id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject_room_preference ALTER COLUMN id SET DEFAULT nextval('public.subject_room_preference_id_seq'::regclass);


--
-- Name: supported_currencies id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.supported_currencies ALTER COLUMN id SET DEFAULT nextval('public.supported_currencies_id_seq'::regclass);


--
-- Name: teacher_availability id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_availability ALTER COLUMN id SET DEFAULT nextval('public.teacher_availability_id_seq'::regclass);


--
-- Name: teacher_student id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_student ALTER COLUMN id SET DEFAULT nextval('public.teacher_student_id_seq'::regclass);


--
-- Name: teacher_subject id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_subject ALTER COLUMN id SET DEFAULT nextval('public.teacher_subject_id_seq'::regclass);


--
-- Name: ticket_qr_code id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_qr_code ALTER COLUMN id SET DEFAULT nextval('public.ticket_qr_code_id_seq'::regclass);


--
-- Name: ticket_refund_request id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_refund_request ALTER COLUMN id SET DEFAULT nextval('public.ticket_refund_request_id_seq'::regclass);


--
-- Name: ticket_transfer id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_transfer ALTER COLUMN id SET DEFAULT nextval('public.ticket_transfer_id_seq'::regclass);


--
-- Name: trial_assignment id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_assignment ALTER COLUMN id SET DEFAULT nextval('public.trial_assignment_id_seq'::regclass);


--
-- Name: trial_request id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_request ALTER COLUMN id SET DEFAULT nextval('public.trial_request_id_seq'::regclass);


--
-- Name: trial_throttle id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_throttle ALTER COLUMN id SET DEFAULT nextval('public.trial_throttle_id_seq'::regclass);


--
-- Name: user_credential id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credential ALTER COLUMN id SET DEFAULT nextval('public.user_credential_id_seq'::regclass);


--
-- Name: user_locale_preferences id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_locale_preferences ALTER COLUMN id SET DEFAULT nextval('public.user_locale_preferences_id_seq'::regclass);


--
-- Name: venue id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.venue ALTER COLUMN id SET DEFAULT nextval('public.venue_id_seq'::regclass);


--
-- Name: whats_app_consent id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_consent ALTER COLUMN id SET DEFAULT nextval('public.whats_app_consent_id_seq'::regclass);


--
-- Name: whats_app_message id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message ALTER COLUMN id SET DEFAULT nextval('public.whats_app_message_id_seq'::regclass);


--
-- Name: academy_lesson academy_lesson_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_lesson
    ADD CONSTRAINT academy_lesson_pkey PRIMARY KEY (id);


--
-- Name: academy_microcourse academy_microcourse_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_microcourse
    ADD CONSTRAINT academy_microcourse_pkey PRIMARY KEY (id);


--
-- Name: academy_progress academy_progress_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_progress
    ADD CONSTRAINT academy_progress_pkey PRIMARY KEY (user_id, lesson_id);


--
-- Name: academy_user academy_user_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_user
    ADD CONSTRAINT academy_user_pkey PRIMARY KEY (id);


--
-- Name: ad_conversation_example ad_conversation_example_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_conversation_example
    ADD CONSTRAINT ad_conversation_example_pkey PRIMARY KEY (id);


--
-- Name: ad_creative ad_creative_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_creative
    ADD CONSTRAINT ad_creative_pkey PRIMARY KEY (id);


--
-- Name: api_token api_token_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.api_token
    ADD CONSTRAINT api_token_pkey PRIMARY KEY (id);


--
-- Name: artist_enrichment_run artist_enrichment_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_run
    ADD CONSTRAINT artist_enrichment_run_pkey PRIMARY KEY (id);


--
-- Name: artist_enrichment_suggestion artist_enrichment_suggestion_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion
    ADD CONSTRAINT artist_enrichment_suggestion_pkey PRIMARY KEY (id);


--
-- Name: artist_field_change artist_field_change_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_field_change
    ADD CONSTRAINT artist_field_change_pkey PRIMARY KEY (id);


--
-- Name: artist_follow artist_follow_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_follow
    ADD CONSTRAINT artist_follow_pkey PRIMARY KEY (artist_id, follower_party_id);


--
-- Name: artist_genre artist_genre_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_genre
    ADD CONSTRAINT artist_genre_pkey PRIMARY KEY (artist_id, genre);


--
-- Name: artist_identity_candidate artist_identity_candidate_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate
    ADD CONSTRAINT artist_identity_candidate_pkey PRIMARY KEY (id);


--
-- Name: artist_inventory_reference artist_inventory_reference_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_inventory_reference
    ADD CONSTRAINT artist_inventory_reference_pkey PRIMARY KEY (id);


--
-- Name: artist_media_asset artist_media_asset_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset
    ADD CONSTRAINT artist_media_asset_pkey PRIMARY KEY (id);


--
-- Name: artist_profile_enrichment artist_profile_enrichment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile_enrichment
    ADD CONSTRAINT artist_profile_enrichment_pkey PRIMARY KEY (id);


--
-- Name: artist_profile artist_profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile
    ADD CONSTRAINT artist_profile_pkey PRIMARY KEY (id);


--
-- Name: artist_promo_slot artist_promo_slot_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_promo_slot
    ADD CONSTRAINT artist_promo_slot_pkey PRIMARY KEY (id);


--
-- Name: artist_release artist_release_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_release
    ADD CONSTRAINT artist_release_pkey PRIMARY KEY (id);


--
-- Name: artist_research_source artist_research_source_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_research_source
    ADD CONSTRAINT artist_research_source_pkey PRIMARY KEY (id);


--
-- Name: artist_tip artist_tip_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_tip
    ADD CONSTRAINT artist_tip_pkey PRIMARY KEY (id);


--
-- Name: asset_audit asset_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_audit
    ADD CONSTRAINT asset_audit_pkey PRIMARY KEY (id);


--
-- Name: asset_checkout asset_checkout_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_checkout
    ADD CONSTRAINT asset_checkout_pkey PRIMARY KEY (id);


--
-- Name: asset_kit_member asset_kit_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_kit_member
    ADD CONSTRAINT asset_kit_member_pkey PRIMARY KEY (id);


--
-- Name: asset asset_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset
    ADD CONSTRAINT asset_pkey PRIMARY KEY (id);


--
-- Name: attendance attendance_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_pkey PRIMARY KEY (id);


--
-- Name: audit_log audit_log_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.audit_log
    ADD CONSTRAINT audit_log_pkey PRIMARY KEY (id);


--
-- Name: band_member band_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band_member
    ADD CONSTRAINT band_member_pkey PRIMARY KEY (id);


--
-- Name: band band_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band
    ADD CONSTRAINT band_pkey PRIMARY KEY (id);


--
-- Name: booking booking_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking
    ADD CONSTRAINT booking_pkey PRIMARY KEY (id);


--
-- Name: booking_resource booking_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking_resource
    ADD CONSTRAINT booking_resource_pkey PRIMARY KEY (id);


--
-- Name: campaign_automation campaign_automation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation
    ADD CONSTRAINT campaign_automation_pkey PRIMARY KEY (id);


--
-- Name: campaign_automation_step campaign_automation_step_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation_step
    ADD CONSTRAINT campaign_automation_step_pkey PRIMARY KEY (id);


--
-- Name: campaign_delivery campaign_delivery_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT campaign_delivery_pkey PRIMARY KEY (id);


--
-- Name: campaign_enrollment campaign_enrollment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_enrollment
    ADD CONSTRAINT campaign_enrollment_pkey PRIMARY KEY (id);


--
-- Name: campaign campaign_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign
    ADD CONSTRAINT campaign_pkey PRIMARY KEY (id);


--
-- Name: catalog_asset catalog_asset_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_asset
    ADD CONSTRAINT catalog_asset_pkey PRIMARY KEY (id);


--
-- Name: catalog_credit catalog_credit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_credit
    ADD CONSTRAINT catalog_credit_pkey PRIMARY KEY (id);


--
-- Name: catalog_deal catalog_deal_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal
    ADD CONSTRAINT catalog_deal_pkey PRIMARY KEY (id);


--
-- Name: catalog_deal_territory catalog_deal_territory_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal_territory
    ADD CONSTRAINT catalog_deal_territory_pkey PRIMARY KEY (id);


--
-- Name: catalog_identifier catalog_identifier_entity_id_entity_type_scheme_value_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_identifier
    ADD CONSTRAINT catalog_identifier_entity_id_entity_type_scheme_value_key UNIQUE (entity_id, entity_type, scheme, value);


--
-- Name: catalog_identifier catalog_identifier_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_identifier
    ADD CONSTRAINT catalog_identifier_pkey PRIMARY KEY (id);


--
-- Name: catalog_release catalog_release_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release
    ADD CONSTRAINT catalog_release_pkey PRIMARY KEY (id);


--
-- Name: catalog_release_resource catalog_release_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release_resource
    ADD CONSTRAINT catalog_release_resource_pkey PRIMARY KEY (id);


--
-- Name: catalog_release_resource catalog_release_resource_release_id_disc_number_sequence_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release_resource
    ADD CONSTRAINT catalog_release_resource_release_id_disc_number_sequence_key UNIQUE (release_id, disc_number, sequence);


--
-- Name: catalog_resource catalog_resource_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_resource
    ADD CONSTRAINT catalog_resource_pkey PRIMARY KEY (id);


--
-- Name: catalog_source_link catalog_source_link_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_source_link
    ADD CONSTRAINT catalog_source_link_pkey PRIMARY KEY (id);


--
-- Name: chat_message chat_message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_message
    ADD CONSTRAINT chat_message_pkey PRIMARY KEY (id);


--
-- Name: chat_thread chat_thread_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_thread
    ADD CONSTRAINT chat_thread_pkey PRIMARY KEY (id);


--
-- Name: class_package_purchase class_package_purchase_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_package_purchase
    ADD CONSTRAINT class_package_purchase_pkey PRIMARY KEY (id);


--
-- Name: class_session class_session_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_session
    ADD CONSTRAINT class_session_pkey PRIMARY KEY (id);


--
-- Name: cms_content cms_content_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cms_content
    ADD CONSTRAINT cms_content_pkey PRIMARY KEY (id);


--
-- Name: cohort_enrollment cohort_enrollment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_enrollment
    ADD CONSTRAINT cohort_enrollment_pkey PRIMARY KEY (cohort_id, user_id);


--
-- Name: cohort cohort_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT cohort_pkey PRIMARY KEY (id);


--
-- Name: commission commission_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.commission
    ADD CONSTRAINT commission_pkey PRIMARY KEY (id);


--
-- Name: country country_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.country
    ADD CONSTRAINT country_pkey PRIMARY KEY (id);


--
-- Name: course_email_event course_email_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_email_event
    ADD CONSTRAINT course_email_event_pkey PRIMARY KEY (id);


--
-- Name: course course_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course
    ADD CONSTRAINT course_pkey PRIMARY KEY (id);


--
-- Name: course_registration_follow_up course_registration_follow_up_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up
    ADD CONSTRAINT course_registration_follow_up_pkey PRIMARY KEY (id);


--
-- Name: course_registration course_registration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration
    ADD CONSTRAINT course_registration_pkey PRIMARY KEY (id);


--
-- Name: course_registration_receipt course_registration_receipt_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_receipt
    ADD CONSTRAINT course_registration_receipt_pkey PRIMARY KEY (id);


--
-- Name: course_session_model course_session_model_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_session_model
    ADD CONSTRAINT course_session_model_pkey PRIMARY KEY (id);


--
-- Name: course_syllabus_item course_syllabus_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_syllabus_item
    ADD CONSTRAINT course_syllabus_item_pkey PRIMARY KEY (id);


--
-- Name: currency_conversion_audit currency_conversion_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.currency_conversion_audit
    ADD CONSTRAINT currency_conversion_audit_pkey PRIMARY KEY (id);


--
-- Name: ddex_document ddex_document_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_document
    ADD CONSTRAINT ddex_document_pkey PRIMARY KEY (id);


--
-- Name: ddex_document ddex_document_sha256_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_document
    ADD CONSTRAINT ddex_document_sha256_key UNIQUE (sha256);


--
-- Name: ddex_export ddex_export_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_export
    ADD CONSTRAINT ddex_export_pkey PRIMARY KEY (id);


--
-- Name: ddex_import_change ddex_import_change_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_change
    ADD CONSTRAINT ddex_import_change_pkey PRIMARY KEY (id);


--
-- Name: ddex_import_plan ddex_import_plan_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_plan
    ADD CONSTRAINT ddex_import_plan_pkey PRIMARY KEY (id);


--
-- Name: ddex_import_run ddex_import_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_run
    ADD CONSTRAINT ddex_import_run_pkey PRIMARY KEY (id);


--
-- Name: ddex_job ddex_job_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_job
    ADD CONSTRAINT ddex_job_pkey PRIMARY KEY (id);


--
-- Name: ddex_message_header ddex_message_header_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_message_header
    ADD CONSTRAINT ddex_message_header_pkey PRIMARY KEY (id);


--
-- Name: ddex_partner ddex_partner_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_partner
    ADD CONSTRAINT ddex_partner_name_key UNIQUE (name);


--
-- Name: ddex_partner ddex_partner_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_partner
    ADD CONSTRAINT ddex_partner_pkey PRIMARY KEY (id);


--
-- Name: ddex_validation_issue ddex_validation_issue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_issue
    ADD CONSTRAINT ddex_validation_issue_pkey PRIMARY KEY (id);


--
-- Name: ddex_validation_run ddex_validation_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_run
    ADD CONSTRAINT ddex_validation_run_pkey PRIMARY KEY (id);


--
-- Name: dropdown_option dropdown_option_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dropdown_option
    ADD CONSTRAINT dropdown_option_pkey PRIMARY KEY (id);


--
-- Name: event_artist event_artist_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_artist
    ADD CONSTRAINT event_artist_pkey PRIMARY KEY (event_id, artist_id);


--
-- Name: event_budget_line event_budget_line_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_budget_line
    ADD CONSTRAINT event_budget_line_pkey PRIMARY KEY (id);


--
-- Name: event_city event_city_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city
    ADD CONSTRAINT event_city_pkey PRIMARY KEY (id);


--
-- Name: event_city_subscription event_city_subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city_subscription
    ADD CONSTRAINT event_city_subscription_pkey PRIMARY KEY (id);


--
-- Name: event_discovery_source event_discovery_source_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_discovery_source
    ADD CONSTRAINT event_discovery_source_pkey PRIMARY KEY (id);


--
-- Name: event_finance_entry event_finance_entry_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_finance_entry
    ADD CONSTRAINT event_finance_entry_pkey PRIMARY KEY (id);


--
-- Name: event_invitation event_invitation_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_invitation
    ADD CONSTRAINT event_invitation_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_activity event_logistics_activity_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity
    ADD CONSTRAINT event_logistics_activity_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_alert_delivery event_logistics_alert_delivery_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_alert_delivery
    ADD CONSTRAINT event_logistics_alert_delivery_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_assignment event_logistics_assignment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_assignment
    ADD CONSTRAINT event_logistics_assignment_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_dependency event_logistics_dependency_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_dependency
    ADD CONSTRAINT event_logistics_dependency_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_member event_logistics_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_member
    ADD CONSTRAINT event_logistics_member_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_place event_logistics_place_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_place
    ADD CONSTRAINT event_logistics_place_pkey PRIMARY KEY (id);


--
-- Name: event_logistics_plan event_logistics_plan_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_plan
    ADD CONSTRAINT event_logistics_plan_pkey PRIMARY KEY (id);


--
-- Name: event_moment_comment event_moment_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment_comment
    ADD CONSTRAINT event_moment_comment_pkey PRIMARY KEY (id);


--
-- Name: event_moment event_moment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment
    ADD CONSTRAINT event_moment_pkey PRIMARY KEY (id);


--
-- Name: event_moment_reaction event_moment_reaction_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment_reaction
    ADD CONSTRAINT event_moment_reaction_pkey PRIMARY KEY (moment_id, reaction, reactor_party_id);


--
-- Name: event_route_verification event_route_verification_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_route_verification
    ADD CONSTRAINT event_route_verification_pkey PRIMARY KEY (id);


--
-- Name: event_rsvp event_rsvp_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_rsvp
    ADD CONSTRAINT event_rsvp_pkey PRIMARY KEY (id);


--
-- Name: event_ticket_order event_ticket_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order
    ADD CONSTRAINT event_ticket_order_pkey PRIMARY KEY (id);


--
-- Name: event_ticket event_ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket
    ADD CONSTRAINT event_ticket_pkey PRIMARY KEY (id);


--
-- Name: event_ticket_tier event_ticket_tier_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_tier
    ADD CONSTRAINT event_ticket_tier_pkey PRIMARY KEY (id);


--
-- Name: event_waitlist event_waitlist_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_waitlist
    ADD CONSTRAINT event_waitlist_pkey PRIMARY KEY (id);


--
-- Name: external_artist_ref external_artist_ref_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_artist_ref
    ADD CONSTRAINT external_artist_ref_pkey PRIMARY KEY (id);


--
-- Name: external_calendar_mapping external_calendar_mapping_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_calendar_mapping
    ADD CONSTRAINT external_calendar_mapping_pkey PRIMARY KEY (id);


--
-- Name: external_event_discovery_run external_event_discovery_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_discovery_run
    ADD CONSTRAINT external_event_discovery_run_pkey PRIMARY KEY (id);


--
-- Name: external_event_ref external_event_ref_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_ref
    ADD CONSTRAINT external_event_ref_pkey PRIMARY KEY (id);


--
-- Name: external_venue_ref external_venue_ref_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_venue_ref
    ADD CONSTRAINT external_venue_ref_pkey PRIMARY KEY (id);


--
-- Name: facebook_message facebook_message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.facebook_message
    ADD CONSTRAINT facebook_message_pkey PRIMARY KEY (id);


--
-- Name: fan_club_candidacy fan_club_candidacy_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_candidacy
    ADD CONSTRAINT fan_club_candidacy_pkey PRIMARY KEY (id);


--
-- Name: fan_club_election fan_club_election_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_election
    ADD CONSTRAINT fan_club_election_pkey PRIMARY KEY (id);


--
-- Name: fan_club_event fan_club_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_event
    ADD CONSTRAINT fan_club_event_pkey PRIMARY KEY (id);


--
-- Name: fan_club_member_profile fan_club_member_profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_member_profile
    ADD CONSTRAINT fan_club_member_profile_pkey PRIMARY KEY (id);


--
-- Name: fan_club_memory fan_club_memory_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory
    ADD CONSTRAINT fan_club_memory_pkey PRIMARY KEY (id);


--
-- Name: fan_club_memory_report fan_club_memory_report_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory_report
    ADD CONSTRAINT fan_club_memory_report_pkey PRIMARY KEY (id);


--
-- Name: fan_club_officer fan_club_officer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_officer
    ADD CONSTRAINT fan_club_officer_pkey PRIMARY KEY (id);


--
-- Name: fan_club fan_club_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club
    ADD CONSTRAINT fan_club_pkey PRIMARY KEY (id);


--
-- Name: fan_club_post fan_club_post_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_post
    ADD CONSTRAINT fan_club_post_pkey PRIMARY KEY (id);


--
-- Name: fan_club_vote fan_club_vote_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote
    ADD CONSTRAINT fan_club_vote_pkey PRIMARY KEY (id);


--
-- Name: fan_follow fan_follow_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_follow
    ADD CONSTRAINT fan_follow_pkey PRIMARY KEY (id);


--
-- Name: fan_profile fan_profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_profile
    ADD CONSTRAINT fan_profile_pkey PRIMARY KEY (id);


--
-- Name: feature_access_request_history feature_access_request_history_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_request_history
    ADD CONSTRAINT feature_access_request_history_pkey PRIMARY KEY (id);


--
-- Name: feature_access_requests feature_access_requests_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_requests
    ADD CONSTRAINT feature_access_requests_pkey PRIMARY KEY (id);


--
-- Name: feature_navigation_preferences feature_navigation_preferences_party_feature_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_navigation_preferences
    ADD CONSTRAINT feature_navigation_preferences_party_feature_unique UNIQUE (party_id, feature_id);


--
-- Name: feature_navigation_preferences feature_navigation_preferences_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_navigation_preferences
    ADD CONSTRAINT feature_navigation_preferences_pkey PRIMARY KEY (id);


--
-- Name: feedback feedback_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feedback
    ADD CONSTRAINT feedback_pkey PRIMARY KEY (id);


--
-- Name: chat_message index_chat_message_thread; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_message
    ADD CONSTRAINT index_chat_message_thread UNIQUE (thread_id);


--
-- Name: course_registration_follow_up index_course_registration_follow_up_next; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up
    ADD CONSTRAINT index_course_registration_follow_up_next UNIQUE (next_follow_up_at);


--
-- Name: course_registration_follow_up index_course_registration_follow_up_party; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up
    ADD CONSTRAINT index_course_registration_follow_up_party UNIQUE (party_id, created_at);


--
-- Name: course_registration_follow_up index_course_registration_follow_up_registration; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up
    ADD CONSTRAINT index_course_registration_follow_up_registration UNIQUE (registration_id, created_at);


--
-- Name: course_registration index_course_registration_party; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration
    ADD CONSTRAINT index_course_registration_party UNIQUE (party_id, created_at);


--
-- Name: course_registration_receipt index_course_registration_receipt_party; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_receipt
    ADD CONSTRAINT index_course_registration_receipt_party UNIQUE (party_id, created_at);


--
-- Name: course_registration_receipt index_course_registration_receipt_registration; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_receipt
    ADD CONSTRAINT index_course_registration_receipt_registration UNIQUE (registration_id, created_at);


--
-- Name: whats_app_message index_whats_app_message_party; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message
    ADD CONSTRAINT index_whats_app_message_party UNIQUE (party_id, created_at);


--
-- Name: whats_app_message index_whats_app_message_phone; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message
    ADD CONSTRAINT index_whats_app_message_phone UNIQUE (phone_e164, created_at);


--
-- Name: input_list input_list_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list
    ADD CONSTRAINT input_list_pkey PRIMARY KEY (id);


--
-- Name: input_list_template input_list_template_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template
    ADD CONSTRAINT input_list_template_pkey PRIMARY KEY (id);


--
-- Name: input_list_template_row input_list_template_row_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_pkey PRIMARY KEY (id);


--
-- Name: input_list_version input_list_version_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_version
    ADD CONSTRAINT input_list_version_pkey PRIMARY KEY (id);


--
-- Name: input_row input_row_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_pkey PRIMARY KEY (id);


--
-- Name: instagram_message instagram_message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.instagram_message
    ADD CONSTRAINT instagram_message_pkey PRIMARY KEY (id);


--
-- Name: intern_permission_request intern_permission_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_permission_request
    ADD CONSTRAINT intern_permission_request_pkey PRIMARY KEY (id);


--
-- Name: intern_profile intern_profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_profile
    ADD CONSTRAINT intern_profile_pkey PRIMARY KEY (id);


--
-- Name: intern_project intern_project_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_project
    ADD CONSTRAINT intern_project_pkey PRIMARY KEY (id);


--
-- Name: intern_task intern_task_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_task
    ADD CONSTRAINT intern_task_pkey PRIMARY KEY (id);


--
-- Name: intern_time_entry intern_time_entry_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_time_entry
    ADD CONSTRAINT intern_time_entry_pkey PRIMARY KEY (id);


--
-- Name: intern_todo intern_todo_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_todo
    ADD CONSTRAINT intern_todo_pkey PRIMARY KEY (id);


--
-- Name: invoice_line invoice_line_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice_line
    ADD CONSTRAINT invoice_line_pkey PRIMARY KEY (id);


--
-- Name: invoice invoice_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice
    ADD CONSTRAINT invoice_pkey PRIMARY KEY (id);


--
-- Name: label_track label_track_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.label_track
    ADD CONSTRAINT label_track_pkey PRIMARY KEY (id);


--
-- Name: lead_interest lead_interest_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.lead_interest
    ADD CONSTRAINT lead_interest_pkey PRIMARY KEY (id);


--
-- Name: live_session_intake live_session_intake_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_session_intake
    ADD CONSTRAINT live_session_intake_pkey PRIMARY KEY (id);


--
-- Name: live_session_musician live_session_musician_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_session_musician
    ADD CONSTRAINT live_session_musician_pkey PRIMARY KEY (id);


--
-- Name: live_session_song live_session_song_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_session_song
    ADD CONSTRAINT live_session_song_pkey PRIMARY KEY (id);


--
-- Name: maintenance_attachment maintenance_attachment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.maintenance_attachment
    ADD CONSTRAINT maintenance_attachment_pkey PRIMARY KEY (id);


--
-- Name: maintenance_ticket maintenance_ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.maintenance_ticket
    ADD CONSTRAINT maintenance_ticket_pkey PRIMARY KEY (id);


--
-- Name: marketplace_cart_item marketplace_cart_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_cart_item
    ADD CONSTRAINT marketplace_cart_item_pkey PRIMARY KEY (id);


--
-- Name: marketplace_cart marketplace_cart_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_cart
    ADD CONSTRAINT marketplace_cart_pkey PRIMARY KEY (id);


--
-- Name: marketplace_listing marketplace_listing_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_listing
    ADD CONSTRAINT marketplace_listing_pkey PRIMARY KEY (id);


--
-- Name: marketplace_order_item marketplace_order_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_order_item
    ADD CONSTRAINT marketplace_order_item_pkey PRIMARY KEY (id);


--
-- Name: marketplace_order marketplace_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_order
    ADD CONSTRAINT marketplace_order_pkey PRIMARY KEY (id);


--
-- Name: notification notification_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notification
    ADD CONSTRAINT notification_pkey PRIMARY KEY (id);


--
-- Name: operations_admin_audit operations_admin_audit_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit
    ADD CONSTRAINT operations_admin_audit_pkey PRIMARY KEY (id);


--
-- Name: operations_aggregate_sequence operations_aggregate_sequence_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_aggregate_sequence
    ADD CONSTRAINT operations_aggregate_sequence_pkey PRIMARY KEY (organization_id, aggregate_type, aggregate_id);


--
-- Name: operations_approval_request operations_approval_request_organization_id_idempotency_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_organization_id_idempotency_key_key UNIQUE (organization_id, idempotency_key);


--
-- Name: operations_approval_request operations_approval_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_pkey PRIMARY KEY (id);


--
-- Name: operations_backfill_run operations_backfill_run_organization_id_source_name_dry_run_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_backfill_run
    ADD CONSTRAINT operations_backfill_run_organization_id_source_name_dry_run_key UNIQUE (organization_id, source_name, dry_run, started_at);


--
-- Name: operations_backfill_run operations_backfill_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_backfill_run
    ADD CONSTRAINT operations_backfill_run_pkey PRIMARY KEY (id);


--
-- Name: operations_branch operations_branch_id_organization_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_branch
    ADD CONSTRAINT operations_branch_id_organization_id_key UNIQUE (id, organization_id);


--
-- Name: operations_branch operations_branch_organization_id_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_branch
    ADD CONSTRAINT operations_branch_organization_id_slug_key UNIQUE (organization_id, slug);


--
-- Name: operations_branch operations_branch_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_branch
    ADD CONSTRAINT operations_branch_pkey PRIMARY KEY (id);


--
-- Name: operations_business_hours operations_business_hours_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_business_hours
    ADD CONSTRAINT operations_business_hours_pkey PRIMARY KEY (organization_id, branch_id, iso_weekday);


--
-- Name: operations_domain_event operations_domain_event_organization_id_deduplication_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_domain_event
    ADD CONSTRAINT operations_domain_event_organization_id_deduplication_key_key UNIQUE (organization_id, deduplication_key);


--
-- Name: operations_domain_event operations_domain_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_domain_event
    ADD CONSTRAINT operations_domain_event_pkey PRIMARY KEY (id);


--
-- Name: operations_holiday operations_holiday_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_holiday
    ADD CONSTRAINT operations_holiday_pkey PRIMARY KEY (organization_id, branch_id, holiday_date);


--
-- Name: operations_inbound_receipt operations_inbound_receipt_organization_id_provider_provide_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_inbound_receipt
    ADD CONSTRAINT operations_inbound_receipt_organization_id_provider_provide_key UNIQUE (organization_id, provider, provider_event_id);


--
-- Name: operations_inbound_receipt operations_inbound_receipt_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_inbound_receipt
    ADD CONSTRAINT operations_inbound_receipt_pkey PRIMARY KEY (id);


--
-- Name: operations_integration_failure operations_integration_failure_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_integration_failure
    ADD CONSTRAINT operations_integration_failure_pkey PRIMARY KEY (id);


--
-- Name: operations_mention operations_mention_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_mention
    ADD CONSTRAINT operations_mention_pkey PRIMARY KEY (note_id, mentioned_party_id);


--
-- Name: operations_note operations_note_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_note
    ADD CONSTRAINT operations_note_pkey PRIMARY KEY (id);


--
-- Name: operations_organization operations_organization_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_organization
    ADD CONSTRAINT operations_organization_pkey PRIMARY KEY (id);


--
-- Name: operations_organization operations_organization_slug_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_organization
    ADD CONSTRAINT operations_organization_slug_key UNIQUE (slug);


--
-- Name: operations_outbound_delivery operations_outbound_delivery_organization_id_idempotency_ke_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbound_delivery
    ADD CONSTRAINT operations_outbound_delivery_organization_id_idempotency_ke_key UNIQUE (organization_id, idempotency_key);


--
-- Name: operations_outbound_delivery operations_outbound_delivery_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbound_delivery
    ADD CONSTRAINT operations_outbound_delivery_pkey PRIMARY KEY (id);


--
-- Name: operations_outbox operations_outbox_event_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbox
    ADD CONSTRAINT operations_outbox_event_id_key UNIQUE (event_id);


--
-- Name: operations_outbox operations_outbox_organization_id_aggregate_type_aggregate__key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbox
    ADD CONSTRAINT operations_outbox_organization_id_aggregate_type_aggregate__key UNIQUE (organization_id, aggregate_type, aggregate_id, aggregate_sequence);


--
-- Name: operations_outbox operations_outbox_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbox
    ADD CONSTRAINT operations_outbox_pkey PRIMARY KEY (id);


--
-- Name: operations_provider_config operations_provider_config_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_provider_config
    ADD CONSTRAINT operations_provider_config_pkey PRIMARY KEY (organization_id, provider, country_code, currency);


--
-- Name: operations_push_subscription operations_push_subscription_organization_id_party_id_devic_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_push_subscription
    ADD CONSTRAINT operations_push_subscription_organization_id_party_id_devic_key UNIQUE (organization_id, party_id, device_token_digest);


--
-- Name: operations_push_subscription operations_push_subscription_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_push_subscription
    ADD CONSTRAINT operations_push_subscription_pkey PRIMARY KEY (id);


--
-- Name: operations_saved_view operations_saved_view_organization_id_owner_party_id_name_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_saved_view
    ADD CONSTRAINT operations_saved_view_organization_id_owner_party_id_name_key UNIQUE (organization_id, owner_party_id, name);


--
-- Name: operations_saved_view operations_saved_view_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_saved_view
    ADD CONSTRAINT operations_saved_view_pkey PRIMARY KEY (id);


--
-- Name: operations_scope_member operations_scope_member_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_scope_member
    ADD CONSTRAINT operations_scope_member_pkey PRIMARY KEY (organization_id, branch_id, party_id);


--
-- Name: operations_sla_reminder operations_sla_reminder_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_reminder
    ADD CONSTRAINT operations_sla_reminder_pkey PRIMARY KEY (id);


--
-- Name: operations_sla_reminder operations_sla_reminder_timer_id_threshold_percent_target_r_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_reminder
    ADD CONSTRAINT operations_sla_reminder_timer_id_threshold_percent_target_r_key UNIQUE (timer_id, threshold_percent, target_role);


--
-- Name: operations_sla_timer operations_sla_timer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_timer
    ADD CONSTRAINT operations_sla_timer_pkey PRIMARY KEY (id);


--
-- Name: operations_sla_timer operations_sla_timer_work_item_id_phase_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_timer
    ADD CONSTRAINT operations_sla_timer_work_item_id_phase_key UNIQUE (work_item_id, phase);


--
-- Name: operations_stream_event operations_stream_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event
    ADD CONSTRAINT operations_stream_event_pkey PRIMARY KEY (id);


--
-- Name: operations_work_item_event operations_work_item_event_domain_event_id_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_domain_event_id_key UNIQUE (domain_event_id);


--
-- Name: operations_work_item_event operations_work_item_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_pkey PRIMARY KEY (id);


--
-- Name: operations_work_item operations_work_item_organization_id_correlation_key_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_organization_id_correlation_key_key UNIQUE (organization_id, correlation_key);


--
-- Name: operations_work_item operations_work_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_pkey PRIMARY KEY (id);


--
-- Name: package_catalog package_catalog_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_catalog
    ADD CONSTRAINT package_catalog_pkey PRIMARY KEY (id);


--
-- Name: package_ledger package_ledger_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_ledger
    ADD CONSTRAINT package_ledger_pkey PRIMARY KEY (id);


--
-- Name: package_product package_product_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_product
    ADD CONSTRAINT package_product_pkey PRIMARY KEY (id);


--
-- Name: package_purchase package_purchase_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_purchase
    ADD CONSTRAINT package_purchase_pkey PRIMARY KEY (id);


--
-- Name: party_follow party_follow_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_follow
    ADD CONSTRAINT party_follow_pkey PRIMARY KEY (id);


--
-- Name: party party_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party
    ADD CONSTRAINT party_pkey PRIMARY KEY (id);


--
-- Name: party_radio_presence party_radio_presence_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_radio_presence
    ADD CONSTRAINT party_radio_presence_pkey PRIMARY KEY (id);


--
-- Name: party_role party_role_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_role
    ADD CONSTRAINT party_role_pkey PRIMARY KEY (id);


--
-- Name: payment payment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_pkey PRIMARY KEY (id);


--
-- Name: payment_split payment_split_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_split
    ADD CONSTRAINT payment_split_pkey PRIMARY KEY (id);


--
-- Name: pipeline_card pipeline_card_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.pipeline_card
    ADD CONSTRAINT pipeline_card_pkey PRIMARY KEY (id);


--
-- Name: promo_code promo_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code
    ADD CONSTRAINT promo_code_pkey PRIMARY KEY (id);


--
-- Name: promo_code_redemption promo_code_redemption_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code_redemption
    ADD CONSTRAINT promo_code_redemption_pkey PRIMARY KEY (id);


--
-- Name: proposal proposal_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.proposal
    ADD CONSTRAINT proposal_pkey PRIMARY KEY (id);


--
-- Name: proposal_version proposal_version_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.proposal_version
    ADD CONSTRAINT proposal_version_pkey PRIMARY KEY (id);


--
-- Name: radio_stream radio_stream_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.radio_stream
    ADD CONSTRAINT radio_stream_pkey PRIMARY KEY (id);


--
-- Name: rag_chunk rag_chunk_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.rag_chunk
    ADD CONSTRAINT rag_chunk_pkey PRIMARY KEY (id);


--
-- Name: receipt_line receipt_line_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt_line
    ADD CONSTRAINT receipt_line_pkey PRIMARY KEY (id);


--
-- Name: receipt receipt_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt
    ADD CONSTRAINT receipt_pkey PRIMARY KEY (id);


--
-- Name: referral_claim referral_claim_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_claim
    ADD CONSTRAINT referral_claim_pkey PRIMARY KEY (id);


--
-- Name: referral_code referral_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_code
    ADD CONSTRAINT referral_code_pkey PRIMARY KEY (id);


--
-- Name: resource resource_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.resource
    ADD CONSTRAINT resource_pkey PRIMARY KEY (id);


--
-- Name: room_default_gear room_default_gear_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_default_gear
    ADD CONSTRAINT room_default_gear_pkey PRIMARY KEY (id);


--
-- Name: room_feature room_feature_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_feature
    ADD CONSTRAINT room_feature_pkey PRIMARY KEY (id);


--
-- Name: room room_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room
    ADD CONSTRAINT room_pkey PRIMARY KEY (id);


--
-- Name: service_ad service_ad_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad
    ADD CONSTRAINT service_ad_pkey PRIMARY KEY (id);


--
-- Name: service_ad_slot service_ad_slot_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad_slot
    ADD CONSTRAINT service_ad_slot_pkey PRIMARY KEY (id);


--
-- Name: service_catalog service_catalog_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_catalog
    ADD CONSTRAINT service_catalog_pkey PRIMARY KEY (id);


--
-- Name: service_escrow service_escrow_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_pkey PRIMARY KEY (id);


--
-- Name: service_order service_order_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_order
    ADD CONSTRAINT service_order_pkey PRIMARY KEY (id);


--
-- Name: service_status_change service_status_change_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_status_change
    ADD CONSTRAINT service_status_change_pkey PRIMARY KEY (id);


--
-- Name: session_deliverable session_deliverable_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_deliverable
    ADD CONSTRAINT session_deliverable_pkey PRIMARY KEY (id);


--
-- Name: session_invoice session_invoice_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_invoice
    ADD CONSTRAINT session_invoice_pkey PRIMARY KEY (id);


--
-- Name: session session_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_pkey PRIMARY KEY (id);


--
-- Name: session_room session_room_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_room
    ADD CONSTRAINT session_room_pkey PRIMARY KEY (id);


--
-- Name: social_artist_profile social_artist_profile_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_artist_profile
    ADD CONSTRAINT social_artist_profile_pkey PRIMARY KEY (id);


--
-- Name: social_discovery_review social_discovery_review_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_discovery_review
    ADD CONSTRAINT social_discovery_review_pkey PRIMARY KEY (id);


--
-- Name: social_event social_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_event
    ADD CONSTRAINT social_event_pkey PRIMARY KEY (id);


--
-- Name: social_sync_account social_sync_account_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_account
    ADD CONSTRAINT social_sync_account_pkey PRIMARY KEY (id);


--
-- Name: social_sync_post social_sync_post_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post
    ADD CONSTRAINT social_sync_post_pkey PRIMARY KEY (id);


--
-- Name: social_sync_run social_sync_run_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_run
    ADD CONSTRAINT social_sync_run_pkey PRIMARY KEY (id);


--
-- Name: stock_item stock_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_item
    ADD CONSTRAINT stock_item_pkey PRIMARY KEY (id);


--
-- Name: stock_movement stock_movement_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_movement
    ADD CONSTRAINT stock_movement_pkey PRIMARY KEY (id);


--
-- Name: stripe_payment_intent stripe_payment_intent_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_payment_intent
    ADD CONSTRAINT stripe_payment_intent_pkey PRIMARY KEY (id);


--
-- Name: stripe_webhook_event stripe_webhook_event_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_webhook_event
    ADD CONSTRAINT stripe_webhook_event_pkey PRIMARY KEY (id);


--
-- Name: studio_brain_entry studio_brain_entry_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.studio_brain_entry
    ADD CONSTRAINT studio_brain_entry_pkey PRIMARY KEY (id);


--
-- Name: subject subject_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject
    ADD CONSTRAINT subject_pkey PRIMARY KEY (id);


--
-- Name: subject_room_preference subject_room_preference_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject_room_preference
    ADD CONSTRAINT subject_room_preference_pkey PRIMARY KEY (id);


--
-- Name: supported_currencies supported_currencies_currency_code_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.supported_currencies
    ADD CONSTRAINT supported_currencies_currency_code_key UNIQUE (currency_code);


--
-- Name: supported_currencies supported_currencies_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.supported_currencies
    ADD CONSTRAINT supported_currencies_pkey PRIMARY KEY (id);


--
-- Name: tdf_release_lease tdf_release_lease_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tdf_release_lease
    ADD CONSTRAINT tdf_release_lease_pkey PRIMARY KEY (singleton);


--
-- Name: tdf_schema_migration tdf_schema_migration_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.tdf_schema_migration
    ADD CONSTRAINT tdf_schema_migration_pkey PRIMARY KEY (migration_id);


--
-- Name: teacher_availability teacher_availability_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_availability
    ADD CONSTRAINT teacher_availability_pkey PRIMARY KEY (id);


--
-- Name: teacher_student teacher_student_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_student
    ADD CONSTRAINT teacher_student_pkey PRIMARY KEY (id);


--
-- Name: teacher_subject teacher_subject_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_subject
    ADD CONSTRAINT teacher_subject_pkey PRIMARY KEY (id);


--
-- Name: ticket_qr_code ticket_qr_code_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_qr_code
    ADD CONSTRAINT ticket_qr_code_pkey PRIMARY KEY (id);


--
-- Name: ticket_refund_request ticket_refund_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_refund_request
    ADD CONSTRAINT ticket_refund_request_pkey PRIMARY KEY (id);


--
-- Name: ticket_transfer ticket_transfer_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_transfer
    ADD CONSTRAINT ticket_transfer_pkey PRIMARY KEY (id);


--
-- Name: trial_assignment trial_assignment_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_assignment
    ADD CONSTRAINT trial_assignment_pkey PRIMARY KEY (id);


--
-- Name: trial_request trial_request_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_request
    ADD CONSTRAINT trial_request_pkey PRIMARY KEY (id);


--
-- Name: trial_throttle trial_throttle_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_throttle
    ADD CONSTRAINT trial_throttle_pkey PRIMARY KEY (id);


--
-- Name: academy_lesson unique_academy_lesson; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_lesson
    ADD CONSTRAINT unique_academy_lesson UNIQUE (microcourse_id, day);


--
-- Name: academy_microcourse unique_academy_microcourse_slug; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_microcourse
    ADD CONSTRAINT unique_academy_microcourse_slug UNIQUE (slug);


--
-- Name: academy_user unique_academy_user_email; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_user
    ADD CONSTRAINT unique_academy_user_email UNIQUE (email);


--
-- Name: api_token unique_api_token; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.api_token
    ADD CONSTRAINT unique_api_token UNIQUE (token);


--
-- Name: artist_enrichment_run unique_artist_enrichment_run; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_run
    ADD CONSTRAINT unique_artist_enrichment_run UNIQUE (run_key);


--
-- Name: artist_enrichment_suggestion unique_artist_enrichment_suggestion; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion
    ADD CONSTRAINT unique_artist_enrichment_suggestion UNIQUE (idempotency_key);


--
-- Name: artist_field_change unique_artist_field_change; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_field_change
    ADD CONSTRAINT unique_artist_field_change UNIQUE (idempotency_key);


--
-- Name: artist_identity_candidate unique_artist_identity_candidate; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate
    ADD CONSTRAINT unique_artist_identity_candidate UNIQUE (idempotency_key);


--
-- Name: artist_inventory_reference unique_artist_inventory_reference; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_inventory_reference
    ADD CONSTRAINT unique_artist_inventory_reference UNIQUE (idempotency_key);


--
-- Name: artist_media_asset unique_artist_media_asset; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset
    ADD CONSTRAINT unique_artist_media_asset UNIQUE (idempotency_key);


--
-- Name: artist_media_asset unique_artist_media_drive_file; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset
    ADD CONSTRAINT unique_artist_media_drive_file UNIQUE (drive_file_id);


--
-- Name: artist_profile unique_artist_profile; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile
    ADD CONSTRAINT unique_artist_profile UNIQUE (artist_party_id);


--
-- Name: artist_profile_enrichment unique_artist_profile_enrichment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile_enrichment
    ADD CONSTRAINT unique_artist_profile_enrichment UNIQUE (artist_party_id);


--
-- Name: artist_research_source unique_artist_research_source; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_research_source
    ADD CONSTRAINT unique_artist_research_source UNIQUE (idempotency_key);


--
-- Name: asset unique_asset_qr; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset
    ADD CONSTRAINT unique_asset_qr UNIQUE (qr_code);


--
-- Name: asset unique_asset_serial; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset
    ADD CONSTRAINT unique_asset_serial UNIQUE (serial_number);


--
-- Name: attendance unique_attendance; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT unique_attendance UNIQUE (booking_id, party_id);


--
-- Name: band_member unique_band_member; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band_member
    ADD CONSTRAINT unique_band_member UNIQUE (band_id, party_id);


--
-- Name: band unique_band_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band
    ADD CONSTRAINT unique_band_name UNIQUE (name);


--
-- Name: band unique_band_party; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band
    ADD CONSTRAINT unique_band_party UNIQUE (party_id);


--
-- Name: booking_resource unique_booking_res; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking_resource
    ADD CONSTRAINT unique_booking_res UNIQUE (booking_id, resource_id, role);


--
-- Name: external_calendar_mapping unique_cal_map; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_calendar_mapping
    ADD CONSTRAINT unique_cal_map UNIQUE (resource_id);


--
-- Name: campaign_automation unique_campaign_automation_campaign; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation
    ADD CONSTRAINT unique_campaign_automation_campaign UNIQUE (campaign_id);


--
-- Name: campaign_automation_step unique_campaign_automation_step; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation_step
    ADD CONSTRAINT unique_campaign_automation_step UNIQUE (automation_id, "position");


--
-- Name: campaign_automation unique_campaign_automation_template; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation
    ADD CONSTRAINT unique_campaign_automation_template UNIQUE (template_key);


--
-- Name: campaign_delivery unique_campaign_delivery; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT unique_campaign_delivery UNIQUE (enrollment_id, step_id);


--
-- Name: campaign_enrollment unique_campaign_enrollment; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_enrollment
    ADD CONSTRAINT unique_campaign_enrollment UNIQUE (automation_id, party_id);


--
-- Name: chat_thread unique_chat_thread; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_thread
    ADD CONSTRAINT unique_chat_thread UNIQUE (dm_party_a, dm_party_b);


--
-- Name: cms_content unique_cms_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cms_content
    ADD CONSTRAINT unique_cms_version UNIQUE (slug, locale, version);


--
-- Name: cohort unique_cohort_slug; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort
    ADD CONSTRAINT unique_cohort_slug UNIQUE (slug);


--
-- Name: country unique_country_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.country
    ADD CONSTRAINT unique_country_code UNIQUE (code);


--
-- Name: course unique_course_slug; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course
    ADD CONSTRAINT unique_course_slug UNIQUE (slug);


--
-- Name: user_credential unique_credential_username; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credential
    ADD CONSTRAINT unique_credential_username UNIQUE (username);


--
-- Name: dropdown_option unique_dropdown_option; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.dropdown_option
    ADD CONSTRAINT unique_dropdown_option UNIQUE (category, value);


--
-- Name: event_budget_line unique_event_budget_line_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_budget_line
    ADD CONSTRAINT unique_event_budget_line_code UNIQUE (event_id, code);


--
-- Name: event_city unique_event_city; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city
    ADD CONSTRAINT unique_event_city UNIQUE (normalized_name, country_code);


--
-- Name: event_city_subscription unique_event_city_subscription; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city_subscription
    ADD CONSTRAINT unique_event_city_subscription UNIQUE (party_id, city_id);


--
-- Name: event_discovery_source unique_event_discovery_source; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_discovery_source
    ADD CONSTRAINT unique_event_discovery_source UNIQUE (source_key);


--
-- Name: event_logistics_alert_delivery unique_event_logistics_alert; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_alert_delivery
    ADD CONSTRAINT unique_event_logistics_alert UNIQUE (activity_id, activity_version, checkpoint, recipient_party_id, channel);


--
-- Name: event_logistics_dependency unique_event_logistics_dependency; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_dependency
    ADD CONSTRAINT unique_event_logistics_dependency UNIQUE (activity_id, depends_on_activity_id);


--
-- Name: event_logistics_member unique_event_logistics_member; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_member
    ADD CONSTRAINT unique_event_logistics_member UNIQUE (event_id, party_id);


--
-- Name: event_logistics_plan unique_event_logistics_plan; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_plan
    ADD CONSTRAINT unique_event_logistics_plan UNIQUE (event_id);


--
-- Name: event_ticket_order unique_event_ticket_checkout; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order
    ADD CONSTRAINT unique_event_ticket_checkout UNIQUE (buyer_party_id, checkout_idempotency_key);


--
-- Name: event_ticket unique_event_ticket_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket
    ADD CONSTRAINT unique_event_ticket_code UNIQUE (code);


--
-- Name: event_ticket_tier unique_event_ticket_tier_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_tier
    ADD CONSTRAINT unique_event_ticket_tier_code UNIQUE (event_id, code);


--
-- Name: external_artist_ref unique_external_artist_ref; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_artist_ref
    ADD CONSTRAINT unique_external_artist_ref UNIQUE (provider, external_id);


--
-- Name: external_event_ref unique_external_event_ref; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_ref
    ADD CONSTRAINT unique_external_event_ref UNIQUE (provider, external_id);


--
-- Name: external_venue_ref unique_external_venue_ref; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_venue_ref
    ADD CONSTRAINT unique_external_venue_ref UNIQUE (provider, external_id);


--
-- Name: facebook_message unique_facebook_message; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.facebook_message
    ADD CONSTRAINT unique_facebook_message UNIQUE (external_id);


--
-- Name: fan_club unique_fan_club_artist; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club
    ADD CONSTRAINT unique_fan_club_artist UNIQUE (artist_party_id);


--
-- Name: fan_club_candidacy unique_fan_club_candidacy; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_candidacy
    ADD CONSTRAINT unique_fan_club_candidacy UNIQUE (election_id, fan_party_id, role);


--
-- Name: fan_club_election unique_fan_club_election; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_election
    ADD CONSTRAINT unique_fan_club_election UNIQUE (club_id, year);


--
-- Name: fan_club_member_profile unique_fan_club_member_profile; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_member_profile
    ADD CONSTRAINT unique_fan_club_member_profile UNIQUE (party_id, club_id);


--
-- Name: fan_club_officer unique_fan_club_officer; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_officer
    ADD CONSTRAINT unique_fan_club_officer UNIQUE (club_id, role);


--
-- Name: fan_club_vote unique_fan_club_vote; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote
    ADD CONSTRAINT unique_fan_club_vote UNIQUE (election_id, fan_party_id, role);


--
-- Name: fan_follow unique_fan_follow; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_follow
    ADD CONSTRAINT unique_fan_follow UNIQUE (fan_party_id, artist_party_id);


--
-- Name: fan_profile unique_fan_profile; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_profile
    ADD CONSTRAINT unique_fan_profile UNIQUE (fan_party_id);


--
-- Name: instagram_message unique_instagram_message; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.instagram_message
    ADD CONSTRAINT unique_instagram_message UNIQUE (external_id);


--
-- Name: intern_profile unique_intern_profile; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_profile
    ADD CONSTRAINT unique_intern_profile UNIQUE (party_id);


--
-- Name: invoice unique_invoice_number; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice
    ADD CONSTRAINT unique_invoice_number UNIQUE (number);


--
-- Name: asset_kit_member unique_kit_member; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_kit_member
    ADD CONSTRAINT unique_kit_member UNIQUE (kit_id, member_id);


--
-- Name: input_list_version unique_list_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_version
    ADD CONSTRAINT unique_list_version UNIQUE (input_list_id, version);


--
-- Name: marketplace_listing unique_marketplace_asset; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_listing
    ADD CONSTRAINT unique_marketplace_asset UNIQUE (asset_id, purpose);


--
-- Name: marketplace_cart_item unique_marketplace_cart_item; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_cart_item
    ADD CONSTRAINT unique_marketplace_cart_item UNIQUE (cart_id, listing_id);


--
-- Name: package_catalog unique_package_per_subject; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_catalog
    ADD CONSTRAINT unique_package_per_subject UNIQUE (subject_id, name);


--
-- Name: party_follow unique_party_follow; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_follow
    ADD CONSTRAINT unique_party_follow UNIQUE (follower_party_id, following_party_id);


--
-- Name: party_radio_presence unique_party_presence; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_radio_presence
    ADD CONSTRAINT unique_party_presence UNIQUE (party_id);


--
-- Name: party_role unique_party_role; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_role
    ADD CONSTRAINT unique_party_role UNIQUE (party_id, role);


--
-- Name: promo_code unique_promo_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code
    ADD CONSTRAINT unique_promo_code UNIQUE (code);


--
-- Name: proposal_version unique_proposal_version; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.proposal_version
    ADD CONSTRAINT unique_proposal_version UNIQUE (proposal_id, version);


--
-- Name: radio_stream unique_radio_stream_url; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.radio_stream
    ADD CONSTRAINT unique_radio_stream_url UNIQUE (stream_url);


--
-- Name: receipt unique_receipt_number; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt
    ADD CONSTRAINT unique_receipt_number UNIQUE (number);


--
-- Name: referral_claim unique_referral_claim; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_claim
    ADD CONSTRAINT unique_referral_claim UNIQUE (code_id, email);


--
-- Name: resource unique_resource_slug; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.resource
    ADD CONSTRAINT unique_resource_slug UNIQUE (slug);


--
-- Name: room_default_gear unique_room_default_gear; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_default_gear
    ADD CONSTRAINT unique_room_default_gear UNIQUE (room_id, asset_id);


--
-- Name: room_feature unique_room_feature; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_feature
    ADD CONSTRAINT unique_room_feature UNIQUE (room_id, key);


--
-- Name: room unique_room_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room
    ADD CONSTRAINT unique_room_name UNIQUE (name);


--
-- Name: input_row unique_row_per_channel; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT unique_row_per_channel UNIQUE (version_id, channel_number);


--
-- Name: service_ad unique_service_ad; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad
    ADD CONSTRAINT unique_service_ad UNIQUE (provider_party_id, headline);


--
-- Name: service_ad_slot unique_service_ad_slot; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad_slot
    ADD CONSTRAINT unique_service_ad_slot UNIQUE (ad_id, starts_at, ends_at);


--
-- Name: service_escrow unique_service_escrow_booking; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT unique_service_escrow_booking UNIQUE (booking_id);


--
-- Name: session_invoice unique_session_invoice; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_invoice
    ADD CONSTRAINT unique_session_invoice UNIQUE (session_id, invoice_id);


--
-- Name: session_room unique_session_room; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_room
    ADD CONSTRAINT unique_session_room UNIQUE (session_id, room_id);


--
-- Name: social_discovery_review unique_social_discovery_review; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_discovery_review
    ADD CONSTRAINT unique_social_discovery_review UNIQUE (social_sync_post_id);


--
-- Name: social_sync_account unique_social_sync_account; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_account
    ADD CONSTRAINT unique_social_sync_account UNIQUE (platform, external_user_id);


--
-- Name: social_sync_post unique_social_sync_post; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post
    ADD CONSTRAINT unique_social_sync_post UNIQUE (platform, external_post_id);


--
-- Name: stock_item unique_stock_sku; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_item
    ADD CONSTRAINT unique_stock_sku UNIQUE (sku);


--
-- Name: stripe_payment_intent unique_stripe_payment_intent; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_payment_intent
    ADD CONSTRAINT unique_stripe_payment_intent UNIQUE (stripe_payment_intent_id);


--
-- Name: stripe_webhook_event unique_stripe_webhook_event; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_webhook_event
    ADD CONSTRAINT unique_stripe_webhook_event UNIQUE (stripe_event_id);


--
-- Name: subject unique_subject_name; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject
    ADD CONSTRAINT unique_subject_name UNIQUE (name);


--
-- Name: subject_room_preference unique_subject_room; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject_room_preference
    ADD CONSTRAINT unique_subject_room UNIQUE (subject_id, room_id);


--
-- Name: teacher_student unique_teacher_student; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_student
    ADD CONSTRAINT unique_teacher_student UNIQUE (teacher_id, student_id);


--
-- Name: teacher_subject unique_teacher_subject; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_subject
    ADD CONSTRAINT unique_teacher_subject UNIQUE (teacher_id, subject_id);


--
-- Name: input_list_template_row unique_template_channel; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT unique_template_channel UNIQUE (template_id, channel_number);


--
-- Name: ticket_qr_code unique_ticket_qr_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_qr_code
    ADD CONSTRAINT unique_ticket_qr_code UNIQUE (ticket_id);


--
-- Name: ticket_transfer unique_ticket_transfer_code; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_transfer
    ADD CONSTRAINT unique_ticket_transfer_code UNIQUE (transfer_code);


--
-- Name: trial_assignment unique_trial_assignment_request; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_assignment
    ADD CONSTRAINT unique_trial_assignment_request UNIQUE (request_id);


--
-- Name: trial_throttle unique_trial_throttle; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_throttle
    ADD CONSTRAINT unique_trial_throttle UNIQUE (party_id, day);


--
-- Name: whats_app_consent unique_whats_app_consent; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_consent
    ADD CONSTRAINT unique_whats_app_consent UNIQUE (phone_e164);


--
-- Name: whats_app_message unique_whats_app_message; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message
    ADD CONSTRAINT unique_whats_app_message UNIQUE (external_id);


--
-- Name: user_credential user_credential_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credential
    ADD CONSTRAINT user_credential_pkey PRIMARY KEY (id);


--
-- Name: user_locale_preferences user_locale_preferences_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_locale_preferences
    ADD CONSTRAINT user_locale_preferences_pkey PRIMARY KEY (id);


--
-- Name: user_locale_preferences user_locale_preferences_user_unique; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_locale_preferences
    ADD CONSTRAINT user_locale_preferences_user_unique UNIQUE (user_id);


--
-- Name: venue venue_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.venue
    ADD CONSTRAINT venue_pkey PRIMARY KEY (id);


--
-- Name: whats_app_consent whats_app_consent_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_consent
    ADD CONSTRAINT whats_app_consent_pkey PRIMARY KEY (id);


--
-- Name: whats_app_message whats_app_message_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message
    ADD CONSTRAINT whats_app_message_pkey PRIMARY KEY (id);


--
-- Name: artist_promo_slot_artist_day_time_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX artist_promo_slot_artist_day_time_idx ON public.artist_promo_slot USING btree (artist_party_id, day, start_time, id);


--
-- Name: currency_conversion_audit_user_created_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX currency_conversion_audit_user_created_idx ON public.currency_conversion_audit USING btree (user_id, created_at DESC);


--
-- Name: feature_access_request_history_request_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_access_request_history_request_idx ON public.feature_access_request_history USING btree (request_id, created_at);


--
-- Name: feature_access_requests_duplicate_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_access_requests_duplicate_idx ON public.feature_access_requests USING btree (requester_party_id, feature_id, action, status);


--
-- Name: feature_access_requests_one_pending_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX feature_access_requests_one_pending_idx ON public.feature_access_requests USING btree (requester_party_id, feature_id, action) WHERE (status = 'pending'::text);


--
-- Name: feature_access_requests_queue_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_access_requests_queue_idx ON public.feature_access_requests USING btree (status, reviewer_group, requested_at);


--
-- Name: feature_access_requests_requester_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_access_requests_requester_idx ON public.feature_access_requests USING btree (requester_party_id, requested_at DESC);


--
-- Name: feature_navigation_preferences_pinned_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_navigation_preferences_pinned_idx ON public.feature_navigation_preferences USING btree (party_id, pinned, pin_order);


--
-- Name: feature_navigation_preferences_recent_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX feature_navigation_preferences_recent_idx ON public.feature_navigation_preferences USING btree (party_id, last_visited_at DESC);


--
-- Name: idx_artist_enrichment_run_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_enrichment_run_status ON public.artist_enrichment_run USING btree (status, started_at DESC);


--
-- Name: idx_artist_field_change_history; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_field_change_history ON public.artist_field_change USING btree (artist_party_id, changed_at DESC);


--
-- Name: idx_artist_identity_candidate_queue; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_identity_candidate_queue ON public.artist_identity_candidate USING btree (status, confidence DESC, updated_at DESC);


--
-- Name: idx_artist_inventory_artist; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_inventory_artist ON public.artist_inventory_reference USING btree (artist_party_id);


--
-- Name: idx_artist_inventory_disposition; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_inventory_disposition ON public.artist_inventory_reference USING btree (disposition, last_seen_at DESC);


--
-- Name: idx_artist_inventory_normalized_name; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_inventory_normalized_name ON public.artist_inventory_reference USING btree (normalized_name);


--
-- Name: idx_artist_media_asset_artist; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_media_asset_artist ON public.artist_media_asset USING btree (artist_party_id, asset_kind, created_at DESC);


--
-- Name: idx_artist_media_asset_hash; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_media_asset_hash ON public.artist_media_asset USING btree (content_hash);


--
-- Name: idx_artist_research_source_artist; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_research_source_artist ON public.artist_research_source USING btree (artist_party_id, retrieved_at DESC);


--
-- Name: idx_artist_research_source_type; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_research_source_type ON public.artist_research_source USING btree (source_type, retrieved_at DESC);


--
-- Name: idx_artist_suggestion_artist; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_suggestion_artist ON public.artist_enrichment_suggestion USING btree (artist_party_id, status);


--
-- Name: idx_artist_suggestion_queue; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_suggestion_queue ON public.artist_enrichment_suggestion USING btree (status, confidence DESC, updated_at DESC);


--
-- Name: idx_artist_tip_artist_created; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_artist_tip_artist_created ON public.artist_tip USING btree (artist_profile_id, created_at DESC);


--
-- Name: idx_asset_checkout_asset; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_asset_checkout_asset ON public.asset_checkout USING btree (asset_id);


--
-- Name: idx_asset_checkout_returned; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_asset_checkout_returned ON public.asset_checkout USING btree (asset_id, returned_at) WHERE (returned_at IS NULL);


--
-- Name: idx_catalog_asset_sha256; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_catalog_asset_sha256 ON public.catalog_asset USING btree (sha256);


--
-- Name: idx_catalog_credit_party; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_credit_party ON public.catalog_credit USING btree (party_id);


--
-- Name: idx_catalog_identifier_entity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_identifier_entity ON public.catalog_identifier USING btree (entity_id, entity_type);


--
-- Name: idx_catalog_identifier_value; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_identifier_value ON public.catalog_identifier USING btree (value);


--
-- Name: idx_catalog_release_resource_release; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_release_resource_release ON public.catalog_release_resource USING btree (release_id);


--
-- Name: idx_catalog_release_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_release_status ON public.catalog_release USING btree (status);


--
-- Name: idx_catalog_release_type; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_catalog_release_type ON public.catalog_release USING btree (release_type);


--
-- Name: idx_ddex_document_sha256; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ddex_document_sha256 ON public.ddex_document USING btree (sha256);


--
-- Name: idx_ddex_document_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ddex_document_status ON public.ddex_document USING btree (status);


--
-- Name: idx_ddex_job_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ddex_job_status ON public.ddex_job USING btree (status, leased_until);


--
-- Name: idx_event_city_subscription_city; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_city_subscription_city ON public.event_city_subscription USING btree (city_id);


--
-- Name: idx_event_discovery_source_enabled_priority; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_discovery_source_enabled_priority ON public.event_discovery_source USING btree (enabled, priority);


--
-- Name: idx_event_logistics_activity_event_start; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_activity_event_start ON public.event_logistics_activity USING btree (event_id, start_time);


--
-- Name: idx_event_logistics_activity_recheck; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_activity_recheck ON public.event_logistics_activity USING btree (activity_type, status, start_time);


--
-- Name: idx_event_logistics_assignment_activity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_assignment_activity ON public.event_logistics_assignment USING btree (activity_id);


--
-- Name: idx_event_logistics_dependency_activity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_dependency_activity ON public.event_logistics_dependency USING btree (activity_id);


--
-- Name: idx_event_logistics_dependency_parent; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_dependency_parent ON public.event_logistics_dependency USING btree (depends_on_activity_id);


--
-- Name: idx_event_logistics_member_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_member_event ON public.event_logistics_member USING btree (event_id);


--
-- Name: idx_event_logistics_place_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_logistics_place_event ON public.event_logistics_place USING btree (event_id);


--
-- Name: idx_event_route_verification_activity; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_event_route_verification_activity ON public.event_route_verification USING btree (activity_id, verified_at DESC);


--
-- Name: idx_event_route_verification_checkpoint_once; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX idx_event_route_verification_checkpoint_once ON public.event_route_verification USING btree (activity_id, activity_version, checkpoint) WHERE (checkpoint IS NOT NULL);


--
-- Name: idx_external_event_ref_city; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_external_event_ref_city ON public.external_event_ref USING btree (lower(city));


--
-- Name: idx_external_event_ref_city_country; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_external_event_ref_city_country ON public.external_event_ref USING btree (lower(city), country_code);


--
-- Name: idx_external_event_ref_event_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_external_event_ref_event_id ON public.external_event_ref USING btree (event_id);


--
-- Name: idx_notification_recipient; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_notification_recipient ON public.notification USING btree (recipient_party_id, is_read, created_at DESC);


--
-- Name: idx_party_stripe_customer_id; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_party_stripe_customer_id ON public.party USING btree (stripe_customer_id) WHERE (stripe_customer_id IS NOT NULL);


--
-- Name: idx_promo_code_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_promo_code_event ON public.promo_code USING btree (event_id) WHERE (event_id IS NOT NULL);


--
-- Name: idx_promo_code_lookup; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_promo_code_lookup ON public.promo_code USING btree (code, is_active);


--
-- Name: idx_promo_redemption_order; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_promo_redemption_order ON public.promo_code_redemption USING btree (order_id);


--
-- Name: idx_refund_order; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_refund_order ON public.ticket_refund_request USING btree (order_id);


--
-- Name: idx_refund_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_refund_status ON public.ticket_refund_request USING btree (status, created_at);


--
-- Name: idx_social_discovery_review_status_updated; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_discovery_review_status_updated ON public.social_discovery_review USING btree (status, updated_at DESC);


--
-- Name: idx_social_sync_account_artist_profile; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_account_artist_profile ON public.social_sync_account USING btree (artist_profile_id) WHERE (artist_profile_id IS NOT NULL);


--
-- Name: idx_social_sync_account_party; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_account_party ON public.social_sync_account USING btree (party_id) WHERE (party_id IS NOT NULL);


--
-- Name: idx_social_sync_post_account; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_post_account ON public.social_sync_post USING btree (account_id) WHERE (account_id IS NOT NULL);


--
-- Name: idx_social_sync_post_artist_party; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_post_artist_party ON public.social_sync_post USING btree (artist_party_id) WHERE (artist_party_id IS NOT NULL);


--
-- Name: idx_social_sync_post_artist_profile; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_post_artist_profile ON public.social_sync_post USING btree (artist_profile_id) WHERE (artist_profile_id IS NOT NULL);


--
-- Name: idx_social_sync_post_platform_posted; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_post_platform_posted ON public.social_sync_post USING btree (platform, posted_at DESC, fetched_at DESC);


--
-- Name: idx_social_sync_run_platform_started; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_social_sync_run_platform_started ON public.social_sync_run USING btree (platform, started_at DESC);


--
-- Name: idx_ticket_current_holder; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ticket_current_holder ON public.event_ticket USING btree (current_holder_party_id) WHERE (current_holder_party_id IS NOT NULL);


--
-- Name: idx_ticket_order_promo; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_ticket_order_promo ON public.event_ticket_order USING btree (promo_code_id) WHERE (promo_code_id IS NOT NULL);


--
-- Name: idx_transfer_ticket; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_transfer_ticket ON public.ticket_transfer USING btree (ticket_id);


--
-- Name: idx_waitlist_event; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX idx_waitlist_event ON public.event_waitlist USING btree (event_id, status);


--
-- Name: index_campaign_automation_status; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_campaign_automation_status ON public.campaign_automation USING btree (status, start_at);


--
-- Name: index_campaign_automation_step_active; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_campaign_automation_step_active ON public.campaign_automation_step USING btree (automation_id, active, "position");


--
-- Name: index_campaign_delivery_automation; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_campaign_delivery_automation ON public.campaign_delivery USING btree (automation_id, status, created_at);


--
-- Name: index_campaign_enrollment_due; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_campaign_enrollment_due ON public.campaign_enrollment USING btree (automation_id, status, next_run_at);


--
-- Name: index_campaign_enrollment_party; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX index_campaign_enrollment_party ON public.campaign_enrollment USING btree (party_id, created_at);


--
-- Name: operations_admin_audit_retention_brin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_admin_audit_retention_brin_idx ON public.operations_admin_audit USING brin (created_at) WITH (pages_per_range='64');


--
-- Name: operations_admin_audit_target_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_admin_audit_target_idx ON public.operations_admin_audit USING btree (organization_id, target_entity_type, target_entity_id, created_at DESC);


--
-- Name: operations_backfill_run_key_uidx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX operations_backfill_run_key_uidx ON public.operations_backfill_run USING btree (organization_id, source_name, run_key, dry_run);


--
-- Name: operations_domain_event_aggregate_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_domain_event_aggregate_idx ON public.operations_domain_event USING btree (organization_id, aggregate_type, aggregate_id, occurred_at, id);


--
-- Name: operations_domain_event_provider_uidx; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX operations_domain_event_provider_uidx ON public.operations_domain_event USING btree (organization_id, source_system, provider_event_id) WHERE (provider_event_id IS NOT NULL);


--
-- Name: operations_domain_event_retention_brin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_domain_event_retention_brin_idx ON public.operations_domain_event USING brin (recorded_at) WITH (pages_per_range='64');


--
-- Name: operations_integration_failure_queue_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_integration_failure_queue_idx ON public.operations_integration_failure USING btree (organization_id, status, created_at DESC);


--
-- Name: operations_outbound_claim_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_outbound_claim_idx ON public.operations_outbound_delivery USING btree (next_attempt_at, created_at) WHERE (status = ANY (ARRAY['pending'::text, 'failed'::text]));


--
-- Name: operations_outbox_claim_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_outbox_claim_idx ON public.operations_outbox USING btree (next_attempt_at, created_at, id) WHERE (status = ANY (ARRAY['pending'::text, 'processing'::text]));


--
-- Name: operations_scope_member_party_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_scope_member_party_idx ON public.operations_scope_member USING btree (party_id, organization_id, branch_id) WHERE active;


--
-- Name: operations_stream_resume_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_stream_resume_idx ON public.operations_stream_event USING btree (organization_id, id);


--
-- Name: operations_work_item_assignee_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_assignee_idx ON public.operations_work_item USING btree (organization_id, assignee_party_id, status, updated_at DESC);


--
-- Name: operations_work_item_branch_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_branch_idx ON public.operations_work_item USING btree (organization_id, branch_id, status, updated_at DESC);


--
-- Name: operations_work_item_event_retention_brin_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_event_retention_brin_idx ON public.operations_work_item_event USING brin (occurred_at) WITH (pages_per_range='64');


--
-- Name: operations_work_item_event_thread_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_event_thread_idx ON public.operations_work_item_event USING btree (organization_id, work_item_id, occurred_at, id);


--
-- Name: operations_work_item_inbox_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_inbox_idx ON public.operations_work_item USING btree (organization_id, status, priority, updated_at DESC, id DESC);


--
-- Name: operations_work_item_search_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_search_idx ON public.operations_work_item USING gin (to_tsvector('simple'::regconfig, ((((((((((COALESCE(title_es, ''::text) || ' '::text) || COALESCE(title_en, ''::text)) || ' '::text) || COALESCE(description_es, ''::text)) || ' '::text) || COALESCE(description_en, ''::text)) || ' '::text) || COALESCE(entity_id, ''::text)) || ' '::text) || COALESCE(correlation_key, ''::text))));


--
-- Name: operations_work_item_sla_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX operations_work_item_sla_idx ON public.operations_work_item USING btree (organization_id, due_at, status) WHERE (status <> ALL (ARRAY['resolved'::text, 'archived'::text]));


--
-- Name: rag_chunk_embedding_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX rag_chunk_embedding_idx ON public.rag_chunk USING ivfflat (embedding public.vector_cosine_ops) WITH (lists='100');


--
-- Name: rag_chunk_source_id_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX rag_chunk_source_id_idx ON public.rag_chunk USING btree (source_id);


--
-- Name: rag_chunk_source_idx; Type: INDEX; Schema: public; Owner: -
--

CREATE INDEX rag_chunk_source_idx ON public.rag_chunk USING btree (source);


--
-- Name: rag_chunk_source_key; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX rag_chunk_source_key ON public.rag_chunk USING btree (source, source_id, chunk_index);


--
-- Name: unique_external_event_discovery_slot; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX unique_external_event_discovery_slot ON public.external_event_discovery_run USING btree (provider, scheduled_for) WHERE (scheduled_for IS NOT NULL);


--
-- Name: uq_artist_enrichment_active_full_run; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_artist_enrichment_active_full_run ON public.artist_enrichment_run USING btree (scope) WHERE ((status = 'running'::text) AND (scope = 'full'::text));


--
-- Name: uq_artist_profile_slug_ci; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_artist_profile_slug_ci ON public.artist_profile USING btree (lower((slug)::text)) WHERE ((slug IS NOT NULL) AND (btrim((slug)::text) <> ''::text));


--
-- Name: uq_artist_profile_stripe_account; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_artist_profile_stripe_account ON public.artist_profile USING btree (stripe_account_id) WHERE (stripe_account_id IS NOT NULL);


--
-- Name: uq_artist_tip_stripe_payment_intent; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_artist_tip_stripe_payment_intent ON public.artist_tip USING btree (stripe_payment_intent_id) WHERE (stripe_payment_intent_id IS NOT NULL);


--
-- Name: uq_course_registration_stripe_payment_intent; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_course_registration_stripe_payment_intent ON public.course_registration USING btree (stripe_payment_intent_id) WHERE (stripe_payment_intent_id IS NOT NULL);


--
-- Name: uq_course_registration_stripe_subscription; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_course_registration_stripe_subscription ON public.course_registration USING btree (stripe_subscription_id) WHERE (stripe_subscription_id IS NOT NULL);


--
-- Name: uq_event_ticket_order_stripe_payment_intent; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_event_ticket_order_stripe_payment_intent ON public.event_ticket_order USING btree (stripe_payment_intent_id) WHERE (stripe_payment_intent_id IS NOT NULL);


--
-- Name: uq_marketplace_cart_active_stripe_payment; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_marketplace_cart_active_stripe_payment ON public.marketplace_order USING btree (cart_id) WHERE ((cart_id IS NOT NULL) AND ((status)::text = 'stripe_pending'::text));


--
-- Name: uq_marketplace_order_stripe_payment_intent; Type: INDEX; Schema: public; Owner: -
--

CREATE UNIQUE INDEX uq_marketplace_order_stripe_payment_intent ON public.marketplace_order USING btree (stripe_payment_intent_id) WHERE (stripe_payment_intent_id IS NOT NULL);


--
-- Name: operations_admin_audit operations_admin_audit_immutable; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_admin_audit_immutable BEFORE DELETE OR UPDATE ON public.operations_admin_audit FOR EACH ROW EXECUTE FUNCTION public.operations_reject_mutation();


--
-- Name: artist_profile operations_artist_profile_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_artist_profile_capture AFTER INSERT ON public.artist_profile FOR EACH ROW EXECUTE FUNCTION public.operations_artist_profile_event();


--
-- Name: booking operations_booking_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_booking_capture AFTER INSERT OR UPDATE OF status, starts_at, ends_at ON public.booking FOR EACH ROW EXECUTE FUNCTION public.operations_booking_event();


--
-- Name: course_registration operations_course_registration_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_course_registration_capture AFTER INSERT OR UPDATE OF status ON public.course_registration FOR EACH ROW EXECUTE FUNCTION public.operations_course_registration_event();


--
-- Name: operations_domain_event operations_domain_event_immutable; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_domain_event_immutable BEFORE DELETE OR UPDATE ON public.operations_domain_event FOR EACH ROW EXECUTE FUNCTION public.operations_reject_mutation();


--
-- Name: operations_domain_event operations_domain_event_outbox; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_domain_event_outbox AFTER INSERT ON public.operations_domain_event FOR EACH ROW EXECUTE FUNCTION public.operations_enqueue_domain_event();


--
-- Name: facebook_message operations_facebook_inbound_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_facebook_inbound_capture AFTER INSERT ON public.facebook_message FOR EACH ROW EXECUTE FUNCTION public.operations_social_inbound_event();


--
-- Name: feature_access_requests operations_feature_access_request_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_feature_access_request_capture AFTER INSERT OR UPDATE OF status, reviewer_party_id ON public.feature_access_requests FOR EACH ROW EXECUTE FUNCTION public.operations_feature_access_request_event();


--
-- Name: instagram_message operations_instagram_inbound_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_instagram_inbound_capture AFTER INSERT ON public.instagram_message FOR EACH ROW EXECUTE FUNCTION public.operations_social_inbound_event();


--
-- Name: operations_integration_failure operations_integration_failure_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_integration_failure_capture AFTER INSERT ON public.operations_integration_failure FOR EACH ROW EXECUTE FUNCTION public.operations_integration_failure_event();


--
-- Name: intern_project operations_intern_project_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_intern_project_capture AFTER INSERT OR UPDATE OF status, due_at ON public.intern_project FOR EACH ROW EXECUTE FUNCTION public.operations_intern_project_event();


--
-- Name: intern_task operations_intern_task_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_intern_task_capture AFTER INSERT OR UPDATE OF status, assigned_to, due_at ON public.intern_task FOR EACH ROW EXECUTE FUNCTION public.operations_intern_task_event();


--
-- Name: invoice operations_invoice_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_invoice_capture AFTER INSERT OR UPDATE OF status, due_date ON public.invoice FOR EACH ROW EXECUTE FUNCTION public.operations_invoice_event();


--
-- Name: lead_interest operations_lead_interest_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_lead_interest_capture AFTER INSERT OR UPDATE OF status ON public.lead_interest FOR EACH ROW EXECUTE FUNCTION public.operations_lead_interest_event();


--
-- Name: maintenance_ticket operations_maintenance_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_maintenance_capture AFTER INSERT OR UPDATE OF status ON public.maintenance_ticket FOR EACH ROW EXECUTE FUNCTION public.operations_maintenance_event();


--
-- Name: marketplace_order operations_marketplace_order_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_marketplace_order_capture AFTER INSERT OR UPDATE OF status ON public.marketplace_order FOR EACH ROW EXECUTE FUNCTION public.operations_marketplace_order_event();


--
-- Name: package_purchase operations_package_purchase_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_package_purchase_capture AFTER INSERT OR UPDATE OF status, remaining_units, expires_at ON public.package_purchase FOR EACH ROW EXECUTE FUNCTION public.operations_package_purchase_event();


--
-- Name: party_role operations_party_role_scope_sync; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_party_role_scope_sync AFTER INSERT OR UPDATE OF role, active ON public.party_role FOR EACH ROW EXECUTE FUNCTION public.operations_sync_scope_member_from_role();


--
-- Name: payment operations_payment_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_payment_capture AFTER INSERT ON public.payment FOR EACH ROW EXECUTE FUNCTION public.operations_payment_event();


--
-- Name: proposal operations_proposal_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_proposal_capture AFTER INSERT OR UPDATE OF status, client_party_id ON public.proposal FOR EACH ROW EXECUTE FUNCTION public.operations_proposal_event();


--
-- Name: course_registration_receipt operations_registration_receipt_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_registration_receipt_capture AFTER INSERT ON public.course_registration_receipt FOR EACH ROW EXECUTE FUNCTION public.operations_registration_receipt_event();


--
-- Name: service_order operations_service_order_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_service_order_capture AFTER INSERT OR UPDATE OF status, scheduled_start, scheduled_end ON public.service_order FOR EACH ROW EXECUTE FUNCTION public.operations_service_order_event();


--
-- Name: social_event operations_social_event_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_social_event_capture AFTER INSERT OR UPDATE OF start_time, end_time, venue_id ON public.social_event FOR EACH ROW EXECUTE FUNCTION public.operations_social_event_event();


--
-- Name: stock_item operations_stock_item_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_stock_item_capture AFTER INSERT OR UPDATE OF on_hand, reorder_point ON public.stock_item FOR EACH ROW WHEN ((new.reorder_point IS NOT NULL)) EXECUTE FUNCTION public.operations_stock_item_event();


--
-- Name: trial_request operations_trial_request_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_trial_request_capture AFTER INSERT OR UPDATE OF status, assigned_teacher_id ON public.trial_request FOR EACH ROW EXECUTE FUNCTION public.operations_trial_request_event();


--
-- Name: whats_app_message operations_whatsapp_inbound_capture; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER operations_whatsapp_inbound_capture AFTER INSERT ON public.whats_app_message FOR EACH ROW EXECUTE FUNCTION public.operations_whatsapp_inbound_event();


--
-- Name: operations_work_item operations_work_item_entity_reference; Type: TRIGGER; Schema: public; Owner: -
--

CREATE CONSTRAINT TRIGGER operations_work_item_entity_reference AFTER INSERT OR UPDATE OF entity_type, entity_id, uncorrelated ON public.operations_work_item DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION public.operations_validate_entity_reference();


--
-- Name: catalog_release trg_catalog_release_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_catalog_release_updated_at BEFORE UPDATE ON public.catalog_release FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: catalog_resource trg_catalog_resource_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_catalog_resource_updated_at BEFORE UPDATE ON public.catalog_resource FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: ddex_job trg_ddex_job_updated_at; Type: TRIGGER; Schema: public; Owner: -
--

CREATE TRIGGER trg_ddex_job_updated_at BEFORE UPDATE ON public.ddex_job FOR EACH ROW EXECUTE FUNCTION public.trigger_set_timestamp();


--
-- Name: academy_lesson academy_lesson_microcourse_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_lesson
    ADD CONSTRAINT academy_lesson_microcourse_id_fkey FOREIGN KEY (microcourse_id) REFERENCES public.academy_microcourse(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: academy_progress academy_progress_lesson_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_progress
    ADD CONSTRAINT academy_progress_lesson_id_fkey FOREIGN KEY (lesson_id) REFERENCES public.academy_lesson(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: academy_progress academy_progress_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.academy_progress
    ADD CONSTRAINT academy_progress_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.academy_user(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: ad_conversation_example ad_conversation_example_ad_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_conversation_example
    ADD CONSTRAINT ad_conversation_example_ad_id_fkey FOREIGN KEY (ad_id) REFERENCES public.ad_creative(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: ad_creative ad_creative_campaign_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ad_creative
    ADD CONSTRAINT ad_creative_campaign_id_fkey FOREIGN KEY (campaign_id) REFERENCES public.campaign(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: api_token api_token_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.api_token
    ADD CONSTRAINT api_token_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: artist_enrichment_run artist_enrichment_run_requested_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_run
    ADD CONSTRAINT artist_enrichment_run_requested_artist_id_fkey FOREIGN KEY (requested_artist_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_enrichment_suggestion artist_enrichment_suggestion_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion
    ADD CONSTRAINT artist_enrichment_suggestion_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_enrichment_suggestion artist_enrichment_suggestion_decided_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion
    ADD CONSTRAINT artist_enrichment_suggestion_decided_by_fkey FOREIGN KEY (decided_by) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_enrichment_suggestion artist_enrichment_suggestion_inventory_reference_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_enrichment_suggestion
    ADD CONSTRAINT artist_enrichment_suggestion_inventory_reference_id_fkey FOREIGN KEY (inventory_reference_id) REFERENCES public.artist_inventory_reference(id) ON DELETE SET NULL;


--
-- Name: artist_field_change artist_field_change_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_field_change
    ADD CONSTRAINT artist_field_change_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE RESTRICT;


--
-- Name: artist_field_change artist_field_change_suggestion_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_field_change
    ADD CONSTRAINT artist_field_change_suggestion_id_fkey FOREIGN KEY (suggestion_id) REFERENCES public.artist_enrichment_suggestion(id) ON DELETE SET NULL;


--
-- Name: artist_follow artist_follow_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_follow
    ADD CONSTRAINT artist_follow_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.social_artist_profile(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: artist_genre artist_genre_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_genre
    ADD CONSTRAINT artist_genre_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.social_artist_profile(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: artist_identity_candidate artist_identity_candidate_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate
    ADD CONSTRAINT artist_identity_candidate_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_identity_candidate artist_identity_candidate_decided_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate
    ADD CONSTRAINT artist_identity_candidate_decided_by_fkey FOREIGN KEY (decided_by) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_identity_candidate artist_identity_candidate_inventory_reference_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_identity_candidate
    ADD CONSTRAINT artist_identity_candidate_inventory_reference_id_fkey FOREIGN KEY (inventory_reference_id) REFERENCES public.artist_inventory_reference(id) ON DELETE RESTRICT;


--
-- Name: artist_inventory_reference artist_inventory_reference_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_inventory_reference
    ADD CONSTRAINT artist_inventory_reference_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_media_asset artist_media_asset_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset
    ADD CONSTRAINT artist_media_asset_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE RESTRICT;


--
-- Name: artist_media_asset artist_media_asset_parent_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_media_asset
    ADD CONSTRAINT artist_media_asset_parent_asset_id_fkey FOREIGN KEY (parent_asset_id) REFERENCES public.artist_media_asset(id) ON DELETE SET NULL;


--
-- Name: artist_profile artist_profile_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile
    ADD CONSTRAINT artist_profile_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: artist_profile_enrichment artist_profile_enrichment_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_profile_enrichment
    ADD CONSTRAINT artist_profile_enrichment_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE RESTRICT;


--
-- Name: artist_promo_slot artist_promo_slot_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_promo_slot
    ADD CONSTRAINT artist_promo_slot_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE CASCADE;


--
-- Name: artist_release artist_release_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_release
    ADD CONSTRAINT artist_release_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: artist_research_source artist_research_source_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_research_source
    ADD CONSTRAINT artist_research_source_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: artist_research_source artist_research_source_inventory_reference_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_research_source
    ADD CONSTRAINT artist_research_source_inventory_reference_id_fkey FOREIGN KEY (inventory_reference_id) REFERENCES public.artist_inventory_reference(id) ON DELETE SET NULL;


--
-- Name: artist_tip artist_tip_artist_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_tip
    ADD CONSTRAINT artist_tip_artist_profile_id_fkey FOREIGN KEY (artist_profile_id) REFERENCES public.artist_profile(id) ON DELETE CASCADE;


--
-- Name: artist_tip artist_tip_tipper_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_tip
    ADD CONSTRAINT artist_tip_tipper_party_id_fkey FOREIGN KEY (tipper_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: asset_audit asset_audit_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_audit
    ADD CONSTRAINT asset_audit_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset_checkout asset_checkout_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_checkout
    ADD CONSTRAINT asset_checkout_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset_checkout asset_checkout_target_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_checkout
    ADD CONSTRAINT asset_checkout_target_room_id_fkey FOREIGN KEY (target_room_id) REFERENCES public.room(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset_checkout asset_checkout_target_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_checkout
    ADD CONSTRAINT asset_checkout_target_session_id_fkey FOREIGN KEY (target_session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset_kit_member asset_kit_member_kit_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_kit_member
    ADD CONSTRAINT asset_kit_member_kit_id_fkey FOREIGN KEY (kit_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset_kit_member asset_kit_member_member_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset_kit_member
    ADD CONSTRAINT asset_kit_member_member_id_fkey FOREIGN KEY (member_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: asset asset_location_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.asset
    ADD CONSTRAINT asset_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.room(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: attendance attendance_booking_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES public.booking(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: attendance attendance_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: audit_log audit_log_actor_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.audit_log
    ADD CONSTRAINT audit_log_actor_id_fkey FOREIGN KEY (actor_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: band_member band_member_band_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.band_member
    ADD CONSTRAINT band_member_band_id_fkey FOREIGN KEY (band_id) REFERENCES public.band(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking booking_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking
    ADD CONSTRAINT booking_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking booking_engineer_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking
    ADD CONSTRAINT booking_engineer_party_id_fkey FOREIGN KEY (engineer_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking booking_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking
    ADD CONSTRAINT booking_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking_resource booking_resource_booking_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking_resource
    ADD CONSTRAINT booking_resource_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES public.booking(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking_resource booking_resource_resource_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking_resource
    ADD CONSTRAINT booking_resource_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES public.resource(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: booking booking_service_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.booking
    ADD CONSTRAINT booking_service_order_id_fkey FOREIGN KEY (service_order_id) REFERENCES public.service_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: campaign_automation campaign_automation_campaign_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation
    ADD CONSTRAINT campaign_automation_campaign_id_fkey FOREIGN KEY (campaign_id) REFERENCES public.campaign(id);


--
-- Name: campaign_automation_step campaign_automation_step_automation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_automation_step
    ADD CONSTRAINT campaign_automation_step_automation_id_fkey FOREIGN KEY (automation_id) REFERENCES public.campaign_automation(id) ON DELETE CASCADE;


--
-- Name: campaign_delivery campaign_delivery_automation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT campaign_delivery_automation_id_fkey FOREIGN KEY (automation_id) REFERENCES public.campaign_automation(id) ON DELETE CASCADE;


--
-- Name: campaign_delivery campaign_delivery_enrollment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT campaign_delivery_enrollment_id_fkey FOREIGN KEY (enrollment_id) REFERENCES public.campaign_enrollment(id) ON DELETE CASCADE;


--
-- Name: campaign_delivery campaign_delivery_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT campaign_delivery_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: campaign_delivery campaign_delivery_step_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_delivery
    ADD CONSTRAINT campaign_delivery_step_id_fkey FOREIGN KEY (step_id) REFERENCES public.campaign_automation_step(id);


--
-- Name: campaign_enrollment campaign_enrollment_automation_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_enrollment
    ADD CONSTRAINT campaign_enrollment_automation_id_fkey FOREIGN KEY (automation_id) REFERENCES public.campaign_automation(id) ON DELETE CASCADE;


--
-- Name: campaign_enrollment campaign_enrollment_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.campaign_enrollment
    ADD CONSTRAINT campaign_enrollment_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: catalog_deal catalog_deal_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal
    ADD CONSTRAINT catalog_deal_release_id_fkey FOREIGN KEY (release_id) REFERENCES public.catalog_release(id);


--
-- Name: catalog_deal catalog_deal_resource_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal
    ADD CONSTRAINT catalog_deal_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES public.catalog_resource(id);


--
-- Name: catalog_deal_territory catalog_deal_territory_deal_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_deal_territory
    ADD CONSTRAINT catalog_deal_territory_deal_id_fkey FOREIGN KEY (deal_id) REFERENCES public.catalog_deal(id) ON DELETE CASCADE;


--
-- Name: catalog_release_resource catalog_release_resource_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release_resource
    ADD CONSTRAINT catalog_release_resource_release_id_fkey FOREIGN KEY (release_id) REFERENCES public.catalog_release(id) ON DELETE CASCADE;


--
-- Name: catalog_release_resource catalog_release_resource_resource_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.catalog_release_resource
    ADD CONSTRAINT catalog_release_resource_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES public.catalog_resource(id) ON DELETE CASCADE;


--
-- Name: chat_message chat_message_sender_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_message
    ADD CONSTRAINT chat_message_sender_party_id_fkey FOREIGN KEY (sender_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: chat_message chat_message_thread_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_message
    ADD CONSTRAINT chat_message_thread_id_fkey FOREIGN KEY (thread_id) REFERENCES public.chat_thread(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: chat_thread chat_thread_dm_party_a_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_thread
    ADD CONSTRAINT chat_thread_dm_party_a_fkey FOREIGN KEY (dm_party_a) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: chat_thread chat_thread_dm_party_b_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.chat_thread
    ADD CONSTRAINT chat_thread_dm_party_b_fkey FOREIGN KEY (dm_party_b) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: class_package_purchase class_package_purchase_package_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_package_purchase
    ADD CONSTRAINT class_package_purchase_package_id_fkey FOREIGN KEY (package_id) REFERENCES public.package_catalog(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: class_package_purchase class_package_purchase_trial_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_package_purchase
    ADD CONSTRAINT class_package_purchase_trial_request_id_fkey FOREIGN KEY (trial_request_id) REFERENCES public.trial_request(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: class_session class_session_purchase_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_session
    ADD CONSTRAINT class_session_purchase_id_fkey FOREIGN KEY (purchase_id) REFERENCES public.class_package_purchase(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: class_session class_session_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.class_session
    ADD CONSTRAINT class_session_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: cms_content cms_content_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cms_content
    ADD CONSTRAINT cms_content_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE SET NULL;


--
-- Name: cohort_enrollment cohort_enrollment_cohort_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_enrollment
    ADD CONSTRAINT cohort_enrollment_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: cohort_enrollment cohort_enrollment_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.cohort_enrollment
    ADD CONSTRAINT cohort_enrollment_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.academy_user(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: commission commission_purchase_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.commission
    ADD CONSTRAINT commission_purchase_id_fkey FOREIGN KEY (purchase_id) REFERENCES public.class_package_purchase(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: course_email_event course_email_event_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_email_event
    ADD CONSTRAINT course_email_event_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.course_registration(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: course_registration_follow_up course_registration_follow_up_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_follow_up
    ADD CONSTRAINT course_registration_follow_up_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.course_registration(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: course_registration_receipt course_registration_receipt_registration_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_registration_receipt
    ADD CONSTRAINT course_registration_receipt_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.course_registration(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: course_session_model course_session_model_course_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_session_model
    ADD CONSTRAINT course_session_model_course_id_fkey FOREIGN KEY (course_id) REFERENCES public.course(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: course_syllabus_item course_syllabus_item_course_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.course_syllabus_item
    ADD CONSTRAINT course_syllabus_item_course_id_fkey FOREIGN KEY (course_id) REFERENCES public.course(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: currency_conversion_audit currency_conversion_audit_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.currency_conversion_audit
    ADD CONSTRAINT currency_conversion_audit_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: ddex_export ddex_export_release_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_export
    ADD CONSTRAINT ddex_export_release_id_fkey FOREIGN KEY (release_id) REFERENCES public.catalog_release(id);


--
-- Name: ddex_import_change ddex_import_change_import_run_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_change
    ADD CONSTRAINT ddex_import_change_import_run_id_fkey FOREIGN KEY (import_run_id) REFERENCES public.ddex_import_run(id);


--
-- Name: ddex_import_plan ddex_import_plan_document_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_plan
    ADD CONSTRAINT ddex_import_plan_document_id_fkey FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;


--
-- Name: ddex_import_run ddex_import_run_plan_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_import_run
    ADD CONSTRAINT ddex_import_run_plan_id_fkey FOREIGN KEY (plan_id) REFERENCES public.ddex_import_plan(id);


--
-- Name: ddex_message_header ddex_message_header_document_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_message_header
    ADD CONSTRAINT ddex_message_header_document_id_fkey FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;


--
-- Name: ddex_validation_issue ddex_validation_issue_validation_run_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_issue
    ADD CONSTRAINT ddex_validation_issue_validation_run_id_fkey FOREIGN KEY (validation_run_id) REFERENCES public.ddex_validation_run(id) ON DELETE CASCADE;


--
-- Name: ddex_validation_run ddex_validation_run_document_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ddex_validation_run
    ADD CONSTRAINT ddex_validation_run_document_id_fkey FOREIGN KEY (document_id) REFERENCES public.ddex_document(id) ON DELETE CASCADE;


--
-- Name: event_artist event_artist_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_artist
    ADD CONSTRAINT event_artist_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.social_artist_profile(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_artist event_artist_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_artist
    ADD CONSTRAINT event_artist_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_budget_line event_budget_line_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_budget_line
    ADD CONSTRAINT event_budget_line_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_city_subscription event_city_subscription_city_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_city_subscription
    ADD CONSTRAINT event_city_subscription_city_id_fkey FOREIGN KEY (city_id) REFERENCES public.event_city(id) ON DELETE CASCADE;


--
-- Name: event_discovery_source event_discovery_source_city_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_discovery_source
    ADD CONSTRAINT event_discovery_source_city_id_fkey FOREIGN KEY (city_id) REFERENCES public.event_city(id);


--
-- Name: event_finance_entry event_finance_entry_budget_line_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_finance_entry
    ADD CONSTRAINT event_finance_entry_budget_line_id_fkey FOREIGN KEY (budget_line_id) REFERENCES public.event_budget_line(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_finance_entry event_finance_entry_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_finance_entry
    ADD CONSTRAINT event_finance_entry_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_invitation event_invitation_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_invitation
    ADD CONSTRAINT event_invitation_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_logistics_activity event_logistics_activity_destination_place_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity
    ADD CONSTRAINT event_logistics_activity_destination_place_id_fkey FOREIGN KEY (destination_place_id) REFERENCES public.event_logistics_place(id);


--
-- Name: event_logistics_activity event_logistics_activity_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity
    ADD CONSTRAINT event_logistics_activity_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON DELETE CASCADE;


--
-- Name: event_logistics_activity event_logistics_activity_origin_place_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity
    ADD CONSTRAINT event_logistics_activity_origin_place_id_fkey FOREIGN KEY (origin_place_id) REFERENCES public.event_logistics_place(id);


--
-- Name: event_logistics_activity event_logistics_activity_place_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_activity
    ADD CONSTRAINT event_logistics_activity_place_id_fkey FOREIGN KEY (place_id) REFERENCES public.event_logistics_place(id);


--
-- Name: event_logistics_alert_delivery event_logistics_alert_delivery_activity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_alert_delivery
    ADD CONSTRAINT event_logistics_alert_delivery_activity_id_fkey FOREIGN KEY (activity_id) REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE;


--
-- Name: event_logistics_assignment event_logistics_assignment_activity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_assignment
    ADD CONSTRAINT event_logistics_assignment_activity_id_fkey FOREIGN KEY (activity_id) REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE;


--
-- Name: event_logistics_dependency event_logistics_dependency_activity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_dependency
    ADD CONSTRAINT event_logistics_dependency_activity_id_fkey FOREIGN KEY (activity_id) REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE;


--
-- Name: event_logistics_dependency event_logistics_dependency_depends_on_activity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_dependency
    ADD CONSTRAINT event_logistics_dependency_depends_on_activity_id_fkey FOREIGN KEY (depends_on_activity_id) REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE;


--
-- Name: event_logistics_member event_logistics_member_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_member
    ADD CONSTRAINT event_logistics_member_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON DELETE CASCADE;


--
-- Name: event_logistics_place event_logistics_place_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_place
    ADD CONSTRAINT event_logistics_place_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON DELETE CASCADE;


--
-- Name: event_logistics_place event_logistics_place_venue_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_place
    ADD CONSTRAINT event_logistics_place_venue_id_fkey FOREIGN KEY (venue_id) REFERENCES public.venue(id) ON DELETE SET NULL;


--
-- Name: event_logistics_plan event_logistics_plan_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_logistics_plan
    ADD CONSTRAINT event_logistics_plan_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON DELETE CASCADE;


--
-- Name: event_moment_comment event_moment_comment_moment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment_comment
    ADD CONSTRAINT event_moment_comment_moment_id_fkey FOREIGN KEY (moment_id) REFERENCES public.event_moment(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_moment event_moment_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment
    ADD CONSTRAINT event_moment_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_moment_reaction event_moment_reaction_moment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_moment_reaction
    ADD CONSTRAINT event_moment_reaction_moment_id_fkey FOREIGN KEY (moment_id) REFERENCES public.event_moment(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_route_verification event_route_verification_activity_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_route_verification
    ADD CONSTRAINT event_route_verification_activity_id_fkey FOREIGN KEY (activity_id) REFERENCES public.event_logistics_activity(id) ON DELETE CASCADE;


--
-- Name: event_rsvp event_rsvp_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_rsvp
    ADD CONSTRAINT event_rsvp_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket event_ticket_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket
    ADD CONSTRAINT event_ticket_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket_order event_ticket_order_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order
    ADD CONSTRAINT event_ticket_order_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket_order event_ticket_order_promo_code_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order
    ADD CONSTRAINT event_ticket_order_promo_code_id_fkey FOREIGN KEY (promo_code_id) REFERENCES public.promo_code(id);


--
-- Name: event_ticket event_ticket_order_ref_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket
    ADD CONSTRAINT event_ticket_order_ref_id_fkey FOREIGN KEY (order_ref_id) REFERENCES public.event_ticket_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket_order event_ticket_order_tier_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_order
    ADD CONSTRAINT event_ticket_order_tier_id_fkey FOREIGN KEY (tier_id) REFERENCES public.event_ticket_tier(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket_tier event_ticket_tier_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket_tier
    ADD CONSTRAINT event_ticket_tier_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_ticket event_ticket_tier_ref_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_ticket
    ADD CONSTRAINT event_ticket_tier_ref_id_fkey FOREIGN KEY (tier_ref_id) REFERENCES public.event_ticket_tier(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: event_waitlist event_waitlist_converted_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_waitlist
    ADD CONSTRAINT event_waitlist_converted_order_id_fkey FOREIGN KEY (converted_order_id) REFERENCES public.event_ticket_order(id);


--
-- Name: event_waitlist event_waitlist_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_waitlist
    ADD CONSTRAINT event_waitlist_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id);


--
-- Name: event_waitlist event_waitlist_tier_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.event_waitlist
    ADD CONSTRAINT event_waitlist_tier_id_fkey FOREIGN KEY (tier_id) REFERENCES public.event_ticket_tier(id);


--
-- Name: external_artist_ref external_artist_ref_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_artist_ref
    ADD CONSTRAINT external_artist_ref_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.social_artist_profile(id);


--
-- Name: external_calendar_mapping external_calendar_mapping_resource_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_calendar_mapping
    ADD CONSTRAINT external_calendar_mapping_resource_id_fkey FOREIGN KEY (resource_id) REFERENCES public.resource(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: external_event_ref external_event_ref_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_event_ref
    ADD CONSTRAINT external_event_ref_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id);


--
-- Name: external_venue_ref external_venue_ref_venue_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.external_venue_ref
    ADD CONSTRAINT external_venue_ref_venue_id_fkey FOREIGN KEY (venue_id) REFERENCES public.venue(id);


--
-- Name: fan_club fan_club_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club
    ADD CONSTRAINT fan_club_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id);


--
-- Name: fan_club_candidacy fan_club_candidacy_election_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_candidacy
    ADD CONSTRAINT fan_club_candidacy_election_id_fkey FOREIGN KEY (election_id) REFERENCES public.fan_club_election(id);


--
-- Name: fan_club_candidacy fan_club_candidacy_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_candidacy
    ADD CONSTRAINT fan_club_candidacy_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id);


--
-- Name: fan_club_election fan_club_election_club_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_election
    ADD CONSTRAINT fan_club_election_club_id_fkey FOREIGN KEY (club_id) REFERENCES public.fan_club(id);


--
-- Name: fan_club_event fan_club_event_club_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_event
    ADD CONSTRAINT fan_club_event_club_id_fkey FOREIGN KEY (club_id) REFERENCES public.fan_club(id);


--
-- Name: fan_club_event fan_club_event_created_by_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_event
    ADD CONSTRAINT fan_club_event_created_by_party_id_fkey FOREIGN KEY (created_by_party_id) REFERENCES public.party(id);


--
-- Name: fan_club_member_profile fan_club_member_profile_club_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_member_profile
    ADD CONSTRAINT fan_club_member_profile_club_id_fkey FOREIGN KEY (club_id) REFERENCES public.fan_club(id);


--
-- Name: fan_club_member_profile fan_club_member_profile_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_member_profile
    ADD CONSTRAINT fan_club_member_profile_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: fan_club_memory fan_club_memory_member_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory
    ADD CONSTRAINT fan_club_memory_member_profile_id_fkey FOREIGN KEY (member_profile_id) REFERENCES public.fan_club_member_profile(id);


--
-- Name: fan_club_memory_report fan_club_memory_report_memory_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory_report
    ADD CONSTRAINT fan_club_memory_report_memory_id_fkey FOREIGN KEY (memory_id) REFERENCES public.fan_club_memory(id);


--
-- Name: fan_club_memory_report fan_club_memory_report_reporter_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_memory_report
    ADD CONSTRAINT fan_club_memory_report_reporter_id_fkey FOREIGN KEY (reporter_id) REFERENCES public.party(id);


--
-- Name: fan_club_officer fan_club_officer_club_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_officer
    ADD CONSTRAINT fan_club_officer_club_id_fkey FOREIGN KEY (club_id) REFERENCES public.fan_club(id);


--
-- Name: fan_club_officer fan_club_officer_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_officer
    ADD CONSTRAINT fan_club_officer_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id);


--
-- Name: fan_club_post fan_club_post_club_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_post
    ADD CONSTRAINT fan_club_post_club_id_fkey FOREIGN KEY (club_id) REFERENCES public.fan_club(id);


--
-- Name: fan_club_post fan_club_post_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_post
    ADD CONSTRAINT fan_club_post_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id);


--
-- Name: fan_club_post fan_club_post_parent_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_post
    ADD CONSTRAINT fan_club_post_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES public.fan_club_post(id);


--
-- Name: fan_club_vote fan_club_vote_candidacy_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote
    ADD CONSTRAINT fan_club_vote_candidacy_id_fkey FOREIGN KEY (candidacy_id) REFERENCES public.fan_club_candidacy(id);


--
-- Name: fan_club_vote fan_club_vote_election_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote
    ADD CONSTRAINT fan_club_vote_election_id_fkey FOREIGN KEY (election_id) REFERENCES public.fan_club_election(id);


--
-- Name: fan_club_vote fan_club_vote_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_club_vote
    ADD CONSTRAINT fan_club_vote_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id);


--
-- Name: fan_follow fan_follow_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_follow
    ADD CONSTRAINT fan_follow_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: fan_follow fan_follow_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_follow
    ADD CONSTRAINT fan_follow_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: fan_profile fan_profile_fan_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.fan_profile
    ADD CONSTRAINT fan_profile_fan_party_id_fkey FOREIGN KEY (fan_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: feature_access_request_history feature_access_request_history_actor_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_request_history
    ADD CONSTRAINT feature_access_request_history_actor_party_id_fkey FOREIGN KEY (actor_party_id) REFERENCES public.party(id);


--
-- Name: feature_access_request_history feature_access_request_history_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_request_history
    ADD CONSTRAINT feature_access_request_history_request_id_fkey FOREIGN KEY (request_id) REFERENCES public.feature_access_requests(id);


--
-- Name: feature_access_requests feature_access_requests_requester_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_requests
    ADD CONSTRAINT feature_access_requests_requester_party_id_fkey FOREIGN KEY (requester_party_id) REFERENCES public.party(id);


--
-- Name: feature_access_requests feature_access_requests_reviewer_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_access_requests
    ADD CONSTRAINT feature_access_requests_reviewer_party_id_fkey FOREIGN KEY (reviewer_party_id) REFERENCES public.party(id);


--
-- Name: feature_navigation_preferences feature_navigation_preferences_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.feature_navigation_preferences
    ADD CONSTRAINT feature_navigation_preferences_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON DELETE CASCADE;


--
-- Name: artist_inventory_reference fk_artist_inventory_social_artist; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.artist_inventory_reference
    ADD CONSTRAINT fk_artist_inventory_social_artist FOREIGN KEY (social_artist_id) REFERENCES public.social_artist_profile(id) ON DELETE SET NULL;


--
-- Name: input_list input_list_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list
    ADD CONSTRAINT input_list_session_id_fkey FOREIGN KEY (session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_cable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_cable_id_fkey FOREIGN KEY (cable_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_insert_outboard_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_insert_outboard_id_fkey FOREIGN KEY (insert_outboard_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_mic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_mic_id_fkey FOREIGN KEY (mic_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_preamp_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_preamp_id_fkey FOREIGN KEY (preamp_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_stand_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_stand_id_fkey FOREIGN KEY (stand_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_template_row input_list_template_row_template_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_template_row
    ADD CONSTRAINT input_list_template_row_template_id_fkey FOREIGN KEY (template_id) REFERENCES public.input_list_template(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_list_version input_list_version_input_list_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_list_version
    ADD CONSTRAINT input_list_version_input_list_id_fkey FOREIGN KEY (input_list_id) REFERENCES public.input_list(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_cable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_cable_id_fkey FOREIGN KEY (cable_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_insert_outboard_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_insert_outboard_id_fkey FOREIGN KEY (insert_outboard_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_mic_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_mic_id_fkey FOREIGN KEY (mic_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_preamp_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_preamp_id_fkey FOREIGN KEY (preamp_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_stand_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_stand_id_fkey FOREIGN KEY (stand_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: input_row input_row_version_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.input_row
    ADD CONSTRAINT input_row_version_id_fkey FOREIGN KEY (version_id) REFERENCES public.input_list_version(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: intern_task intern_task_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.intern_task
    ADD CONSTRAINT intern_task_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.intern_project(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: invoice invoice_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice
    ADD CONSTRAINT invoice_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: invoice_line invoice_line_invoice_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice_line
    ADD CONSTRAINT invoice_line_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES public.invoice(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: invoice_line invoice_line_package_purchase_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice_line
    ADD CONSTRAINT invoice_line_package_purchase_id_fkey FOREIGN KEY (package_purchase_id) REFERENCES public.package_purchase(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: invoice_line invoice_line_service_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.invoice_line
    ADD CONSTRAINT invoice_line_service_order_id_fkey FOREIGN KEY (service_order_id) REFERENCES public.service_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: lead_interest lead_interest_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.lead_interest
    ADD CONSTRAINT lead_interest_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: live_session_musician live_session_musician_intake_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_session_musician
    ADD CONSTRAINT live_session_musician_intake_id_fkey FOREIGN KEY (intake_id) REFERENCES public.live_session_intake(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: live_session_song live_session_song_intake_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.live_session_song
    ADD CONSTRAINT live_session_song_intake_id_fkey FOREIGN KEY (intake_id) REFERENCES public.live_session_intake(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: maintenance_attachment maintenance_attachment_ticket_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.maintenance_attachment
    ADD CONSTRAINT maintenance_attachment_ticket_id_fkey FOREIGN KEY (ticket_id) REFERENCES public.maintenance_ticket(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: maintenance_ticket maintenance_ticket_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.maintenance_ticket
    ADD CONSTRAINT maintenance_ticket_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_cart_item marketplace_cart_item_cart_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_cart_item
    ADD CONSTRAINT marketplace_cart_item_cart_id_fkey FOREIGN KEY (cart_id) REFERENCES public.marketplace_cart(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_cart_item marketplace_cart_item_listing_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_cart_item
    ADD CONSTRAINT marketplace_cart_item_listing_id_fkey FOREIGN KEY (listing_id) REFERENCES public.marketplace_listing(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_listing marketplace_listing_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_listing
    ADD CONSTRAINT marketplace_listing_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_order marketplace_order_cart_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_order
    ADD CONSTRAINT marketplace_order_cart_id_fkey FOREIGN KEY (cart_id) REFERENCES public.marketplace_cart(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_order_item marketplace_order_item_listing_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_order_item
    ADD CONSTRAINT marketplace_order_item_listing_id_fkey FOREIGN KEY (listing_id) REFERENCES public.marketplace_listing(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: marketplace_order_item marketplace_order_item_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.marketplace_order_item
    ADD CONSTRAINT marketplace_order_item_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.marketplace_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: notification notification_recipient_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.notification
    ADD CONSTRAINT notification_recipient_party_id_fkey FOREIGN KEY (recipient_party_id) REFERENCES public.party(id);


--
-- Name: operations_admin_audit operations_admin_audit_actor_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit
    ADD CONSTRAINT operations_admin_audit_actor_party_id_fkey FOREIGN KEY (actor_party_id) REFERENCES public.party(id);


--
-- Name: operations_admin_audit operations_admin_audit_approval_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit
    ADD CONSTRAINT operations_admin_audit_approval_request_id_fkey FOREIGN KEY (approval_request_id) REFERENCES public.operations_approval_request(id);


--
-- Name: operations_admin_audit operations_admin_audit_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit
    ADD CONSTRAINT operations_admin_audit_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_admin_audit operations_admin_audit_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_admin_audit
    ADD CONSTRAINT operations_admin_audit_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_aggregate_sequence operations_aggregate_sequence_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_aggregate_sequence
    ADD CONSTRAINT operations_aggregate_sequence_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_approval_request operations_approval_request_approver_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_approver_party_id_fkey FOREIGN KEY (approver_party_id) REFERENCES public.party(id);


--
-- Name: operations_approval_request operations_approval_request_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_approval_request operations_approval_request_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_approval_request operations_approval_request_requester_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_requester_party_id_fkey FOREIGN KEY (requester_party_id) REFERENCES public.party(id);


--
-- Name: operations_approval_request operations_approval_request_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_approval_request
    ADD CONSTRAINT operations_approval_request_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_backfill_run operations_backfill_run_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_backfill_run
    ADD CONSTRAINT operations_backfill_run_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_branch operations_branch_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_branch
    ADD CONSTRAINT operations_branch_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_business_hours operations_business_hours_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_business_hours
    ADD CONSTRAINT operations_business_hours_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_business_hours operations_business_hours_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_business_hours
    ADD CONSTRAINT operations_business_hours_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_domain_event operations_domain_event_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_domain_event
    ADD CONSTRAINT operations_domain_event_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_domain_event operations_domain_event_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_domain_event
    ADD CONSTRAINT operations_domain_event_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_holiday operations_holiday_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_holiday
    ADD CONSTRAINT operations_holiday_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_holiday operations_holiday_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_holiday
    ADD CONSTRAINT operations_holiday_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_inbound_receipt operations_inbound_receipt_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_inbound_receipt
    ADD CONSTRAINT operations_inbound_receipt_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_inbound_receipt operations_inbound_receipt_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_inbound_receipt
    ADD CONSTRAINT operations_inbound_receipt_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: operations_integration_failure operations_integration_failure_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_integration_failure
    ADD CONSTRAINT operations_integration_failure_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_integration_failure operations_integration_failure_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_integration_failure
    ADD CONSTRAINT operations_integration_failure_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_mention operations_mention_mentioned_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_mention
    ADD CONSTRAINT operations_mention_mentioned_party_id_fkey FOREIGN KEY (mentioned_party_id) REFERENCES public.party(id);


--
-- Name: operations_mention operations_mention_note_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_mention
    ADD CONSTRAINT operations_mention_note_id_fkey FOREIGN KEY (note_id) REFERENCES public.operations_note(id);


--
-- Name: operations_note operations_note_author_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_note
    ADD CONSTRAINT operations_note_author_party_id_fkey FOREIGN KEY (author_party_id) REFERENCES public.party(id);


--
-- Name: operations_note operations_note_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_note
    ADD CONSTRAINT operations_note_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_note operations_note_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_note
    ADD CONSTRAINT operations_note_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_outbound_delivery operations_outbound_delivery_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbound_delivery
    ADD CONSTRAINT operations_outbound_delivery_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_outbound_delivery operations_outbound_delivery_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbound_delivery
    ADD CONSTRAINT operations_outbound_delivery_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_outbox operations_outbox_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbox
    ADD CONSTRAINT operations_outbox_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.operations_domain_event(id);


--
-- Name: operations_outbox operations_outbox_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_outbox
    ADD CONSTRAINT operations_outbox_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_provider_config operations_provider_config_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_provider_config
    ADD CONSTRAINT operations_provider_config_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_provider_config operations_provider_config_updated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_provider_config
    ADD CONSTRAINT operations_provider_config_updated_by_fkey FOREIGN KEY (updated_by) REFERENCES public.party(id);


--
-- Name: operations_push_subscription operations_push_subscription_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_push_subscription
    ADD CONSTRAINT operations_push_subscription_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_push_subscription operations_push_subscription_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_push_subscription
    ADD CONSTRAINT operations_push_subscription_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: operations_saved_view operations_saved_view_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_saved_view
    ADD CONSTRAINT operations_saved_view_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_saved_view operations_saved_view_owner_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_saved_view
    ADD CONSTRAINT operations_saved_view_owner_party_id_fkey FOREIGN KEY (owner_party_id) REFERENCES public.party(id);


--
-- Name: operations_scope_member operations_scope_member_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_scope_member
    ADD CONSTRAINT operations_scope_member_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_scope_member operations_scope_member_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_scope_member
    ADD CONSTRAINT operations_scope_member_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_scope_member operations_scope_member_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_scope_member
    ADD CONSTRAINT operations_scope_member_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id);


--
-- Name: operations_sla_reminder operations_sla_reminder_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_reminder
    ADD CONSTRAINT operations_sla_reminder_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_sla_reminder operations_sla_reminder_timer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_reminder
    ADD CONSTRAINT operations_sla_reminder_timer_id_fkey FOREIGN KEY (timer_id) REFERENCES public.operations_sla_timer(id);


--
-- Name: operations_sla_reminder operations_sla_reminder_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_reminder
    ADD CONSTRAINT operations_sla_reminder_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_sla_timer operations_sla_timer_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_timer
    ADD CONSTRAINT operations_sla_timer_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_sla_timer operations_sla_timer_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_sla_timer
    ADD CONSTRAINT operations_sla_timer_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_stream_event operations_stream_event_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event
    ADD CONSTRAINT operations_stream_event_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_stream_event operations_stream_event_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event
    ADD CONSTRAINT operations_stream_event_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_stream_event operations_stream_event_visible_to_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event
    ADD CONSTRAINT operations_stream_event_visible_to_party_id_fkey FOREIGN KEY (visible_to_party_id) REFERENCES public.party(id);


--
-- Name: operations_stream_event operations_stream_event_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_stream_event
    ADD CONSTRAINT operations_stream_event_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_work_item operations_work_item_assignee_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_assignee_party_id_fkey FOREIGN KEY (assignee_party_id) REFERENCES public.party(id);


--
-- Name: operations_work_item operations_work_item_branch_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_branch_id_fkey FOREIGN KEY (branch_id) REFERENCES public.operations_branch(id);


--
-- Name: operations_work_item operations_work_item_customer_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_customer_party_id_fkey FOREIGN KEY (customer_party_id) REFERENCES public.party(id);


--
-- Name: operations_work_item_event operations_work_item_event_actor_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_actor_party_id_fkey FOREIGN KEY (actor_party_id) REFERENCES public.party(id);


--
-- Name: operations_work_item_event operations_work_item_event_domain_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_domain_event_id_fkey FOREIGN KEY (domain_event_id) REFERENCES public.operations_domain_event(id);


--
-- Name: operations_work_item_event operations_work_item_event_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: operations_work_item_event operations_work_item_event_work_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item_event
    ADD CONSTRAINT operations_work_item_event_work_item_id_fkey FOREIGN KEY (work_item_id) REFERENCES public.operations_work_item(id);


--
-- Name: operations_work_item operations_work_item_first_seen_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_first_seen_by_fkey FOREIGN KEY (first_seen_by) REFERENCES public.party(id);


--
-- Name: operations_work_item operations_work_item_organization_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.operations_work_item
    ADD CONSTRAINT operations_work_item_organization_id_fkey FOREIGN KEY (organization_id) REFERENCES public.operations_organization(id);


--
-- Name: package_catalog package_catalog_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_catalog
    ADD CONSTRAINT package_catalog_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: package_ledger package_ledger_booking_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_ledger
    ADD CONSTRAINT package_ledger_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES public.booking(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: package_ledger package_ledger_purchase_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_ledger
    ADD CONSTRAINT package_ledger_purchase_id_fkey FOREIGN KEY (purchase_id) REFERENCES public.package_purchase(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: package_purchase package_purchase_buyer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_purchase
    ADD CONSTRAINT package_purchase_buyer_id_fkey FOREIGN KEY (buyer_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: package_purchase package_purchase_product_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.package_purchase
    ADD CONSTRAINT package_purchase_product_id_fkey FOREIGN KEY (product_id) REFERENCES public.package_product(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: party_follow party_follow_follower_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_follow
    ADD CONSTRAINT party_follow_follower_party_id_fkey FOREIGN KEY (follower_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: party_follow party_follow_following_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_follow
    ADD CONSTRAINT party_follow_following_party_id_fkey FOREIGN KEY (following_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: party_radio_presence party_radio_presence_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_radio_presence
    ADD CONSTRAINT party_radio_presence_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: party_role party_role_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.party_role
    ADD CONSTRAINT party_role_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment payment_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment payment_invoice_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES public.invoice(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment payment_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.service_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment payment_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment_split payment_split_payer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_split
    ADD CONSTRAINT payment_split_payer_id_fkey FOREIGN KEY (payer_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: payment_split payment_split_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.payment_split
    ADD CONSTRAINT payment_split_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: promo_code promo_code_event_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code
    ADD CONSTRAINT promo_code_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.social_event(id);


--
-- Name: promo_code_redemption promo_code_redemption_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code_redemption
    ADD CONSTRAINT promo_code_redemption_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);


--
-- Name: promo_code_redemption promo_code_redemption_promo_code_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.promo_code_redemption
    ADD CONSTRAINT promo_code_redemption_promo_code_id_fkey FOREIGN KEY (promo_code_id) REFERENCES public.promo_code(id);


--
-- Name: proposal proposal_pipeline_card_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.proposal
    ADD CONSTRAINT proposal_pipeline_card_id_fkey FOREIGN KEY (pipeline_card_id) REFERENCES public.pipeline_card(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: proposal_version proposal_version_proposal_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.proposal_version
    ADD CONSTRAINT proposal_version_proposal_id_fkey FOREIGN KEY (proposal_id) REFERENCES public.proposal(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: receipt receipt_buyer_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt
    ADD CONSTRAINT receipt_buyer_party_id_fkey FOREIGN KEY (buyer_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: receipt receipt_invoice_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt
    ADD CONSTRAINT receipt_invoice_id_fkey FOREIGN KEY (invoice_id) REFERENCES public.invoice(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: receipt_line receipt_line_receipt_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.receipt_line
    ADD CONSTRAINT receipt_line_receipt_id_fkey FOREIGN KEY (receipt_id) REFERENCES public.receipt(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: referral_claim referral_claim_claimant_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_claim
    ADD CONSTRAINT referral_claim_claimant_user_id_fkey FOREIGN KEY (claimant_user_id) REFERENCES public.academy_user(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: referral_claim referral_claim_code_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_claim
    ADD CONSTRAINT referral_claim_code_id_fkey FOREIGN KEY (code_id) REFERENCES public.referral_code(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: referral_code referral_code_owner_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.referral_code
    ADD CONSTRAINT referral_code_owner_user_id_fkey FOREIGN KEY (owner_user_id) REFERENCES public.academy_user(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: room_default_gear room_default_gear_asset_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_default_gear
    ADD CONSTRAINT room_default_gear_asset_id_fkey FOREIGN KEY (asset_id) REFERENCES public.asset(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: room_default_gear room_default_gear_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_default_gear
    ADD CONSTRAINT room_default_gear_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: room_feature room_feature_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.room_feature
    ADD CONSTRAINT room_feature_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_ad service_ad_provider_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad
    ADD CONSTRAINT service_ad_provider_party_id_fkey FOREIGN KEY (provider_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_ad service_ad_service_catalog_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad
    ADD CONSTRAINT service_ad_service_catalog_id_fkey FOREIGN KEY (service_catalog_id) REFERENCES public.service_catalog(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_ad_slot service_ad_slot_ad_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_ad_slot
    ADD CONSTRAINT service_ad_slot_ad_id_fkey FOREIGN KEY (ad_id) REFERENCES public.service_ad(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_ad_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_ad_id_fkey FOREIGN KEY (ad_id) REFERENCES public.service_ad(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_booking_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_booking_id_fkey FOREIGN KEY (booking_id) REFERENCES public.booking(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_held_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_held_payment_id_fkey FOREIGN KEY (held_payment_id) REFERENCES public.payment(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_patron_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_patron_party_id_fkey FOREIGN KEY (patron_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_provider_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_provider_party_id_fkey FOREIGN KEY (provider_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_released_payment_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_released_payment_id_fkey FOREIGN KEY (released_payment_id) REFERENCES public.payment(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_escrow service_escrow_service_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_escrow
    ADD CONSTRAINT service_escrow_service_order_id_fkey FOREIGN KEY (service_order_id) REFERENCES public.service_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_order service_order_artist_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_order
    ADD CONSTRAINT service_order_artist_id_fkey FOREIGN KEY (artist_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_order service_order_catalog_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_order
    ADD CONSTRAINT service_order_catalog_id_fkey FOREIGN KEY (catalog_id) REFERENCES public.service_catalog(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_order service_order_customer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_order
    ADD CONSTRAINT service_order_customer_id_fkey FOREIGN KEY (customer_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_status_change service_status_change_changed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_status_change
    ADD CONSTRAINT service_status_change_changed_by_fkey FOREIGN KEY (changed_by) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: service_status_change service_status_change_service_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.service_status_change
    ADD CONSTRAINT service_status_change_service_order_id_fkey FOREIGN KEY (service_order_id) REFERENCES public.service_order(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: session session_band_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session
    ADD CONSTRAINT session_band_id_fkey FOREIGN KEY (band_id) REFERENCES public.band(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: session_deliverable session_deliverable_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_deliverable
    ADD CONSTRAINT session_deliverable_session_id_fkey FOREIGN KEY (session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: session_invoice session_invoice_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_invoice
    ADD CONSTRAINT session_invoice_session_id_fkey FOREIGN KEY (session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: session_room session_room_room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_room
    ADD CONSTRAINT session_room_room_id_fkey FOREIGN KEY (room_id) REFERENCES public.room(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: session_room session_room_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.session_room
    ADD CONSTRAINT session_room_session_id_fkey FOREIGN KEY (session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_discovery_review social_discovery_review_reviewed_by_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_discovery_review
    ADD CONSTRAINT social_discovery_review_reviewed_by_party_id_fkey FOREIGN KEY (reviewed_by_party_id) REFERENCES public.party(id) ON DELETE SET NULL;


--
-- Name: social_discovery_review social_discovery_review_social_sync_post_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_discovery_review
    ADD CONSTRAINT social_discovery_review_social_sync_post_id_fkey FOREIGN KEY (social_sync_post_id) REFERENCES public.social_sync_post(id) ON DELETE CASCADE;


--
-- Name: social_event social_event_venue_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_event
    ADD CONSTRAINT social_event_venue_id_fkey FOREIGN KEY (venue_id) REFERENCES public.venue(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_sync_account social_sync_account_artist_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_account
    ADD CONSTRAINT social_sync_account_artist_profile_id_fkey FOREIGN KEY (artist_profile_id) REFERENCES public.artist_profile(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_sync_account social_sync_account_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_account
    ADD CONSTRAINT social_sync_account_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_sync_post social_sync_post_account_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post
    ADD CONSTRAINT social_sync_post_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.social_sync_account(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_sync_post social_sync_post_artist_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post
    ADD CONSTRAINT social_sync_post_artist_party_id_fkey FOREIGN KEY (artist_party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: social_sync_post social_sync_post_artist_profile_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.social_sync_post
    ADD CONSTRAINT social_sync_post_artist_profile_id_fkey FOREIGN KEY (artist_profile_id) REFERENCES public.artist_profile(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: stock_movement stock_movement_ref_checkout_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_movement
    ADD CONSTRAINT stock_movement_ref_checkout_id_fkey FOREIGN KEY (ref_checkout_id) REFERENCES public.asset_checkout(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: stock_movement stock_movement_ref_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_movement
    ADD CONSTRAINT stock_movement_ref_session_id_fkey FOREIGN KEY (ref_session_id) REFERENCES public.session(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: stock_movement stock_movement_stock_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stock_movement
    ADD CONSTRAINT stock_movement_stock_item_id_fkey FOREIGN KEY (stock_item_id) REFERENCES public.stock_item(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: stripe_payment_intent stripe_payment_intent_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.stripe_payment_intent
    ADD CONSTRAINT stripe_payment_intent_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);


--
-- Name: subject_room_preference subject_room_preference_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.subject_room_preference
    ADD CONSTRAINT subject_room_preference_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: teacher_availability teacher_availability_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_availability
    ADD CONSTRAINT teacher_availability_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: teacher_subject teacher_subject_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.teacher_subject
    ADD CONSTRAINT teacher_subject_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: ticket_qr_code ticket_qr_code_ticket_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_qr_code
    ADD CONSTRAINT ticket_qr_code_ticket_id_fkey FOREIGN KEY (ticket_id) REFERENCES public.event_ticket(id);


--
-- Name: ticket_refund_request ticket_refund_request_order_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_refund_request
    ADD CONSTRAINT ticket_refund_request_order_id_fkey FOREIGN KEY (order_id) REFERENCES public.event_ticket_order(id);


--
-- Name: ticket_transfer ticket_transfer_ticket_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.ticket_transfer
    ADD CONSTRAINT ticket_transfer_ticket_id_fkey FOREIGN KEY (ticket_id) REFERENCES public.event_ticket(id);


--
-- Name: trial_assignment trial_assignment_request_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_assignment
    ADD CONSTRAINT trial_assignment_request_id_fkey FOREIGN KEY (request_id) REFERENCES public.trial_request(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: trial_request trial_request_subject_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.trial_request
    ADD CONSTRAINT trial_request_subject_id_fkey FOREIGN KEY (subject_id) REFERENCES public.subject(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: user_credential user_credential_party_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_credential
    ADD CONSTRAINT user_credential_party_id_fkey FOREIGN KEY (party_id) REFERENCES public.party(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- Name: user_locale_preferences user_locale_preferences_currency_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_locale_preferences
    ADD CONSTRAINT user_locale_preferences_currency_fkey FOREIGN KEY (currency) REFERENCES public.supported_currencies(currency_code);


--
-- Name: user_locale_preferences user_locale_preferences_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.user_locale_preferences
    ADD CONSTRAINT user_locale_preferences_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.party(id) ON DELETE CASCADE;


--
-- Name: whats_app_message whats_app_message_resend_of_message_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.whats_app_message
    ADD CONSTRAINT whats_app_message_resend_of_message_id_fkey FOREIGN KEY (resend_of_message_id) REFERENCES public.whats_app_message(id) ON UPDATE RESTRICT ON DELETE RESTRICT;


--
-- PostgreSQL database dump complete
--
