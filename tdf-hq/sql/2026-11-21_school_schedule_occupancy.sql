-- SCHOOL-SLOT-SUGGESTION-001: one calendar for teachers, students and resources.
--
-- Lessons (class_session) and trial lessons (trial_assignment) had no overlap
-- constraint; only studio resource allocations did. This table receives one row
-- per occupied party or resource and a single exclusion constraint rejects every
-- overlap, including a lesson against a studio booking of the same room.
--
-- Additive. Rows that end in the past are not projected by the backfill, as in
-- 2026-10-05_booking_calendar_projection.sql. A pre-existing future overlap or a
-- non-positive interval aborts this migration; it never picks a winner. Writers
-- must be drained before it runs.
BEGIN;
CREATE EXTENSION IF NOT EXISTS btree_gist;
LOCK TABLE class_session, trial_assignment, service_booking_resource_allocation
  IN SHARE ROW EXCLUSIVE MODE;

CREATE TABLE IF NOT EXISTS school_schedule_occupancy (
  id BIGSERIAL PRIMARY KEY,
  source_kind TEXT NOT NULL CHECK (source_kind IN (
    'class_session','trial_assignment','resource_allocation'
  )),
  source_id TEXT NOT NULL,
  subject_kind TEXT NOT NULL CHECK (subject_kind IN ('teacher','student','resource')),
  subject_id BIGINT NOT NULL,
  starts_at TIMESTAMPTZ NOT NULL,
  ends_at TIMESTAMPTZ NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT school_schedule_occupancy_source_key
    UNIQUE (source_kind, source_id, subject_kind),
  CONSTRAINT school_schedule_occupancy_interval CHECK (ends_at > starts_at),
  CONSTRAINT school_schedule_occupancy_no_overlap EXCLUDE USING gist (
    subject_kind WITH =,
    subject_id WITH =,
    tstzrange(starts_at, ends_at, '[)') WITH &&
  )
);

CREATE INDEX IF NOT EXISTS idx_school_schedule_occupancy_window
  ON school_schedule_occupancy(subject_kind, subject_id, starts_at, ends_at);

-- A lesson occupies its teacher, its student and its room.
CREATE OR REPLACE FUNCTION school_schedule_project_class_session()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    DELETE FROM school_schedule_occupancy
      WHERE source_kind = 'class_session' AND source_id = OLD.id::text;
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  INSERT INTO school_schedule_occupancy
    (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
  VALUES
    ('class_session', NEW.id::text, 'teacher', NEW.teacher_id, NEW.start_at, NEW.end_at),
    ('class_session', NEW.id::text, 'student', NEW.student_id, NEW.start_at, NEW.end_at),
    ('class_session', NEW.id::text, 'resource', NEW.room_id, NEW.start_at, NEW.end_at);
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_school_schedule_class_session_write ON class_session;
CREATE TRIGGER trg_school_schedule_class_session_write
  AFTER INSERT OR UPDATE OF teacher_id, student_id, room_id, start_at, end_at ON class_session
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_class_session();
DROP TRIGGER IF EXISTS trg_school_schedule_class_session_delete ON class_session;
CREATE TRIGGER trg_school_schedule_class_session_delete
  AFTER DELETE ON class_session
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_class_session();

-- A trial lesson occupies its teacher and its room. Its student is not projected:
-- anonymous public requests share one fallback party.
CREATE OR REPLACE FUNCTION school_schedule_project_trial_assignment()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    DELETE FROM school_schedule_occupancy
      WHERE source_kind = 'trial_assignment' AND source_id = OLD.id::text;
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  INSERT INTO school_schedule_occupancy
    (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
  VALUES
    ('trial_assignment', NEW.id::text, 'teacher', NEW.teacher_id, NEW.start_at, NEW.end_at),
    ('trial_assignment', NEW.id::text, 'resource', NEW.room_id, NEW.start_at, NEW.end_at);
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_school_schedule_trial_assignment_write ON trial_assignment;
CREATE TRIGGER trg_school_schedule_trial_assignment_write
  AFTER INSERT OR UPDATE OF teacher_id, room_id, start_at, end_at ON trial_assignment
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_trial_assignment();
DROP TRIGGER IF EXISTS trg_school_schedule_trial_assignment_delete ON trial_assignment;
CREATE TRIGGER trg_school_schedule_trial_assignment_delete
  AFTER DELETE ON trial_assignment
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_trial_assignment();

-- An active studio allocation occupies its resource; any other status frees it.
CREATE OR REPLACE FUNCTION school_schedule_project_resource_allocation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  IF TG_OP <> 'INSERT' THEN
    DELETE FROM school_schedule_occupancy
      WHERE source_kind = 'resource_allocation' AND source_id = OLD.id::text;
  END IF;
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  END IF;
  IF NEW.allocation_status IN ('holding','reserved') THEN
    INSERT INTO school_schedule_occupancy
      (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
    VALUES
      ('resource_allocation', NEW.id::text, 'resource', NEW.resource_id,
       NEW.starts_at, NEW.ends_at);
  END IF;
  RETURN NEW;
END $$;

DROP TRIGGER IF EXISTS trg_school_schedule_resource_allocation_write
  ON service_booking_resource_allocation;
CREATE TRIGGER trg_school_schedule_resource_allocation_write
  AFTER INSERT OR UPDATE OF resource_id, starts_at, ends_at, allocation_status
  ON service_booking_resource_allocation
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_resource_allocation();
DROP TRIGGER IF EXISTS trg_school_schedule_resource_allocation_delete
  ON service_booking_resource_allocation;
CREATE TRIGGER trg_school_schedule_resource_allocation_delete
  AFTER DELETE ON service_booking_resource_allocation
  FOR EACH ROW EXECUTE FUNCTION school_schedule_project_resource_allocation();

-- Backfill what is still ahead. ON CONFLICT covers a repeated run only; an
-- overlap raises the exclusion violation and aborts.
INSERT INTO school_schedule_occupancy
  (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
SELECT 'class_session', c.id::text, occupied.kind, occupied.subject_id, c.start_at, c.end_at
FROM class_session c
CROSS JOIN LATERAL (VALUES
  ('teacher', c.teacher_id), ('student', c.student_id), ('resource', c.room_id)
) AS occupied(kind, subject_id)
WHERE c.end_at > NOW()
ON CONFLICT ON CONSTRAINT school_schedule_occupancy_source_key DO NOTHING;

INSERT INTO school_schedule_occupancy
  (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
SELECT 'trial_assignment', t.id::text, occupied.kind, occupied.subject_id, t.start_at, t.end_at
FROM trial_assignment t
CROSS JOIN LATERAL (VALUES
  ('teacher', t.teacher_id), ('resource', t.room_id)
) AS occupied(kind, subject_id)
WHERE t.end_at > NOW()
ON CONFLICT ON CONSTRAINT school_schedule_occupancy_source_key DO NOTHING;

INSERT INTO school_schedule_occupancy
  (source_kind, source_id, subject_kind, subject_id, starts_at, ends_at)
SELECT 'resource_allocation', a.id::text, 'resource', a.resource_id, a.starts_at, a.ends_at
FROM service_booking_resource_allocation a
WHERE a.allocation_status IN ('holding','reserved')
ON CONFLICT ON CONSTRAINT school_schedule_occupancy_source_key DO NOTHING;
COMMIT;
