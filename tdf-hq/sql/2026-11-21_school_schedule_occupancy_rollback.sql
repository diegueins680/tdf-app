-- Rollback of 2026-11-21_school_schedule_occupancy.sql. Removes the projection
-- only; class_session, trial_assignment and allocations are untouched. After it,
-- lessons are again protected by handler checks alone.
BEGIN;
DROP TRIGGER IF EXISTS trg_school_schedule_class_session_write ON class_session;
DROP TRIGGER IF EXISTS trg_school_schedule_class_session_delete ON class_session;
DROP TRIGGER IF EXISTS trg_school_schedule_trial_assignment_write ON trial_assignment;
DROP TRIGGER IF EXISTS trg_school_schedule_trial_assignment_delete ON trial_assignment;
DROP TRIGGER IF EXISTS trg_school_schedule_resource_allocation_write
  ON service_booking_resource_allocation;
DROP TRIGGER IF EXISTS trg_school_schedule_resource_allocation_delete
  ON service_booking_resource_allocation;
DROP FUNCTION IF EXISTS school_schedule_project_class_session();
DROP FUNCTION IF EXISTS school_schedule_project_trial_assignment();
DROP FUNCTION IF EXISTS school_schedule_project_resource_allocation();
DROP TABLE IF EXISTS school_schedule_occupancy;
COMMIT;
