-- Helper script: migrate trial_lesson -> lesson (if you created a separate table previously)
-- Adjust schema/table/column names if they differ.
BEGIN;
INSERT INTO lesson (subject_id, teacher_id, student_id, kind, status, requested_at, scheduled_at, duration_min, room_id, notes)
SELECT subject_id, teacher_id, student_id, 'Trial', status, requested_at, scheduled_at, COALESCE(duration_min, 60), room_id, notes
FROM trial_lesson;
DROP TABLE IF EXISTS trial_lesson;
COMMIT;
