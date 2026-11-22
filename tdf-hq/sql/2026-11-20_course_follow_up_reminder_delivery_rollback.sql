-- Roll back 2026-11-20_course_follow_up_reminder_delivery.sql.
-- Dropping the claim table only forgets which reminders were sent; due
-- reminders from the last two days would be sent again after a forward replay.
BEGIN;
SET LOCAL lock_timeout = '10s';
DROP TABLE IF EXISTS public.course_follow_up_reminder_delivery;
COMMIT;
