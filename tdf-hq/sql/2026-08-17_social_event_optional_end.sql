BEGIN;

ALTER TABLE social_event
    ALTER COLUMN end_time DROP NOT NULL;

DO $migration$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conrelid = 'public.social_event'::regclass
          AND conname = 'social_event_time_order'
          AND contype = 'c'
    ) THEN
        ALTER TABLE social_event
            ADD CONSTRAINT social_event_time_order
            CHECK (end_time IS NULL OR start_time < end_time)
            NOT VALID;
    END IF;
END
$migration$;

ALTER TABLE social_event
    VALIDATE CONSTRAINT social_event_time_order;

COMMIT;
