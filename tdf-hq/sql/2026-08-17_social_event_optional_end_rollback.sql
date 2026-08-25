BEGIN;

DO $rollback$
BEGIN
    IF EXISTS (SELECT 1 FROM social_event WHERE end_time IS NULL) THEN
        RAISE EXCEPTION
            'Cannot restore social_event.end_time NOT NULL while events without a confirmed end exist';
    END IF;
END
$rollback$;

ALTER TABLE social_event
    DROP CONSTRAINT IF EXISTS social_event_time_order;

ALTER TABLE social_event
    ALTER COLUMN end_time SET NOT NULL;

COMMIT;
