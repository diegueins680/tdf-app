-- Set SOCIAL_DISCOVER_AFFINITY_ENABLED=false and restart before running this.
BEGIN;

DROP FUNCTION IF EXISTS social_v2_discover(bigint,integer,boolean);
DROP INDEX IF EXISTS social_v2_reply_author;
DROP INDEX IF EXISTS social_v2_post_reaction_reactor;

COMMIT;
