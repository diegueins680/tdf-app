-- Non-destructive rollback: retain every consent, block, preference and command.
-- Do NOT route these users to legacy endpoints that ignore their new restrictions.
BEGIN;
UPDATE social_v2_runtime SET enabled=false WHERE singleton;
COMMIT;
