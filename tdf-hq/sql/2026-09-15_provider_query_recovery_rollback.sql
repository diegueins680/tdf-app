-- Roll back only an unused installation. Disable the worker and callback
-- query consumers before rollback; older code has no distributed query budget.
\set ON_ERROR_STOP on
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE commerce_provider_query_job, commerce_provider_query_budget IN ACCESS EXCLUSIVE MODE;
DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM commerce_provider_query_job)
     OR EXISTS (SELECT 1 FROM commerce_provider_query_budget) THEN
    RAISE EXCEPTION 'Provider query recovery history exists; use a forward repair';
  END IF;
END $$;
DROP TABLE commerce_provider_query_job;
DROP FUNCTION commerce_protect_provider_query_job();
DROP TABLE commerce_provider_query_budget;
DELETE FROM revenue_feature_flag
 WHERE flag_key = 'checkout.provider_query_recovery' AND enabled = FALSE
   AND environment IN ('sandbox','production')
   AND reason = 'Requires qualified account, shared query budget, lease fencing and sandbox verification';
COMMIT;
