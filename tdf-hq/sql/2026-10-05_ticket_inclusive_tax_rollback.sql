-- Reinterpreting issued tax-inclusive prices as additive could overcharge or
-- misstate liabilities. Retain the additive columns when rolling back binaries.
DO $$ BEGIN
 RAISE EXCEPTION 'Inclusive ticket tax is forward-only; do not remove purchased tax mode or rewrite financial history';
END $$;
