-- Canonical DDEX writers retain legacy columns as historical evidence, but
-- write active severity_id/layer_id references. The existing integrity trigger
-- requires legacy values to be NULL; the original core-era NOT NULL constraints
-- make every canonical issue insertion impossible. Keep the trigger and values.
BEGIN;
ALTER TABLE public.ddex_validation_issue
  ALTER COLUMN severity DROP NOT NULL,
  ALTER COLUMN layer DROP NOT NULL;
COMMIT;
