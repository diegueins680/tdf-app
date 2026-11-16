-- Never discard Apple bindings or leave them outside the restored allowlist.
BEGIN;
SET LOCAL lock_timeout = '10s';
LOCK TABLE public.auth_provider_identity IN ACCESS EXCLUSIVE MODE;
DO $$ DECLARE current_definition text; validated boolean; BEGIN
  SELECT pg_get_constraintdef(oid), convalidated INTO current_definition, validated
  FROM pg_constraint WHERE conrelid='public.auth_provider_identity'::regclass
    AND conname='auth_provider_identity_issuer_check' AND contype='c';
  IF validated IS DISTINCT FROM true OR current_definition NOT IN (
    'CHECK ((issuer = ''https://accounts.google.com''::text))',
    'CHECK ((issuer = ANY (ARRAY[''https://accounts.google.com''::text, ''https://appleid.apple.com''::text])))'
  ) THEN RAISE EXCEPTION 'Unrecognized provider issuer constraint'; END IF;
  IF EXISTS (SELECT 1 FROM public.auth_provider_identity WHERE issuer='https://appleid.apple.com') THEN
    RAISE EXCEPTION 'Apple bindings exist; issuer rollback requires an explicit data-preservation plan';
  END IF;
END $$;
ALTER TABLE public.auth_provider_identity DROP CONSTRAINT auth_provider_identity_issuer_check;
ALTER TABLE public.auth_provider_identity ADD CONSTRAINT auth_provider_identity_issuer_check
  CHECK (issuer='https://accounts.google.com');
COMMIT;
