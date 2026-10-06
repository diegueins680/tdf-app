-- Only unused schema can be removed. Never discard an established auth binding.
DO $$ BEGIN
  IF to_regclass('public.auth_provider_identity') IS NOT NULL THEN
    IF EXISTS (SELECT 1 FROM auth_provider_identity) THEN
      RAISE EXCEPTION 'Provider bindings exist; account-specific security review required';
    END IF;
    DROP TABLE auth_provider_identity;
  END IF;
END $$;
