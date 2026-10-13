BEGIN;
-- Extend the existing validator without dropping independently added trigger codes.
DO $$ DECLARE definition text; BEGIN
  SELECT pg_get_functiondef('security_validate_assignment_policy()'::regprocedure) INTO definition;
  IF position('''apple-account-create''' IN definition)=0 THEN
    IF position('''google-account-create''' IN definition)=0 THEN
      RAISE EXCEPTION 'Unrecognized automatic assignment policy validator';
    END IF;
    EXECUTE replace(definition, '''google-account-create''', '''google-account-create'',''apple-account-create''');
  END IF;
END $$;

INSERT INTO security_role_assignment_policy
  (id, code, trigger_code, role_id, name_es, name_en, requires_verified_email, active, version)
SELECT '00000000-0000-4000-8000-000000000314'::uuid,
  'account.apple.customer', 'apple-account-create', role.id,
  'Cliente con cuenta Apple', 'Customer on Apple account creation', true, true, 1
FROM security_role role
WHERE role.code = 'customer' AND role.active AND role.automatic_assignable
  AND NOT role.emergency_administrator
ON CONFLICT (code) DO UPDATE SET active=true, updated_at=now(), version=security_role_assignment_policy.version+1
WHERE NOT security_role_assignment_policy.active
  AND security_role_assignment_policy.role_id=EXCLUDED.role_id
  AND security_role_assignment_policy.trigger_code=EXCLUDED.trigger_code
  AND security_role_assignment_policy.requires_verified_email;
DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM security_role_assignment_policy policy JOIN security_role role ON role.id=policy.role_id
    WHERE policy.code='account.apple.customer' AND policy.active
      AND policy.trigger_code='apple-account-create' AND policy.requires_verified_email
      AND role.code='customer' AND role.active AND role.automatic_assignable AND NOT role.emergency_administrator
  ) THEN RAISE EXCEPTION 'Apple account policy is missing or incompatible'; END IF;
END $$;
COMMIT;
