BEGIN;
-- Extend the existing validator without dropping independently added trigger codes.
DO $$ DECLARE definition text; BEGIN
  SELECT pg_get_functiondef('security_validate_assignment_policy()'::regprocedure) INTO definition;
  IF position('''artist-self-service-activated''' IN definition)=0 THEN
    IF position('''artist-profile-created''' IN definition)=0 THEN
      RAISE EXCEPTION 'Unrecognized automatic assignment policy validator';
    END IF;
    EXECUTE replace(definition, '''artist-profile-created''', '''artist-profile-created'',''artist-self-service-activated''');
  END IF;
END $$;

INSERT INTO security_role_assignment_policy
  (id, code, trigger_code, role_id, name_es, name_en, requires_verified_email, active, version)
SELECT '00000000-0000-4000-8000-000000000313'::uuid,
  'artist.self-service.artist', 'artist-self-service-activated', role.id,
  'Artista por creación de perfil propio', 'Artist on own profile creation', false, true, 1
FROM security_role role
WHERE role.code = 'artist' AND role.active AND role.automatic_assignable
  AND NOT role.emergency_administrator
ON CONFLICT (code) DO UPDATE SET active=true, updated_at=now(), version=security_role_assignment_policy.version+1
WHERE NOT security_role_assignment_policy.active
  AND security_role_assignment_policy.role_id=EXCLUDED.role_id
  AND security_role_assignment_policy.trigger_code=EXCLUDED.trigger_code
  AND NOT security_role_assignment_policy.requires_verified_email;
DO $$ BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM security_role_assignment_policy policy JOIN security_role role ON role.id=policy.role_id
    WHERE policy.code='artist.self-service.artist' AND policy.active
      AND policy.trigger_code='artist-self-service-activated' AND NOT policy.requires_verified_email
      AND role.code='artist' AND role.active AND role.automatic_assignable AND NOT role.emergency_administrator
  ) THEN RAISE EXCEPTION 'Artist self-service policy is missing or incompatible'; END IF;
END $$;
COMMIT;
