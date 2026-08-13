\set ON_ERROR_STOP on

-- Run after the normal backend migrations and catalog seed have completed.
-- Every fixture and mutation is contained in this transaction and rolled back.
BEGIN;

DO $security_registry_test$
DECLARE
  actor_one bigint;
  actor_two bigint;
  target_party bigint;
  admin_role uuid;
  manager_role uuid;
  engineer_role uuid;
  published_state uuid;
  source_revision uuid;
  emergency_assignment record;
  remaining_emergency_party bigint;
  protected_failures integer := 0;
BEGIN
  SELECT id INTO STRICT admin_role FROM security_role WHERE code = 'admin' AND active;
  SELECT id INTO STRICT manager_role FROM security_role WHERE code = 'manager' AND active;
  SELECT id INTO STRICT engineer_role FROM security_role WHERE code = 'engineer' AND active;
  SELECT state.id INTO STRICT published_state
  FROM workflow_state state
  JOIN workflow_definition workflow ON workflow.id = state.workflow_id
  WHERE workflow.code = 'sensitive-publication'
    AND state.code = 'published'
    AND workflow.active
    AND state.active;

  IF (
    SELECT array_agg(permission.code::text ORDER BY permission.code)
    FROM security_permission permission
    JOIN security_module module_row ON module_row.id = permission.module_id
    JOIN security_action action ON action.id = permission.action_id
    WHERE permission.active AND permission.code LIKE 'pipeline.%'
      AND module_row.code = 'scheduling'
      AND permission.resource_scope = 'pipeline'
      AND action.code = split_part(permission.code, '.', 2)
  ) IS DISTINCT FROM ARRAY[
    'pipeline.create', 'pipeline.delete', 'pipeline.read', 'pipeline.update'
  ]::text[] THEN
    RAISE EXCEPTION 'pipeline capability registry is incomplete or misbound';
  END IF;

  IF EXISTS (
    WITH expected(role_code, permission_code) AS (
      VALUES
        ('admin', 'pipeline.read'), ('admin', 'pipeline.create'),
        ('admin', 'pipeline.update'), ('admin', 'pipeline.delete'),
        ('manager', 'pipeline.read'), ('manager', 'pipeline.create'),
        ('manager', 'pipeline.update'), ('manager', 'pipeline.delete'),
        ('studio-manager', 'pipeline.read'), ('studio-manager', 'pipeline.create'),
        ('studio-manager', 'pipeline.update'), ('studio-manager', 'pipeline.delete'),
        ('reception', 'pipeline.read'), ('reception', 'pipeline.create'),
        ('reception', 'pipeline.update'),
        ('engineer', 'pipeline.read'), ('engineer', 'pipeline.update'),
        ('teacher', 'pipeline.read'), ('teacher', 'pipeline.update'),
        ('live-sessions-producer', 'pipeline.read'),
        ('live-sessions-producer', 'pipeline.create'),
        ('live-sessions-producer', 'pipeline.update'),
        ('producer', 'pipeline.read'), ('producer', 'pipeline.create'),
        ('producer', 'pipeline.update'), ('a-and-r', 'pipeline.read')
    ), actual AS (
      SELECT role.code AS role_code, permission.code AS permission_code
      FROM role_permission grant_row
      JOIN security_role role ON role.id = grant_row.role_id AND role.active
      JOIN security_permission permission
        ON permission.id = grant_row.permission_id AND permission.active
      WHERE grant_row.active AND permission.code LIKE 'pipeline.%'
    )
    (SELECT role_code, permission_code FROM expected
     EXCEPT
     SELECT role_code, permission_code FROM actual)
    UNION ALL
    (SELECT role_code, permission_code FROM actual
     EXCEPT
     SELECT role_code, permission_code FROM expected)
  ) THEN
    RAISE EXCEPTION 'pipeline role capability matrix differs from the reviewed bootstrap';
  END IF;

  INSERT INTO party (display_name, is_org, created_at)
  VALUES ('Catalog security integration author', FALSE, now())
  RETURNING id INTO actor_one;

  INSERT INTO party (display_name, is_org, created_at)
  VALUES ('Catalog security integration approver', FALSE, now())
  RETURNING id INTO actor_two;

  INSERT INTO party (display_name, is_org, created_at)
  VALUES ('Catalog security integration target', FALSE, now())
  RETURNING id INTO target_party;

  INSERT INTO user_credential (party_id, username, password_hash, active)
  VALUES
    (actor_one, 'catalog-security-author-' || pg_backend_pid(), 'integration-test-only', TRUE),
    (actor_two, 'catalog-security-approver-' || pg_backend_pid(), 'integration-test-only', TRUE);

  INSERT INTO party_security_role
    (party_id, role_id, approval_mode, active, created_at, version)
  VALUES
    (actor_one, admin_role, 'bootstrap', TRUE, now(), 1),
    (actor_two, admin_role, 'bootstrap', TRUE, now(), 1);

  BEGIN
    INSERT INTO party_security_role
      (party_id, role_id, granted_by, approved_by, approval_mode, active, created_at, version)
    VALUES
      (target_party, engineer_role, actor_one, actor_two, 'normal', TRUE, now(), 1);
    RAISE EXCEPTION 'grant without a source revision was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%require an approved source revision%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    INSERT INTO security_grant_revision
      (change_kind, party_id, role_id, desired_active, expected_version,
       workflow_state_id, created_by, submitted_at, reviewed_by, reviewed_at,
       approved_by, approved_at, approval_mode, source_platform, correlation_id,
       reason, result, version)
    VALUES
      ('party-role', target_party, manager_role, TRUE, 0,
       published_state, actor_one, now(), actor_one, now(), actor_one, now(),
       'normal', 'postgres-integration-test',
       'security-self-approval-' || pg_backend_pid(),
       'Normal self approval must be rejected', 'published', 1);
    RAISE EXCEPTION 'normal security self approval was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%documented emergency override%' THEN
        RAISE;
      END IF;
  END;

  INSERT INTO security_grant_revision
    (change_kind, party_id, role_id, desired_active, expected_version,
     workflow_state_id, created_by, submitted_at, reviewed_by, reviewed_at,
     approved_by, approved_at, reviewer_notes, approval_mode, source_platform,
     correlation_id, reason, result, version)
  VALUES
    ('party-role', target_party, manager_role, TRUE, 0,
     published_state, actor_one, now(), actor_two, now(), actor_two, now(),
     'Independent security review', 'normal', 'postgres-integration-test',
     'security-two-person-approval-' || pg_backend_pid(),
     'Two-person security approval integration test', 'published', 1)
  RETURNING id INTO source_revision;

  INSERT INTO party_security_role
    (party_id, role_id, granted_by, approved_by, approval_mode,
     source_revision_id, active, created_at, version)
  VALUES
    (target_party, manager_role, actor_one, actor_two, 'normal',
     source_revision, TRUE, now(), 1);

  BEGIN
    INSERT INTO party_security_role
      (party_id, role_id, granted_by, approved_by, approval_mode,
       source_revision_id, active, created_at, version)
    VALUES
      (target_party, engineer_role, actor_one, actor_two, 'normal',
       source_revision, TRUE, now(), 1);
    RAISE EXCEPTION 'grant with a mismatched source revision was accepted';
  EXCEPTION
    WHEN check_violation THEN
      IF SQLERRM NOT LIKE '%does not match an approved source revision%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    UPDATE party_security_role
    SET role_id = engineer_role
    WHERE party_id = target_party AND role_id = manager_role;
    RAISE EXCEPTION 'security grant identity mutation was accepted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
      IF SQLERRM NOT LIKE '%identity is immutable%' THEN
        RAISE;
      END IF;
  END;

  INSERT INTO security_audit_event
    (revision_id, entity_kind, party_id, role_id, operation, previous_active,
     new_active, actor_id, reviewer_id, approver_id, occurred_at,
     source_platform, reason, correlation_id, approval_mode, result)
  VALUES
    (source_revision, 'party-role', target_party, manager_role, 'published',
     FALSE, TRUE, actor_one, actor_two, actor_two, now(),
     'postgres-integration-test', 'Immutable audit integration test',
     'security-audit-' || pg_backend_pid(), 'normal', 'success');

  BEGIN
    UPDATE security_audit_event
    SET result = 'tampered'
    WHERE revision_id = source_revision;
    RAISE EXCEPTION 'security audit mutation was accepted';
  EXCEPTION
    WHEN object_not_in_prerequisite_state THEN
      IF SQLERRM NOT LIKE '%audit events are immutable%' THEN
        RAISE;
      END IF;
  END;

  FOR emergency_assignment IN
    SELECT assignment.id
    FROM party_security_role assignment
    JOIN security_role role ON role.id = assignment.role_id
    JOIN user_credential credential
      ON credential.party_id = assignment.party_id AND credential.active
    WHERE assignment.active AND role.active AND role.emergency_administrator
    ORDER BY assignment.id
  LOOP
    BEGIN
      UPDATE party_security_role
      SET active = FALSE, revoked_at = now(), version = version + 1
      WHERE id = emergency_assignment.id;
    EXCEPTION
      WHEN insufficient_privilege THEN
        IF SQLERRM NOT LIKE '%last coherent emergency administrator%' THEN
          RAISE;
        END IF;
        protected_failures := protected_failures + 1;
    END;
  END LOOP;

  IF protected_failures <> 1 THEN
    RAISE EXCEPTION
      'expected exactly one protected final emergency assignment, observed %',
      protected_failures;
  END IF;

  BEGIN
    UPDATE role_permission grant_row
    SET active = FALSE, revoked_at = now(), version = grant_row.version + 1
    FROM security_role role, security_permission permission
    WHERE grant_row.role_id = role.id
      AND grant_row.permission_id = permission.id
      AND role.code = 'admin'
      AND permission.code = 'security.read'
      AND grant_row.active;
    RAISE EXCEPTION 'critical emergency capability removal was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%capability required by the last coherent emergency administrator%' THEN
        RAISE;
      END IF;
  END;

  SELECT assignment.party_id INTO STRICT remaining_emergency_party
  FROM party_security_role assignment
  JOIN security_role role
    ON role.id = assignment.role_id
   AND role.active
   AND role.emergency_administrator
  JOIN user_credential credential
    ON credential.party_id = assignment.party_id
   AND credential.active
  WHERE assignment.active
  LIMIT 1;

  IF NOT security_is_coherent_emergency_administrator(remaining_emergency_party) THEN
    RAISE EXCEPTION 'remaining emergency administrator is not coherent';
  END IF;

  BEGIN
    UPDATE user_credential
    SET active = FALSE
    WHERE party_id = remaining_emergency_party AND active;
    RAISE EXCEPTION 'last emergency credential deactivation was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%last coherent emergency administrator credential%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    UPDATE security_role
    SET active = FALSE, version = version + 1
    WHERE id = admin_role;
    RAISE EXCEPTION 'last emergency role deactivation was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%last coherent emergency administrator role%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    UPDATE security_permission
    SET active = FALSE, version = version + 1
    WHERE code = 'security.read';
    RAISE EXCEPTION 'critical security permission deactivation was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%security registry data required by the last coherent emergency administrator%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    UPDATE security_action
    SET active = FALSE, version = version + 1
    WHERE code = 'emergency-recover';
    RAISE EXCEPTION 'critical security action deactivation was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%security registry data required by the last coherent emergency administrator%' THEN
        RAISE;
      END IF;
  END;

  BEGIN
    UPDATE security_module
    SET active = FALSE, version = version + 1
    WHERE code = 'admin';
    RAISE EXCEPTION 'critical security module deactivation was accepted';
  EXCEPTION
    WHEN insufficient_privilege THEN
      IF SQLERRM NOT LIKE '%security registry data required by the last coherent emergency administrator%' THEN
        RAISE;
      END IF;
  END;

  RAISE NOTICE 'security registry PostgreSQL integration checks passed';
END;
$security_registry_test$;

ROLLBACK;
