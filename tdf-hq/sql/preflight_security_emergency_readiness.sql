\set ON_ERROR_STOP on

-- Read-only release gate for emergency-administrator continuity.
--
-- This intentionally returns aggregate counts only. It never selects party,
-- credential, username, email, token, or password-hash values. The seven
-- permission codes below are stable enforcement identifiers mirrored by
-- TDF.Catalog.Security and the database integrity triggers; labels, grants,
-- and assignments remain database-authoritative.
BEGIN TRANSACTION READ ONLY;
SET LOCAL statement_timeout = '15s';
SET LOCAL lock_timeout = '2s';
SET LOCAL idle_in_transaction_session_timeout = '20s';

SELECT CASE
  WHEN to_regclass('public.party_security_role') IS NOT NULL
   AND to_regclass('public.security_role') IS NOT NULL
   AND to_regclass('public.role_permission') IS NOT NULL
   AND to_regclass('public.security_permission') IS NOT NULL
   AND to_regclass('public.user_credential') IS NOT NULL
  THEN $canonical$
    WITH required_permission(code) AS (
      VALUES
        ('admin.access'),
        ('security.read'),
        ('security.create'),
        ('security.review'),
        ('security.approve'),
        ('security.assign'),
        ('security.emergency-recover')
    ),
    active_assignment AS (
      SELECT assignment.party_id, assignment.role_id
      FROM party_security_role assignment
      JOIN security_role role
        ON role.id = assignment.role_id
       AND role.active
       AND role.emergency_administrator
      WHERE assignment.active
    ),
    authenticatable_party AS (
      SELECT DISTINCT assignment.party_id, assignment.role_id
      FROM active_assignment assignment
      JOIN user_credential credential
        ON credential.party_id = assignment.party_id
       AND credential.active
    ),
    legacy_authenticatable_party AS (
      SELECT DISTINCT assignment.party_id
      FROM party_role assignment
      JOIN user_credential credential
        ON credential.party_id = assignment.party_id
       AND credential.active
      WHERE assignment.active
        AND assignment.role::text = 'Admin'
    ),
    coherent_legacy_target_role AS (
      SELECT role.id
      FROM security_role role
      WHERE role.active
        AND role.emergency_administrator
        AND role.code = 'admin'
        AND NOT EXISTS (
          SELECT 1
          FROM required_permission required
          WHERE NOT EXISTS (
            SELECT 1
            FROM role_permission grant_row
            JOIN security_permission permission
              ON permission.id = grant_row.permission_id
             AND permission.active
            JOIN security_action action
              ON action.id = permission.action_id
             AND action.active
            JOIN security_module module_row
              ON module_row.id = permission.module_id
             AND module_row.active
            WHERE grant_row.role_id = role.id
              AND grant_row.active
              AND permission.code = required.code
          )
        )
    ),
    coherent_party AS (
      SELECT DISTINCT candidate.party_id
      FROM authenticatable_party candidate
      WHERE NOT EXISTS (
        SELECT 1
        FROM required_permission required
        WHERE NOT EXISTS (
          SELECT 1
          FROM role_permission grant_row
          JOIN security_permission permission
            ON permission.id = grant_row.permission_id
           AND permission.active
          JOIN security_action action
            ON action.id = permission.action_id
           AND action.active
          JOIN security_module module_row
            ON module_row.id = permission.module_id
           AND module_row.active
          WHERE grant_row.role_id = candidate.role_id
            AND grant_row.active
            AND permission.code = required.code
        )
      )
    ),
    counts AS (
      SELECT
        (SELECT count(*)::int FROM active_assignment) AS active_assignments,
        (SELECT count(DISTINCT party_id)::int FROM active_assignment) AS assigned_parties,
        (SELECT count(DISTINCT party_id)::int FROM authenticatable_party) AS authenticatable_parties,
        (SELECT count(*)::int FROM legacy_authenticatable_party) AS legacy_authenticatable_parties,
        (SELECT count(*)::int FROM coherent_legacy_target_role) AS coherent_legacy_target_roles,
        (SELECT count(*)::int FROM coherent_party) AS database_coherent_paths
    )
    SELECT json_build_object(
      'kind', 'security-emergency-readiness',
      'schemaMode', 'canonical',
      'transactionReadOnly', current_setting('transaction_read_only'),
      'requiredIndependentPaths', 2,
      'activeEmergencyAssignments', active_assignments,
      'distinctAssignedParties', assigned_parties,
      'authenticatableParties', authenticatable_parties,
      'legacyAuthenticatableParties', legacy_authenticatable_parties,
      'coherentLegacyTargetRoles', coherent_legacy_target_roles,
      'databaseCoherentPaths', database_coherent_paths,
      'preMigrationReady', database_coherent_paths >= 2 OR (
        legacy_authenticatable_parties >= 2 AND coherent_legacy_target_roles = 1
      ),
      'databaseReady', database_coherent_paths >= 2,
      'manualIndependentLoginVerificationRequired', true,
      'capturedAt', now()
    )::text
    FROM counts;
  $canonical$
  WHEN to_regclass('public.party_role') IS NOT NULL
   AND to_regclass('public.user_credential') IS NOT NULL
  THEN $legacy$
    WITH active_assignment AS (
      SELECT assignment.party_id
      FROM party_role assignment
      WHERE assignment.active
        AND assignment.role::text = 'Admin'
    ),
    counts AS (
      SELECT
        (SELECT count(*)::int FROM active_assignment) AS active_assignments,
        (SELECT count(DISTINCT party_id)::int FROM active_assignment) AS assigned_parties,
        (
          SELECT count(DISTINCT assignment.party_id)::int
          FROM active_assignment assignment
          JOIN user_credential credential
            ON credential.party_id = assignment.party_id
           AND credential.active
        ) AS authenticatable_parties
    )
    SELECT json_build_object(
      'kind', 'security-emergency-readiness',
      'schemaMode', 'legacy',
      'transactionReadOnly', current_setting('transaction_read_only'),
      'requiredIndependentPaths', 2,
      'activeEmergencyAssignments', active_assignments,
      'distinctAssignedParties', assigned_parties,
      'authenticatableParties', authenticatable_parties,
      'databaseCoherentPaths', null,
      'preMigrationReady', authenticatable_parties >= 2,
      'databaseReady', false,
      'reason', 'canonical-security-registry-not-deployed',
      'manualIndependentLoginVerificationRequired', true,
      'capturedAt', now()
    )::text
    FROM counts;
  $legacy$
  ELSE $missing$
    SELECT json_build_object(
      'kind', 'security-emergency-readiness',
      'schemaMode', 'missing',
      'transactionReadOnly', current_setting('transaction_read_only'),
      'requiredIndependentPaths', 2,
      'preMigrationReady', false,
      'databaseReady', false,
      'reason', 'security-assignment-or-credential-tables-missing',
      'manualIndependentLoginVerificationRequired', true,
      'capturedAt', now()
    )::text;
  $missing$
END
\gexec

ROLLBACK;
