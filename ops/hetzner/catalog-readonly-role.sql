-- Operational access only: no application schema/data changes or Fly access.
-- Execute once on the verified tdf-production database through the existing
-- authenticated operator connection. A pre-existing name requires review.
\set ON_ERROR_STOP on
BEGIN;
DO $guard$
BEGIN
  IF current_database() <> 'tdf_hq' THEN
    RAISE EXCEPTION 'Not the TDF production database';
  END IF;
  IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'tdf_catalog_inventory') THEN
    RAISE EXCEPTION 'Role already exists; inspect its privileges instead of overwriting';
  END IF;
END
$guard$;
-- Preserve current named roles' effective CREATE capability, then remove the
-- ambient PUBLIC grant so a newly provisioned reader cannot create objects.
DO $preserve$
DECLARE existing_role record;
BEGIN
  FOR existing_role IN SELECT rolname FROM pg_roles
    WHERE rolname !~ '^pg_' AND has_schema_privilege(rolname, 'public', 'CREATE')
  LOOP
    EXECUTE format('GRANT CREATE ON SCHEMA public TO %I', existing_role.rolname);
  END LOOP;
END
$preserve$;
REVOKE CREATE ON SCHEMA public FROM PUBLIC;
CREATE ROLE tdf_catalog_inventory LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE
  NOINHERIT NOREPLICATION NOBYPASSRLS CONNECTION LIMIT 2 PASSWORD NULL;
ALTER ROLE tdf_catalog_inventory SET default_transaction_read_only = on;
GRANT CONNECT ON DATABASE tdf_hq TO tdf_catalog_inventory;
GRANT USAGE ON SCHEMA public TO tdf_catalog_inventory;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO tdf_catalog_inventory;
DO $guard$
BEGIN
  IF has_schema_privilege('tdf_catalog_inventory', 'public', 'CREATE') THEN
    RAISE EXCEPTION 'Unexpected inherited schema-write permission';
  END IF;
END
$guard$;
COMMIT;
-- No password is provisioned: network SCRAM authentication cannot use this role.
-- Only the existing authenticated Docker/operator path uses its local socket.
-- New tables require reviewed SELECT grants; the adapter fails on coverage gaps.
