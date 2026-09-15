-- Canonical auth-query tables for an isolated subrouter test, not a production migration.
CREATE TABLE api_token (
 id BIGSERIAL PRIMARY KEY, token TEXT NOT NULL UNIQUE, party_id BIGINT NOT NULL REFERENCES party(id),
 label TEXT, active BOOLEAN NOT NULL
);
CREATE TABLE party_security_role (
 id UUID PRIMARY KEY DEFAULT gen_random_uuid(), party_id BIGINT NOT NULL REFERENCES party(id),
 role_id UUID NOT NULL, granted_by BIGINT, approved_by BIGINT, approval_mode TEXT NOT NULL DEFAULT 'bootstrap',
 emergency_reason TEXT, source_revision_id UUID, source_policy_id UUID, active BOOLEAN NOT NULL DEFAULT TRUE,
 created_at TIMESTAMPTZ NOT NULL DEFAULT now(), revoked_at TIMESTAMPTZ, version INT NOT NULL DEFAULT 1
);
-- Canonical Fan roles exercise the real role decoder as well as token lookup. Empty permission
-- catalogs confer no global module access; contextual event grants remain authoritative.
CREATE TABLE security_role (
 id UUID PRIMARY KEY DEFAULT gen_random_uuid(), code TEXT NOT NULL, name_es TEXT NOT NULL,
 name_en TEXT NOT NULL, description_es TEXT, description_en TEXT, sort_order INT NOT NULL DEFAULT 0,
 system_role BOOLEAN NOT NULL DEFAULT FALSE, emergency_administrator BOOLEAN NOT NULL DEFAULT FALSE,
 self_assignable BOOLEAN NOT NULL DEFAULT FALSE, automatic_assignable BOOLEAN NOT NULL DEFAULT FALSE,
 active BOOLEAN NOT NULL DEFAULT TRUE, workflow_state_id UUID NOT NULL REFERENCES workflow_state(id),
 created_by BIGINT, updated_by BIGINT, approved_by BIGINT, created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
 updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), published_revision INT NOT NULL DEFAULT 1,
 version INT NOT NULL DEFAULT 1
);
CREATE TABLE role_permission (role_id UUID, permission_id UUID, active BOOLEAN NOT NULL);
CREATE TABLE security_permission (id UUID PRIMARY KEY, module_id UUID, action_id UUID,
 resource_scope TEXT NOT NULL, active BOOLEAN NOT NULL);
CREATE TABLE security_action (id UUID PRIMARY KEY, code TEXT NOT NULL, active BOOLEAN NOT NULL);
CREATE TABLE security_module (id UUID PRIMARY KEY, code TEXT NOT NULL, active BOOLEAN NOT NULL);
INSERT INTO workflow_state(id,code) VALUES ('60000000-0000-4000-8000-000000000001','published');
INSERT INTO security_role(id,code,name_es,name_en,workflow_state_id)
 VALUES ('60000000-0000-4000-8000-000000000002','fan','Seguidor de prueba','Test fan',
         '60000000-0000-4000-8000-000000000001');
INSERT INTO party_security_role(party_id,role_id)
 SELECT id,'60000000-0000-4000-8000-000000000002'::uuid FROM party;
INSERT INTO api_token(token,party_id,label,active) VALUES
 ('http-owner-test-token',1,NULL,TRUE),('http-collaborator-test-token',2,NULL,TRUE),
 ('http-outsider-test-token',3,NULL,TRUE),('http-inactive-test-token',1,NULL,FALSE),
 ('http-reset-test-token',1,'password-reset:test-only',TRUE),
 ('http-revocable-test-token',1,NULL,TRUE);
INSERT INTO api_token(token,party_id,label,active) VALUES ('http-flight-test-token',1,NULL,TRUE);
INSERT INTO social_event(id,organizer_party_id) VALUES (90,'1');
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 VALUES (90,'draft',1,'in-flight session fixture');
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
 VALUES (90,1,'primary_owner');
INSERT INTO social_event(id,organizer_party_id)
 SELECT n,'1' FROM generate_series(60,74) n;
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 SELECT n,'draft',1,'disposable HTTP fixture' FROM generate_series(60,74) n;
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
 SELECT n,1,'primary_owner' FROM generate_series(60,74) n;
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,issued_by_party_id)
 VALUES (64,2,'event.manage',1),(65,2,'event.manage',1),(66,2,'event.manage',1),
 (67,1,'event.approve',1),(67,2,'event.approve',1),(74,2,'event.read',1);
UPDATE event_operation_feature_flag SET enabled=TRUE,updated_at=clock_timestamp(),
 updated_by_party_id=1,change_reason='disposable HTTP tests' WHERE feature_code='event.operations.api';

BEGIN;
INSERT INTO social_event(id,organizer_party_id) VALUES (80,'1'),(81,'1');
INSERT INTO event_operation_event_state(event_id,canonical_state,version,migration_evidence)
 VALUES (80,'planning',1,'task HTTP fixture'),(81,'planning',1,'task HTTP fixture');
INSERT INTO event_operation_relationship(event_id,party_id,relationship_kind)
 VALUES (80,1,'primary_owner'),(81,1,'primary_owner');
INSERT INTO event_logistics_activity(id,event_id,status,version)
 VALUES (8000,80,'planned',1),(8001,80,'confirmed',1),(8100,81,'planned',1);
INSERT INTO event_operation_raci_assignment(activity_id,party_id,raci_role,assigned_by_party_id)
 VALUES (8000,1,'accountable',1),(8000,3,'responsible',1);
INSERT INTO event_operation_task_policy(activity_id) VALUES (8000);
INSERT INTO event_operation_grant(event_id,grantee_party_id,scope_code,resource_kind,resource_id,issued_by_party_id)
 VALUES (80,2,'task.read','task','8000',1);
COMMIT;
