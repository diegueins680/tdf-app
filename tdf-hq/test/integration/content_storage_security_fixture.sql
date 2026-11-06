CREATE TABLE party(id bigint PRIMARY KEY);
INSERT INTO party VALUES(1),(2),(3);
CREATE TABLE api_token(id bigserial PRIMARY KEY,token text UNIQUE NOT NULL,party_id bigint NOT NULL,label text,active boolean NOT NULL,issued_at_epoch bigint,expires_at_epoch bigint);
INSERT INTO api_token(token,party_id,label,active) VALUES ('storage-owner-a',1,'fixture',true),('storage-owner-b',2,'fixture',true),('storage-admin',3,'fixture',true);
CREATE TABLE course_registration_receipt(id bigint PRIMARY KEY);
CREATE TABLE course_registration_follow_up(id bigint PRIMARY KEY);
CREATE TABLE payment(id bigint PRIMARY KEY);
CREATE TABLE security_role(id uuid,code text,name_es text,name_en text,description_es text,description_en text,sort_order integer,system_role boolean,emergency_administrator boolean,self_assignable boolean,automatic_assignable boolean,active boolean,workflow_state_id uuid,created_by bigint,updated_by bigint,approved_by bigint,created_at timestamptz,updated_at timestamptz,published_revision integer,version integer);
CREATE TABLE party_security_role(id uuid,party_id bigint,role_id uuid,granted_by bigint,approved_by bigint,approval_mode text,emergency_reason text,source_revision_id uuid,source_policy_id uuid,active boolean,created_at timestamptz,revoked_at timestamptz,version integer);
CREATE TABLE security_module(id uuid PRIMARY KEY,code text,active boolean);
CREATE TABLE security_action(id uuid PRIMARY KEY,code text,active boolean);
CREATE TABLE security_permission(id uuid PRIMARY KEY,module_id uuid,action_id uuid,resource_scope text,active boolean);
CREATE TABLE role_permission(id uuid PRIMARY KEY,role_id uuid,permission_id uuid,active boolean);
INSERT INTO security_role(id,code,name_es,name_en,sort_order,system_role,emergency_administrator,self_assignable,automatic_assignable,active,workflow_state_id,created_at,updated_at,published_revision,version)
VALUES ('00000000-0000-4000-8000-000000000001','admin','Admin','Admin',0,true,false,false,false,true,'00000000-0000-4000-8000-000000000099',now(),now(),1,1);
INSERT INTO party_security_role(id,party_id,role_id,approval_mode,active,created_at,version)
VALUES ('00000000-0000-4000-8000-000000000002',3,'00000000-0000-4000-8000-000000000001','bootstrap',true,now(),1);
INSERT INTO security_module VALUES('00000000-0000-4000-8000-000000000003','admin',true);
INSERT INTO security_action VALUES('00000000-0000-4000-8000-000000000004','access',true);
INSERT INTO security_permission VALUES('00000000-0000-4000-8000-000000000005','00000000-0000-4000-8000-000000000003','00000000-0000-4000-8000-000000000004','module',true);
INSERT INTO role_permission VALUES('00000000-0000-4000-8000-000000000006','00000000-0000-4000-8000-000000000001','00000000-0000-4000-8000-000000000005',true);
