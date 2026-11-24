-- Synthetic data for a fresh database loaded by load-payment-test-schema.sh.
-- Keep the canonical emergency administrators and all production constraints.
BEGIN;
INSERT INTO party(id,display_name,is_org,created_at) VALUES
 (7401,'Synthetic storage owner A',false,now()),
 (7402,'Synthetic storage owner B',false,now()),
 (7403,'Synthetic storage administrator',false,now());
-- Custom-label fixture sessions intentionally have no time window, matching
-- the existing storage fixture. The test revokes the owner session explicitly.
INSERT INTO api_token(token,party_id,label,active) VALUES
 ('storage-owner-a',7401,'storage-security:fixture',true),
 ('storage-owner-b',7402,'storage-security:fixture',true),
 ('storage-admin',7403,'storage-security:fixture',true);
INSERT INTO party_security_role(party_id,role_id,approval_mode,active)
 SELECT 7403,id,'bootstrap',true FROM security_role WHERE code='admin';
INSERT INTO content_storage_account(party_id,legacy_usage_verified)
 SELECT id,true FROM party
 ON CONFLICT(party_id) DO UPDATE SET legacy_usage_verified=true;
INSERT INTO course(slug,title,price_cents,currency,capacity)
 VALUES('storage-security-fixture','Synthetic storage fixture',12515,'USD',10);
INSERT INTO course_registration(id,course_slug,source,status)
 VALUES(1,'storage-security-fixture','test','pending_payment');
COMMIT;
