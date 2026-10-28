-- Committed synthetic fixtures for the owned-database concurrent dispatcher test.
BEGIN;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
 SELECT n,'Concurrent memory fixture '||n,false,now() FROM generate_series(919142001,919142002) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
 SELECT n,'concurrent-memory-'||n,'not-a-login-hash',true FROM generate_series(919142001,919142002) n;
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919142001,'919142001','Concurrent synthetic event',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":true}' FROM event_type WHERE code='concert';
INSERT INTO event_rsvp(event_id,party_id,status) VALUES(919142001,'919142002','accepted');
SELECT event_memory_preferences(919142002,true);
SELECT event_memory_summary(919142001,919142001,true);
COMMIT;
