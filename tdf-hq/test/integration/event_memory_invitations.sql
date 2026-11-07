-- Full migrated PostgreSQL authority, synthetic identities only; rollback fixtures.
BEGIN;
UPDATE interaction_runtime SET enabled=true WHERE singleton;
INSERT INTO party(id,display_name,is_org,created_at)
SELECT n,'Memory participant '||n,false,now() FROM generate_series(919141001,919141008) n;
INSERT INTO user_credential(party_id,username,password_hash,active)
SELECT n,'memory-test-'||n,'not-a-login-hash',true FROM generate_series(919141001,919141007) n;
INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
SELECT 919141001,'919141001','Synthetic memories',now()-interval '2 days',now()-interval '1 day',id,
 '00000000-0000-4000-8000-000000000238','{"isPublic":true}' FROM event_type WHERE code='concert';
INSERT INTO event_rsvp(event_id,party_id,status) VALUES
 (919141001,'919141001','accepted'),(919141001,'919141002','accepted'),
 (919141001,'919141003','accepted'),(919141001,'919141004','declined'),(919141001,'919141008','accepted');
INSERT INTO social_artist_profile(id,party_id,name) VALUES(919141001,'919141006','Synthetic Artist');
INSERT INTO event_artist(event_id,artist_id) VALUES(919141001,919141001);
INSERT INTO event_logistics_member(event_id,party_id,member_role) VALUES(919141001,'919141005','viewer');
INSERT INTO event_ticket_tier(id,event_id,code,name,price_cents,currency,quantity_total,quantity_sold,is_active)
 VALUES(919141001,919141001,'SYNTHETIC','Synthetic',0,'USD',10,1,true);
INSERT INTO event_ticket_order(id,event_id,tier_id,buyer_party_id,quantity,amount_cents,currency,status,purchased_at)
 VALUES(919141001,919141001,919141001,'919141002',1,0,'USD','paid',now());
INSERT INTO event_ticket(id,event_id,tier_ref_id,order_ref_id,code,status,checked_in_at,current_holder_party_id)
 VALUES(919141001,919141001,919141001,919141001,'SYNTHETIC-NOT-A-TICKET','checked_in',now()-interval '1 day','919141002');
INSERT INTO event_ticket_admission_audit(ticket_id,event_id,order_id,actor_party_id,admitted_at)
 VALUES(919141001,919141001,919141001,'919141001',now()-interval '1 day');
DO $$
DECLARE notice bigint; invite uuid; summary jsonb;
BEGIN
 ASSERT EXISTS(SELECT 1 FROM directory_public_rsvp_event WHERE id=919141001 AND public_share_eligible AND NOT rsvp_eligible), 'Completed community stays public without RSVP';
 ASSERT event_memory_finished(919141001), 'Completed and elapsed event qualifies';
 ASSERT (SELECT count(*)=5 FROM event_memory_candidates(919141001)), 'Sources deduplicate and missing accounts/declines are excluded';
 ASSERT (SELECT evidence='verified_attendance' FROM event_memory_candidates(919141001) WHERE party_id=919141002), 'Audited admission outranks RSVP';
 ASSERT (SELECT evidence='rsvp_intent' FROM event_memory_candidates(919141001) WHERE party_id=919141003), 'RSVP never proves presence';
 ASSERT (SELECT evidence='professional' FROM event_memory_candidates(919141001) WHERE party_id=919141001), 'Organizer has professional context';
 ASSERT event_memory_preferences(919141003)->>'inApp'='false', 'Consent is never backfilled';
 ASSERT event_memory_summary(919141001,919141005,true)->>'error'='not_found', 'Logistics member cannot enable campaign';
 ASSERT event_memory_summary(919141001,919141004)->>'error'='not_found', 'No cross-event metrics';
 ASSERT event_memory_dispatch_batch(20)=0, 'Disabled campaign sends nothing';
 PERFORM event_memory_summary(919141001,919141001,true);
 ASSERT event_memory_dispatch_batch(20)=0, 'Enabled campaign without consent sends nothing';
 PERFORM event_memory_preferences(919141002,true);
 PERFORM event_memory_preferences(919141003,true);
 PERFORM event_memory_preferences(919141005,true);
 UPDATE interaction_runtime SET enabled=false WHERE singleton;
 ASSERT event_memory_dispatch_batch(20)=0, 'Platform notification pause fences delivery';
 UPDATE interaction_runtime SET enabled=true WHERE singleton;
 ASSERT event_memory_dispatch_batch(20)=3, 'Only opted-in eligible accounts receive invitations';
 ASSERT event_memory_dispatch_batch(20)=0, 'Repeated tick has no duplicate effect';
 ASSERT (SELECT count(*)=3 FROM event_memory_delivery WHERE event_id=919141001 AND step=1), 'One invitation per account';
 SELECT id,notification_id INTO invite,notice FROM event_memory_delivery WHERE party_id=919141003;
 ASSERT interaction_notification_visible(notice,919141003), 'Own notice visible';
 ASSERT NOT interaction_notification_visible(notice,919141004), 'Another account cannot read invitation';
 ASSERT (SELECT n.body LIKE 'El evento que te interesó%' FROM notification n WHERE n.id=notice), 'RSVP copy is conditional';
 PERFORM event_memory_open(919141001,919141004,invite);
 ASSERT (SELECT opened_at IS NULL FROM event_memory_delivery WHERE id=invite), 'Attribution is recipient-bound';
 PERFORM event_memory_open(919141001,919141003,invite);
 ASSERT (SELECT opened_at IS NOT NULL FROM event_memory_delivery WHERE id=invite), 'Authenticated opening is observed';
 PERFORM event_memory_preferences(919141003,false);
 ASSERT NOT interaction_notification_visible(notice,919141003), 'Opt-out hides old notice';
 INSERT INTO event_moment(event_id,author_party_id,author_name,media_url,media_type)
 VALUES(919141001,'919141002','Synthetic author','https://example.test/photo.jpg','image');
 ASSERT (SELECT bool_and(published_at IS NOT NULL) FROM event_memory_delivery WHERE party_id=919141002), 'Publication suppresses even a later deleted moment';
 DELETE FROM event_moment WHERE event_id=919141001 AND author_party_id='919141002';
 UPDATE event_memory_delivery SET delivered_at=now()-interval '8 days';
 ASSERT event_memory_dispatch_batch(20)=1, 'Only non-publisher with current consent receives a reminder';
 ASSERT (SELECT count(*)=1 FROM event_memory_delivery WHERE step=2 AND party_id=919141005), 'At most one reminder';
 UPDATE event_memory_delivery SET delivered_at=now()-interval '8 days';
 ASSERT event_memory_dispatch_batch(20)=0, 'Third message is impossible';
 UPDATE social_event SET end_time=NULL WHERE id=919141001;
 ASSERT NOT event_memory_finished(919141001), 'Unknown end fails closed';
 UPDATE social_event SET end_time=now()+interval '1 day' WHERE id=919141001;
 ASSERT NOT event_memory_finished(919141001), 'Future end fails closed';
 UPDATE social_event SET end_time=now()-interval '30 minutes' WHERE id=919141001;
 ASSERT NOT event_memory_finished(919141001), 'Cooldown is required';
 INSERT INTO social_event(id,organizer_party_id,title,start_time,end_time,event_type_id,workflow_state_id,metadata)
 SELECT 919141002,'919141001','Synthetic on-sale event',now()-interval '2 days',now()-interval '1 day',event_type_id,'00000000-0000-4000-8000-000000000233',metadata FROM social_event WHERE id=919141001;
 ASSERT NOT event_memory_finished(919141002), 'On sale is not confirmed completion';
 UPDATE social_event SET metadata='{"isPublic":false}' WHERE id=919141001;
 ASSERT NOT EXISTS(SELECT 1 FROM directory_public_rsvp_event WHERE id=919141001), 'Completed does not expose a private parent';
 ASSERT NOT interaction_event_access(919141001,919141003), 'RSVP does not grant private community access';
 UPDATE social_event SET metadata='{"isPublic":true}' WHERE id=919141001;
 summary:=event_memory_summary(919141001,919141001);
 ASSERT summary->>'invitations'='3' AND summary->>'reminders'='1' AND summary->>'opened'='1' AND summary->>'published'='1', 'Metrics are persisted observations';
 ASSERT NOT summary ? 'recipients', 'Organizer summary contains no recipient list';
END $$;
ROLLBACK;
