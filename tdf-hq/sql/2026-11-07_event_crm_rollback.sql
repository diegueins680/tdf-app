BEGIN;
-- A rollback never discards invitations, contacts, audits or payment evidence.
LOCK TABLE event_crm_control,event_crm_membership,event_crm_invitation,event_crm_email_challenge,
 event_crm_verified_email,event_crm_lead,event_crm_audit,event_crm_import,event_crm_command_receipt,
 event_crm_transport_slot,event_crm_waitlist_link IN ACCESS EXCLUSIVE MODE;
DO $$ DECLARE n bigint; item text; BEGIN
 FOREACH item IN ARRAY ARRAY['event_crm_control','event_crm_membership','event_crm_invitation','event_crm_email_challenge',
  'event_crm_verified_email','event_crm_lead','event_crm_audit','event_crm_import','event_crm_command_receipt',
  'event_crm_transport_slot','event_crm_waitlist_link'] LOOP
  EXECUTE format('SELECT count(*) FROM %I',item) INTO n;
  IF n<>0 THEN RAISE EXCEPTION 'Populated CRM rollback refused; retain data and apply a forward repair' USING ERRCODE='55000'; END IF;
 END LOOP;
 IF EXISTS(SELECT 1 FROM commerce_manual_payment_evidence WHERE sale_occurred_at IS NOT NULL) THEN
  RAISE EXCEPTION 'Manual sale evidence rollback refused' USING ERRCODE='55000'; END IF;
END $$;
DROP TRIGGER IF EXISTS event_crm_bundle_order_guard ON event_ticket_order;
DROP TRIGGER IF EXISTS event_crm_order_projection ON event_ticket_checkout_runtime;
DROP TRIGGER IF EXISTS event_crm_verified_claim_guard ON commerce_manual_payment_evidence;
DROP FUNCTION IF EXISTS event_crm_guard_bundle_order(),event_crm_sync_order(),event_crm_protect_verified_claim();
DROP FUNCTION IF EXISTS event_crm_command(bigint,bigint,uuid,jsonb,text),event_crm_snapshot(bigint,bigint,jsonb),
 event_crm_invitation_action(bigint,text,boolean),event_crm_upsert_row(bigint,bigint,jsonb),
 event_crm_prepare_row(bigint,bigint,jsonb),event_crm_email_proof(bigint,text,boolean),
 event_crm_role(bigint,bigint),event_crm_candidates(text,text),event_crm_email(text),event_crm_phone(text),event_crm_timestamp(text);
DROP TABLE event_crm_waitlist_link,event_crm_invitation,event_crm_membership,event_crm_lead,event_crm_audit,
 event_crm_import,event_crm_command_receipt,event_crm_transport_slot,event_crm_control,
 event_crm_email_challenge,event_crm_verified_email;
DROP FUNCTION IF EXISTS event_crm_immutable_audit(),event_crm_fence_membership();
ALTER TABLE commerce_manual_payment_evidence DROP COLUMN sale_occurred_at;
COMMIT;
