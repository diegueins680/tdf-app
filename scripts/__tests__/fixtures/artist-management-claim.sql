-- Run only in the isolated directory migration fixture. All synthetic data rolls back.
BEGIN;
DO $$
<<fixture>>
DECLARE
  account_id bigint;
  artist_id bigint;
  outsider_id bigint;
  profile_id uuid := 'df170000-0000-4000-8000-000000000001';
  claim_id uuid := 'df170000-0000-4000-8000-000000000002';
  credential_id bigint;
  follow_id bigint;
BEGIN
  INSERT INTO party(display_name,is_org,created_at) VALUES ('Claim account fixture',false,now()) RETURNING id INTO account_id;
  INSERT INTO party(display_name,is_org,created_at) VALUES ('Imported artist fixture',false,now()) RETURNING id INTO artist_id;
  INSERT INTO party(display_name,is_org,created_at) VALUES ('Unrelated claim fixture',false,now()) RETURNING id INTO outsider_id;
  INSERT INTO user_credential(party_id,username,password_hash,active)
    VALUES(account_id,'artist-management-fixture','synthetic-not-a-password',true) RETURNING id INTO credential_id;
  INSERT INTO fan_follow(fan_party_id,artist_party_id,created_at)
    VALUES(account_id,artist_id,now()) RETURNING id INTO follow_id;
  INSERT INTO directory_profile(id,subject_party_id,profile_kind,public_name,slug,profile_status,visibility,moderation_status,onsite,remote)
    VALUES(profile_id,artist_id,'artist','Imported artist fixture','artist-management-claim-fixture','published','public','allowed',true,false);
  INSERT INTO directory_claim(id,profile_id,claimant_party_id,claim_type,status,evidence,submitted_at)
    VALUES(claim_id,profile_id,account_id,'administration','submitted','[{"description":"Synthetic ownership review evidence"}]',now());
  BEGIN
    INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,source_claim_id)
      VALUES(profile_id,account_id,true,claim_id);
    RAISE EXCEPTION 'pending claim granted management access';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;
  UPDATE directory_claim SET status='approved',reviewer_party_id=outsider_id,reviewed_at=now()
    WHERE id=claim_id;
  BEGIN
    INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,source_claim_id)
      VALUES(profile_id,outsider_id,true,claim_id);
    RAISE EXCEPTION 'approved claim granted a different account access';
  EXCEPTION WHEN insufficient_privilege THEN NULL;
  END;
  INSERT INTO directory_profile_manager(profile_id,account_party_id,can_edit,can_manage,source_claim_id)
    VALUES(profile_id,account_id,true,true,claim_id);
  ASSERT (SELECT subject_party_id=artist_id FROM directory_profile WHERE id=fixture.profile_id), 'artist identity changed';
  ASSERT (SELECT party_id=account_id FROM user_credential WHERE id=credential_id), 'account credential moved';
  ASSERT (SELECT fan_party_id=account_id AND artist_party_id=artist_id FROM fan_follow WHERE id=follow_id), 'follow history moved';
  ASSERT NOT EXISTS(SELECT 1 FROM user_credential WHERE party_id=artist_id), 'artist credentials were created';
  ASSERT (SELECT count(*)=3 FROM party WHERE id IN (account_id,artist_id,outsider_id)), 'identities were combined';
  UPDATE directory_profile_manager SET active=false,revoked_at=now()
    WHERE directory_profile_manager.profile_id=fixture.profile_id AND account_party_id=account_id;
  ASSERT NOT EXISTS(SELECT 1 FROM directory_profile_manager manager
    WHERE manager.profile_id=fixture.profile_id AND manager.account_party_id=account_id AND manager.active), 'revoked grant stayed active';
END $$;
ROLLBACK;
