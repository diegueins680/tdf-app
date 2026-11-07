-- Synthetic historical evidence; each independent case rolls back all fixture rows.
BEGIN;DO $$
DECLARE a bigint;b bigint;c bigint;ci uuid:=gen_random_uuid();op uuid:=gen_random_uuid();cred uuid:=gen_random_uuid();file uuid:=gen_random_uuid();
BEGIN
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived A',false,now())RETURNING id INTO a;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived B',false,now())RETURNING id INTO b;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard canonical C',false,now())RETURNING id INTO c;
 INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(cred,b,decode(repeat('00',29),'hex'),now()+interval '1 day');
 INSERT INTO content_storage_account(party_id)VALUES(c);
 INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
 VALUES(file,c,repeat('1',64),repeat('2',64),1,'tdf','private','synthetic','text/plain',repeat('3',64),'uploading',gen_random_uuid(),now()+interval '1 day');
 INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900705,file,b);
 INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)VALUES(ci,ARRAY[a,b,c],'{}','[]','Synthetic storage historical fixture');
 INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)VALUES(op,ci,c,ARRAY[a,b],'[]','{}');
 INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)VALUES(a,c,op),(b,c,op);
 BEGIN INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(gen_random_uuid(),a,decode(repeat('00',29),'hex'),now()+interval '1 day'); RAISE EXCEPTION 'UNSAFE_ARCHIVED_ADMISSION'; EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
END $$;ROLLBACK;
BEGIN;DO $$
DECLARE a bigint;b bigint;c bigint;ci uuid:=gen_random_uuid();op uuid:=gen_random_uuid();cred uuid:=gen_random_uuid();file uuid:=gen_random_uuid();
BEGIN
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived A',false,now())RETURNING id INTO a;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived B',false,now())RETURNING id INTO b;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard canonical C',false,now())RETURNING id INTO c;
 INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(cred,b,decode(repeat('00',29),'hex'),now()+interval '1 day');
 INSERT INTO content_storage_account(party_id)VALUES(c);
 INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
 VALUES(file,c,repeat('1',64),repeat('2',64),1,'tdf','private','synthetic','text/plain',repeat('3',64),'uploading',gen_random_uuid(),now()+interval '1 day');
 INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900705,file,b);
 INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)VALUES(ci,ARRAY[a,b,c],'{}','[]','Synthetic storage historical fixture');
 INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)VALUES(op,ci,c,ARRAY[a,b],'[]','{}');
 INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)VALUES(a,c,op),(b,c,op);
 BEGIN UPDATE content_storage_credential SET owner_party_id=a WHERE id=cred; RAISE EXCEPTION 'UNSAFE_ARCHIVED_ADMISSION'; EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
END $$;ROLLBACK;
BEGIN;DO $$
DECLARE a bigint;b bigint;c bigint;ci uuid:=gen_random_uuid();op uuid:=gen_random_uuid();cred uuid:=gen_random_uuid();file uuid:=gen_random_uuid();
BEGIN
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived A',false,now())RETURNING id INTO a;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived B',false,now())RETURNING id INTO b;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard canonical C',false,now())RETURNING id INTO c;
 INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(cred,b,decode(repeat('00',29),'hex'),now()+interval '1 day');
 INSERT INTO content_storage_account(party_id)VALUES(c);
 INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
 VALUES(file,c,repeat('1',64),repeat('2',64),1,'tdf','private','synthetic','text/plain',repeat('3',64),'uploading',gen_random_uuid(),now()+interval '1 day');
 INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900705,file,b);
 INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)VALUES(ci,ARRAY[a,b,c],'{}','[]','Synthetic storage historical fixture');
 INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)VALUES(op,ci,c,ARRAY[a,b],'[]','{}');
 INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)VALUES(a,c,op),(b,c,op);
 BEGIN INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900706,file,a); RAISE EXCEPTION 'UNSAFE_ARCHIVED_ADMISSION'; EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
END $$;ROLLBACK;
BEGIN;DO $$
DECLARE a bigint;b bigint;c bigint;ci uuid:=gen_random_uuid();op uuid:=gen_random_uuid();cred uuid:=gen_random_uuid();file uuid:=gen_random_uuid();
BEGIN
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived A',false,now())RETURNING id INTO a;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived B',false,now())RETURNING id INTO b;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard canonical C',false,now())RETURNING id INTO c;
 INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(cred,b,decode(repeat('00',29),'hex'),now()+interval '1 day');
 INSERT INTO content_storage_account(party_id)VALUES(c);
 INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
 VALUES(file,c,repeat('1',64),repeat('2',64),1,'tdf','private','synthetic','text/plain',repeat('3',64),'uploading',gen_random_uuid(),now()+interval '1 day');
 INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900705,file,b);
 INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)VALUES(ci,ARRAY[a,b,c],'{}','[]','Synthetic storage historical fixture');
 INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)VALUES(op,ci,c,ARRAY[a,b],'[]','{}');
 INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)VALUES(a,c,op),(b,c,op);
 BEGIN UPDATE content_storage_reference SET bound_by=a WHERE resource_kind='payment' AND resource_id=900705; RAISE EXCEPTION 'UNSAFE_ARCHIVED_ADMISSION'; EXCEPTION WHEN SQLSTATE '55000' THEN NULL; END;
END $$;ROLLBACK;
BEGIN;DO $$
DECLARE a bigint;b bigint;c bigint;ci uuid:=gen_random_uuid();op uuid:=gen_random_uuid();cred uuid:=gen_random_uuid();file uuid:=gen_random_uuid();
BEGIN
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived A',false,now())RETURNING id INTO a;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard archived B',false,now())RETURNING id INTO b;
 INSERT INTO party(display_name,is_org,created_at)VALUES('Storage guard canonical C',false,now())RETURNING id INTO c;
 INSERT INTO content_storage_credential(id,owner_party_id,sealed,expires_at)VALUES(cred,b,decode(repeat('00',29),'hex'),now()+interval '1 day');
 INSERT INTO content_storage_account(party_id)VALUES(c);
 INSERT INTO content_storage_upload(id,owner_party_id,request_key,fingerprint,byte_size,destination,visibility,file_name,media_type,sha256,status,lease_id,lease_until)
 VALUES(file,c,repeat('1',64),repeat('2',64),1,'tdf','private','synthetic','text/plain',repeat('3',64),'uploading',gen_random_uuid(),now()+interval '1 day');
 INSERT INTO content_storage_reference(resource_kind,resource_id,file_id,bound_by)VALUES('payment',900705,file,b);
 INSERT INTO identity_reconciliation_case(id,member_ids,evidence,before_parties,reason)VALUES(ci,ARRAY[a,b,c],'{}','[]','Synthetic storage historical fixture');
 INSERT INTO identity_merge_history(operation_id,case_id,canonical_party_id,retired_party_ids,before_parties,after_parties)VALUES(op,ci,c,ARRAY[a,b],'[]','{}');
 INSERT INTO identity_party_archive(party_id,canonical_party_id,operation_id)VALUES(a,c,op),(b,c,op);
UPDATE content_storage_credential SET revision=revision+1 WHERE id=cred; UPDATE content_storage_reference SET created_at=clock_timestamp() WHERE resource_kind='payment' AND resource_id=900705; IF (SELECT owner_party_id FROM content_storage_credential WHERE id=cred)IS DISTINCT FROM b OR (SELECT bound_by FROM content_storage_reference WHERE resource_kind='payment'AND resource_id=900705)IS DISTINCT FROM b THEN RAISE EXCEPTION 'Historical identity changed'; END IF; END $$;ROLLBACK;
