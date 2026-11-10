#!/usr/bin/env python3
"""Real PostgreSQL migration/concurrency tests. Synthetic data, no SMTP/providers.

Creates and removes only a new random *_test database on loopback. No inherited
libpq routing. TDF_EVENT_CRM_TEST_PORT/USER/PASSWORD select the isolated runner.
"""
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import uuid
import time

ROOT=Path(__file__).resolve().parent.parent
PSQL=shutil.which('psql') or '/usr/local/opt/postgresql@16/bin/psql'
BIN=Path(PSQL).parent
PORT=os.environ.get('TDF_EVENT_CRM_TEST_PORT','5432')
USER=os.environ.get('TDF_EVENT_CRM_TEST_USER',os.environ.get('USER','postgres'))
assert PORT.isdigit() and 1<=int(PORT)<=65535
DB='tdf_event_crm_'+uuid.uuid4().hex[:12]+'_test'
ENV={k:v for k,v in os.environ.items() if not k.startswith('PG')}
ENV['PGPASSWORD']=os.environ.get('TDF_EVENT_CRM_TEST_PASSWORD','')
ARGS=['-h','127.0.0.1','-p',PORT,'-U',USER]
checks=[]
def raw(sql,ok=True):
 r=subprocess.run([PSQL,*ARGS,'-d',DB,'-X','-qAt','-v','ON_ERROR_STOP=1'],
   input=sql,text=True,capture_output=True,env=ENV,timeout=35)
 if ok and r.returncode:raise AssertionError(r.stderr)
 return r
def query(sql):
 out=raw(sql).stdout.strip()
 if out in ('t','f'):return out=='t'
 return json.loads(out) if out else None
def lit(x):return "'"+str(x).replace("'","''")+"'"
def cmd(actor,body,key=None,event=10):
 return query(f"SELECT event_crm_command({event},{actor},{lit(key or str(uuid.uuid4()))}::uuid,{lit(json.dumps(body))}::jsonb);")
def check(name,condition):
 assert condition,name
 checks.append(name)
 print('PASS',name,flush=True)
def issue(role='coordinator',recipient=2,email=None,event=10,issuer=1):
 secret=uuid.uuid4().hex+uuid.uuid4().hex[:11]
 key=str(uuid.uuid4());body={'operation':'invitation.create','role':role}
 body.update({'recipientEmailHash':hashlib.sha256(email.strip().lower().encode()).hexdigest()} if email else {'recipientPartyId':recipient})
 result=query(f"SELECT event_crm_command({event},{issuer},{lit(key)}::uuid,{lit(json.dumps(body))}::jsonb,{lit(hashlib.sha256(secret.encode()).hexdigest())});")
 return key,secret,result
def invite(secret,actor=2,accept=True):return query(f"SELECT event_crm_invitation_action({actor},{lit(hashlib.sha256(secret.encode()).hexdigest())},{str(accept).lower()});")
def snapshot(actor=1,event=10,filters=None):return query(f"SELECT event_crm_snapshot({event},{actor},{lit(json.dumps(filters or {}))}::jsonb);")
def apply(path):raw((ROOT/path).read_text())

subprocess.run([str(BIN/'createdb'),*ARGS,DB],env=ENV,check=True,capture_output=True)
try:
 apply('tdf-hq/test/integration/event_crm_fixture.sql')
 apply('tdf-hq/sql/2026-09-17_identity_reconciliation.sql')
 apply('tdf-hq/sql/2026-08-13_unified_checkout_core.sql')
 apply('tdf-hq/sql/2026-08-17_service_booking_manual_payments.sql')
 apply('tdf-hq/sql/2026-08-18_public_ticket_checkout_runtime.sql')
 apply('tdf-hq/sql/2026-10-06_ticket_manual_bank_transfer.sql')
 apply('tdf-hq/sql/2026-11-07_event_crm.sql')
 apply('tdf-hq/sql/2026-11-07_event_crm.sql')
 check('migration replay and default-off',snapshot().get('error')=='not_found')
 apply('tdf-hq/sql/2026-11-07_event_crm_rollback.sql')
 check('empty rollback removes only new CRM schema',query("SELECT to_regclass('event_crm_lead')::text;") is None and query('SELECT count(*) FROM party;')==7)
 apply('tdf-hq/sql/2026-11-07_event_crm.sql')
 raw("INSERT INTO event_crm_control(event_id,enabled,transport_required,updated_by) VALUES(10,true,true,1),(11,true,false,3);")
 check('outside-event read denied',snapshot(2,11).get('error')=='not_found')
 key,secret,result=issue()
 check('invitation creates exactly seven days',query(f"SELECT expires_at-issued_at=interval '7 days' FROM event_crm_invitation WHERE id={lit(key)}::uuid;") is True)
 check('another authenticated account refused',invite(secret,3).get('error')=='recipient_verification_required')
 preview=invite(secret,accept=False)
 check('preview binds title venue date role and permissions',preview['eventId']==10 and preview['role']=='coordinator' and len(preview['permissions'])==4 and 'venue' in preview and 'startTime' in preview)
 check('preview does not consume',query(f"SELECT accepted_at IS NULL FROM event_crm_invitation WHERE id={lit(key)}::uuid;") is True)
 with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
  race=list(pool.map(lambda _:invite(secret),range(2)))
 check('two concurrent acceptances produce one membership',sum('membershipId' in x for x in race)==1 and query('SELECT count(*) FROM event_crm_membership WHERE event_id=10 AND party_id=2;')==1)
 check('single-use link remains consumed',invite(secret).get('error')=='invitation_unavailable')
 membership=next(x['membershipId'] for x in race if 'membershipId' in x)
 check('coordinator has event-only access',snapshot(2)['role']=='coordinator' and snapshot(2,11).get('error')=='not_found')
 check('pending invitation revocation differs from membership removal',cmd(1,{'operation':'invitation.revoke','invitationId':key}).get('error')=='revoke_membership_separately')
 for variant in ['expired','revoked']:
  k,s,_=issue('sales',4)
  if variant=='expired':raw(f"UPDATE event_crm_invitation SET issued_at=issued_at-interval '8 days',expires_at=expires_at-interval '8 days' WHERE id={lit(k)}::uuid;")
  else:cmd(1,{'operation':'invitation.revoke','invitationId':k})
  check(variant+' invitation refused',invite(s,4).get('error')=='invitation_unavailable')
 check('coordinator cannot grant coordinator or verifier',issue('coordinator',4,issuer=2)[2].get('error')=='forbidden' and issue('payment_verifier',4,issuer=2)[2].get('error')=='forbidden')
 check('financial commands cannot be manufactured by coordinator',cmd(2,{'operation':'payment.approve','orderId':1}).get('error')=='invalid_request')
 k,s,_=issue('sales',4,issuer=2);invite(s,4)
 email_secret=uuid.uuid4().hex+uuid.uuid4().hex[:11]
 raw("UPDATE user_credential SET active=false WHERE party_id=3;")
 k,s,_=issue('sales',3,email='other@example.test')
 raw("UPDATE user_credential SET active=true WHERE party_id=3;")
 check('email recipient must verify possession',invite(s,3).get('error')=='recipient_verification_required')
 digest=hashlib.sha256(email_secret.encode()).hexdigest()
 query(f"SELECT event_crm_email_proof(3,{lit(digest)},false);")
 check('email proof is account-bound',query(f"SELECT event_crm_email_proof(4,{lit(digest)},true);").get('error')=='verification_unavailable')
 check('email proof consumed exactly once',query(f"SELECT event_crm_email_proof(3,{lit(digest)},true);").get('verified') and query(f"SELECT event_crm_email_proof(3,{lit(digest)},true);").get('error')=='verification_unavailable')
 check('verified matching email accepts',invite(s,3).get('role')=='sales')
 row={'name':'Customer A','phone':'0991234567','email':'customer@example.test','quantity':2,'stage':'paid'}
 preview=cmd(2,{'operation':'import.preview','rows':[row]})
 check('ambiguous normalized contact is flagged',preview['preview'][0].get('error')=='contact_conflict')
 row['contactId']=5
 row['responsibleId']=4
 preview=cmd(2,{'operation':'import.preview','rows':[row,row]})
 check('unreconciled spreadsheet paid is not a sale',preview['preview'][0]['stage']=='contacted' and preview['preview'][0]['paymentReconciliationRequired'])
 result=cmd(2,{'operation':'import.confirm','importId':preview['importId']})
 check('duplicate import rows create a single event contact',len(snapshot(2)['leads'])==1 and result['rows'][1]['existing'])
 count=query('SELECT count(*) FROM party;')
 repeated=cmd(2,{'operation':'import.confirm','importId':preview['importId']})
 check('repeated import has same contacts and links, no sales or holds',repeated['replayed'] and query('SELECT count(*) FROM party;')==count and query('SELECT count(*) FROM event_ticket_order;')==0)
 lead=snapshot(2)['leads'][0]
 check('seller sees only assigned contact and no full funnel',len(snapshot(4)['leads'])==1 and len(snapshot(3)['leads'])==0)
 check('seller cannot change ownership',cmd(4,{'operation':'lead.update','leadId':lead['id'],'expectedVersion':1,'responsibleId':3}).get('error')=='forbidden')
 action={'operation':'lead.update','leadId':lead['id'],'expectedVersion':1,'stage':'contacted','notes':'Called customer','nextAction':'Confirm attendance','followUpAt':'2026-11-01T15:00:00Z'}
 retry=str(uuid.uuid4());first=cmd(4,action,retry);second=cmd(4,action,retry)
 check('follow-up command retry is idempotent',first['version']==2 and second['replayed'])
 check('stage owner and overdue filters work',len(snapshot(2,filters={'stage':'contacted','responsibleId':4,'overdue':True})['leads'])==1)
 check('manual paid transition requires canonical order evidence',cmd(2,{'operation':'lead.update','leadId':lead['id'],'expectedVersion':2,'stage':'paid'}).get('error')=='order_evidence_required')
 new={'name':'Synthetic new contact','email':'new@example.test','phone':'+593998887766','stage':'reserved'}
 p=cmd(2,{'operation':'import.preview','rows':[new]});cmd(2,{'operation':'import.confirm','importId':p['importId']})
 check('new contact uses canonical identity service, reservation claim holds no places',query("SELECT count(*) FROM identity_contact_request WHERE request_key LIKE 'event_crm_%';")==1 and query('SELECT count(*) FROM event_ticket_order;')==0)
 raw("INSERT INTO event_crm_transport_slot(event_id,direction,departure_time) VALUES(10,'outbound','11:00'),(10,'return','17:00');")
 move=cmd(2,{'operation':'lead.update','leadId':lead['id'],'expectedVersion':2,'outboundTime':'11:00','returnTime':'17:00'})
 queued=cmd(2,{'operation':'waitlist.join','leadId':lead['id']})
 queued_again=cmd(2,{'operation':'waitlist.join','leadId':lead['id']})
 check('combination waitlist reuses one canonical queue record without occupying inventory',queued['waitlistId']==queued_again['waitlistId'] and query('SELECT count(*) FROM event_waitlist;')==1 and query('SELECT count(*) FROM event_ticket_order;')==0)
 edit=cmd(2,{'operation':'lead.update','leadId':lead['id'],'expectedVersion':3,'quantity':3})
 check('waitlist follows requested quantity without allocating stock',edit.get('version')==4 and query('SELECT quantity FROM event_waitlist;')==3)
 raw("SELECT setval('event_logistics_activity_id_seq',100);")
 created_task=cmd(2,{'operation':'task.save','title':'Preparar logística','notes':'Sólo ensayo','startTime':'2026-12-05T10:00:00-05:00'})
 updated_task=cmd(2,{'operation':'task.save','activityId':created_task['activityId'],'expectedVersion':1,'title':'Preparar logística revisada','notes':'Ensayo revisado','startTime':'2026-12-05T10:30:00-05:00'})
 check('coordinator reuses canonical logistics records with versioned edits',updated_task['version']==2 and cmd(2,{'operation':'task.save','activityId':created_task['activityId'],'expectedVersion':1,'title':'Stale','startTime':'2026-12-05T10:00:00-05:00'}).get('error')=='version_conflict')
 check('ambiguous local dates are rejected during import preview',cmd(2,{'operation':'import.preview','rows':[dict(new,followUpAt='01/12/2026') ]})['preview'][0].get('error')=='invalid_row_date_or_identifier')
 pending_key,pending_secret,_=issue('sales',7,issuer=2)
 task=cmd(2,{'operation':'task.assign','activityId':1,'responsibleId':4})
 check('coordinator assigns existing logistics task',task.get('assigned') and query('SELECT count(*) FROM event_logistics_assignment WHERE activity_id=1;')==1)
 check('seller cannot read another event or export full data',snapshot(4,11).get('error')=='not_found' and snapshot(4)['audit']==[])
 cmd(1,{'operation':'membership.revoke','membershipId':membership})
 check('membership revocation blocks reads writes and replay',snapshot(2).get('error')=='not_found' and cmd(2,{'operation':'task.assign','activityId':1,'responsibleId':4}).get('error')=='not_found')
 check('pending team invitation cannot outlive issuer authority',invite(pending_secret,7).get('error')=='invitation_unavailable')
 check('event owner retains access',snapshot(1)['role']=='owner')
 # Synchronize actual PostgreSQL row-lock waiters, not a timing-only race.
 for isolation in ['READ COMMITTED','REPEATABLE READ']:
  raw("UPDATE event_crm_membership SET revoked_at=NULL,revoked_by=NULL WHERE event_id=10 AND party_id=4;")
  reader=subprocess.Popen([PSQL,*ARGS,'-d',DB,'-X','-qAt','-v','ON_ERROR_STOP=1'],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True,env=ENV)
  reader.stdin.write('BEGIN ISOLATION LEVEL '+isolation+"; SELECT 'snapshot-ready' FROM event_crm_membership LIMIT 1;\n");reader.stdin.flush()
  assert reader.stdout.readline().strip()=='snapshot-ready'
  revoker=subprocess.Popen([PSQL,*ARGS,'-d',DB,'-X','-qAt','-v','ON_ERROR_STOP=1'],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True,env=ENV)
  revoker.stdin.write("BEGIN; UPDATE event_crm_membership SET revoked_at=clock_timestamp(),revoked_by=1 WHERE event_id=10 AND party_id=4 RETURNING 'revoke-locked';\n");revoker.stdin.flush()
  assert revoker.stdout.readline().strip()=='revoke-locked'
  reader.stdin.write("SET application_name='event_crm_revocation_waiter'; SELECT event_crm_snapshot(10,4,'{}'::jsonb); COMMIT;\n");reader.stdin.flush()
  waiting=False
  for _ in range(100):
   waiting=query("SELECT EXISTS(SELECT 1 FROM pg_stat_activity WHERE datname=current_database() AND application_name='event_crm_revocation_waiter' AND wait_event_type='Lock');")
   if waiting:break
   time.sleep(.02)
  assert waiting,'Reader must wait on the revocation authorization fence'
  revoker.stdin.write('COMMIT;\n');revoker.stdin.close();assert revoker.wait(timeout=5)==0
  reader.stdin.close();out=reader.stdout.read();error=reader.stderr.read();code=reader.wait(timeout=5)
  check('blocked '+isolation+' read loses authority after membership withdrawal',('not_found' in out and code==0) if isolation=='READ COMMITTED' else code!=0 and 'concurrent update' in error)
 digest=hashlib.sha256((uuid.uuid4().hex+uuid.uuid4().hex).encode()).hexdigest()
 query(f"SELECT event_crm_email_proof(3,{lit(digest)},false);")
 raw("UPDATE party SET primary_email='changed@example.test' WHERE id=3;")
 check('changing account email invalidates previous proof and pending challenge',query(f"SELECT event_crm_email_proof(3,{lit(digest)},true);").get('error')=='verification_unavailable' and query("SELECT EXISTS(SELECT 1 FROM event_crm_verified_email v JOIN party p ON p.id=v.party_id WHERE p.id=3 AND v.email_hash=encode(digest(event_crm_email(p.primary_email),'sha256'),'hex'));") is False)

 def blocked_order(_):return raw("INSERT INTO event_ticket_order(event_id,tier_id,buyer_party_id,quantity,amount_cents,currency,status) VALUES(10,1,'5',1,2000,'USD','pending');",False).returncode!=0
 with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:blocked=list(pool.map(blocked_order,range(2)))
 check('competing transport-bundle purchases both fail closed pending joint checkout',all(blocked) and query('SELECT count(*) FROM event_ticket_order;')==0)
 check('no grant or financial permission outside event is created',query("SELECT count(*) FROM event_crm_membership WHERE event_id=11;")==0)
 check('populated rollback refuses to destroy operational data',raw((ROOT/'tdf-hq/sql/2026-11-07_event_crm_rollback.sql').read_text(),False).returncode!=0 and snapshot(1)['role']=='owner')
 report={'suite':'event-crm-postgresql','checks':checks,'count':len(checks),'synthetic':True,'productionWrites':False,
   'jointCheckout':'Not implemented/activated. Combination orders refused by database trigger, including concurrent attempts.','databaseRemovedAfterRun':True}
 out=os.environ.get('TDF_EVENT_CRM_TEST_REPORT')
 if out:Path(out).write_text(json.dumps(report,indent=2)+'\n')
 print(json.dumps({'passed':len(checks),'failed':0,'combinationSalesEnabled':False}))
finally:
 subprocess.run([str(BIN/'dropdb'),*ARGS,DB],env=ENV,check=True,capture_output=True)
