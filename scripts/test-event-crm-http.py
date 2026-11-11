#!/usr/bin/env python3
"""Own random loopback database + actual production schema/migrations + real HTTP.
No production services, SMTP or provider credentials are loaded. Browser runner
starts only its own local Vite process and records synthetic desktop/mobile UI.
"""
import json,os,subprocess,shutil,tempfile,uuid
from pathlib import Path
root=Path(__file__).resolve().parent.parent
psql=shutil.which('psql') or '/usr/local/opt/postgresql@16/bin/psql'
pgbin=Path(psql).parent
port=os.environ.get('TDF_EVENT_CRM_TEST_PORT','5432');assert port.isdigit()
user=os.environ.get('TDF_EVENT_CRM_TEST_USER',os.environ.get('USER','postgres'))
db='tdf_event_crm_http_'+uuid.uuid4().hex[:12]+'_test'
env={k:v for k,v in os.environ.items() if not k.startswith('PG')}
env['PGPASSWORD']=os.environ.get('TDF_EVENT_CRM_TEST_PASSWORD','')
args=['-h','127.0.0.1','-p',port,'-U',user]
work=Path(tempfile.mkdtemp(prefix='tdf-event-crm-http-'))
def sql_file(path):
 with (work/'schema.log').open('a') as log:
  subprocess.run([psql,*args,'-d',db,'-X','-v','ON_ERROR_STOP=1','-f',str(path)],env=env,stdout=log,stderr=log,check=True)
subprocess.run([str(pgbin/'createdb'),*args,db],env=env,check=True,capture_output=True)
try:
 env['SOURCE_COMMIT']=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()
 with (work/'migrations.sql').open('w') as output:
  subprocess.run(['node','scripts/render-production-migration-batch.mjs'],cwd=root,env=env,stdout=output,check=True)
 for path in ['scripts/__tests__/fixtures/production-schema-20260814.sql','scripts/__tests__/fixtures/catalog-production-source-fixture.sql']:
  sql_file(root/path)
 sql_file(work/'migrations.sql')
 sql_file(root/'tdf-hq/sql/2026-11-07_event_crm.sql')
 sql_file(root/'tdf-hq/test/integration/ticket_manual_review_fixture.sql')
 sql_file(root/'tdf-hq/test/integration/event_crm_http_fixture.sql')
 # Optional local-only labels: no recipient, organizer, capacity or sale is seeded.
 private_pilot=os.environ.get('TDF_EVENT_CRM_PRIVATE_PILOT_JSON')
 if private_pilot:
  labels=json.loads(Path(private_pilot).read_text())
  def literal(value):
   assert isinstance(value,str) and 0<len(value)<=200
   return "'"+value.replace("'","''")+"'"
  overlay=work/'private-pilot.sql'
  overlay.write_text('UPDATE social_event SET title='+literal(labels['title'])+' WHERE id=7002;\n'
   +'UPDATE venue SET name='+literal(labels['venue'])+' WHERE id=7002;\n')
  sql_file(overlay)
 env.update(TDF_EVENT_CRM_DISPOSABLE_HTTP='1',TDF_EVENT_CRM_BROWSER='1',TDF_EVENT_CRM_HTTP_DSN=f'host=127.0.0.1 port={port} user={user} dbname={db}',
   TDF_EVENT_CRM_HTTP_RUNNER=str(root/'scripts/__tests__/event-crm-http.mjs'),TDF_EVENT_CRM_TEST_DB=db,
   TDF_EVENT_CRM_TEST_PORT=port,TDF_EVENT_CRM_TEST_USER=user,TDF_EVENT_CRM_PSQL=psql,TDF_EVENT_CRM_ARTIFACTS=str(work))
 env.update(COMMERCE_CHECKOUT_ENV='sandbox',COMMERCE_LOOKUP_TOKEN_SECRET=os.urandom(32).hex(),
   COMMERCE_BANK_TRANSFER_INSTRUCTIONS='Ensayo local: no transferir dinero. No es una cuenta bancaria real.')
 binary=root/'tdf-hq/.stack-work/event-crm-http/event-crm-http-test'
 binary.parent.mkdir(parents=True,exist_ok=True)
 subprocess.run(['stack','exec','--','ghc','-O0','-threaded','-package','tdf-hq','-itest','-outputdir',str(binary.parent),
   'test/EventCrmHttpMain.hs','-o',str(binary)],cwd=root/'tdf-hq',env=env,check=True)
 subprocess.run([str(binary)],cwd=root,env=env,check=True)
 print(json.dumps({'httpAndBrowser':'PASS','artifacts':str(work),'databaseRemovedAfterRun':True}))
except Exception:
 print(json.dumps({'httpAndBrowser':'FAILED','artifacts':str(work),'schemaLog':str(work/'schema.log')}))
 raise
finally:
 subprocess.run([str(pgbin/'dropdb'),*args,db],env=env,check=True,capture_output=True)
