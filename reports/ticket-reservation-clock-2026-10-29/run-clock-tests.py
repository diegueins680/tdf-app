from pathlib import Path
import os,subprocess,json,hashlib,sys,time
work=Path('/private/tmp/tdf-correctness-cycle-20261028');out=Path('/private/tmp/tdf-ticket-admission-clock-20261029');label=sys.argv[1];selector=sys.argv[2] if len(sys.argv)>2 else 'ticket-reservation-clock';env=os.environ.copy()
for k in ['PGHOSTADDR','PGSERVICE','PGSERVICEFILE']:env.pop(k,None)
env.update({'TDF_PROVIDER_RETRY_DATABASE_URL':'postgresql://postgres@127.0.0.1:55728/tdf_provider_retry_test','TDF_TICKET_PAYMENT_GATE_TEST_DB':os.environ.get('TDF_CLOCK_GATE_DB','postgresql://postgres@127.0.0.1:55728/tdf_provider_retry_test'),'PGOPTIONS':'-c statement_timeout=30000 -c lock_timeout=20000','COMMERCE_CHECKOUT_ENV':'sandbox','COMMERCE_LOOKUP_TOKEN_SECRET':'synthetic-ticket-payment-gate-lookup-secret-000000','COMMERCE_EVENT_ENCRYPTION_KEY':'0'*64,'COMMERCE_BANK_TRANSFER_INSTRUCTIONS':'Synthetic test account; no transfer is ever made'})
psql='/usr/local/opt/postgresql@16/bin/psql'
def sql(text):return subprocess.check_output([psql,env['TDF_PROVIDER_RETRY_DATABASE_URL'],'-X','-qAt','-v','ON_ERROR_STOP=1','-c',text],env=env).decode().strip()
assert sql('SHOW data_directory')=='/private/tmp/tdf-correctness-cycle-pg-20261028';assert sql('SELECT current_database()')=='tdf_provider_retry_test';assert sql('SELECT count(*) FROM tdf_schema_migration')=='227'
paths=['tdf-hq/src/TDF/Ticketing/Inventory.hs','tdf-hq/src/TDF/Server/SocialEventsHandlers.hs','tdf-hq/src/TDF/Server/EventTicketCheckout.hs','tdf-hq/test/TDF/Commerce/ProviderRetrySpec.hs','tdf-hq/test/TDF/TicketPaymentGateSpec.hs'];hashes={p:hashlib.sha256((work/p).read_bytes()).hexdigest() for p in paths}
args=['/Users/macpro/.ghcup/bin/stack','--no-install-ghc','--stack-yaml','tdf-hq/stack.yaml','test','tdf-hq:test:tdf-hq-test','--test-arguments='+(('--match='+selector+' --fail-on=empty ') if selector!='FULL' else '')+'--seed=20261029 +RTS -N2 -RTS']
receipt={'head':subprocess.check_output(['git','-C',str(work),'rev-parse','HEAD']).decode().strip(),'startedAt':time.time(),'inputHashes':hashes,'command':args,'schemaMigrations':227,'providerDatabase':'tdf_provider_retry_test','gateDatabase':env['TDF_TICKET_PAYMENT_GATE_TEST_DB'].split('/')[-1],'scope':'In-process actual handlers/SQL, synthetic fixtures; no HTTP authentication/provider completion or production claim.'}
(out/(label+'-inputs.json')).write_text(json.dumps(receipt,indent=2)+'\n')
with (out/(label+'.txt')).open('w') as f:r=subprocess.run(args,cwd=work,env=env,stdout=f,stderr=subprocess.STDOUT)
receipt.update({'actualExit':r.returncode,'finishedAt':time.time(),'inputHashesStable':all(hashlib.sha256((work/p).read_bytes()).hexdigest()==h for p,h in hashes.items())});(out/(label+'-execution.json')).write_text(json.dumps(receipt,indent=2)+'\n');assert receipt['inputHashesStable'],'Input drift during actual checks'
print(label+' actual Stack exit '+str(r.returncode),flush=True);raise SystemExit(r.returncode)
