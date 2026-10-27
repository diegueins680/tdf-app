from pathlib import Path
import datetime,hashlib,json,os,socket,subprocess,sys,tempfile
assert sys.platform=='darwin' and __debug__
candidate=Path('/private/tmp/tdf-music-source-main608-20261027')
def git(*args):return subprocess.check_output(['git',*args],cwd=candidate,text=True).strip()
source=git('rev-parse','HEAD');main='301810ae3861ea36b1fa790beee940bfe2461a02'
assert not git('status','--porcelain','--untracked-files=no')
assert subprocess.run(['git','merge-base','--is-ancestor',main,source],cwd=candidate).returncode==0
manifest=json.loads((candidate/'scripts/production-migrations.json').read_text())['migrations'];assert len(manifest)==227
inputs=['scripts/test-music-source-projection.mjs','scripts/render-production-migration-batch.mjs','scripts/production-migrations.json','scripts/lib/production-release.mjs','scripts/lib/music-test-database.mjs','scripts/__tests__/fixtures/production-schema-20260814.sql','scripts/__tests__/fixtures/catalog-production-source-fixture.sql','tdf-hq/test/TDF/MusicPublicationSpec.hs','tdf-hq/src/TDF/Server/MusicRelease.hs']+[x['path'] for x in manifest]
hashes={x:hashlib.sha256((candidate/x).read_bytes()).hexdigest() for x in inputs}
assert subprocess.check_output(['node','--version'],text=True).strip()=='v22.14.0'
sock=socket.socket();sock.bind(('127.0.0.1',0));port=sock.getsockname()[1];sock.close()
out=Path(tempfile.mkdtemp(prefix='tdf-music-source-227-sql-20261027-',dir='/private/tmp'));out.chmod(0o700);print(out,flush=True)
user=subprocess.check_output(['id','-un'],text=True).strip();env={k:v for k,v in os.environ.items() if k in ['HOME','USER','LOGNAME','PATH','LANG','LC_CTYPE','SHELL','TZ']};env['PATH']='/usr/local/opt/postgresql@16/bin:'+env['PATH'];env.update(PGHOST='127.0.0.1',PGPORT=str(port),PGUSER=user,PGPASSFILE=str(out/'no-passfile'),TMPDIR=str(out),CI='false')
url=f'postgresql://{user}@127.0.0.1:{port}/tdf_music_source_permissions_20261027_test'
profile=f'(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:{port}"))(deny network-inbound)'
r={'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source':source,'protectedMain':main,'migrations':227,'status':'RUNNING','scope':'New actual PostgreSQL16 full227 candidate apply/replay/readiness, four real SQL projections and scoped negative controls. Historical c66 baseline/35a223 execution retained separately, not relabeled. No HTTP/media/provider/production or universal correctness claim.','inputSha256':hashes,'protocolSha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'productionWrites':False,'newHaskellCompilation':False,'steps':[],'cleanup':{}}
started=False
def save():(out/'results.json').write_text(json.dumps(r,indent=2)+'\n')
def run(name,args,extra=None,timeout=240):
 with (out/(name+'.log')).open('wb') as log:code=subprocess.run(args,cwd=candidate,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,timeout=timeout).returncode
 r['steps'].append({'name':name,'exitCode':code});save();assert code==0,(name,code);print(name+' PASS',flush=True)
def sql(text):return subprocess.check_output(['psql','-XqAt','-v','ON_ERROR_STOP=1','-d','postgres','-c',text],env=env,text=True).strip()
try:
 (out/'socket').mkdir();run('init',['initdb','-D',str(out/'cluster'),'--username='+user,'--encoding=UTF8','--locale=en_US.UTF-8','--auth=trust'])
 run('start',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o',f'-p {port} -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
 listeners=subprocess.check_output(['lsof','-nP',f'-iTCP:{port}','-sTCP:LISTEN'],text=True);assert f'127.0.0.1:{port}' in listeners and f'*:{port}' not in listeners
 r['postgresVersion']=sql('SELECT version()');before=sql('SELECT datname FROM pg_database ORDER BY datname')
 run('candidate',['sandbox-exec','-p',profile,'node','scripts/test-music-source-projection.mjs'],extra={'TDF_MUSIC_WORKER_TEST_DATABASE_URL':url})
 text=(out/'candidate.log').read_text();receipts=[json.loads(x) for x in text.splitlines() if x.startswith('{"kind":"music-source-projection-qualification"')];assert len(receipts)==1;receipt=receipts[0];assert receipt['sourceRevision']==source and receipt['migrations']==227 and receipt['surfaces']==4 and receipt['policyRemovalControls']==4 and receipt['bindingArgumentControls']==8
 assert 'PASS schema: complete 227-entry canonical apply/replay' in text and 'Cleaned only this run' in text;assert sql('SELECT datname FROM pg_database ORDER BY datname')==before
 for path,digest in hashes.items():assert hashlib.sha256((candidate/path).read_bytes()).hexdigest()==digest,path
 assert git('rev-parse','HEAD')==source;r['qualification']=receipt;r['cleanup']['ownedDatabasesRemoved']=True;r['status']='PASS_SCOPED_FULL227_SQL_AND_BINDING_CONTROLS'
except BaseException as error:
 r['status']='FAILED';r['failureType']=type(error).__name__;r['failure']=str(error);save();raise
finally:
 if started:run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);r['cleanup']['clusterStopped']=True
 r['cleanup']['postgresListenerClosed']=subprocess.run(['lsof',f'-tiTCP:{port}','-sTCP:LISTEN'],capture_output=True).returncode!=0
 r['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
