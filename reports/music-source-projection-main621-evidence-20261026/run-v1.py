from pathlib import Path
import datetime, hashlib, json, os, socket, subprocess, sys, tempfile

assert sys.platform == 'darwin' and not sys.flags.optimize
candidate = Path('/private/tmp/tdf-music-source-permissions-main621-20261026')
baseline = Path('/private/tmp/tdf-music-source-main621-baseline-20261026')
source = '5a33e3f6c56c0e1fb0b7737cb7dfafb8f113c637'
main = '78fa351b469c1d2031df3232204dd722ac480e3b'
def git(root, *args): return subprocess.check_output(['git', *args], cwd=root, text=True).strip()
assert git(candidate, 'rev-parse', 'HEAD') == source
assert git(baseline, 'rev-parse', 'HEAD') == main
handler = 'tdf-hq/src/TDF/Server/MusicRelease.hs'
subprocess.run(['git','diff','--exit-code','744e75fcd2d0ec74e988436b2ccbad7c9cea5eaa',source,'--',handler],cwd=candidate,check=True)
subprocess.run(['git','diff','--exit-code',main,'--','tdf-hq'],cwd=baseline,check=True)
inputs = ['scripts/test-music-source-projection.mjs','scripts/render-production-migration-batch.mjs','scripts/production-migrations.json','scripts/lib/production-release.mjs','scripts/lib/music-test-database.mjs','scripts/__tests__/fixtures/production-schema-20260814.sql','scripts/__tests__/fixtures/catalog-production-source-fixture.sql','tdf-hq/test/TDF/MusicPublicationSpec.hs']
for name in inputs: assert (candidate/name).read_bytes() == (baseline/name).read_bytes(), name
assert subprocess.check_output(['node','--version'],text=True).strip() == 'v22.14.0'
assert subprocess.run(['lsof','-tiTCP:55594','-sTCP:LISTEN'],capture_output=True).returncode != 0
out=Path(tempfile.mkdtemp(prefix='tdf-music-source-main621-20261026-',dir='/private/tmp'));out.chmod(0o700)
print(out,flush=True)
env={k:v for k,v in os.environ.items() if k in ['HOME','USER','LOGNAME','PATH','LANG','LC_CTYPE','SHELL','TZ']}
env['PATH']='/usr/local/opt/postgresql@16/bin:'+env['PATH']
env.update(PGHOST='127.0.0.1',PGPORT='55594',PGUSER=os.getlogin(),PGPASSFILE=str(out/'no-passfile'),TMPDIR=str(out),CI='false')
url='postgresql://'+env['PGUSER']+'@127.0.0.1:55594/tdf_music_source_permissions_20261026_test'
profile='(version 1)(allow default)(deny network-outbound)(allow network-outbound (remote tcp "localhost:55594"))(deny network-inbound)'
report={'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source':source,'mainBaseline':main,'status':'RUNNING','newCompilation':False,'productionWrites':False,'steps':[],'cleanup':{},'inputSha256':{name:hashlib.sha256((candidate/name).read_bytes()).hexdigest() for name in inputs},'handlerHashes':{'baseline':hashlib.sha256((baseline/handler).read_bytes()).hexdigest(),'candidate':hashlib.sha256((candidate/handler).read_bytes()).hexdigest()},'protocolSha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'scope':'Real disposable PostgreSQL16 full222 canonical schema and same source runner on main and candidate. FiniteSQL/lexical tests, not nativeHTTP/media/production proof. Baseline adds only untracked qualification runner; candidate application code stays frozen.'}
started=False
def save(): (out/'results.json').write_text(json.dumps(report,indent=2)+'\n')
def run(name,args,cwd=candidate,extra=None,expected=0,timeout=180):
    with (out/(name+'.log')).open('wb') as log:
        code=subprocess.run(args,cwd=cwd,env={**env,**(extra or {})},stdout=log,stderr=subprocess.STDOUT,timeout=timeout).returncode
    report['steps'].append({'name':name,'exitCode':code,'expectedExitCode':expected});save()
    assert code==expected,(name,code)
    print(name+' expected outcome observed',flush=True)
def sql(query):return subprocess.check_output(['psql','-XqAt','-v','ON_ERROR_STOP=1','-d','postgres','-c',query],env=env,text=True).strip()
try:
    (out/'socket').mkdir()
    run('init',['initdb','-D',str(out/'cluster'),'--username='+env['PGUSER'],'--encoding=UTF8','--locale=en_US.UTF-8','--auth=trust'])
    run('start',['pg_ctl','-D',str(out/'cluster'),'-l',str(out/'postgres.log'),'-o','-p 55594 -h 127.0.0.1 -k '+str(out/'socket'),'start']);started=True
    listeners=subprocess.check_output(['lsof','-nP','-iTCP:55594','-sTCP:LISTEN'],text=True)
    assert '127.0.0.1:55594' in listeners and '*:55594' not in listeners
    report['postgresVersion']=sql('SELECT version()')
    before=sql('SELECT datname FROM pg_database ORDER BY datname')
    run('baseline',['sandbox-exec','-p',profile,'node','scripts/test-music-source-projection.mjs'],cwd=baseline,extra={'TDF_MUSIC_WORKER_TEST_DATABASE_URL':url},expected=1)
    text=(out/'baseline.log').read_text();failures=[line for line in text.splitlines() if line.startswith('FAIL:')]
    assert len(failures)==10 and 'Source arrays must agree with authorized delivery' in text
    assert 'PASS schema: complete 222-entry canonical apply/replay' in text
    assert 'Cleaned only this run' in text and sql('SELECT datname FROM pg_database ORDER BY datname')==before
    report['baselineAgreementFailures']=failures
    run('candidate',['sandbox-exec','-p',profile,'node','scripts/test-music-source-projection.mjs'],extra={'TDF_MUSIC_WORKER_TEST_DATABASE_URL':url})
    text=(out/'candidate.log').read_text();receipts=[json.loads(line) for line in text.splitlines() if line.startswith('{"kind":"music-source-projection-qualification"')]
    assert len(receipts)==1 and receipts[0]['sourceRevision']==source and receipts[0]['migrations']==222
    assert receipts[0]['surfaces']==4 and receipts[0]['policyRemovalControls']==4 and receipts[0]['bindingArgumentControls']==8
    assert sql('SELECT datname FROM pg_database ORDER BY datname')==before
    for name,digest in report['inputSha256'].items():assert hashlib.sha256((candidate/name).read_bytes()).hexdigest()==digest
    assert git(candidate,'rev-parse','HEAD')==source and hashlib.sha256((candidate/handler).read_bytes()).hexdigest()==report['handlerHashes']['candidate']
    report['candidate']=receipts[0];report['cleanup']['ownedDatabasesRemoved']=True;report['status']='PASS_SCOPED_FULL222_SQL_AND_BINDING_CONTROLS'
finally:
    if started:run('stop',['pg_ctl','-D',str(out/'cluster'),'-m','fast','-w','stop']);report['cleanup']['clusterStopped']=True
    report['cleanup']['postgresListenerClosed']=subprocess.run(['lsof','-tiTCP:55594','-sTCP:LISTEN'],capture_output=True).returncode!=0
    report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();save()
