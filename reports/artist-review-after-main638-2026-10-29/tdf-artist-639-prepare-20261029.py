from pathlib import Path
import subprocess, os, json, hashlib, re
w = Path('/private/tmp/tdf-artist-639-after638-20261029')
o = Path('/private/tmp/tdf-artist-639-qualification-20261029')
c = Path('/private/tmp/tdf-correctness-cycle-20261028')
prep = '5788f07b0013a067c359f67a36290011e59f8414'
main = '6197b87326a437350dca239ee64902e78da6964c'
dd8 = 'dd8f2c9c14cc0b3c77ef9c84ab1a112a8c6b2f4d'
env = os.environ.copy()
env['PATH'] = '/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin'
env['PYTHONDONTWRITEBYTECODE'] = '1'
for key in ['TESTOMATIO','ADMIN_TOKEN','SPOTIFY_CLIENT_ID','SPOTIFY_CLIENT_SECRET','YOUTUBE_API_KEY','MUSICBRAINZ_USER_AGENT','DRIVE_CLIENT_ID','DRIVE_CLIENT_SECRET','DRIVE_REFRESH_TOKEN','DRIVE_UPLOAD_FOLDER_ID','UPLOAD_FOLDER_ID','TDF_API_BASE','API_BASE','DATABASE_URL']:
    env.pop(key, None)
env['NODE_OPTIONS'] = '--experimental-vm-modules'
def git(*args): return subprocess.check_output(['git','-C',str(w),*args])
def sha(data): return hashlib.sha256(data).hexdigest()
def run(label, args, expected=0, cwd=w):
    with (o/(label+'.txt')).open('w') as log:
        result = subprocess.run(args,cwd=cwd,env=env,stdout=log,stderr=subprocess.STDOUT)
    assert result.returncode == expected,(label,result.returncode)
    print(label+' actualexit'+str(expected),flush=True)
    return (o/(label+'.txt')).read_text()
assert git('rev-parse','HEAD').decode().strip()==prep
assert git('rev-parse','MERGE_HEAD').decode().strip()==main
conflicts={'formal/system/inventory.json','formal/system/traceability.json','docs/catalog-persistence/reports/static-list-inventory.json'}
assert set(git('diff','--name-only','--diff-filter=U').decode().splitlines())==conflicts
initial=json.loads((o/'initial-correspondence.json').read_text())
for p,h in initial['sevenOwnedInputHashes'].items(): assert sha((w/p).read_bytes())==h,p
assert (w/'package-lock.json').read_bytes()==(c/'package-lock.json').read_bytes()
assert (w/'tdf-hq-ui/package.json').read_bytes()==(c/'tdf-hq-ui/package.json').read_bytes()
made=[]; scans=[]; controls=[]
try:
    for n in ['node_modules','tdf-hq-ui/node_modules']:
        p,t=w/n,c/n;assert not(p.exists() or p.is_symlink());p.symlink_to(t,target_is_directory=True);made.append((p,t))
    # Existing tests, unchanged. Replace only one implementation file with the
    # actual current-main baseline and require the intended behavioral failures.
    p=w/'scripts/artist-enrichment.mjs';correct=p.read_bytes();baseline=git('show',main+':scripts/artist-enrichment.mjs')
    try:
        p.write_bytes(baseline)
        log=run('baseline-worker-rights-counterexamples',['node','--test','--test-name-pattern=external image HTTP','scripts/__tests__/artist-enrichment.test.mjs'],1)
        assert 'media_rights_review' in log and 'link_validation_broken' in log
        assert re.search(r'(?:#|ℹ)\s*fail\s+2\b',log),log[-2000:]
        controls.append({'baselineFile':'scripts/artist-enrichment.mjs','baselineSha256':sha(baseline),'actualExit':1,'actualBehavioralFailures':2,'point':'Persisted source type incorrectly conflates HTTP availability with unconfirmed reuse rights'})
    finally: p.write_bytes(correct)
    p=w/'tdf-hq-ui/src/pages/ArtistEnrichmentReviewPage.tsx';correct=p.read_bytes();baseline=git('show',main+':tdf-hq-ui/src/pages/ArtistEnrichmentReviewPage.tsx')
    try:
        p.write_bytes(baseline)
        run('baseline-identity-scope-counterexample',['node',str(c/'node_modules/jest/bin/jest.js'),'--runInBand','--runTestsByPath','src/pages/ArtistEnrichmentReviewPage.test.tsx','--testNamePattern=scopes identity counts','--json','--outputFile='+str(o/'baseline-identity-scope.json')],1,w/'tdf-hq-ui')
        result=json.loads((o/'baseline-identity-scope.json').read_text());assert result['numFailedTests']==1
        message='\n'.join(t.get('message','') for t in result['testResults'])
        assert 'Identidades (3)' in message and 'Unable to find role=' in message
        assert result['numRuntimeErrorTestSuites']==0
        assert 'ArtistEnrichmentReviewPage.test.tsx:67' in message
        controls.append({'baselineFile':'tdf-hq-ui/src/pages/ArtistEnrichmentReviewPage.tsx','baselineSha256':sha(baseline),'actualExit':1,'actualBehavioralFailures':1,'initialCountObservedByTest':4,'expectedSelectedCount':3,'actualAfterSelection':'Expected3absent; postselectionDOMcountnotcaptured. Oldunscopedsourcewouldretainall4 fixturecandidates byinspection.','point':'Existingselected-artistcount assertionfails, not a setup/runtimeerror'})
    finally: p.write_bytes(correct)
    log=run('correct-worker-unit',['node','--test','scripts/__tests__/artist-enrichment.test.mjs'])
    count=re.search(r'(?:#|ℹ)\s*pass\s+(\d+)\b',log);assert count and int(count[1])>0
    (o/'counterexamples.json').write_text(json.dumps({'controls':controls,'exactSourceRestored':True,'correctWorkerActualExit':0,'correctWorkerCases':int(count[1]),'scope':'Existing exact tests against actual-main baseline; all API/provider operations are fixture-intercepted.'},indent=2)+'\n')
    for fmt in ['json','csv']:
        log=(o/('strict-catalog-'+fmt+'.txt')).open('w')
        proc=subprocess.Popen(['node','scripts/catalog-list-audit.mjs','--decisions','docs/catalog-persistence/catalog-list-decisions.json','--fail-on-unreviewed','--format',fmt,'--output',str(o/('current-catalog.'+fmt))],cwd=w,env=env,stdout=log,stderr=subprocess.STDOUT)
        scans.append((fmt,proc,log))
    codes=[]
    for fmt,proc,log in scans:
        code=proc.wait();log.close();codes.append(code);print('strictcatalog '+fmt+' actualexit'+str(code),flush=True)
    assert codes==[0,0]
    for fmt,dest in [('json','static-list-inventory.json'),('csv','list-consumer-matrix.csv')]:
        (w/'docs/catalog-persistence/reports'/dest).write_bytes((o/('current-catalog.'+fmt)).read_bytes())
    for label,script in [('inventory','specification-inventory'),('conformance','specification-conformance')]:
        run(label,['python3','scripts/'+script+'.py']);run(label+'-check',['python3','scripts/'+script+'.py','--check'])
finally:
    for fmt,proc,log in scans:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
        log.close()
    for p,t in reversed(made):assert p.is_symlink() and p.resolve()==t.resolve();p.unlink()
for p,h in initial['sevenOwnedInputHashes'].items():assert sha((w/p).read_bytes())==h,p
incoming=json.loads(git('show',main+':formal/system/requirements.json'))['requirements']
rows=json.loads((w/'formal/system/requirements.json').read_text())['requirements'];byid={r['id']:r for r in rows}
assert len(incoming)==156 and len(rows)==157 and all(byid[r['id']]==r for r in incoming)
assert byid['ART-ENRICH-REVIEW-001']==next(r for r in json.loads(git('show',dd8+':formal/system/requirements.json'))['requirements'] if r['id']=='ART-ENRICH-REVIEW-001')
run('source-diff-check',['git','diff','--check','--',':(exclude)reports/**',':(exclude)formal/system/evidence/**'])
run('resolve-generated-conflicts',['git','add','--',*sorted(conflicts),'docs/catalog-persistence/reports/list-consumer-matrix.csv'])
assert not git('diff','--name-only','--diff-filter=U').strip()
run('source-merge-commit',['git','commit','-m','Merge protected main into artist review correction'])
source=git('rev-parse','HEAD').decode().strip()
assert git('show','-s','--format=%P',source).decode().split()==[prep,main]
assert not git('status','--porcelain').strip()
(o/'source-M.json').write_text(json.dumps({'sourceCommit':source,'baselineMain':main,'reusedPrep':prep,'publishedDD8':dd8,'sixFunctionalFilesAndRunbookExactDD8':True,'all156IncomingMainRequirementsPlusOwnARTPreserved':True,'clean':True,'fullUIAndRepositoryQualification':'PENDING'},indent=2)+'\n')
print('Current-main artist source committed '+source+'; full-source qualification next.',flush=True)
