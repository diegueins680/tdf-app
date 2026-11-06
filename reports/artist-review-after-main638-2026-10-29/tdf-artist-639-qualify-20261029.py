from pathlib import Path
import subprocess,os,json,hashlib,re
w=Path('/private/tmp/tdf-artist-639-after638-20261029')
c=Path('/private/tmp/tdf-correctness-cycle-20261028')
o=Path('/private/tmp/tdf-artist-639-qualification-20261029')
source=json.loads((o/'source-M.json').read_text())['sourceCommit']
env=os.environ.copy();env['PATH']='/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin';env['PYTHONDONTWRITEBYTECODE']='1'
for key in ['TESTOMATIO','VITE_UI_COMMIT','SOURCE_COMMIT','SOURCE_VERSION','GIT_SHA','ADMIN_TOKEN','SPOTIFY_CLIENT_ID','SPOTIFY_CLIENT_SECRET','YOUTUBE_API_KEY','DRIVE_CLIENT_ID','DRIVE_CLIENT_SECRET','DRIVE_REFRESH_TOKEN','DATABASE_URL','TDF_API_BASE','API_BASE']:
    env.pop(key,None)
def git(*args):return subprocess.check_output(['git','-C',str(w),*args])
def sha(data):return hashlib.sha256(data).hexdigest()
assert git('rev-parse','HEAD').decode().strip()==source
assert not git('status','--porcelain').strip()
inputs=git('ls-tree','-r','--name-only',source,'--','tdf-hq-ui','scripts/artist-enrichment.mjs','scripts/__tests__/artist-enrichment.test.mjs','package.json','package-lock.json').decode().splitlines()
hashes={p:sha((w/p).read_bytes()) for p in inputs}
for p in inputs:assert (w/p).read_bytes()==git('show',source+':'+p),p
assert (w/'package-lock.json').read_bytes()==(c/'package-lock.json').read_bytes()
assert (w/'tdf-hq-ui/package.json').read_bytes()==(c/'tdf-hq-ui/package.json').read_bytes()
(o/'full-validation-inputs.json').write_text(json.dumps({'sourceCommit':source,'gitAndWorkingInputHashes':hashes,'lockedRootAndUIManifestsMatchCache':True,'environment':'Node24.13.1;Testomatioandprovidercredentialsunset;onlyownedtemporarydependencyaliases','scope':'UI/source-input identity, not complete installed-package or compiler attestation.'},indent=2)+'\n')
made=[];jobs=[]
try:
    for name in ['node_modules','tdf-hq-ui/node_modules']:
        p,t=w/name,c/name;assert not(p.exists() or p.is_symlink());p.symlink_to(t,target_is_directory=True);made.append((p,t))
    for label,cmd in [('ui-quality',['bash','scripts/quality-ui.sh']),('root-quality',['bash','scripts/quality-repo.sh'])]:
        log=(o/(label+'.txt')).open('w');proc=subprocess.Popen(cmd,cwd=w,env=env,stdout=log,stderr=subprocess.STDOUT);jobs.append((label,proc,log));print(label+' startedPID'+str(proc.pid),flush=True)
    results={}
    for label,proc,log in jobs:
        code=proc.wait();log.close();results[label]=code;print(label+' actualexit'+str(code),flush=True)
    assert all(code==0 for code in results.values()),results
    with (o/'catalog-unit.txt').open('w') as log:r=subprocess.run(['node','--test','scripts/__tests__/catalog-list-audit.test.mjs'],cwd=w,env=env,stdout=log,stderr=subprocess.STDOUT)
    assert r.returncode==0
finally:
    for label,proc,log in jobs:
        if proc.poll() is None:proc.terminate();proc.wait(timeout=15)
        log.close()
    for p,t in reversed(made):assert p.is_symlink() and p.resolve()==t.resolve();p.unlink()
assert git('rev-parse','HEAD').decode().strip()==source
for p,h in hashes.items():assert sha((w/p).read_bytes())==h,p
assert not git('status','--porcelain').strip()
ui=(o/'ui-quality.txt').read_text();ui=re.sub(r'\x1b\[[0-9;]*m','',ui)
tests=re.findall(r'Tests:\s+(\d+) passed, (\d+) total',ui);suites=re.findall(r'Test Suites:\s+(\d+) passed, (\d+) total',ui)
assert tests and suites and tests[-1][0]==tests[-1][1] and suites[-1][0]==suites[-1][1],ui[-2000:]
dist={str(p.relative_to(w/'tdf-hq-ui/dist')):sha(p.read_bytes()) for p in (w/'tdf-hq-ui/dist').rglob('*') if p.is_file()}
assert dist and any(source.encode() in p.read_bytes() for p in (w/'tdf-hq-ui/dist/assets').glob('*.js'))
qualification={'sourceCommit':source,'actualExitCodes':results,'fullUIPassedCases':int(tests[-1][0]),'fullUIPassedSuites':int(suites[-1][0]),'lintTypecheckProductionBuildAndBudget':'PASS canonicalquality-ui','rootQuality':'PASS canonicalquality-repo','catalogUnitCases':8,'allUIAndOwnedInputsStableAndMatchCommittedSource':True,'ownedAliasesRemoved':True,'distArtifactHashes':dist,'browserQualification':'PENDING','formalAssurance':'PENDING clean-source current report; no universal proof from tests'}
(o/'full-quality.json').write_text(json.dumps(qualification,indent=2)+'\n');print(json.dumps({k:v for k,v in qualification.items() if k!='distArtifactHashes'},indent=2),flush=True)
