from pathlib import Path
import subprocess,os,json,hashlib,re,datetime
root=Path('/private/tmp/tdf-ledger-protected-main-20261025');out=Path('/private/tmp/tdf-ledger-combined-aggregate-v2-results-20261025');out.mkdir(mode=0o700)
binary=root/'tdf-hq/.stack-work/dist/x86_64-osx/ghc-9.10.3/build/tdf-hq-test/tdf-hq-test'
env={k:v for k,v in os.environ.items() if k in ['HOME','PATH','USER','LOGNAME','SHELL','LANG','LC_CTYPE','TZ','TMPDIR']}
report={'sourceCandidate':subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip(),'worktreeDirty':bool(subprocess.check_output(['git','status','--porcelain'],cwd=root)),'startedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'testBinarySha256':hashlib.sha256(binary.read_bytes()).hexdigest(),'stackVersion':subprocess.check_output(['stack','--numeric-version'],text=True).strip(),'compiler':'GHC9.10.3, observed in canonical Stack build log','workingDirectory':str(root/'tdf-hq'),'seed':20261025,'runs':[],'productionOperations':False}
assert not report['worktreeDirty'];assert 'with ghc-9.10.3' in Path('/tmp/tdf-ledger-combined-stack-20261025.log').read_text()
paths=subprocess.check_output(['git','ls-files','tdf-hq'],cwd=root,text=True).splitlines();report['sourceSha256']={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in paths if p.endswith(('.hs','.cabal','.yaml','.lock'))}
for group in [None,'music-unit-domain','music-unit-content','music-unit-ddex','music-unit-storage']:
 label=group or 'aggregate';args=[str(binary),'--seed=20261025','--fail-on=empty']+(['--match='+group] if group else [])
 result=subprocess.run(args,cwd=root/'tdf-hq',env=env,capture_output=True,text=True,timeout=300);(out/(label+'.log')).write_text(result.stdout+result.stderr)
 summary=re.search(r'(\d+) examples?, (\d+) failures?(?:, (\d+) pending)?',result.stdout);row={'name':label,'exitCode':result.returncode,'summary':summary[0] if summary else None};report['runs'].append(row);(out/'receipt.json').write_text(json.dumps(report,indent=2)+'\n')
 assert result.returncode==0 and summary and int(summary[1])>0 and int(summary[2])==0,(label,result.stdout[-3000:],result.stderr[-1500:])
 if group:assert int(summary[3] or 0)==0
 print(label,row['summary'],flush=True)
assert hashlib.sha256(binary.read_bytes()).hexdigest()==report['testBinarySha256'];assert all(hashlib.sha256((root/p).read_bytes()).hexdigest()==h for p,h in report['sourceSha256'].items())
report['finishedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat();report['passed']=True;(out/'receipt.json').write_text(json.dumps(report,indent=2)+'\n')
