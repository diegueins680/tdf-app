from pathlib import Path
import os,subprocess,json,hashlib
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028');cache=Path('/private/tmp/tdf-correctness-cycle-20261028');out=Path('/private/tmp/tdf-booking-resize-phase-source-20261029');out.mkdir();base='a9095bc3e3bf53f5a76244564472fe53cfff7691'
env=os.environ.copy();env['PATH']='/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin';env['PYTHONDONTWRITEBYTECODE']='1'
def git(*a):return subprocess.check_output(['git','-C',str(work),*a])
def run(label,args):
 with (out/(label+'.txt')).open('w') as f:r=subprocess.run(args,cwd=work,env=env,stdout=f,stderr=subprocess.STDOUT)
 assert r.returncode==0,(label,r.returncode);print(label+' actualexit0',flush=True)
assert git('rev-parse','HEAD').decode().strip()==base
controls=json.loads(Path('/private/tmp/tdf-booking-resize-phase-controls-20261029/controls.json').read_text());assert controls['exactCorrectSourceAndDistRestored']
for name in ['final','uninstrumented']:
 d=json.loads(Path('/private/tmp/tdf-booking-resize-phase-'+name+'-matrix-20261029/matrix.json').read_text());assert d['status']=='PASS' and d['inputs']['workingFinalTestSha256']==hashlib.sha256((work/'e2e/web/booking-catalog-recovery.spec.mjs').read_bytes()).hexdigest()
run('inventory',['python3','scripts/specification-inventory.py']);run('conformance',['python3','scripts/specification-conformance.py']);run('inventory-check',['python3','scripts/specification-inventory.py','--check']);run('conformance-check',['python3','scripts/specification-conformance.py','--check'])
made=[]
try:
 for name in ['node_modules','tdf-hq-ui/node_modules']:
  p=work/name;t=cache/name;assert not(p.exists() or p.is_symlink()),str(p);p.symlink_to(t,target_is_directory=True);made.append((p,t))
 run('root-quality',['bash','scripts/quality-repo.sh']);run('catalog-unit',['node','--test','scripts/__tests__/catalog-list-audit.test.mjs'])
 run('fresh-catalog',['node','scripts/catalog-list-audit.mjs','--decisions','docs/catalog-persistence/catalog-list-decisions.json','--fail-on-unreviewed','--output',str(out/'fresh-catalog.json')])
finally:
 for p,t in reversed(made):assert p.is_symlink() and p.resolve()==t.resolve();p.unlink()
fresh=json.loads((out/'fresh-catalog.json').read_text());tracked=json.loads((work/'docs/catalog-persistence/reports/static-list-inventory.json').read_text());fresh.pop('generatedAt');tracked.pop('generatedAt');assert fresh==tracked
for p in ['formal/system/requirements.json','docs/catalog-persistence/catalog-list-decisions.json','docs/catalog-persistence/reports/static-list-inventory.json','docs/catalog-persistence/reports/list-consumer-matrix.csv']:assert (work/p).read_bytes()==git('show',base+':'+p),p
run('syntax',['node','--check','e2e/web/booking-catalog-recovery.spec.mjs']);run('diff-check',['git','diff','--check'])
(out/'qualification.json').write_text(json.dumps({'baseline':base,'appUIUnchanged':True,'strictCatalogCurrent1352UnchangedExceptClock':True,'wholeHistoricalCatalogFileUnchanged':True,'finalPhaseBrowserCases':'9instrumented+9uninstrumented/0skip/0retry','controlledFaults':'helper-only edgefailure withroot320;sole-option rootfailure362/320;synthetic/missing-native ack fails8s','rootAndIndices':'PASS','purpose':'Explicit specification/harness phase correction; historical initial-overlay failures retained; hosted qualification pending'},indent=2)+'\n');print('PHASE quality/catalog/indices complete0; historical requirements/catalog preserved.',flush=True)
