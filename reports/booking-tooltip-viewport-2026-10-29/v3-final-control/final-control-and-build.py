from pathlib import Path
import subprocess,os,json,hashlib,base64
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028')
cache=Path('/private/tmp/tdf-correctness-cycle-20261028')
root=Path('/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028')
source=json.loads(Path('/private/tmp/tdf-booking-tooltip-receipts-v3-20261028/source-C2.json').read_text())['sourceCommit']
ui=work/'tdf-hq-ui/src/pages/PublicBookingPage.tsx'
e2e=work/'e2e/web/booking-catalog-recovery.spec.mjs'
def git(*a):return subprocess.check_output(['git','-C',str(work),*a])
def sha(b):return hashlib.sha256(b).hexdigest()
def require(ok,msg):
 if not ok:raise RuntimeError(msg)
require(git('rev-parse','HEAD').decode().strip()==source,'source drift')
require(not git('status','--porcelain').strip(),'source not clean')
correct=ui.read_bytes();test=e2e.read_bytes()
require(correct==git('show',source+':tdf-hq-ui/src/pages/PublicBookingPage.tsx'),'UI not committed')
require(test==git('show',source+':e2e/web/booking-catalog-recovery.spec.mjs'),'test not committed')
needle=b"                                      slotProps={{ popper: { popperOptions: { strategy: 'fixed' } } }}\n"
require(correct.count(needle)==1,'ambiguous mutation')
mutant=correct.replace(needle,b'',1)
executed=test.decode().replace("from '@playwright/test'", "from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs'").replace("from 'axe-core'", "from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/axe-core/axe.js'")
(root/'booking.spec.mjs').write_text(executed)
config=(root/'playwright.phone.config.mjs').read_text().replace('positive-initial-phone','negative-final-phone')
(root/'playwright.negative-final-phone.config.mjs').write_text(config)
runner=(root/'run-positive-initial-phone.py').read_text().replace('execution-positive-initial-phone.json','execution-negative-final-phone.json').replace('run-positive-initial-phone.log','run-negative-final-phone.log').replace('preview-positive-initial-phone.log','preview-negative-final-phone.log').replace('playwright.phone.config.mjs','playwright.negative-final-phone.config.mjs').replace('One permanent-regression initial phone positive case','One exact committed-harness sole-option negative case')
runner += "\nraise SystemExit(record['browserExit'] if record.get('browserExit') is not None else 1)\n"
(root/'run-negative-final-phone.py').write_text(runner)
env=os.environ.copy();env['PATH']='/Users/macpro/.nvm/versions/node/v24.13.1/bin:'+env['PATH'];env['CI']='true'
for key in ['VITE_UI_COMMIT','SOURCE_COMMIT','SOURCE_VERSION','GIT_SHA','TESTOMATIO']:env.pop(key,None)
handoff=Path('/Users/macpro/GitHub/tdf-app/.private/booking-tooltip-viewport-20261028/handoff.json')
def phase(**values):
 data=json.loads(handoff.read_text());data.update(values);handoff.write_text(json.dumps(data,indent=2)+'\n')
def build(label,command):
 made=[]
 try:
  for name in ['node_modules','tdf-hq-ui/node_modules']:
   path=work/name;target=cache/name
   require(not(path.exists() or path.is_symlink()),'unexpected dependency path')
   path.symlink_to(target,target_is_directory=True);made.append((path,target))
  with open('/private/tmp/tdf-booking-tooltip-'+label+'-20261028.log','w') as log:
   result=subprocess.run(command,cwd=work/'tdf-hq-ui',env=env,stdout=log,stderr=subprocess.STDOUT)
  require(result.returncode==0,label+' failed')
 finally:
  for path,target in reversed(made):
   require(path.is_symlink() and path.resolve()==target.resolve(),'alias ownership changed');path.unlink()
 return {str(f.relative_to(work/'tdf-hq-ui/dist')):sha(f.read_bytes()) for f in (work/'tdf-hq-ui/dist').rglob('*') if f.is_file()}
receipt={'sourceCommit':source,'correctUIFileSha256':sha(correct),'mutantUIFileSha256':sha(mutant),'trackedE2ESha256':sha(test),'executedE2ESha256':sha(executed.encode()),'harnessTransform':'Only two package imports point to the matched cache; complete final test bodies identical to committed source.','control':'Remove ONLY fixed positioning option; same final Date-only clock, actualTab, first root assertion and all subsequent positive assertions.'}
phase(currentPhase='FINAL_EXACT_COMMITTED_CONTROL_BUILD_RUNNING',mutantActive=True)
try:
 ui.write_bytes(mutant)
 receipt['mutantArtifacts']=build('mutant-v3-final-build',['node',str(cache/'node_modules/vite/bin/vite.js'),'build'])
 require(ui.read_bytes()==mutant,'mutant changed during build')
finally:
 ui.write_bytes(correct);require(ui.read_bytes()==correct,'source restore failed')
 phase(mutantActive=False,servedDistIsMutant=True)
receipt['correctSourceRestored']=True
(root/'final-sole-strategy-removal.json').write_text(json.dumps(receipt,indent=2)+'\n')
print('Final mutant built; exact committed UI restored. Running one negative case.',flush=True)
negative=subprocess.run(['python3',str(root/'run-negative-final-phone.py')],env=env)
execution=json.loads((root/'execution-negative-final-phone.json').read_text())
results=json.loads((root/'negative-final-phone.json').read_text())
found=[]
def visit(value):
 if isinstance(value,dict):
  for attachment in value.get('attachments',[]):
   if attachment['name']=='booking-viewport-geometry':
    found.append(json.loads(base64.b64decode(attachment['body'])) if attachment.get('body') else json.loads(Path(attachment['path']).read_text()))
  for child in value.values():visit(child)
 elif isinstance(value,list):
  for child in value:visit(child)
visit(results)
require(negative.returncode==1 and execution['browserExit']==1,'negative process not expected1')
require(results['stats']['unexpected']==1 and len(found)==1,'negative count/geometry not established')
geometry=found[0];initial=geometry['initial']
require(initial['overflow'] and initial['clientWidth']==320 and initial['scrollWidth']>320,'failure is not actual root overflow')
require('viewportMeasurement.overflow' in (root/'run-negative-final-phone.log').read_text(),'wrong failure point')
(root/'negative-final-geometry.json').write_text(json.dumps(geometry,indent=2)+'\n')
receipt['negative']={'driverExit':negative.returncode,'browserExit':execution['browserExit'],'unexpected':1,'initial':initial,'laterWidths':[(x['clientWidth'],x['scrollWidth']) for x in geometry['samples']],'cleanup':execution}
(root/'final-sole-strategy-removal.json').write_text(json.dumps(receipt,indent=2)+'\n')
phase(currentPhase='FINAL_EXACT_NEGATIVE_CONFIRMED_CORRECT_C_BUILD_RUNNING',finalExactNegative='1 actual phone failure at unchanged first root predicate; exactC harness; source restored',mutantActive=False,previewActive=False)
print('Exact final negative detected actual overflow; source restored. Building immutable correct candidate.',flush=True)
artifacts=build('source-C2-v3-build',['npm','run','build'])
require(ui.read_bytes()==correct and e2e.read_bytes()==test,'correct code changed')
require(not git('status','--porcelain').strip(),'source dirty after build')
require(any(source.encode() in p.read_bytes() for p in (work/'tdf-hq-ui/dist/assets').glob('*.js')),'compiled UI commit stamp absent')
(root/'correct-C-build-inputs.json').write_text(json.dumps({'sourceCommit':source,'sourceTree':git('rev-parse',source+'^{tree}').decode().strip(),'correctUIFileSha256':sha(correct),'trackedE2ESha256':sha(test),'artifactHashes':artifacts,'embeddedExactCommitFound':True,'deploymentOverridesRemoved':['VITE_UI_COMMIT','SOURCE_COMMIT','SOURCE_VERSION','GIT_SHA'],'scope':'Local immutable UI build and input/artifact hashes, not hosted deployment or whole-browser proof.'},indent=2)+'\n')
phase(currentPhase='IMMUTABLE_C_BUILD_PASS_FINAL_PERMANENT_MATRIX_PENDING',servedDistIsMutant=False,correctSourceCBuildPassed=True,previewActive=False)
print('Immutable source-C TypeScript/production/bundle-budget build passed; embedded commit verified; all own aliases removed.',flush=True)
