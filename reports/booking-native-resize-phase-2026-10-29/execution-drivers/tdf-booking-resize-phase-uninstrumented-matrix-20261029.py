from pathlib import Path
import subprocess,os,json,hashlib
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028');old=Path('/private/tmp/tdf-booking-tooltip-regression-20261028');root=Path('/private/tmp/tdf-booking-resize-phase-uninstrumented-matrix-20261029');root.mkdir()
compiled='a5535c59e49064fec535daa3682a4a1dc31c68f4'
def require(ok,msg):
 if not ok:raise RuntimeError(msg)
def git(*a):return subprocess.check_output(['git','-C',str(work),*a])
require(git('rev-parse','HEAD').decode().strip()=='a9095bc3e3bf53f5a76244564472fe53cfff7691','baseline drift')
changes=set(git('diff','--name-only','a9095bc3e3bf53f5a76244564472fe53cfff7691').decode().splitlines())
require(changes=={'e2e/web/booking-catalog-recovery.spec.mjs','formal/system/booking-tooltip-viewport.md'},'unrelated working changes')
build=json.loads(Path('/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028/correct-C-build-inputs.json').read_text());require(build['sourceCommit']==compiled,'wrong build')
def verify_artifacts():
 actual={str(f.relative_to(work/'tdf-hq-ui/dist')):hashlib.sha256(f.read_bytes()).hexdigest() for f in (work/'tdf-hq-ui/dist').rglob('*') if f.is_file()}
 require(actual==build['artifactHashes'],'served artifact drift')
verify_artifacts()
ui=work/'tdf-hq-ui/src/pages/PublicBookingPage.tsx';require(ui.read_bytes()==git('show',compiled+':tdf-hq-ui/src/pages/PublicBookingPage.tsx'),'UI changed')
raw=(work/'e2e/web/booking-catalog-recovery.spec.mjs').read_text();test=raw.replace('    await observeBookingViewportEvents(page);\n', '    // Diagnostic observer deliberately disabled in this control.\n',1).replace("from '@playwright/test'", "from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs'").replace("from 'axe-core'", "from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/axe-core/axe.js'")
(root/'booking.spec.mjs').write_text(test);(root/'playwright.config.mjs').write_text((old/'playwright.config.mjs').read_text().replace(str(old),str(root)))
inputs={'UICompilationCommit':compiled,'UIFileSha256':hashlib.sha256(ui.read_bytes()).hexdigest(),'workingFinalTestSha256':hashlib.sha256(raw.encode()).hexdigest(),'executedTestSha256':hashlib.sha256(test.encode()).hexdigest(),'testTransform':'Two package import locations and omission of the diagnostic observer call only; native acknowledgment, first root/scale, strict helper/focus/scroll assertions unchanged.','diagnosticObserverDisabled':True,'change':'Explicit native-event overlay phase correction; immutable first root/scale retained, event acknowledgment before one strict helper measurement.','allUISourceAndDependenciesEqualCompiledC2':True,'scope':'New phase-corrected test body against exact unchanged immutable C2 UI artifacts; no new product repair or hosted WebKit guarantee.'};(root/'inputs.json').write_text(json.dumps(inputs,indent=2)+'\n')
env=os.environ.copy();env['PATH']='/Users/macpro/.nvm/versions/node/v24.13.1/bin:'+env['PATH'];env.pop('TESTOMATIO',None);results=[]
for project,port in [('chromium-desktop',41331),('chromium-phone',41332),('firefox',41333)]:
 label='final-'+project
 config="import base from './playwright.config.mjs';\nimport {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';\nexport default defineConfig({...base,use:{...base.use,baseURL:'http://127.0.0.1:"+str(port)+"'},projects:base.projects.filter(p=>p.name==='"+project+"'),reporter:[['line'],['json',{outputFile:'"+str(root)+'/'+label+".json'}]],outputDir:'"+str(root)+'/'+label+"'});\n"
 (root/('playwright.'+label+'.config.mjs')).write_text(config)
 runner=(old/'run-positive-initial-phone.py').read_text().replace(str(old),str(root)).replace('41039',str(port)).replace('execution-positive-initial-phone.json','execution-'+label+'.json').replace('run-positive-initial-phone.log','run-'+label+'.log').replace('preview-positive-initial-phone.log','preview-'+label+'.log').replace('playwright.phone.config.mjs','playwright.'+label+'.config.mjs').replace(",'--grep','recovers an error'",'').replace('timeout=120','timeout=180').replace('One permanent-regression initial phone positive case','Three Observation permanent cases '+project)
 runner += "\nraise SystemExit(record['browserExit'] if record.get('browserExit') is not None else 1)\n"
 path=root/('run-'+label+'.py');path.write_text(runner);print('Observation final profile',project,port,flush=True)
 run=subprocess.run(['python3',str(path)],env=env)
 execution=json.loads((root/('execution-'+label+'.json')).read_text());require(run.returncode==0 and execution['browserExit']==0 and not execution.get('timeout'),project+' runtime not complete')
 report=json.loads((root/(label+'.json')).read_text());stats=report['stats'];require(stats['expected']==3 and stats['unexpected']==0 and stats['skipped']==0 and stats['flaky']==0,project+' cases not qualified')
 results.append({'project':project,'port':port,'passed':3,'failures':0,'skipped':0,'retries':0,'driverExit':0,'browserExit':0,'actualStats':stats,'cleanup':execution})
 (root/'matrix.json').write_text(json.dumps({'inputs':inputs,'results':results,'status':'PASS' if len(results)==3 else 'PARTIAL'},indent=2)+'\n');print('Observation profile PASS',project,flush=True)
require((work/'e2e/web/booking-catalog-recovery.spec.mjs').read_text()==raw,'harness changed during matrix')
require(ui.read_bytes()==git('show',compiled+':tdf-hq-ui/src/pages/PublicBookingPage.tsx'),'UI changed during matrix')
verify_artifacts()
print('Observation permanent9PASS; every profile3cases/CLI0/skip0/retry0; own servers/aliases removed.',flush=True)
