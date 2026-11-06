from pathlib import Path
import subprocess,os,json,hashlib,socket,time,urllib.request,signal,base64
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028');cache=Path('/private/tmp/tdf-correctness-cycle-20261028');root=Path('/private/tmp/tdf-booking-resize-phase-controls-20261029');root.mkdir();baseconfig=Path('/private/tmp/tdf-booking-resize-phase-final-matrix-20261029/playwright.config.mjs')
def require(ok,msg):
 if not ok:raise RuntimeError(msg)
def sha(b):return hashlib.sha256(b).hexdigest()
raw=(work/'e2e/web/booking-catalog-recovery.spec.mjs').read_text();ui=work/'tdf-hq-ui/src/pages/PublicBookingPage.tsx';correct=ui.read_bytes();compiled=json.loads(Path('/private/tmp/tdf-booking-tooltip-regression-qualified-v3-20261028/correct-C-build-inputs.json').read_text())
require(json.loads(Path('/private/tmp/tdf-booking-resize-phase-final-matrix-20261029/matrix.json').read_text())['status']=='PASS','final9notcomplete')
def hashes(folder):return {str(f.relative_to(folder)):sha(f.read_bytes()) for f in folder.rglob('*') if f.is_file()}
require(hashes(work/'tdf-hq-ui/dist')==compiled['artifactHashes'],'correct build drift');require(sha(correct)==compiled['correctUIFileSha256'],'UI source drift')
env=os.environ.copy();env['PATH']='/Users/macpro/.nvm/versions/node/v24.13.1/bin:'+env['PATH']
for k in ['TESTOMATIO','VITE_UI_COMMIT','SOURCE_COMMIT','SOURCE_VERSION','GIT_SHA']:env.pop(k,None)
def transformed(s):return s.replace("from '@playwright/test'","from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs'").replace("from 'axe-core'","from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/axe-core/axe.js'")
def stop(p):
 if p and p.poll() is None:
  os.killpg(p.pid,signal.SIGTERM)
  try:p.wait(timeout=8)
  except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait(timeout=5)
def run(label,port,testSource):
 made=[];server=None;browser=None;record={'label':label,'port':port,'actualExit':None,'trackedDiagnosticSha256':sha(raw.encode()),'executedTestSha256':sha(testSource.encode()),'startedAt':time.time()}
 (root/(label+'.spec.mjs')).write_text(testSource)
 config="import base from '"+str(baseconfig)+"';\nimport {defineConfig} from '/private/tmp/tdf-correctness-cycle-20261028/node_modules/@playwright/test/index.mjs';\nexport default defineConfig({...base,testDir:'"+str(root)+"',use:{...base.use,baseURL:'http://127.0.0.1:"+str(port)+"'},projects:base.projects.filter(p=>p.name==='chromium-phone'),reporter:[['line'],['json',{outputFile:'"+str(root/(label+'.json'))+"'}]],outputDir:'"+str(root/label)+"'});\n";(root/(label+'.config.mjs')).write_text(config)
 try:
  with socket.socket() as s:s.bind(('127.0.0.1',port))
  for name in ['node_modules','tdf-hq-ui/node_modules']:
   p=work/name;t=cache/name;require(not(p.exists() or p.is_symlink()),'unexpected alias '+str(p));p.symlink_to(t,target_is_directory=True);made.append((p,t))
  with (root/(label+'-preview.txt')).open('w') as f:
   server=subprocess.Popen(['node',str(cache/'node_modules/vite/bin/vite.js'),'preview','--host','127.0.0.1','--port',str(port),'--strictPort'],cwd=work/'tdf-hq-ui',env=env,stdout=f,stderr=subprocess.STDOUT,start_new_session=True)
   for _ in range(100):
    require(server.poll() is None,'preview exited')
    try:
     with urllib.request.urlopen('http://127.0.0.1:'+str(port),timeout=1) as response:
      if response.status==200:break
    except OSError:time.sleep(.1)
   else:raise RuntimeError('preview unavailable')
   with (root/(label+'-browser.txt')).open('w') as log:
    browser=subprocess.Popen(['node',str(cache/'node_modules/@playwright/test/cli.js'),'test','--config',str(root/(label+'.config.mjs')),label+'.spec.mjs','--grep','recovers an error'],cwd=root,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
    try:record['actualExit']=browser.wait(timeout=120)
    except subprocess.TimeoutExpired:record['timeout']=True;stop(browser);record['actualExit']=browser.returncode
 finally:
  stop(browser);stop(server)
  for p,t in reversed(made):require(p.is_symlink() and p.resolve()==t.resolve(),'alias owner changed');p.unlink()
  record.update({'finishedAt':time.time(),'serverStopped':server is None or server.poll() is not None,'ownedAliasesRemoved':all(not p.is_symlink() for p,_ in made)})
  (root/(label+'-execution.json')).write_text(json.dumps(record,indent=2)+'\n')
 require(record['actualExit']==1 and not record.get('timeout'),'not actual expectedbrowser1')
 report=json.loads((root/(label+'.json')).read_text());require(report['stats']['unexpected']==1 and report['stats']['expected']==0 and report['stats']['skipped']==0 and report['stats']['flaky']==0,'not1actualfailure')
 attached=[]
 def visit(v):
  if isinstance(v,dict):
   for a in v.get('attachments',[]):
    if a['name']=='booking-viewport-geometry':attached.append(json.loads(base64.b64decode(a['body'])))
   for c in v.values():visit(c)
  elif isinstance(v,list):
   for c in v:visit(c)
 visit(report);require(len(attached)==1,'no any-bound diagnostic')
 geometry=attached[0];(root/(label+'-geometry.json')).write_text(json.dumps(geometry,indent=2)+'\n');return record,geometry
# This fixture changes only a CSS fault after resize/client establishment. All
# source guards remain and product source/artifacts stay correct.
needle='      const resizedViewport = await measureBookingViewport(page);';require(raw.count(needle)==1,'ambiguous fault injection')
fault='''    await page.evaluate(() => {
      const fault = document.createElement('style');
      fault.id = 'injected-helper-edge-fault';
      fault.textContent = '[role="tooltip"] {position:fixed!important;transform:translate3d(104px,300px,0)!important;}';
      document.head.append(fault);
    });
'''
record,geometry=run('helper-only-fault',41321,transformed(raw.replace(needle,fault+needle,1)));initial=geometry['afterNativeResize'];require(not initial['overflow'] and initial['clientWidth']==320 and initial['scrollWidth']==320,'helper fixture changed root bounds');require(initial['tooltip']['right']>320 and 'resizedViewport.tooltip.right' in (root/'helper-only-fault-browser.txt').read_text(),'wrong helper failurepoint');print('Helper-only negative QUALIFIED:rootfits320/scale1, actualhelperedgefails and event/later diagnosticsattached.',flush=True)
# Preserve the immutable correct build by rename; mutate only product option for
# a separate root-negative build, then restore all source/artifacts exactly.
needle=b"                                      slotProps={{ popper: { popperOptions: { strategy: 'fixed' } } }}\n";require(correct.count(needle)==1,'ambiguous product mutant');mutant=correct.replace(needle,b'',1);dist=work/'tdf-hq-ui/dist';saved=root/'correct-C2-dist';saved.parent.mkdir(exist_ok=True);require(not saved.exists(),'saved dist exists');dist.rename(saved);made=[]
try:
 try:
  ui.write_bytes(mutant)
  for name in ['node_modules','tdf-hq-ui/node_modules']:
   p=work/name;t=cache/name;require(not(p.exists() or p.is_symlink()),'unexpected build alias');p.symlink_to(t,target_is_directory=True);made.append((p,t))
  with (root/'root-mutant-build.txt').open('w') as f:r=subprocess.run(['node',str(cache/'node_modules/vite/bin/vite.js'),'build'],cwd=work/'tdf-hq-ui',env=env,stdout=f,stderr=subprocess.STDOUT)
  require(r.returncode==0,'mutant build failed')
 finally:
  ui.write_bytes(correct)
  for p,t in reversed(made):require(p.is_symlink() and p.resolve()==t.resolve(),'alias owner changed');p.unlink()
 record,geometry=run('root-option-removal',41322,transformed(raw));initial=geometry['initial'];require(initial['overflow'] and initial['clientWidth']==320 and initial['scrollWidth']>320,'not actual root overflow');require('viewportMeasurement.overflow' in (root/'root-option-removal-browser.txt').read_text(),'wrong root failurepoint')
 (root/'root-mutant-inputs.json').write_text(json.dumps({'correctUIHash':sha(correct),'mutantUIHash':sha(mutant),'testTransform':'Two imports ONLY; complete final diagnostic test body unchanged','mutantArtifacts':hashes(dist),'correctSourceRestoredBeforePreview':True},indent=2)+'\n');print('Root option-removal negative QUALIFIED:originalfirstrootpredicatefails; diagnosticscannotheal.',flush=True)
finally:
 ui.write_bytes(correct)
 if dist.exists():dist.rename(root/'root-mutant-dist')
 saved.rename(dist)
 require(hashes(dist)==compiled['artifactHashes'],'correct dist restore mismatch');require(ui.read_bytes()==correct,'UI restore mismatch');require((work/'e2e/web/booking-catalog-recovery.spec.mjs').read_text()==raw,'test drift')

# Keep every assertion. Supply only a synthetic event on an isolated fixture
# channel; the real resize cannot acknowledge this listener. The untrusted
# event at requested dimensions must be rejected and the unchanged8s bound fire.
synthetic=raw
start=synthetic.index('async function armBookingResizeAcknowledgement(page)')
end=synthetic.index('// Observe delivered events',start)
part=synthetic[start:end].replace("'resize', onResize", "'fixture-resize', onResize")
synthetic=synthetic[:start]+part+synthetic[end:]
needle='      const viewportMeasurement = await measureBookingViewport(page);'
require(synthetic.count(needle)==1,'ambiguous synthetic injection')
synthetic=synthetic.replace(needle,"      await page.evaluate(() => window.dispatchEvent(new Event('fixture-resize')));\n"+needle,1)
# The existing runner validates actual count/exit/cleanup before reading the
# expected attachment. Intercept only attachment decoding for this control.
runSource=run
import inspect
function=inspect.getsource(runSource).replace('def run(', 'def run_ack(')
function=function.replace("if a['name']=='booking-viewport-geometry'", "if a['name']=='booking-resize-acknowledgement'")
function=function.replace("'no any-bound diagnostic'", "'no resize acknowledgment'")
exec(function,globals())
record,ack=run_ack('synthetic-without-native-ack',41323,transformed(synthetic))
require(ack=={'delivered':False,'reason':'native resize not delivered within 8s'},'ack failed for wrong reason')
log=(root/'synthetic-without-native-ack-browser.txt').read_text()
require('resizeAcknowledgement.delivered' in log,'wrong fail-closed assertion')
print('Synthetic/missing-native negative QUALIFIED: unchanged8s deadline, trusted-event guard rejects synthetic; actual acknowledgment assertion fails.',flush=True)

(root/'controls.json').write_text(json.dumps({'finalDiagnosticSourceSha256':sha(raw.encode()),'helperOnlyFault':'1 actual edge failure/rootfits320; initial guard unchanged, later+event attachment present','rootOptionRemoval':'1actual rootfailure; fullfinaltestbody, soleproductoption removed','exactCorrectSourceAndDistRestored':True,'correctDistUICompilationCommit':compiled['sourceCommit'],'allOwnedServersStoppedAndAliasesRemoved':True,'nativeAcknowledgementFault':'Synthetic event at requested width rejected; no real registered native acknowledgment, actual8s timeout/delivered assertion fails','scope':'Explicit phase oracle sensitivity/immutable root failure protection; no new product repair or universal convergence guarantee.'},indent=2)+'\n');print('Both distinct diagnostic negative controls qualified; exact C2 build/source restored; own processes/aliases removed.',flush=True)
