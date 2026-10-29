from pathlib import Path
import subprocess,json,hashlib,os
work=Path('/private/tmp/tdf-provider-evidence-actualmain-20261028');out=Path('/private/tmp/tdf-booking-resize-phase-source-20261029');base='a9095bc3e3bf53f5a76244564472fe53cfff7691'
def git(*a,root=work):return subprocess.check_output(['git','-C',str(root),*a])
def sha(b):return hashlib.sha256(b).hexdigest()
env=os.environ.copy();env['PATH']='/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin';env['PYTHONDONTWRITEBYTECODE']='1'
def run(label,args,expected=0):
 with (out/(label+'.txt')).open('w') as f:r=subprocess.run(args,cwd=work,env=env,stdout=f,stderr=subprocess.STDOUT)
 assert r.returncode==expected,(label,r.returncode)
 print(label+' expected exit'+str(expected)+' PASS',flush=True)
assert (out/'qualification.json').exists();assert git('rev-parse','HEAD').decode().strip()==base
allowed={'e2e/web/booking-catalog-recovery.spec.mjs','formal/system/booking-tooltip-viewport.md','formal/system/inventory.json','formal/system/traceability.json'};assert set(git('diff','--name-only').decode().splitlines())==allowed;assert not git('ls-files','--others','--exclude-standard').strip()
raw=(work/'e2e/web/booking-catalog-recovery.spec.mjs').read_bytes();controls=json.loads(Path('/private/tmp/tdf-booking-resize-phase-controls-20261029/controls.json').read_text());assert controls['finalDiagnosticSourceSha256']==sha(raw) and controls['exactCorrectSourceAndDistRestored'];matrix=json.loads(Path('/private/tmp/tdf-booking-resize-phase-final-matrix-20261029/matrix.json').read_text());assert matrix['status']=='PASS' and matrix['inputs']['workingFinalTestSha256']==sha(raw)
for p in ['formal/system/requirements.json','docs/catalog-persistence/catalog-list-decisions.json','docs/catalog-persistence/reports/static-list-inventory.json','docs/catalog-persistence/reports/list-consumer-matrix.csv']:
 assert (work/p).read_bytes()==git('show',base+':'+p),p
run('diff-check',['git','diff','--check']);run('source-add',['git','add','--',*sorted(allowed)]);run('source-commit',['git','commit','-m','test(booking): check helper placement after native resize delivery'])
source=git('rev-parse','HEAD').decode().strip();assert not git('status','--porcelain').strip();(out/'source-C.json').write_text(json.dumps({'baseline':base,'sourceCommit':source,'testSha256':sha(raw),'productUIUnchanged':True,'specificationAndHarnessCorrection':True,'historicalWebKitInitialOverlayInvariant':'FAILED at a909 before native notification; retained.','newHostedPhaseQualification':'PENDING; no new product repair.'},indent=2)+'\n')
run('assurance-committed',['python3','scripts/verification-report.py','--output',str(out/'assurance-committed')]);run('source-admission',['python3','scripts/check-new-specification-surfaces.py','--base',base,'--head',source]);run('main-admission',['python3','scripts/check-new-specification-surfaces.py','--base','e302dc7fa76ac1c20e4e95cb946c82019ee17bb2','--head',source])
for label,flags in [('width',[]),('width-optimized',['-O'])]:
 run(label,['python3',*flags,'reports/booking-viewport-bounds-2026-10-28/check-width-comparison.py','e2e/web/booking-catalog-recovery.spec.mjs'])
 mutant=out/'inner-width-checker-mutant.mjs';needle='overflow: viewport.scrollWidth > viewport.clientWidth';text=raw.decode();assert text.count(needle)==1;mutant.write_text(text.replace(needle,'overflow: viewport.scrollWidth > viewport.innerWidth',1))
 run(label+'-negative',['python3',*flags,'reports/booking-viewport-bounds-2026-10-28/check-width-comparison.py',str(mutant)],1)
s=json.loads((out/'assurance-committed/sources.json').read_text());assert s['revision']==source and not s['worktreeDirty'];files=s['files'];assert len(files)==1413;mobile=git('rev-parse',source+':tdf-mobile').decode().strip();assert git('rev-parse','HEAD',root=work/'tdf-mobile').decode().strip()==mobile
for root,revision,prefix in [(work,source,''),(work/'tdf-mobile',mobile,'tdf-mobile/')]:
 selected=[(p,p[len(prefix):]) for p in sorted(files) if p.startswith('tdf-mobile/')==bool(prefix)];refs=[revision+':'+local for _,local in selected];blobs=subprocess.check_output(['git','-C',str(root),'cat-file','--batch'],input=('\n'.join(refs)+'\n').encode());offset=0
 for full,local in selected:
  end=blobs.index(b'\n',offset);header=blobs[offset:end].split();assert header[1]==b'blob';length=int(header[2]);start=end+1;assert sha(blobs[start:start+length])==files[full],full;offset=start+length;assert blobs[offset:offset+1]==b'\n';offset+=1
 assert offset==len(blobs)
for path in ['tdf-hq-ui','tdf-hq','scripts','.github','ops','package.json','package-lock.json','tdf-mobile','formal/system/requirements.json']:
 assert git('rev-parse',source+':'+path)==git('rev-parse',base+':'+path),path
contractHash=sha((work/'formal/system/booking-tooltip-viewport.md').read_bytes());inventory=json.loads((work/'formal/system/inventory.json').read_text());rows=[]
def visit(v):
 if isinstance(v,dict):
  if v.get('path')=='formal/system/booking-tooltip-viewport.md':rows.append(v)
  for c in v.values():visit(c)
 elif isinstance(v,list):
  for c in v:visit(c)
visit(inventory);assert rows and all(r.get('sha256')==contractHash for r in rows)
seal={'baseline':base,'sourceCommit':source,'sourceDigest':s['digest'],'registeredSourceInputs':1413,'allRegisteredGitBlobAndMobileHashesVerified':True,'mobileRevision':mobile,'updatedContractSha256':contractHash,'currentDiscoveryMatches':True,'all156RequirementObjectsByteExactBaseline':True,'whole1352HistoricalDecisionFileAndDerivedCatalogByteExactBaseline':True,'allProductUIBackendAPIDependenciesWorkflowsOpsUnchanged':True,'priorProviderArchiveUnchanged':True,'localPhasePositives':'9instrumented+9uninstrumented/0fail/0skip/0retry','realControlledFailures':'1post-native helper edgefailure/rootfits320;1sole-option firstrootfailure362/320;1synthetic-only ack rejected/fails8s; exactproductsourceandC2distrestored','existingWidthClaims':'2manualconditionalinteger claims normal/-O andrejectmutant; notCSSOM/JS/refinementproof','globalAssurance':'156/312UNPROVEN11existingconditionalSMT;releaseBLOCKED','historicalHostedWebKitInitialOverlay':'FAILED at21628/a909; a909 timeline identifies pre-notification helper phase error.','correctedHostedPhase':'PENDING; local cases TESTED, no universal convergence proof or product repair claimed','mainComposition':'Contains protectedmainE302; requiredreview/all20/FIFOpending','cleanSource':not bool(git('status','--porcelain').strip())};assert seal['cleanSource'];(out/'source-seal.json').write_text(json.dumps(seal,indent=2)+'\n');print('Resize phase C committed/sealed '+source+' digest '+s['digest'],flush=True)
