from pathlib import Path
import json,subprocess,hashlib
w=Path('/private/tmp/tdf-artist-639-after638-20261029');o=Path('/private/tmp/tdf-artist-639-qualification-20261029');m='2e4288bba74ba7c6943c769f64e0447b6555f56b';base='6197b87326a437350dca239ee64902e78da6964c';d='dd8f2c9c14cc0b3c77ef9c84ab1a112a8c6b2f4d';mobile='7000df1da934aa467e443ad267c0a8caa1e9cb72'
def git(*args):return subprocess.check_output(['git','-C',str(w),*args])
def sha(b):return hashlib.sha256(b).hexdigest()
s=json.loads((o/'assurance-committed/sources.json').read_text());assert s['revision']==m and not s['worktreeDirty']
rootfiles={p:h for p,h in s['files'].items() if not p.startswith('tdf-mobile/')};mobfiles={p.removeprefix('tdf-mobile/'):h for p,h in s['files'].items() if p.startswith('tdf-mobile/')}
def check(cwd,ref,files):
 req=''.join(ref+':'+p+'\n' for p in files).encode();proc=subprocess.run(['git','-C',str(cwd),'cat-file','--batch'],input=req,stdout=subprocess.PIPE,check=True);raw=proc.stdout;pos=0
 for p,h in files.items():
  end=raw.index(b'\n',pos);head=raw[pos:end].decode().split();assert len(head)==3 and head[1]=='blob',(p,head);n=int(head[2]);body=raw[end+1:end+1+n];assert sha(body)==h,p;pos=end+1+n+1
 assert pos==len(raw)
check(w,m,rootfiles);check(w/'tdf-mobile',mobile,mobfiles)
reqpath='formal/system/requirements.json';old=json.loads(git('show',base+':'+reqpath))['requirements'];new=json.loads(git('show',m+':'+reqpath))['requirements'];dd=json.loads(git('show',d+':'+reqpath))['requirements'];by={x['id']:x for x in new};assert all(by[x['id']]==x for x in old);assert len(new)==len(old)+1==157;assert by['ART-ENRICH-REVIEW-001']==next(x for x in dd if x['id']=='ART-ENRICH-REVIEW-001')
owned=json.loads((o/'initial-correspondence.json').read_text())['sevenOwnedInputHashes'];assert all(sha(git('show',m+':'+p))==h and git('show',m+':'+p)==git('show',d+':'+p) for p,h in owned.items())
changed=git('diff','--name-only',base,m).decode().splitlines();allowed=set(owned)|{'formal/system/inventory.json','formal/system/requirements.json','formal/system/traceability.json','docs/catalog-persistence/reports/static-list-inventory.json','docs/catalog-persistence/reports/list-consumer-matrix.csv'};assert set(changed)==allowed,(changed,allowed)
reviews=git('ls-tree','-r','--name-only',m,'--','formal').decode().splitlines();reviews=[p for p in reviews if 'review' in p and p.endswith('.json')];reviewhash={}
for p in reviews:assert git('show',m+':'+p)==git('show',base+':'+p);reviewhash[p]=sha(git('show',m+':'+p))
md={p:sha(git('show',m+':'+p)) for p in owned if p.endswith('.md')};assert all(p not in s['files'] for p in md)
report={'sourceCommit':m,'baselineMain':base,'publishedArtistSource':d,'sourceDigest':s['digest'],'registeredGitBlobsChecked':len(s['files']),'rootBlobs':len(rootfiles),'mobileBlobs':len(mobfiles),'mobileCommit':mobile,'everyRegisteredHashVerifiedAgainstCommittedGitBlob':True,'sevenArtistInputsByteExactDD8':True,'all156IncomingRequirementsExact':True,'artistRequirementExactDD8':True,'totalRequirements':len(new),'changedPathsExactlyOwnedAndCanonicalGenerated':changed,'historicalReviewFilesByteExactMain':reviewhash,'markdownOutsideSourceFingerprintSeparatelyBoundGitSha256':md,'scope':'Git/source correspondence only, not runtime correctness or complete compiler/package attestation.'}
catalogPath='docs/catalog-persistence/catalog-list-decisions.json';catalog=git('show',m+':'+catalogPath);assert catalog==git('show',base+':'+catalogPath);assert len(json.loads(catalog)['decisions'])==1352
report['wholeHistoricalCatalogDecisionFileByteExactMain']={'path':catalogPath,'sha256':sha(catalog),'byteExact':True,'decisions':1352}
(o/'source-correspondence-final.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps({k:v for k,v in report.items() if k not in ['changedPathsExactlyOwnedAndCanonicalGenerated','historicalReviewFilesByteExactMain']},indent=2))
