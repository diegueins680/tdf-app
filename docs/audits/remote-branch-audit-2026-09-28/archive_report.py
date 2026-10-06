import pathlib,shutil,json,hashlib,datetime
P=pathlib.Path(__file__).parent
D=pathlib.Path('/Users/diegosaa/GitHub/tdf-app/docs/audits/remote-branch-audit-2026-09-28')
manifest=D/'artifact-manifest.json'
prior=json.load(open(manifest))['files'] if manifest.exists() else {}
if D.exists() and not manifest.exists():raise SystemExit('Destination exists without audit ownership manifest; preserving it')
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
files={f.name:f for f in P.iterdir() if f.is_file() and f.suffix in ('.json','.jsonl','.txt','.log','.md','.py','.sh','.sql','.mjs')}
files['concurrent-deletions-recovery.bundle']=P/'concurrent-deletions-recovery.bundle'
files['security-classifier-check.cjs']=P/'security-classifier-check.cjs'
files['catalog-readonly-role.sql']=P/'cutover/ops/hetzner/catalog-readonly-role.sql'
directories=['cutover-provenance-mail-live','report','branch-evidence','branch-rules','checks','status','prs','issues','protection','deployment-matches','deployment-status','refresh','discovery-social-browser-final','discovery-social-browser-recheck','cutover-final-review-checkpoint']
directories += [p.name for p in P.glob('final-*') if p.is_dir() and (p/'completed.txt').exists()]
for name in directories:
 for f in (P/name).rglob('*'):
  if f.is_file() and '__pycache__' not in f.parts:files[str(f.relative_to(P))]=f
cleanup=pathlib.Path('/private/tmp/tdf-disk-cleanup-20260928.json')
if cleanup.exists():files[cleanup.name]=cleanup
for rel in files:
 target=D/rel
 if target.exists() and (rel not in prior or digest(target)!=prior[rel]):raise SystemExit('Concurrent destination edit preserved: '+rel)
D.mkdir(parents=True,exist_ok=True)
result={}
for rel,src in files.items():
 dest=D/rel;dest.parent.mkdir(parents=True,exist_ok=True)
 source_hash=digest(src)
 # Ownership/content was checked above. Avoid rewriting unchanged large evidence.
 if not dest.exists() or prior.get(rel)!=source_hash:
  shutil.copyfile(src,dest)
  assert digest(dest)==source_hash, 'Archive verification failed: '+rel
 result[rel]=source_hash
manifest.write_text(json.dumps({'captured_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'source':str(P),'files':result,'excluded':'Source checkouts, existing worktrees, node_modules, build caches, downloaded formal tools and database directories. Originals preserved.'},indent=2)+'\n')
print('Archived',len(result),'evidence files;',sum((D/rel).stat().st_size for rel in result),'bytes; report:',D/'report/README.md')
