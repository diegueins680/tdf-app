from pathlib import Path
import os,subprocess,json,hashlib,time,urllib.request,socket,signal
w=Path('/private/tmp/tdf-artist-639-after638-20261029');c=Path('/private/tmp/tdf-correctness-cycle-20261028');o=Path('/private/tmp/tdf-artist-639-qualification-20261029')
q=json.loads((o/'full-quality.json').read_text());source=q['sourceCommit']
def sha(b):return hashlib.sha256(b).hexdigest()
def dist():return {str(p.relative_to(w/'tdf-hq-ui/dist')):sha(p.read_bytes()) for p in (w/'tdf-hq-ui/dist').rglob('*') if p.is_file()}
assert dist()==q['distArtifactHashes']
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=w).decode().strip()==source
assert not subprocess.check_output(['git','status','--porcelain'],cwd=w).strip()
with socket.socket() as s:s.bind(('127.0.0.1',41439))
env=os.environ.copy();env['PATH']='/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin'
for k in ['TESTOMATIO','ADMIN_TOKEN','DATABASE_URL','TDF_API_BASE','API_BASE','SPOTIFY_CLIENT_ID','SPOTIFY_CLIENT_SECRET','YOUTUBE_API_KEY','DRIVE_CLIENT_ID','DRIVE_CLIENT_SECRET','DRIVE_REFRESH_TOKEN']:env.pop(k,None)
made=[];server=None
try:
 for name in ['node_modules','tdf-hq-ui/node_modules']:
  p,t=w/name,c/name;assert not(p.exists() or p.is_symlink());p.symlink_to(t,target_is_directory=True);made.append((p,t))
 with (o/'browser-server.txt').open('w') as log:
  server=subprocess.Popen(['node',str(c/'node_modules/vite/bin/vite.js'),'preview','--host','127.0.0.1','--port','41439','--strictPort'],cwd=w/'tdf-hq-ui',env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
  deadline=time.monotonic()+25
  while True:
   assert server.poll() is None,'Preview exited'
   try:
    with urllib.request.urlopen('http://127.0.0.1:41439',timeout=1) as r:
     if r.status==200:break
   except Exception:
    if time.monotonic()>deadline:raise
    time.sleep(.15)
  with (o/'browser-driver.txt').open('w') as out:r=subprocess.run(['node','/private/tmp/tdf-artist-639-browser-20261029.mjs'],cwd=w,env=env,stdout=out,stderr=subprocess.STDOUT,timeout=180)
  print('browser actualexit'+str(r.returncode),flush=True)
  assert r.returncode==0,(o/'browser-driver.txt').read_text()[-6000:]
  assert json.loads((o/'browser-results.json').read_text())['passed']==4
finally:
 if server and server.poll() is None:
  os.killpg(server.pid,signal.SIGTERM)
  try:server.wait(timeout=10)
  except subprocess.TimeoutExpired:os.killpg(server.pid,signal.SIGKILL);server.wait(timeout=5)
 for p,t in reversed(made):assert p.is_symlink() and p.resolve()==t.resolve();p.unlink()
assert dist()==q['distArtifactHashes']
assert not subprocess.check_output(['git','status','--porcelain'],cwd=w).strip()
(o/'browser-cleanup.json').write_text(json.dumps({'sourceCommit':source,'actualExitCode':r.returncode,'passed':4,'distUnchanged':True,'ownedPreviewStopped':True,'ownedAliasesRemoved':True,'sourceClean':True},indent=2)+'\n')
print('four browser cases PASS; preview/aliases cleaned; source/dist unchanged',flush=True)
