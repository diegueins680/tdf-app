import collect,subprocess,json,datetime
P=collect.ROOT;cwd=P/'events';old='86d4b9bb4dad81771a2e3de4739814c62c206d30';target='49f1f0ec087067d17f62d91df1b616eb053eb894'
def api(path):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path],env=collect.ENV,text=True))
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==old
assert api(collect.REPO+'/git/ref/heads/main')['object']['sha']==target
assert subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip()==old
assert subprocess.check_output(['git','rev-parse','origin/main'],cwd=cwd,text=True).strip()==target
status=subprocess.check_output(['git','status','--porcelain'],cwd=cwd,text=True);assert all(s.startswith('?? ') or s==' M tdf-mobile' for s in status.splitlines()),status
mobile=subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd/'tdf-mobile',text=True).strip();assert mobile=='ede1f2a0ccf75f3794c7892f291e1751b279d19e'
subprocess.run(['git','merge-base','--is-ancestor','3ee82fe403b358b405568ed5164cf7798eb45e0b',mobile],cwd=cwd/'tdf-mobile',check=True)
(P/'events-current-main-before.json').write_text(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'pr':x,'target':target,'mobile':mobile,'status':status},indent=2))
r=subprocess.run(['git','merge','--no-commit','--no-ff',target],cwd=cwd,capture_output=True,text=True)
(P/'events-current-main-merge.log').write_text(r.stdout+r.stderr);print(r.stdout+r.stderr)
assert r.returncode in [0,1]
assert subprocess.check_output(['git','rev-parse','MERGE_HEAD'],cwd=cwd,text=True).strip()==target
