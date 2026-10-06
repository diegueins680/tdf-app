import pathlib,json,subprocess,datetime,shlex
P=pathlib.Path(__file__).parent;R=P/'recovery-repo-20261003'
def flat(f):return [x for pg in json.loads(f.read_text()) for x in pg]
initial={b['name']:b for b in flat(P/'branches-pages.json')};history=dict(initial)
for f in sorted(P.glob('final-*/branches.json')):
 for b in flat(f):history[b['name']]=b
current={b['name']:b for b in flat(P/'concurrent-deletions-branches.json')}
events=[e for e in flat(P/'concurrent-deletions-events.json') if e['type']=='DeleteEvent' and e['payload']['ref_type']=='branch']
main=current['main']['commit']['sha'];rows=[]
for name,b in sorted(history.items()):
 if name in current:continue
 sha=b['commit']['sha'];ref='refs/audit/concurrent-deletion-recovery-20261004/'+name
 subprocess.run(['git','cat-file','-e',sha+'^{commit}'],cwd=R,check=True)
 old=subprocess.run(['git','rev-parse','--verify',ref],cwd=R,capture_output=True,text=True)
 if old.returncode==0:assert old.stdout.strip()==sha
 else:subprocess.run(['git','update-ref',ref,sha,'0'*40],cwd=R,check=True)
 ancestor=subprocess.run(['git','merge-base','--is-ancestor',sha,main],cwd=R).returncode==0
 ev=[{'id':e['id'],'actor':e['actor']['login'],'timestamp':e['created_at']} for e in events if e['payload']['ref']==name]
 rows.append({'branch':name,'last_observed_sha':sha,'initial_branch':name in initial,'present':False,'audit_deleted':False,'observed_deletion_events':ev,'ancestor_of_current_main':ancestor,'main':main,'local_recovery_ref':ref,'recovery_command':'git push origin '+sha+':refs/heads/'+name})
assert len(rows)==73
(P/'concurrent-deletions-recovery.json').write_text(json.dumps({'captured_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'observed_branch_count':len(current),'total_historical_names':len(history),'deleted_by_audit':0,'absent_branches':rows},indent=2))
bundle=P/'concurrent-deletions-recovery.bundle';assert not bundle.exists()
subprocess.run(['git','bundle','create',str(bundle),*[r['local_recovery_ref'] for r in rows]],cwd=R,check=True)
subprocess.run(['git','bundle','verify',str(bundle)],cwd=R,check=True,stdout=subprocess.DEVNULL)
print('Verified recoverable bundle for',len(rows),'concurrently absent heads; initial',sum(r['initial_branch'] for r in rows),'all source history ancestor',sum(r['ancestor_of_current_main'] for r in rows),flush=True)
