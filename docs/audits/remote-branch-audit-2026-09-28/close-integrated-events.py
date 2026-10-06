import collect,subprocess,json,datetime,time
P=collect.ROOT;repo=collect.REPO
proof=json.loads((P/'merge465-verified.json').read_text())
target=proof['pr']['merge_commit_sha'];reviewed_head=proof['pr']['head']['sha']
assert proof['pr']['merged'] and proof['tree_matches_validated_candidate']
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
def pages(path):return api(path,'--paginate','--slurp')
def log(**kw):
 with (P/'mutations.jsonl').open('a') as out:out.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),**kw})+'\n')
def target_checks():
 assert api(repo+'/git/ref/heads/main')['object']['sha']==target
 runs=pages(repo+'/actions/runs?head_sha='+target+'&per_page=100');checks=pages(repo+'/commits/'+target+'/check-runs?per_page=100');status=api(repo+'/commits/'+target+'/status')
 rs=[r for pg in runs for r in pg['workflow_runs']];cs=[c for pg in checks for c in pg['check_runs']];assert rs and cs
 failed=[r['name'] for r in rs+cs if r['status']=='completed' and r['conclusion'] not in ('success','skipped')];assert not failed,failed
 green=all(c['status']=='completed' and c['conclusion'] in ('success','skipped') for c in rs+cs) and status['state']=='success'
 evidence={'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'target':target,'runs':runs,'checks':checks,'status':status,'all_passed':green}
 (P/'events-closure-main-ci-latest.json').write_text(json.dumps(evidence,indent=2))
 if green:(P/'events-closure-main-ci.json').write_text(json.dumps(evidence,indent=2))
 return green
while not target_checks():print('Waiting for normal event target CI',flush=True);time.sleep(30)
replacement=api(repo+'/pulls/465');assert replacement['merged'] and replacement['merge_commit_sha']==target and replacement['head']['sha']==reviewed_head
sources=json.loads((P/'events-source-closure-candidates.json').read_text());by_branch={s['branch']:s for s in sources}
def depth(source,seen=None):
 seen=set() if seen is None else set(seen);assert source['branch'] not in seen;seen.add(source['branch'])
 bases=[p['base'] for p in source['prs'] if p['base'] in by_branch]
 return 1+max([depth(by_branch[b],seen) for b in bases] or [0])
ordered=sorted(sources,key=lambda s:(-depth(s),s['branch']))
(P/'events-closure-order.json').write_text(json.dumps([{'branch':s['branch'],'depth':depth(s),'prs':[p['number'] for p in s['prs']]} for s in ordered],indent=2))
for source in ordered:
 sha=source['head'];name=source['branch']
 for original in source['prs']:
  n=original['number'];pr=api(repo+f'/pulls/{n}')
  assert pr['head']['sha']==sha and pr['head']['ref']==name
  assert api(repo+'/git/ref/heads/'+name)['object']['sha']==sha
  comparison=api(repo+'/compare/'+sha+'...'+target);assert comparison['behind_by']==0 and comparison['merge_base_commit']['sha']==sha
  record={'pr':pr,'replacement':replacement,'comparison':comparison,'branch':name,'head':sha,'target':target}
  (P/f'close-{n}-event-proof.json').write_text(json.dumps(record,indent=2))
  if pr['merged']:
   if not any(json.loads(line).get('action')=='observed_automatic_merge' and json.loads(line).get('pr')==n for line in (P/'mutations.jsonl').read_text().splitlines()):
    log(action='observed_automatic_merge',pr=n,sha=sha,url=pr['html_url'],replacement=465,merge_commit=pr['merge_commit_sha'],branch_retained=True)
   print('Verified automatic integration',n,flush=True);continue
  if pr['state']=='closed':
   print('Already closed; preserved',n,flush=True);continue
  assert pr['base']['ref']==original['base'], (n,'Concurrent retargeting')
  assert target_checks()
  fresh=api(repo+f'/pulls/{n}');assert fresh['state']=='open' and fresh['head']['sha']==sha and fresh['base']['ref']==pr['base']['ref']
  body='Classification: ALREADY_MERGED through approved consolidation #465. Recorded branch head `'+sha+'` is an exact ancestor of main `'+target+'`; all contributor commits and useful changes were retained by normal merges, including the separate task-guard contribution. The replacement completes the current web/mobile/API, session/privacy, lifecycle/receipt, task integrity and formal/database integrations while leaving the event feature disabled by default. Current-head independent approval, all prescribed checks and normal post-merge target CI passed. Closing this integrated source PR without rewriting its stack or deleting its branch. Recovery head: `'+sha+'`. Replacement: https://github.com/diegueins680/tdf-app/pull/465.'
  comment=api(repo+f'/issues/{n}/comments','--method','POST','-f','body='+body)
  verified=api(repo+'/issues/comments/'+str(comment['id']));assert verified['body']==body
  log(action='commented_integrated_closure_evidence',pr=n,sha=sha,url=comment['html_url'],replacement=465)
  assert target_checks()
  fresh=api(repo+f'/pulls/{n}');assert fresh['state']=='open' and fresh['head']['sha']==sha and fresh['base']['ref']==pr['base']['ref']
  assert api(repo+'/git/ref/heads/'+name)['object']['sha']==sha
  api(repo+f'/pulls/{n}','--method','PATCH','-f','state=closed')
  after=api(repo+f'/pulls/{n}');assert after['state']=='closed' and not after['merged'] and after['head']['sha']==sha
  assert api(repo+'/git/ref/heads/'+name)['object']['sha']==sha
  (P/f'close-{n}-event-after.json').write_text(json.dumps(after,indent=2))
  log(action='closed_unmerged',pr=n,sha=sha,url=after['html_url'],comment=comment['html_url'],replacement=465,branch_retained=True)
  print('Verified integrated source closure',n,flush=True)
