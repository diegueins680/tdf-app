import json,pathlib,subprocess,urllib.parse,collections,csv,datetime
P=pathlib.Path(__file__).parent; REPO=P/'recovery-repo-20261003';REPORT=P/'report';REPORT.mkdir(exist_ok=True)
def read(p,default=None):
 try:return json.load(open(P/p))
 except FileNotFoundError:return default

def flat(x):return [v for page in x for v in page] if x and isinstance(x[0],list) else x or []
def git(*a):return subprocess.check_output(['git',*a],cwd=REPO,text=True).strip()
def ancestor(a,b):return subprocess.run(['git','merge-base','--is-ancestor',a,b],cwd=REPO,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL).returncode==0
initial=read('git-inventory-enriched.json');branches={b['name']:b for b in flat(read('branches-pages.json'))};event=set(read('event-chain.json'))
initialprs={p['number']:p for p in flat(read('pulls-pages.json'))}
snapshots=sorted(p for p in P.glob('final-*') if p.is_dir() and (p/'completed.txt').exists())
final=snapshots[-1].name if snapshots else 'refresh'
latestprs={p['number']:p for p in flat(read(final+'/pulls.json'))}
livebranches={b['name']:b for b in flat(read(final+'/branches.json'))}
absent={r['branch']:r for r in read('concurrent-deletions-recovery.json')['absent_branches']}
closures={x['pr']:x for x in [json.loads(s) for s in (P/'mutations.jsonl').read_text().splitlines()] if x['action']=='closed_unmerged'}
squash={x['branch']:x for x in read('squash-proof.json')};main=git('rev-parse','origin/main')
operational={'main':'Protected default branch; production workflows and releases target main.', 'audit/provider-recovery-artifact-20260918':'Immutable provider-compatible recovery artifact identified in current recovery documentation.', 'audit/provider-reviewed-release-tooling-20260918':'Exact reviewed recovery runner from PR438; retained for release recovery compatibility.', 'fix/access-request-name-production-20260916':'Production-compatible name preservation / PayPal webhook backport; current critical-completion document records deployed revision.', 'release/identity-compatible-recovery-20260918':'Documented identity-compatible recovery reference; preserved for operational rollback.'}
rows=[]
for b in initial:
 name=b['name'];sha=b['sha'];quoted=urllib.parse.quote(name,safe=''); bp=branches[name]; ev=b['evidence_id']
 prs=[];threads=[]
 for n in b['prs']:
  info=read(f'{final}/prs/{n}/info.json') or read(f'refresh/prs/{n}/info.json') or read(f'prs/{n}/info.json') or initialprs[n]
  if n in closures:info={**info,'state':'closed','merged':False}
  if n==460 and not read(f'{final}/prs/{n}/info.json'): info=read('retarget-460-after.json') or info
  pages=read(f'{final}/prs/{n}/threads.json') or read(f'refresh/prs/{n}/threads.json') or read(f'prs/{n}/threads.json',[])
  nodes=[t for page in pages for t in page['data']['repository']['pullRequest']['reviewThreads']['nodes']]
  reviews=flat(read(f'{final}/prs/{n}/reviews.json') or read(f'refresh/prs/{n}/reviews.json') or read(f'prs/{n}/reviews.json',[]))
  decision=pages[0]['data']['repository']['pullRequest']['reviewDecision'] if pages else None
  prs.append({'number':n,'url':info['html_url'],'title':info['title'],'state':info['state'],'merged':info.get('merged',False),'merged_commit':info.get('merge_commit_sha') if info.get('merged') else None,'base':info['base']['ref'],'base_sha':info['base']['sha'],'head':info['head']['sha'],'draft':info['draft'],'author':info['user']['login'],'labels':[x['name'] for x in info['labels']],'native_stack':info.get('stack'),'mergeable':info.get('mergeable'),'mergeable_state':info.get('mergeable_state'),'review_decision':decision,'reviews':[{'author':x['user']['login'],'state':x['state'],'commit':x.get('commit_id'),'url':x['html_url']} for x in reviews],'unresolved_threads':[{'path':t['path'],'outdated':t['isOutdated'],'comments':t['comments']['nodes']} for t in nodes if not t['isResolved']],'linked_issues':pages[0]['data']['repository']['pullRequest']['closingIssuesReferences']['nodes'] if pages else []})
  threads.extend(t for t in nodes if not t['isResolved'])
 current_sha=livebranches.get(name,{}).get('commit',{}).get('sha',absent.get(name,{}).get('last_observed_sha',sha))
 checkpages=read(f'{final}/checks/{current_sha}.json') or read(f'checks/{sha}.json',[]);checks=[{'name':c['name'],'head':c['head_sha'],'status':c['status'],'conclusion':c['conclusion'],'url':c['html_url']} for page in checkpages for c in page['check_runs']]
 statuses=[{'context':s['context'],'state':s['state'],'url':s['target_url']} for page in (read(f'{final}/status/{current_sha}.json') or read(f'status/{sha}.json',[])) for s in page['statuses']]
 integrated=ancestor(sha,main);proof=squash.get(name); dependencies=[{'pr':p['number'],'branch':p['head']['ref'],'url':p['html_url']} for p in latestprs.values() if p['state']=='open' and p['base']['ref']==name]
 if name in operational:
  classification='PROTECTED_OR_OPERATIONAL';rationale=operational[name];confidence='high';blocker='Must remain available for protected or operational use.'
 elif name in event and integrated and read('merge465-verified.json'):
  classification='ALREADY_MERGED';rationale='Exact source head and contributor history are integrated through approved consolidation465 and its verified normal merge. Current product/API/mobile/session/lifecycle/task integrity repairs and tests are retained in the replacement. Source closure waits for passing normal target CI; branch remains.';confidence='high';blocker='No source integration remains. Final target CI and verified closure status are recorded separately; preserve source branch.'
 elif name in event:
  classification='CONSOLIDATE';rationale='Unique event-stack contributions retained in local consolidation; main authentication, UX and migration ancestry preserved. Parallel PR339 task guards rescued via merge. Replacement PR465 preserves all contributions. Mobile115 is now independently reviewed and concurrently merged with passing post-merge checks. Lifecycle receipt/flag-lock review fixes are published at34ca75d; current CI exposes a task-completion expiry polling defect under repair. Current-head checks and approval remain mandatory.';confidence='high';blocker='Complete the task-completion test timing correction, then require passing exact-head hosted CI and fresh independent approval on PR465. Mobile115 is integrated. Close originals only after replacement merge and passing target CI.'
 elif name.startswith('codex/payment') or name.startswith('codex/payphone'):
  classification='BLOCKED_OR_AMBIGUOUS';rationale='Current payment expansion remains unique. Root PR331 has unresolved no-charge-finality safety feedback; downstream stack depends on it. Current main explicitly defers provider expansion. Some worker safety was merged via PR418, but new reconciliation worker value remains.';confidence='high';blocker='Merchant-qualified authenticated Datafast terminal-resource/cancellation contract excluding pending/late captures, bound sandbox traces, then root review fixes and current-main integration.'
 elif integrated:
  classification='ALREADY_MERGED';rationale=f'Exact branch head is an ancestor of current main {main}; all committed source history is retained. Deletion not proven safe: preview/environment usage and downstream references must be excluded.';confidence='high';blocker='No integration needed. Preserve until external deployment/automation branch usage can be conclusively excluded. PR460 is automatically recognized merged via PR469; PR463 was closed with evidence after final main CI passed following PR472.'
 elif proof and proof['mergeInMain'] and proof['patchEqual'] and proof['mergedRecordHead']==sha:
  classification='ALREADY_MERGED';rationale=f"PR{proof['pr']} merged exact head; stable patch equivalence (whole three-commit range for PR369) matches merge {proof['merge']} already in main. See squash-proof.json.";confidence='high';blocker='No integration needed. Preserve until all operational/deployment references are conclusively excluded.'
 elif name=='fix/notification-mobile-release-pin-20260918':
  classification='ALREADY_MERGED' if read('notification-rescue-integrated-proof.json') else 'CONSOLIDATE';rationale='PR441 already closed in favor of merged PR449 mobile pin. Its unique historical delivery/recovery notes are now preserved with attribution and explicit dated scope in documentation-only PR476; the old mobile pin is already an ancestor of the integrated companion. Replacement476 received current-head independent approval and merged normally atc4d479c7 with exact tested tree. Source text and original mobile-pin ancestry are verified; source branch retained.';confidence='high';blocker='Replacement476 is merged with exact reviewed text and target-tree proof; normal target CI is tracked separately. Preserve source branch pending deletion clearance.'
 elif any(p['number']==460 for p in prs):
  classification='CONSOLIDATE';rationale='Approved source with passing checks and current-main focused tests. Ordinary merge was rejected by native stack466. Standalone PR469 now incorporates its exact history and repaired PR463 through a normal merge into current main, avoiding descendant rewrites while retaining all main review and CI requirements. Original stack preserved pending successful replacement integration.';confidence='high';blocker='Correct the external Datadog API target and obtain a passing check on independently approved PR469, then merge and verify it, then reassess source PR460 closure without native-stack history rewrites.'
 elif any(p['number']==463 for p in prs):
  classification='CONSOLIDATE';rationale='Shared pilot/publication boundaries repaired at 697bd58b and a125a347: canonical capacity counting, persistence failure propagation and source-locked atomic completion. Three original review concerns were addressed after PostgreSQL regression and actual Cron compilation. A subsequent review cites unavailable SHA39120; actual published and replacement ancestry pass, but that new thread is retained open for exact-release review. Standalone PR469 preserves the source history and migration ancestry on current main; native originals remain intact.';confidence='high';blocker='Complete the external Datadog health check on independently approved PR469 (backend/schema/client validation passes), then merge the replacement and verify behavior before closing either original. No native descendant rebase is permitted.'
 else:
  classification='BLOCKED_OR_AMBIGUOUS';rationale='Unique differences remain and equivalence or product intent is not established sufficiently for mutation.';confidence='limited';blocker='Review unique source differences and establish authoritative intended target/acceptance.'
 action='Preserved branch; no audit deletion.' if name in livebranches else 'Remote ref concurrently deleted outside this audit; exact head retained in verified recovery bundle. No audit deletion or remote restoration.';result=[]
 for p in prs:
  n=p['number']
  if n in closures:action+=' Closed redundant unmerged PR after exact-head ancestry verification.';result.extend([closures[n]['url'],closures[n]['comment']])
  elif p['merged']:result.append('https://github.com/diegueins680/tdf-app/commit/'+p['merged_commit'])
  if n==460 and (P/'retarget-460-after.json').exists():action+=' Retargeted PR460 to main; head unchanged.';result.append(p['url'])
  if n==463 and (P/'pr463-fix-after.json').exists():action+=' Published normal repair commits 697bd58b5ea1c8134be490cce588b65e9a08dad8 and a125a347cacb8d941a515171e2a81d4c92f2c2d2; verified all three addressed threads resolved; carried history into standalone PR469, now merged as de76dc7df; the original source PR463 was subsequently closed with verified integration evidence; its branch remains.';result.append(p['url'])
  if n==460 and p['merged']:action+=' GitHub automatically recognized merge through PR469/de76dc7df; no separate direct merge call.'
  if n==462:action+=' PR462 merged concurrently by another actor; not an audit merge.'
 if name in event:action+=(' Source contribution integrated through verified normal merge465; see target CI and closure evidence.' if integrated and read('merge465-verified.json') else ' Preserved source attribution in replacement PR465; replacement not yet merged.');result.append('https://github.com/diegueins680/tdf-app/pull/465')
 target_pr=next((p for p in prs if p['state']=='open'),None)
 current_target=target_pr['base'] if target_pr else b['target']
 target_sha=livebranches.get(current_target,{}).get('commit',{}).get('sha') or (target_pr['base_sha'] if target_pr else None)
 current_relation={'target':current_target,'target_sha':target_sha}
 if target_sha:
  try:
   ca,cb=git('rev-list','--left-right','--count',current_sha+'...'+target_sha).split()
   current_relation.update(ahead=int(ca),behind=int(cb),merge_base=git('merge-base',current_sha,target_sha))
  except subprocess.CalledProcessError:current_relation['limitation']='Current snapshot object not available locally; initial comparison retained.'
 refs=(P/f'branch-evidence/{ev}-references.txt').read_text()
 deployments=read(f'deployment-matches/{quoted}.json',[])
 rows.append({**b,'branch':name,'owner_or_creator':('PR author(s), creator unavailable: '+ '; '.join(sorted({p['author'] for p in prs}))) if prs else 'Creator unavailable; last commit author: '+b['author'],'initial_head':sha,'current_head':current_sha if name in livebranches else None,'last_observed_head':current_sha,'remote_present':name in livebranches,'current_relation':current_relation,'initial_target':b['target'],'current_main':main,'protected':bp['protected'],'rules':read(f'branch-rules/{quoted}.json',[]),'operational_status':operational.get(name,'Not protected by captured GitHub branch rules; external preview configuration not fully verifiable.'),'references_on_main':refs,'deployments':[{'id':d['id'],'environment':d['environment'],'ref':d['ref'],'sha':d['sha'],'url':d['url']} for d in deployments],'pull_requests':prs,'checks':checks,'statuses':statuses,'required_checks':'main protection has no named required status contexts; repository-prescribed validation and required review/conversation rules still apply','conflicts':'; '.join(f"PR{p['number']} {p['mergeable_state']} (mergeable={p['mergeable']})" for p in prs) or 'No PR mergeability; see recorded git comparisons','dependencies_or_overlaps':dependencies,'purpose':'; '.join(p['title'] for p in prs) or b['subject'],'classification':classification,'confidence':confidence,'rationale':rationale,'action':action,'result_links':result,'blocker_or_preservation_reason':blocker,'recovery_sha':absent.get(name,{}).get('last_observed_sha'),'recovery_command':absent.get(name,{}).get('recovery_command'),'evidence':[f'branch-evidence/{ev}-commits.txt',f'branch-evidence/{ev}-changes.txt',f'branch-evidence/{ev}-files.txt',f'branch-evidence/{ev}-references.txt',f'checks/{sha}.json',f'status/{sha}.json']})
(REPORT/'branches.json').write_text(json.dumps(rows,indent=2)+'\n')
columns=['branch','initial_head','associated_prs','base','protection_or_operational','ahead_behind','checks_reviews','conflicts','dependencies_overlaps','unique_purpose_changes','classification','confidence','action','evidence_rationale','result','recovery']
with open(REPORT/'branches.csv','w') as f:
 w=csv.DictWriter(f,fieldnames=columns);w.writeheader()
 for r in rows:
  w.writerow(dict(zip(columns,[r['branch'],r['sha'],'; '.join(p['url'] for p in r['pull_requests']),'; '.join(sorted({p['base'] for p in r['pull_requests']})) or r['target'],r['operational_status'],f"+{r['ahead']}/-{r['behind']} vs initial intended target; +{r['main_ahead']}/-{r['main_behind']} vs initial main; merge base {r['merge_base']}; current head {r['current_head']}, relation {r['current_relation']}",'; '.join(f"{p['number']}: {p['review_decision'] or 'no required approval recorded'}, {len(p['unresolved_threads'])} unresolved" for p in r['pull_requests'])+'; '+str(dict(collections.Counter(c['conclusion'] or c['status'] for c in r['checks']))),r['conflicts'],'; '.join(str(p['pr'])+': '+p['branch'] for p in r['dependencies_or_overlaps']),r['purpose']+'; files: '+', '.join(r['files']),r['classification'],r['confidence'],r['action'],r['rationale']+' Evidence: '+', '.join(r['evidence']),'; '.join(r['result_links']),r['recovery_command'] or 'Not deleted'])))
counts=dict(collections.Counter(r['classification'] for r in rows));(REPORT/'classification-counts.json').write_text(json.dumps(counts,indent=2)+'\n');print('Reconciled',len(rows),'unique branches:',counts)
