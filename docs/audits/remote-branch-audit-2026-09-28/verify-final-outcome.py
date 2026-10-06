import pathlib,json,datetime
P=pathlib.Path(__file__).parent
F=sorted(f for f in P.glob('final-*') if f.is_dir() and (f/'completed.txt').exists())[-1]
def flat(p):return [x for page in json.loads(p.read_text()) for x in page]
branches={b['name']:b['commit']['sha'] for b in flat(F/'branches.json')};prs={p['number']:p for p in flat(F/'pulls.json')};opened={n for n,p in prs.items() if p['state']=='open'}
mutations=[json.loads(s) for s in (P/'mutations.jsonl').read_text().splitlines()];closures={m['pr']:m for m in mutations if m.get('action')=='closed_unmerged'}
assert len(branches)==73 and len(prs)==464 and len(opened)==23
assert branches['main']=='b07f67c3ebe2022d5f70e94578944b6401616696'
assert len(closures)==43
for n,m in closures.items():
 p=prs[n];assert p['state']=='closed' and not p['merged_at'] and p['head']['sha']==m['sha'],n
sources=json.loads((P/'events-source-closure-candidates.json').read_text())
for source in sources:assert branches[source['branch']]==source['head'],source['branch']
assert len(sources)==35
assert prs[337]['merged_at'] and prs[465]['merged_at']
recovery=json.loads((P/'concurrent-deletions-recovery.json').read_text())['absent_branches']
assert len(recovery)==73
# Recovery record field names are retained exactly in the source evidence.
for r in recovery:assert r['branch'] not in branches,r['branch']
assert json.loads((P/'events-closure-main-ci.json').read_text())['all_passed']
proof={'verified_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'snapshot':F.name,'live_branches':len(branches),'open_prs':len(opened),'total_prs':len(prs),'open_pr_numbers':sorted(opened),'verified_unmerged_closures':len(closures),'event_source_branches_retained':len(sources),'concurrent_deletions_retained':len(recovery),'main':branches['main'],'normal_post_merge_ci_passed':True}
(P/'final-outcome-verification.json').write_text(json.dumps(proof,indent=2)+'\n');print(json.dumps(proof))
