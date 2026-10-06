import pathlib,json,re,hashlib,urllib.parse
P=pathlib.Path(__file__).parent;R=P/'report'
F=sorted(p for p in P.glob('final-*') if p.is_dir() and (p/'completed.txt').exists())[-1]
def flat(file):return [x for pg in json.loads(file.read_text()) for x in pg]
initial={b['name']:b['commit']['sha'] for b in flat(P/'branches-pages.json')};live={b['name']:b['commit']['sha'] for b in flat(F/'branches.json')}
rows=json.loads((R/'branches.json').read_text());added=json.loads((R/'added-branches.json').read_text());allrows=rows+added
assert len(rows)==134 and len({r['branch'] for r in rows})==134 and {r['branch'] for r in rows}==set(initial)
assert all(r['initial_head']==initial[r['branch']] for r in rows)
assert len(allrows)==146 and len({r['branch'] for r in allrows})==146
assert {r['branch'] for r in allrows if r['remote_present']}==set(live)
for r in allrows:
 if r['remote_present']:assert (r.get('current_head') or r.get('head_sha'))==live[r['branch']],r['branch']
 else:assert r.get('recovery_command') or r.get('recovery','Not deleted')!='Not deleted',r['branch']
assert (R/'mutations.jsonl').read_bytes()==(P/'mutations.jsonl').read_bytes()
classes={'PROTECTED_OR_OPERATIONAL','READY_TO_MERGE','FIX_THEN_MERGE','CONSOLIDATE','ALREADY_MERGED','REDUNDANT_OR_SUPERSEDED','OBSOLETE','KEEP_UNMERGED','BLOCKED_OR_AMBIGUOUS'}
assert all(r['classification'] in classes for r in allrows)
missing=[]
for file in R.glob('*.md'):
 for link in re.findall(r'\]\(([^)]+)\)',file.read_text()):
  if link.startswith(('http:','https:','#','mailto:')):continue
  target=urllib.parse.unquote(link.split('#')[0]).strip('<>')
  if target and not (file.parent/target).exists():missing.append((file.name,target))
assert not missing,missing
rec=json.loads((R/'reconciliation.json').read_text());assert rec['current_branches']==len(live) and not rec['unaccounted_initial_branches']
proof={'snapshot':F.name,'initial_rows':len(rows),'historical_added_rows':len(added),'all_historical_names':len(allrows),'live_names':len(live),'concurrently_absent':sum(not r['remote_present'] for r in allrows),'unaccounted':0,'broken_local_report_links':missing,'mutation_ledger_matches':True}
(P/'report-verification.json').write_text(json.dumps(proof,indent=2));print(json.dumps(proof))
