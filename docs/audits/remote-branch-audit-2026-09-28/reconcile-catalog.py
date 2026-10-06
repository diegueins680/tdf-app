import json,hashlib,subprocess,pathlib
root=pathlib.Path('/private/tmp/tdf-branch-audit-20260928/events'); folder=root/'docs/catalog-persistence'
r=json.load(open(root.parent/'events-catalog-initialized.json')); candidates={x['id']:x for x in r['candidates']}
p=folder/'catalog-list-decisions.json';d=json.load(open(p)); decisions={x['id']:x for x in d['decisions']}
old=json.loads(subprocess.check_output(['git','show','origin/chore/event-catalog-stale-reconciliation:docs/catalog-persistence/reports/static-list-inventory.json'],cwd=root)); historical={x['id']:x for x in old['candidates']}
ledger=json.load(open(folder/'event-catalog-retirements.json'))
mapping={'466aa6b7847efbc61fda':'b53cc0e14f15058ad16e','465def35066189c2212f':None,'26a352668341118b54ee':'0886e6835c7f5c533d9c','2d43a4c97f70b8e8a5bc':'f9819fa4b81bdd8f3f21','91ec5702d1c3efd7777e':'2223179c95a61ccff02d','ffb84e8fd3972fd0730d':'21babc54d68f0c44d55e','faf664611d7ab0aa954d':'bffc5decb0aa4633b170','82e3ab110124708c621c':None,'29021a526fad50de4bf4':'12abca4fa517da75ad03','34d5f2f94d1a165783ba':'3b37706d3daff89e8890','7578a30aa3cc5aa23870':'07da55e81a186d26df37'}
assert set(mapping)==set(decisions)-set(candidates)
def dump(p,obj): p.write_text(json.dumps(obj,indent=2,ensure_ascii=False)+'\n')
def digest(obj):return hashlib.sha256(json.dumps(obj,separators=(',',':'),ensure_ascii=False).encode()).hexdigest()
ret=[]
for id,new in mapping.items():
 if new:assert new in candidates and decisions[new]['reviewed']
 oldcandidate=historical.get(id)
 if not oldcandidate:
  e=next(x for x in ledger['retirements'] if x['replacementId']==id);oldcandidate={k:e[k] for k in ['file','kind','name']}
 reason='Current main and pinned mobile already review the replacement fingerprint; retain their exact decision and archive the superseded branch fingerprint.'
 if id=='465def35066189c2212f':reason='Current scripts/refresh-instagram-token.mjs implements runLifecycle with an explicit --check/--setup/--refresh validation and if branches; the historical command switch no longer exists. Lifecycle tests cover the replacement; no runtime capability is removed by archiving this scanner fingerprint.'
 if id=='82e3ab110124708c621c':reason='Current pinned mobile imports and re-exports OnboardingIntent from src/api/onboarding.ts. The old local literal union no longer exists; API authority and reviewed current onboarding options remain.'
 ret.append({'originalDecision':decisions[id],'originalDecisionSha256':digest(decisions[id]),'sourceRevision':'ea79b46addea67087c85c03ea6f9c29d45ca19da','source':{k:oldcandidate[k] for k in ['file','kind','name']},'replacementId':new,'reason':reason})
newreviews={
'dd65a3141ce82ff42537':('governed-reference-data','retain-authoritative-schema','event_logistics_activity.status','Wire projection of the persisted task states and matching Haskell EventTaskStatus. Database commit constraints govern completion and graph approvals; listing a state confers no authority.'),
'b97fcef81b98c6db8180':('governed-reference-data','retain-authoritative-schema','event_operation_lifecycle_state','Wire projection of the foundation lifecycle reference data. Allowed transitions, evidence and role checks remain database policy, not a choice granted by the client enum.'),
'4bfb42bc49c0a7b4bd07':('security-system-registry','retain-authoritative-schema','event_operation_lifecycle_transition_policy.required_authority','Projection of the persisted transition-policy authority codes. The server validates the authenticated session and current scoped roles; these response labels cannot grant permissions.'),
'6b64a4633b50c0a4115d':('genuine-technical-constant','retain-in-code','event_operation_error_protocol','Finite sanitized error protocol used by the event API and generated clients. The codes communicate typed failures without exposing SQL exceptions, hidden event identifiers or credentials; they are not business choices.')}
for id,(classification,disposition,model,reason) in newreviews.items():
 c=candidates[id];decisions[id]={'id':id,'classification':classification,'disposition':disposition,'specializedModel':model,'priority':'P3-retain','risk':'Client/schema drift could misrepresent state or authority; enforce persisted guards and regenerate both client projections.','justification':reason,'reviewed':True,'reviewMethod':'manual-source-and-contract-review','reviewBatch':'event-stack-integration-20260928','evidence':{k:c[k] for k in ['file','kind','name','values']}}
d['decisions']=[x for id,x in decisions.items() if id not in mapping]
dump(p,d)
dump(folder/'event-integration-retirements.json',{'schemaVersion':1,'mainRevision':'cc244b1f86603055997b51379b297baebfd3e7ce','mobileRevision':'2a0e5a99535d9ef199a3e3464a660192f882f72b','retirements':ret})
successors=[]
for e in ledger['retirements']:
 id=mapping.get(e['replacementId'],e['replacementId']);c=candidates[id];successors.append({'historicalReplacementId':e['replacementId'],'currentId':id,**{k:c[k] for k in ['file','kind','name','valueCount']}})
dump(folder/'event-catalog-current-successors.json',{'schemaVersion':1,'mainRevision':'cc244b1f86603055997b51379b297baebfd3e7ce','mobileRevision':'2a0e5a99535d9ef199a3e3464a660192f882f72b','successors':successors})
p=root/'tdf-hq-ui/src/pages/LoginPage.tsx';p.write_text(p.read_text().replace('AUTH_PASSWORD_REQUIREMENTS_ES, isValidAuthPassword','isValidAuthPassword'))
print('Reviewed four new projections; archived eleven old fingerprints with evidence; preserved historical ledger.')
