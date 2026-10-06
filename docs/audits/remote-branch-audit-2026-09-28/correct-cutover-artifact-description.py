import collect,json,subprocess,datetime
P=collect.ROOT;repo=collect.REPO;sha='68558f5409f9a976c0a4a9bd742e627f75f089cf'
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
x=api(repo+'/pulls/468');assert x['state']=='open' and x['head']['sha']==sha
old='hosted specification discovery detected the renamed incomplete-validation heading; regenerated the single inventory heading'
new='hosted specification discovery detected the changed course documentation hash; regenerated that single inventory hash'
assert old in x['body'];body=x['body'].replace(old,new)
api(repo+'/pulls/468','--method','PATCH','-f','body='+body)
y=api(repo+'/pulls/468');assert y['head']['sha']==sha and y['body']==body
(P/'cutover-final-description.json').write_text(json.dumps(y,indent=2))
with open(P/'mutations.jsonl','a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'corrected_generated_artifact_description','pr':468,'sha':sha,'url':y['html_url'],'correction':'The preceding generated-inventory mutation changed the SHA256 of docs/courses/README.md, not an inventory heading. The commit diff and tests are authoritative; corrected PR description verified.'})+'\n')
