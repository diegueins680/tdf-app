import collect,json,subprocess,datetime
P=collect.ROOT;sha='5c9bb3b5daf82fa438fe9f0dd1b796afd699d0a6';body=(P/'events-review-description.md').read_text()
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',path,*args],env=collect.ENV,text=True))
x=api(collect.REPO+'/pulls/465');assert x['state']=='open' and x['head']['sha']==sha and x['base']['ref']=='main'
(P/'events-review-description-before.json').write_text(json.dumps(x,indent=2)+'\n')
api(collect.REPO+'/pulls/465','--method','PATCH','-f','body='+body)
y=api(collect.REPO+'/pulls/465');assert y['head']['sha']==sha and y['body']==body
(P/'events-review-description-after.json').write_text(json.dumps(y,indent=2)+'\n')
with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'updated_validation_description','pr':465,'sha':sha,'url':y['html_url']})+'\n')
print('Verified current-head description')
