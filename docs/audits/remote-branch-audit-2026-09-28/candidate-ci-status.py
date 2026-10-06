import collect,subprocess,json,datetime
P=collect.ROOT
def api(path,*args):return json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+path,*args],env=collect.ENV,text=True))
main=api('/git/ref/heads/main')['object']['sha'];data={'captured_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),'main':main}
for label,sha in [('main',main),('event',api('/pulls/465')['head']['sha']),('cutover',api('/pulls/468')['head']['sha'])]:
 checks=[c for pg in api('/commits/'+sha+'/check-runs?per_page=100','--paginate','--slurp') for c in pg['check_runs']]
 data[label+'_checks']=checks
 print(label,[(c['name'],c['status'],c['conclusion'],c['id']) for c in checks if c['conclusion'] not in ('success','skipped')],flush=True)
(P/'candidate-current-ci.json').write_text(json.dumps(data,indent=2))
