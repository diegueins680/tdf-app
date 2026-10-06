import pathlib,sys,subprocess,shlex,json
P=pathlib.Path(__file__).parent;sys.path.insert(0,str(P/'cutover-provenance-followup-20261003/scripts'));import production_access as access
source='''import json,pathlib,subprocess
config=dict(line.split('=',1) for line in pathlib.Path('/opt/tdf/production/.env').read_text().splitlines() if '=' in line and not line.lstrip().startswith('#'))
result={}
for service,key in [('api','TDF_IMAGE'),('db','POSTGRES_IMAGE')]:
 c=json.loads(subprocess.check_output(['docker','inspect','tdf-production-'+service+'-1']))[0]
 i=json.loads(subprocess.check_output(['docker','image','inspect',c['Image']]))[0]
 result[service]={'configured':config.get(key),'running_reference':c['Config'].get('Image'),'image_id':c['Image'],'repo_digests':i.get('RepoDigests')}
print(json.dumps(result))
'''
r=subprocess.run(access.connection_args()+['python3 -c '+shlex.quote(source)],capture_output=True,text=True,timeout=30)
(P/'cutover-provenance-image-binding.json').write_text(r.stdout if r.returncode==0 else json.dumps({'exit':r.returncode,'stderr':r.stderr}))
print('exit',r.returncode);print(r.stdout if r.returncode==0 else r.stderr[:600])
