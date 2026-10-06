import pathlib,subprocess,os,json
P=pathlib.Path(__file__).parent;cwd=P/'events/tdf-mobile';env={k:v for k,v in os.environ.items() if k not in ['EXPO_PUBLIC_API_TOKEN','EXPO_PUBLIC_API_BASE','EXPO_PUBLIC_UPLOAD_URL','EAS_BUILD_PROFILE']};env.update({'EAS_BUILD_PROFILE':'production','EXPO_NO_TELEMETRY':'1','npm_config_cache':str(P/'mobile-cutover-npm-cache')})
results=[]
for cmd,name in [(['npm','run','release:check'],'mobile-cutover-release.log'),(['npm','test','--','--watchman=false'],'mobile-cutover-tests.log'),(['python3','-m','unittest','discover','-s','scripts/tests'],'mobile-cutover-python.log')]:
 with (P/name).open('w') as log:r=subprocess.run(cmd,cwd=cwd,env=env,stdout=log,stderr=subprocess.STDOUT)
 results.append({'command':cmd,'log':name,'exit_code':r.returncode});(P/'mobile-cutover-validation.json').write_text(json.dumps(results,indent=2));print(name,r.returncode,flush=True)
 if r.returncode:raise SystemExit(r.returncode)
