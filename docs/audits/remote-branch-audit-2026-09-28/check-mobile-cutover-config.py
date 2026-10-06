import pathlib,subprocess,os,json
P=pathlib.Path(__file__).parent;cwd=P/'events/tdf-mobile';env={k:v for k,v in os.environ.items() if k not in ['EXPO_PUBLIC_API_TOKEN','EXPO_PUBLIC_API_BASE','EXPO_PUBLIC_UPLOAD_URL','EAS_BUILD_PROFILE']};env.update({'EXPO_NO_TELEMETRY':'1','npm_config_cache':str(P/'mobile-cutover-npm-cache')})
eas=json.loads((cwd/'eas.json').read_text());rows=[]
cases=[('production fallback',{'EAS_BUILD_PROFILE':'production'},'https://api.tdfrecords.net'),('preview EAS',{'EAS_BUILD_PROFILE':'preview',**eas['build']['preview']['env']},'https://api.tdfrecords.net'),('production EAS',{'EAS_BUILD_PROFILE':'production',**eas['build']['production']['env']},'https://api.tdfrecords.net'),('development',{'EAS_BUILD_PROFILE':'development'},'http://localhost:8080'),('explicit isolated override',{'EAS_BUILD_PROFILE':'preview','EXPO_PUBLIC_API_BASE':'https://isolated.invalid','EXPO_PUBLIC_UPLOAD_URL':'https://isolated.invalid/drive/upload'},'https://isolated.invalid')]
for label,extra,expected in cases:
 r=subprocess.run(['node','node_modules/expo/bin/cli','config','--type','public','--json'],cwd=cwd,env=env|extra,text=True,capture_output=True);assert r.returncode==0,r.stderr
 config=json.loads(r.stdout);assert config['extra']['apiBase']==expected and config['extra']['uploadUrl']==expected+'/drive/upload'
 rows.append({'case':label,'api':config['extra']['apiBase'],'upload':config['extra']['uploadUrl'],'exit_code':r.returncode});print(label,'PASS',flush=True)
(P/'mobile-cutover-config.json').write_text(json.dumps(rows,indent=2))
