"""Prepare/activate a separate, private-only TikTok sandbox on the existing host."""
import hashlib,json,os,pathlib,pwd,secrets,subprocess,sys,time
root=pathlib.Path('/opt/tdf-campaign-platform-sandbox');state=pathlib.Path('/var/lib/tdf-campaign-platform-sandbox');etc=pathlib.Path('/etc/tdf-campaign-platform-sandbox');username='tdf-campaign-sandbox';config_file=etc/'config.json';mode=sys.argv[1]
if mode=='prepare':
 for unit in ('tdf-campaign-sandbox-api.service','tdf-campaign-sandbox-worker.service'):
  if subprocess.run(['systemctl','is-active','--quiet',unit]).returncode==0:raise RuntimeError('Stop only the isolated sandbox API and worker before replacing its release.')
 try:user=pwd.getpwnam(username)
 except KeyError:
  subprocess.run(['useradd','--system','--home-dir',str(state),'--no-create-home','--shell','/usr/sbin/nologin',username],check=True);user=pwd.getpwnam(username)
 for path in (state,state/'assets',state/'backups'):
  path.mkdir(parents=True,exist_ok=True);os.chown(path,user.pw_uid,user.pw_gid);path.chmod(0o700)
 etc.mkdir(exist_ok=True);etc.chmod(0o750);os.chown(etc,0,user.pw_gid)
 if config_file.exists():
  cfg=json.loads(config_file.read_text());assert cfg['STATE_DIR']==str(state) and cfg['TIKTOK_MODE']=='sandbox'
 else:
  from cryptography.fernet import Fernet
  cfg={'STATE_DIR':str(state),'PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform-sandbox','AUTH_URL':'https://api.tdfrecords.net/session','BIND':'172.21.0.1','PORT':8792,'ALLOWED_ORIGINS':['https://api.tdfrecords.net','https://www.tdfrecords.net','https://tdfrecords.net'],'FRAME_ORIGINS':['https://www.tdfrecords.net','https://tdfrecords.net'],'ENCRYPTION_KEY':Fernet.generate_key().decode(),'MEDIA_KEY':secrets.token_hex(32),'TIKTOK_MODE':'sandbox','TIKTOK_CLIENT_AUDITED':False,'TIKTOK_SANDBOX_ADMIN_ID':9,'TIKTOK_SANDBOX_USERNAME':'tdf.records.label','TIKTOK_VERIFIED_MEDIA_PREFIX':'https://api.tdfrecords.net/campaign-platform-sandbox/media/','TDF_ACCOUNT_IDS':{}}
 source=root/'candidate';files=sorted(p for p in source.rglob('*') if p.is_file() and '__pycache__' not in str(p));manifest={str(p.relative_to(source)):hashlib.sha256(p.read_bytes()).hexdigest() for p in files};digest=hashlib.sha256(json.dumps(manifest,sort_keys=True).encode()).hexdigest()[:20]
 release=root/'releases'/digest;release.parent.mkdir(parents=True,exist_ok=True)
 if release.exists():assert all(hashlib.sha256((release/n).read_bytes()).hexdigest()==v for n,v in manifest.items())
 else:source.rename(release)
 cfg['VERSION']=digest;config_file.write_text(json.dumps(cfg));config_file.chmod(0o640);os.chown(config_file,0,user.pw_gid)
 subprocess.run(['/opt/tdf-campaign-platform/venv/bin/python','-m','unittest','discover','-s',str(release/'tests'),'-v'],check=True,capture_output=True)
 link=root/'next';link.symlink_to(release);os.replace(link,root/'current')
 (root/'source-manifest.json').write_text(json.dumps(manifest,indent=2))
 print(json.dumps({'phase':'prepared','release':digest,'files':len(manifest),'tests':'PASS','separate_os_user':username,'private_only':True,'production_unchanged':True}))
elif mode=='activate':
 cfg=json.loads(config_file.read_text());assert cfg['TIKTOK_MODE']=='sandbox' and cfg['TIKTOK_CLIENT_AUDITED'] is False and cfg['TIKTOK_CLIENT_KEY'].startswith('sb') and cfg['TIKTOK_CLIENT_SECRET']
 live=json.loads(pathlib.Path('/etc/tdf-campaign-platform/config.json').read_text());assert live['STATE_DIR']!=cfg['STATE_DIR'] and live.get('TIKTOK_CLIENT_KEY')!=cfg['TIKTOK_CLIENT_KEY']
 for name,program,description in [('api','server.py','API'),('worker','worker.py','worker')]:
  unit=f'''[Unit]\nDescription=TDF TikTok isolated sandbox {description}\nAfter=network-online.target\nWants=network-online.target\n\n[Service]\nType=simple\nUser={username}\nGroup={username}\nWorkingDirectory={root}/current\nEnvironment=CAMPAIGN_CONFIG={config_file}\nEnvironment=PYTHONDONTWRITEBYTECODE=1\nExecStart=/opt/tdf-campaign-platform/venv/bin/python {root}/current/{program}\nRestart=on-failure\nRestartSec=5\nUMask=0077\nNoNewPrivileges=true\nPrivateTmp=true\nProtectSystem=strict\nProtectHome=true\nReadWritePaths={state}\nMemoryMax=256M\nCPUQuota=40%\nTasksMax=64\nLimitNOFILE=1024\n\n[Install]\nWantedBy=multi-user.target\n'''
  pathlib.Path('/etc/systemd/system',f'tdf-campaign-sandbox-{name}.service').write_text(unit)
 subprocess.run(['systemctl','daemon-reload'],check=True,capture_output=True)
 subprocess.run(['systemctl','enable','--now','tdf-campaign-sandbox-api.service','tdf-campaign-sandbox-worker.service'],check=True,capture_output=True)
 caddy=pathlib.Path('/opt/tdf/production/Caddyfile');before=caddy.read_text();marker='\thandle /campaign-platform-sandbox/* {'
 if marker not in before:
  assert before.startswith('api.tdfrecords.net {\n') and before.count('handle /campaign-platform/*')==1
  new=before.replace('api.tdfrecords.net {\n','api.tdfrecords.net {\n\tredir /campaign-platform-sandbox /campaign-platform-sandbox/ 308\n\thandle /campaign-platform-sandbox/* {\n\t\treverse_proxy 172.21.0.1:8792\n\t}\n',1)
  subprocess.run(['docker','exec','-i','tdf-production-edge-1','caddy','validate','--config','/dev/stdin','--adapter','caddyfile'],input=new.encode(),check=True,capture_output=True)
  assert caddy.read_text()==before
  (root/'Caddyfile.before-sandbox').write_text(before);caddy.write_text(new)
  result=subprocess.run(['docker','exec','tdf-production-edge-1','caddy','reload','--config','/etc/caddy/Caddyfile','--adapter','caddyfile'],capture_output=True)
  if result.returncode:
   caddy.write_text(before);raise RuntimeError('Sandbox edge reload failed; previous file restored')
 subprocess.run(['systemctl','restart','tdf-campaign-sandbox-api.service','tdf-campaign-sandbox-worker.service'],check=True,capture_output=True)
 print(json.dumps({'phase':'active','release':cfg['VERSION'],'url':cfg['PUBLIC_BASE']+'/','audit_flag':False,'production_config_and_services_unchanged':True}))
else:raise ValueError('Unknown sandbox deployment phase')
