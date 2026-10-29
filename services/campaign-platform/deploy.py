"""Run over verified SSH on the dedicated TDF host. No credentials in output."""
import hashlib,json,os,pathlib,pwd,secrets,subprocess,time
root=pathlib.Path('/opt/tdf-campaign-platform');source=root/'candidate';state=pathlib.Path('/var/lib/tdf-campaign-platform');etc=pathlib.Path('/etc/tdf-campaign-platform')
assert pathlib.Path('/opt/tdf/production/Caddyfile').exists()
assert subprocess.check_output(['docker','inspect','tdf-production-edge-1','--format','{{.Config.Image}}'],text=True).strip()
try:user=pwd.getpwnam('tdf-campaign')
except KeyError:
 subprocess.run(['useradd','--system','--home-dir',str(state),'--no-create-home','--shell','/usr/sbin/nologin','tdf-campaign'],check=True);user=pwd.getpwnam('tdf-campaign')
for p in (state,state/'assets',state/'backups'):
 p.mkdir(parents=True,exist_ok=True);os.chown(p,user.pw_uid,user.pw_gid);p.chmod(0o700)
etc.mkdir(exist_ok=True);etc.chmod(0o750);os.chown(etc,0,user.pw_gid)
# Reuse only relevant configuration values, entirely on the server.
lines=pathlib.Path('/opt/tdf/production/api.env').read_text().splitlines();existing={}
for line in lines:
 if '=' in line and not line.lstrip().startswith('#'):
  key,value=line.split('=',1);existing[key]=value
config_file=etc/'config.json';config=json.loads(config_file.read_text()) if config_file.exists() else {}
from cryptography.fernet import Fernet
config.setdefault('ENCRYPTION_KEY',Fernet.generate_key().decode());config.setdefault('MEDIA_KEY',secrets.token_hex(32))
config.update(STATE_DIR=str(state),PUBLIC_BASE='https://api.tdfrecords.net/campaign-platform',AUTH_URL='https://api.tdfrecords.net/session',BIND='172.21.0.1',PORT=8791,ALLOWED_ORIGINS=['https://api.tdfrecords.net','https://www.tdfrecords.net','https://tdfrecords.net'],FRAME_ORIGINS=['https://www.tdfrecords.net','https://tdfrecords.net'],META_API_VERSION='v23.0',LOGO_PNG=str(root/'tdf-logo.png'))
config.setdefault('TDF_ACCOUNT_IDS',{'youtube':['UCx9Jpaw_XDrMtIdzWYlU51g'],'instagram':['17841445628242005'],'facebook':[existing['FACEBOOK_MESSAGING_PAGE_ID']] if existing.get('FACEBOOK_MESSAGING_PAGE_ID') else []})
for name in ('GOOGLE_CLIENT_ID','GOOGLE_CLIENT_SECRET','FACEBOOK_APP_ID','FACEBOOK_APP_SECRET','YOUTUBE_API_KEY','SMTP_HOST','SMTP_PORT','SMTP_USERNAME','SMTP_PASSWORD','SMTP_FROM'):
 if existing.get(name):config[name]=existing[name]
files=sorted(p for p in source.rglob('*') if p.is_file() and '__pycache__' not in str(p));manifest={str(p.relative_to(source)):hashlib.sha256(p.read_bytes()).hexdigest() for p in files};digest=hashlib.sha256(json.dumps(manifest,sort_keys=True).encode()).hexdigest()[:20];config['VERSION']=digest
release=root/'releases'/digest;release.parent.mkdir(exist_ok=True)
if not release.exists():source.rename(release)
config_file.write_text(json.dumps(config));config_file.chmod(0o640);os.chown(config_file,0,user.pw_gid)
subprocess.run(['python3','-m','venv','--system-site-packages',str(root/'venv')],check=True,capture_output=True)
subprocess.run([str(root/'venv/bin/pip'),'install','--disable-pip-version-check','-q','-r',str(release/'requirements.txt')],check=True,capture_output=True)
subprocess.run([str(root/'venv/bin/python'),'-m','unittest','discover','-s',str(release/'tests'),'-v'],check=True,capture_output=True)
previous=None;current=root/'current'
if current.is_symlink():previous=str(current.resolve())
next_link=root/'next';next_link.symlink_to(release);os.replace(next_link,current)
for name in ('tdf-campaign-api.service','tdf-campaign-worker.service','tdf-campaign-backup.service','tdf-campaign-backup.timer','tdf-campaign-monitor.service','tdf-campaign-monitor.timer','tdf-campaign-backup-request.service','tdf-campaign-backup-request.timer'):
 pathlib.Path('/etc/systemd/system',name).write_text((release/name).read_text())
subprocess.run(['systemctl','daemon-reload'],check=True,capture_output=True)
subprocess.run(['systemctl','enable','--now','tdf-campaign-api.service','tdf-campaign-worker.service','tdf-campaign-backup.timer','tdf-campaign-monitor.timer','tdf-campaign-backup-request.timer'],check=True,capture_output=True)
if previous and previous!=str(release):
 # Restart only this feature's processes. Uncertain provider effects remain quarantined by the durable job state.
 subprocess.run(['systemctl','restart','tdf-campaign-api.service','tdf-campaign-worker.service'],check=True,capture_output=True)
# The existing edge config must be the expected simple proxy or our exact prior extension.
caddy=pathlib.Path('/opt/tdf/production/Caddyfile');before=caddy.read_text();expected='api.tdfrecords.net {\n\treverse_proxy api:8080\n}\n'
if 'handle /campaign-platform/*' not in before:
 if before!=expected:raise RuntimeError('Edge changed; inspect before applying campaign route')
 backup=root/'Caddyfile.before-campaigns';backup.write_text(before);backup.chmod(0o600)
 new='api.tdfrecords.net {\n\thandle /campaign-platform/* {\n\t\treverse_proxy 172.21.0.1:8791\n\t}\n\thandle {\n\t\treverse_proxy api:8080\n\t}\n}\n';candidate=root/'Caddyfile.candidate';candidate.write_text(new)
 # Validate in the existing edge without restarting API or database.
 subprocess.run(['docker','exec','-i','tdf-production-edge-1','caddy','validate','--config','/dev/stdin','--adapter','caddyfile'],input=new.encode(),check=True,capture_output=True)
 caddy.write_text(new)
 reload=subprocess.run(['docker','exec','tdf-production-edge-1','caddy','reload','--config','/etc/caddy/Caddyfile','--adapter','caddyfile'],capture_output=True)
 if reload.returncode:
  caddy.write_text(before);raise RuntimeError('Caddy reload failed; file restored')
receipt={'release':digest,'previous_release':previous,'manifest':manifest,'api_url':config['PUBLIC_BASE']+'/','source_tests':'passed on server','new_units':['tdf-campaign-api.service','tdf-campaign-worker.service','tdf-campaign-backup.timer','tdf-campaign-monitor.timer','tdf-campaign-backup-request.timer'],'note':'New sidecar writer; stop campaign API, worker, both backup services, monitor service and all three campaign timers before future coordinated production writer fencing. Pinned host admission policy was not weakened.','at':time.time()}
(root/'deployment-receipt.json').write_text(json.dumps(receipt,indent=2));print(json.dumps({k:v for k,v in receipt.items() if k!='manifest'}))
