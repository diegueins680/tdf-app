import datetime,hashlib,json,sqlite3,time
from pathlib import Path
from server import make_runtime
s,p,c=make_runtime();root=Path(c['STATE_DIR']);folder=root/'backups';folder.mkdir(exist_ok=True);stamp=datetime.datetime.now(datetime.timezone.utc).strftime('%Y%m%dT%H%M%SZ');destination=folder/(stamp+'.sqlite3')
with s.db() as source:
 with sqlite3.connect(destination) as dest:source.backup(dest)
 assets=[dict(row) for row in source.execute('SELECT id,path,size,sha256,status FROM assets')]
receipt={'at':time.time(),'database':destination.name,'database_sha256':hashlib.sha256(destination.read_bytes()).hexdigest(),'assets':assets,'secret_recovery':'Encryption and signing keys require separately protected custody; not included in this manifest.','off_host_backup':'not_configured; local database snapshot and asset manifest are not a complete disaster recovery backup'}
(folder/(stamp+'.json')).write_text(json.dumps(receipt,indent=2));print(json.dumps({'backup':destination.name,'asset_count':len(assets),'off_host':False}))
