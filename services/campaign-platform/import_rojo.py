"""One-time idempotent Roj(o) migration from the reviewed delivery and native receipts.
Run on the server as an administrator. This imports existing effects; it never sends them again.
"""
import datetime, hashlib, json, os, pwd, sqlite3, subprocess, time
from pathlib import Path
from domain import Store, uid
from server import make_runtime
s,providers,cfg=make_runtime();folder=Path('/opt/tdf-campaign-platform/import/entrega');state_dir=Path(cfg['STATE_DIR']);original=Path('/opt/tdf-campaign-platform/import/master.mp4');actor={'id':9,'name':'Diego Saa','admin':True};key='rojo-20261028-v1'
receipt=json.loads(original.with_name('master-import-receipt.json').read_text());assert receipt['matches_original'] and receipt['sha256']=='b41584f625938a5774803a62697e891179a352c3792248eed1c5b161e76966da'
# Verify the original independently of transport receipt before admitting it.
h=hashlib.sha256()
with original.open('rb') as f:
 for block in iter(lambda:f.read(4*1024*1024),b''):h.update(block)
assert h.hexdigest()==receipt['sha256']
ledger=json.loads((folder/'ejecucion/registro-de-ejecucion.json').read_text())['actions'];calendar=json.loads((folder/'calendario-14-dias.json').read_text())['acciones'];digest=hashlib.sha256(json.dumps({'calendar':calendar,'ledger':ledger},sort_keys=True).encode()).hexdigest()
with s.db() as d:
 existing=d.execute('SELECT * FROM imports WHERE key=?',(key,)).fetchone()
 if existing:
  assert existing['digest']==digest,'Import input changed; reconcile rather than overwrite'
  print(json.dumps({'status':'already_imported','key':key}));raise SystemExit(0)
s.user(actor);cid=hashlib.sha256(key.encode()).hexdigest()[:32];assets={};now=time.time();user=pwd.getpwnam('tdf-campaign')
with s.db(True) as d:
 # Internal pilot allowance is explicit and does not alter Free5GB for other users.
 d.execute('UPDATE users SET storage_limit=25000000000 WHERE id=9')
 d.execute('INSERT INTO campaigns(id,owner,name,objective,timezone,status,version,approved_version,rights_confirmed,destination,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)',(cid,9,'Roj(o) — En vivo desde TDF Records','Quédate en la sala: reproducciones relevantes, tiempo de visualización y suscripciones a TDF.','America/Guayaquil','approved',1,1,1,'https://youtu.be/uRNyIMLiqU8',now,now))
 def add_asset(source,name,original_flag=False):
  aid=hashlib.sha256((key+':'+name).encode()).hexdigest()[:32];target=state_dir/'assets'/aid
  if not target.exists():os.link(source,target)
  os.chown(target,0,user.pw_gid);target.chmod(0o440);size=source.stat().st_size;h=hashlib.sha256()
  with source.open('rb') as f:
   for b in iter(lambda:f.read(4*1024*1024),b''):h.update(b)
  d.execute('INSERT INTO assets(id,owner,campaign,name,path,size,reserved,uploaded,sha256,status,original,created_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)',(aid,9,cid,name,aid,size,size,size,h.hexdigest(),'ready',int(original_flag),now));assets[name]=aid;return aid
 add_asset(original,'Roj(o) en vivo desde TDF.mp4',True)
 for sub in ['clips','stories','carrusel']:
  for f in sorted((folder/sub).glob('*')):
   if f.suffix.lower() in ('.mp4','.png'):add_asset(f,f.name)
 add_asset(folder/'youtube/miniatura.jpg','miniatura.jpg')
 # Reuse the verified existing connection only for its actual owning actor and ID.
 raw=subprocess.check_output(['docker','exec','tdf-production-db-1','psql','-U','postgres','-d','tdf_hq','-Atqc',"SELECT json_build_object('owner',party_id,'id',external_user_id,'handle',handle,'access_token',access_token) FROM social_sync_account WHERE id=3 AND party_id=9 AND platform='instagram'"],text=True);ig=json.loads(raw);assert ig['id']=='17841445628242005' and ig['handle']=='tdf.records.label';ig_id=uid();d.execute('INSERT INTO accounts(id,owner,platform,external_id,handle,scope,credentials,permissions) VALUES(?,?,?,?,?,?,?,?)',(ig_id,9,'instagram',ig['id'],ig['handle'],'tdf',providers.vault.seal({'access_token':ig['access_token']}),json.dumps(json.loads(Path('/opt/tdf-campaign-platform/import/instagram-permissions.json').read_text())['scopes'])))
 clips=json.loads((folder/'clips/fichas-y-copies.json').read_text());clipfiles={c['id']:c['archivo'] for c in clips};published={a['piece']:a for a in ledger if a.get('published_at_quito') or a.get('pinned_at_quito')}
 base='https://youtu.be/uRNyIMLiqU8';piece_map={}
 for action in calendar:
  label=action['pieza'];channel=action['canal_formato'];platform='instagram' if channel.startswith('Instagram') else 'youtube' if channel.startswith('YouTube') else None
  if not platform:continue
  fmt='reel' if 'Reel' in channel else 'story' if 'Stor' in channel else 'carousel' if 'carrusel' in channel.lower() else 'short' if 'Short' in channel else 'community' if 'Comunidad' in channel else 'comment' if 'comentario' in channel else 'video'
  # Expand the launch sequence into distinct ordered Story actions.
  names=['S01','S02','S03'] if label=='S01 + S02 + S03' else [label]
  for n,name in enumerate(names):
   aid=assets.get(clipfiles.get(name,''));settings={}
   if fmt=='story':aid=next((v for k,v in assets.items() if k.startswith(name+'-')),None)
   if fmt=='carousel':settings['children']=[v for k,v in assets.items() if k.startswith('CA') or k.startswith('C0')];aid=settings['children'][0] if settings['children'] else None
   pid=hashlib.sha256((key+':'+name+':'+channel).encode()).hexdigest()[:32];piece_map[name]=pid
   date=datetime.datetime.fromisoformat(action['fecha']+'T'+action['hora_quito']+':00-05:00').timestamp()+n*60;done=name=='YT-FULL' or name in published;item=published.get(name,{})
   url=base if name=='YT-FULL' else item.get('url');ext='uRNyIMLiqU8' if name=='YT-FULL' else 'pUtIlM8Bic4' if name=='CL02' and platform=='youtube' else None
   if name=='YT-FULL':aid=assets['Roj(o) en vivo desde TDF.mp4'];settings={'recording_date':'2025-02-24','imported_publication':'2026-10-28T19:34:10.599-05:00','youtube_checks_reviewed':True}
   status='published' if done else 'approved';account=ig_id if platform=='instagram' else None
   d.execute('INSERT INTO pieces(id,campaign,name,platform,format,account,copy,asset,destination,due_at,version,approved_version,tdf_approval_version,settings,status,external_id,public_url,verified_at,created_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',(pid,cid,name,platform,fmt,account,(folder/'youtube/descripcion.txt').read_text() if name=='YT-FULL' else action['copy'],aid,action.get('destino') or base,date,1,1,1 if platform=='instagram' else None,json.dumps(settings),status,ext,url,now if done else None,now))
 # Published artist-owned Instagram collaboration is an observation, not an agent-created job.
 observed=uid();d.execute('INSERT INTO pieces(id,campaign,name,platform,format,copy,destination,due_at,status,public_url,verified_at,created_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?)',(observed,cid,'Reel de lanzamiento del artista (existente)','instagram','reel','Autor @rojelioelrojorojas; colaboradores @rojo.uio y @tdf.records.label. Publicación observada, no creada por esta migración.',base,datetime.datetime.fromisoformat('2026-10-28T22:37:00-05:00').timestamp(),'published','https://www.instagram.com/rojelioelrojorojas/reel/DfDyUe-Cqgu/',now,now))
 launch=datetime.datetime.fromisoformat('2026-10-28T19:34:10.599-05:00').timestamp()
 for hours in [24,72,168,336]:d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?)',(uid(),cid,piece_map['YT-FULL'],'metrics_'+str(hours),json.dumps({'checkpoint_hours':hours}),launch+hours*3600))
 # Checkpoint kinds are distinct; every measurement has one durable identity.
 d.execute('INSERT INTO imports VALUES(?,?,?)',(key,now,digest));s.audit(d,9,'pilot.import',cid,details={'source_digest':digest,'original_sha256':receipt['sha256'],'published_not_replayed':True,'invitations_already_delivered':True,'internal_storage_allowance':25000000000});s.notice(d,9,'Roj(o) importado. La sesión completa y publicaciones anteriores no se repetirán. Revisa las conexiones y las acciones atrasadas antes de activar.',cid)
print(json.dumps({'campaign':cid,'assets':len(assets),'import_digest':digest,'original_sha256':receipt['sha256'],'publication_jobs_created':0}))
