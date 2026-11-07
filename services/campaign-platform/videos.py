"""Artist video library using the existing quota, assets, authentication and backup database.

No external fetch, video download, account creation or provider publication occurs here.
Artist authority is checked against Core on every management request, including publish.
"""
import json
import re
import subprocess
import time
import urllib.parse
import urllib.request
from pathlib import Path
from domain import Denied, uid

SCHEMA = '''
CREATE TABLE IF NOT EXISTS artist_videos(
 id TEXT PRIMARY KEY, artist_id INTEGER NOT NULL, campaign TEXT NOT NULL REFERENCES campaigns(id),
 owner INTEGER NOT NULL REFERENCES users(id), title TEXT NOT NULL, description TEXT NOT NULL,
 source_url TEXT NOT NULL, asset TEXT REFERENCES assets(id), source_key TEXT NOT NULL,
 poster TEXT NOT NULL DEFAULT '', scope TEXT NOT NULL CHECK(scope IN ('own','tdf')),
 version INTEGER NOT NULL DEFAULT 1, published_version INTEGER, rights_confirmed INTEGER NOT NULL DEFAULT 0,
 created_at REAL NOT NULL, updated_at REAL NOT NULL, UNIQUE(artist_id, source_key));
'''

def initialize(store):
 with store.db(True) as d:d.executescript(SCHEMA)

def positive(value):
 if type(value) is not int or value <= 0:raise Denied('Selecciona un artista válido.')
 return value

def external_source(value):
 if not isinstance(value,str) or len(value)>2048:raise Denied('URL de video inválida.')
 try:u=urllib.parse.urlsplit(value);port=u.port
 except ValueError:raise Denied('URL de video inválida.') from None
 if u.scheme!='https' or not u.hostname or u.username or u.password or port:raise Denied('Usa una URL HTTPS pública del proveedor.')
 host=u.hostname.lower();path=u.path
 if host=='youtu.be':ident=path.strip('/')
 elif host in ('youtube.com','www.youtube.com','m.youtube.com','www.youtube-nocookie.com'):
  m=re.fullmatch(r'/(?:shorts|embed|live)/([\w-]{11})/?',path)
  ident=urllib.parse.parse_qs(u.query).get('v',[''])[0] if path=='/watch' else m[1] if m else ''
 else:ident=None
 if ident and re.fullmatch(r'[\w-]{11}',ident):return 'youtube:'+ident,'https://www.youtube.com/watch?v='+ident
 if host in ('vimeo.com','www.vimeo.com','player.vimeo.com'):
  m=re.fullmatch(r'/(?:video/)?(\d{6,12})(?:/([a-zA-Z0-9]+))?/?',path)
  if m:
   h=m[2] or urllib.parse.parse_qs(u.query).get('h',[''])[0]
   if h and not re.fullmatch('[a-zA-Z0-9]{1,64}',h):raise Denied('Enlace de Vimeo inválido.')
   return 'vimeo:'+m[1],f'https://vimeo.com/{m[1]}'+('/'+h if h else '')
 if host in ('instagram.com','www.instagram.com'):
  m=re.fullmatch(r'/(p|reel|tv)/([\w-]{5,64})/?',path)
  if m:return 'instagram:'+m[2],f'https://www.instagram.com/{m[1]}/{m[2]}/'
 if host in ('tiktok.com','www.tiktok.com'):
  m=re.fullmatch(r'/@([\w.]+)/video/(\d{15,25})/?',path)
  if m:return 'tiktok:'+m[2],f'https://www.tiktok.com/@{m[1]}/video/{m[2]}'
 raise Denied('Usa el enlace completo de YouTube, Vimeo, Instagram o TikTok. Para un video propio, selecciona un archivo de esta campaña.')

def core_json(headers,config,path):
 # Fixed upstream + server-created path, never a user-supplied URL.
 base=config.get('AUTH_URL','https://api.tdfrecords.net/session').removesuffix('/session')
 forwarded={k:headers[k] for k in ('Authorization','Cookie') if headers.get(k)}
 request=urllib.request.Request(base+path,headers={**forwarded,'Accept':'application/json'})
 try:
  with urllib.request.urlopen(request,timeout=10) as r:return json.loads(r.read(2_000_000))
 except Exception:raise Denied('No se pudo comprobar el permiso actual del artista.',503) from None

def authorize_artist(actor,artist_id,headers,config):
 positive(artist_id)
 profile=core_json(headers,config,f'/artists/{artist_id}/public')
 if profile.get('apArtistId')!=artist_id:raise Denied('Artista no encontrado.',404)
 if actor.get('admin') or actor['id']==artist_id:return
 public=core_json(headers,config,f'/directory/party-profiles/{artist_id}')
 managed=core_json(headers,config,'/directory/profiles')
 canonical=public.get('id')
 if not isinstance(canonical,str) or not canonical or not isinstance(managed,list) or not any(p.get('id')==canonical and p.get('kind')=='artist' and p.get('capabilities',{}).get('edit') is True for p in managed):
  raise Denied('Solo el artista, un gestor con permiso vigente o TDF puede administrar estos videos.',403)

def normalized(data):
 title=str(data.get('title','')).strip();description=str(data.get('description','')).strip();poster=str(data.get('poster','')).strip()
 if not 1<=len(title)<=200 or len(description)>20000:raise Denied('Título o descripción inválidos.')
 if poster:
  u=urllib.parse.urlsplit(poster)
  if u.scheme!='https' or not u.hostname or u.username or u.password:raise Denied('La imagen debe tener una URL HTTPS pública.')
 scope=data.get('scope','own')
 if scope not in ('own','tdf'):raise Denied('Destino inválido.')
 asset=data.get('asset') or None
 if asset:
  if not isinstance(asset,str) or not re.fullmatch('[0-9a-f]{32}',asset) or data.get('source_url'):raise Denied('Selecciona un único origen.')
  key,url='asset:'+asset,''
 else:key,url=external_source(data.get('source_url',''))
 return title,description,poster,scope,asset,key,url

def save(store,actor,data,ident=None):
 artist=positive(data.get('artist_id'));title,description,poster,scope,asset,key,url=normalized(data)
 with store.db(True) as d:
  c=store.access(d,actor,data.get('campaign'),'edit')
  if asset and not d.execute('SELECT 1 FROM assets WHERE id=? AND campaign=? AND owner=? AND status="ready" AND (name LIKE "%.mp4" OR name LIKE "%.webm")',(asset,c['id'],c['owner'])).fetchone():raise Denied('Selecciona un MP4/WebM terminado de esta campaña.')
  values=(title,description,url,asset,key,poster,scope,time.time())
  if ident:
   old=d.execute('SELECT * FROM artist_videos WHERE id=?',(ident,)).fetchone()
   if not old or old['artist_id']!=artist or old['campaign']!=c['id']:raise Denied('Video no encontrado.',404)
   if data.get('version')!=old['version']:raise Denied('El video cambió. Recarga antes de editar.',409)
   duplicate=d.execute('SELECT id FROM artist_videos WHERE artist_id=? AND source_key=? AND id!=?',(artist,key,ident)).fetchone()
   if duplicate:raise Denied('Este video ya existe para el artista.',409)
   d.execute('UPDATE artist_videos SET title=?,description=?,source_url=?,asset=?,source_key=?,poster=?,scope=?,updated_at=?,version=version+1,published_version=NULL,rights_confirmed=0 WHERE id=?',values+(ident,))
  else:
   duplicate=d.execute('SELECT * FROM artist_videos WHERE artist_id=? AND source_key=?',(artist,key)).fetchone()
   if duplicate:
    if duplicate['campaign']==c['id'] and all(duplicate[k]==v for k,v in zip(('title','description','source_url','asset','source_key','poster','scope'),values[:-1])):return dict(duplicate)
    raise Denied('Este video ya existe. Revisa su versión; no se creó una copia.',409)
   ident=uid();d.execute('INSERT INTO artist_videos(id,artist_id,campaign,owner,title,description,source_url,asset,source_key,poster,scope,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)',(ident,artist,c['id'],c['owner'],title,description,url,asset,key,poster,scope,time.time(),time.time()))
  store.audit(d,actor['id'],'video.save',c['id'],ident,{'artist_id':artist})
  return dict(d.execute('SELECT * FROM artist_videos WHERE id=?',(ident,)).fetchone())

def probe_asset(store,config,row):
 with store.db() as d:a=d.execute('SELECT * FROM assets WHERE id=? AND status="ready"',(row['asset'],)).fetchone()
 if not a:raise Denied('Archivo no disponible.',409)
 path=Path(config['STATE_DIR'])/'assets'/a['path']
 if path.stat().st_size!=a['size']:raise Denied('El archivo requiere verificación técnica.',409)
 try:
  result=subprocess.run(['ffprobe','-v','error','-protocol_whitelist','file,pipe','-format_whitelist','mov,matroska,webm','-show_entries','format=duration,format_name:stream=codec_type,codec_name','-of','json',str(path)],check=True,capture_output=True,timeout=30)
  probe=json.loads(result.stdout)
  video=[s for s in probe.get('streams',[]) if s.get('codec_type')=='video']
  audio=[s for s in probe.get('streams',[]) if s.get('codec_type')=='audio']
  duration=float(probe['format']['duration'])
 except (OSError,ValueError,KeyError,subprocess.SubprocessError):raise Denied('No se pudo verificar el video. El original se conserva.',409) from None
 codecs=('h264',) if a['name'].lower().endswith('.mp4') else ('vp8','vp9','av1')
 audio_codecs=('aac',) if a['name'].lower().endswith('.mp4') else ('opus','vorbis')
 if duration<=0 or len(video)!=1 or video[0].get('codec_name') not in codecs or any(s.get('codec_name') not in audio_codecs for s in audio):raise Denied('Genera y revisa una versión compatible MP4 H.264/AAC o WebM antes de publicarla.',409)

def publish(store,actor,ident,data,config,recheck=None):
 # Probe outside the write transaction, then compare the exact version again.
 with store.db() as d:
  row=d.execute('SELECT * FROM artist_videos WHERE id=?',(ident,)).fetchone()
  if not row:raise Denied('Video no encontrado.',404)
  row=dict(row);store.access(d,actor,row['campaign'],'approve')
 if row['asset']:probe_asset(store,config,row)
 if recheck is not None:actor=recheck()
 with store.db(True) as d:
  store.access(d,actor,row['campaign'],'approve')
  current=d.execute('SELECT * FROM artist_videos WHERE id=?',(ident,)).fetchone()
  if type(data.get('version')) is not int or data['version']!=current['version'] or row['version']!=current['version']:raise Denied('La versión cambió. Revisa el video actual.',409)
  if current['scope']=='tdf' and not actor.get('admin'):raise Denied('Publicar como TDF requiere aprobación de un administrador de TDF.',403)
  if data.get('rights_confirmed') is not True or data.get('technical_reviewed') is not True:raise Denied('Confirma derechos y revisión de imagen, audio, sincronía y disponibilidad.')
  d.execute('UPDATE artist_videos SET published_version=version,rights_confirmed=1,updated_at=? WHERE id=?',(time.time(),ident));store.audit(d,actor['id'],'video.publish',row['campaign'],ident,{'version':row['version'],'scope':row['scope']})

def public_row(row,base):
 return {k:row[k] for k in ('id','artist_id','title','description','poster','version')} | {'url':base+'/public/videos/'+row['id']+'/media' if row['asset'] else row['source_url'],'provider':'file' if row['asset'] else row['source_key'].split(':')[0]}

def list_public(store,artist,base):
 with store.db() as d:return [public_row(dict(r),base) for r in d.execute('SELECT * FROM artist_videos WHERE artist_id=? AND published_version=version AND rights_confirmed=1 ORDER BY created_at,id',(positive(artist),))]

def public_asset(store,ident):
 with store.db() as d:
  a=d.execute('SELECT a.* FROM artist_videos v JOIN assets a ON a.id=v.asset WHERE v.id=? AND v.published_version=v.version AND v.rights_confirmed=1 AND a.status="ready"',(ident,)).fetchone()
  if not a:raise Denied('Video no encontrado.',404)
  return dict(a)
