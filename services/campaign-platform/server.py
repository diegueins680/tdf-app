"""TDF authenticated campaign service, behind the existing HTTPS edge."""
import hashlib, hmac, http.server, json, mimetypes, os, re, socket, sys, time, urllib.error, urllib.parse, urllib.request
from pathlib import Path
from domain import Store, Denied, uid
from providers import Providers, Vault, ProviderError

PREFIX='/campaign-platform'
STATIC=Path(__file__).with_name('ui')
def load_config():
 path=os.environ.get('CAMPAIGN_CONFIG','/etc/tdf-campaign-platform/config.json')
 return json.loads(Path(path).read_text())
def make_runtime():
 config=load_config();store=Store(Path(config['STATE_DIR'])/'campaigns.sqlite3');vault=Vault(config['ENCRYPTION_KEY'].encode());return store,Providers(store,vault,config),config
def signed_media_url(config,asset):
 expires=str(int(time.time())+7200);sig=hmac.new(config['MEDIA_KEY'].encode(),(asset+':'+expires).encode(),hashlib.sha256).hexdigest()
 return config['PUBLIC_BASE']+'/media/'+asset+'?expires='+expires+'&signature='+sig

def upstream_actor(headers,config):
 forwarded={k:headers[k] for k in ('Authorization','Cookie') if headers.get(k)}
 if not forwarded:raise Denied('Inicia sesión en TDF para abrir tus campañas.',401)
 req=urllib.request.Request(config.get('AUTH_URL','https://api.tdfrecords.net/session'),headers={**forwarded,'Accept':'application/json'})
 try:
  with urllib.request.urlopen(req,timeout=10) as r:data=json.loads(r.read(64000))
 except urllib.error.HTTPError as e:raise Denied('Tu sesión de TDF no está disponible. Vuelve a iniciar sesión.',401 if e.code in (401,403) else 503) from None
 except (OSError,ValueError):raise Denied('No se pudo comprobar tu sesión de TDF.',503) from None
 identity=data.get('partyId')
 if not isinstance(identity,int) or isinstance(identity,bool) or identity<=0:raise Denied('Respuesta de sesión inválida.',401)
 return {'id':identity,'name':str(data.get('displayName') or data.get('username') or 'Usuario'),'admin':'Admin' in data.get('roles',[])}

class Handler(http.server.BaseHTTPRequestHandler):
 protocol_version='HTTP/1.1';server_version='TDFCampaigns/1'
 def log_message(self,fmt,*args):pass # Never log URLs, authorization, OAuth codes or signed media links.
 def setup(self):super().setup();self.connection.settimeout(60)
 def respond(self,status,data=None,content_type='application/json; charset=utf-8',extra=None):
  body=b'' if data is None else (json.dumps(data,ensure_ascii=False).encode() if content_type.startswith('application/json') else (data.encode() if isinstance(data,str) else data))
  self.send_response(status);self.send_header('Content-Type',content_type);self.send_header('Content-Length',str(len(body)));self.send_header('Cache-Control','no-store');self.send_header('X-Content-Type-Options','nosniff');self.send_header('Referrer-Policy','no-referrer');
  if not self.server.config.get('FRAME_ORIGINS'):self.send_header('X-Frame-Options','SAMEORIGIN')
  origin=self.headers.get('Origin')
  if origin in self.server.config.get('ALLOWED_ORIGINS',[]):self.send_header('Access-Control-Allow-Origin',origin);self.send_header('Access-Control-Allow-Credentials','true');self.send_header('Vary','Origin')
  for k,v in (extra or {}).items():self.send_header(k,v)
  self.end_headers()
  if self.command!='HEAD':self.wfile.write(body)
 def read_json(self):
  try:n=int(self.headers.get('Content-Length','0'))
  except ValueError:raise Denied('Tamaño inválido.')
  if not 0<n<=1_000_000:raise Denied('Carga JSON inválida.',413)
  try:data=json.loads(self.rfile.read(n))
  except (ValueError,OSError):raise Denied('JSON inválido.') from None
  if not isinstance(data,dict):raise Denied('Se requiere un objeto JSON.')
  return data
 def actor(self,write=False):
  if write:
   origin=self.headers.get('Origin')
   if origin and origin not in self.server.config['ALLOWED_ORIGINS']:raise Denied('Origen no autorizado.',403)
   if not origin and not self.headers.get('Authorization','').startswith('Bearer '):raise Denied('Falta el origen de la operación.',403)
  actor=upstream_actor(self.headers,self.server.config);self.server.store.user(actor);return actor
 def do_OPTIONS(self):self.respond(204,extra={'Access-Control-Allow-Headers':'Authorization,Content-Type,Upload-Offset','Access-Control-Allow-Methods':'GET,POST,PUT,DELETE,OPTIONS'})
 def do_HEAD(self):self.dispatch()
 def do_GET(self):self.dispatch()
 def do_POST(self):self.dispatch()
 def do_PUT(self):self.dispatch()
 def do_DELETE(self):self.dispatch()
 def dispatch(self):
  try:self.route()
  except Denied as e:self.respond(e.status,{'error':str(e)})
  except ProviderError as e:self.respond(502,{'error':'La plataforma no confirmó la operación.','code':str(e),'uncertain':e.uncertain})
  except (BrokenPipeError,ConnectionResetError,socket.timeout):pass
  except Exception:
   sys.stderr.write('campaign_request_internal_error\n');self.respond(500,{'error':'No se completó la operación. Revisa su estado antes de repetirla.'})
 def route(self):
  url=urllib.parse.urlsplit(self.path);path=url.path
  if not path.startswith(PREFIX+'/') and path!=PREFIX:raise Denied('No encontrado.',404)
  path=path[len(PREFIX):] or '/';method=self.command;s=self.server.store;cfg=self.server.config
  if path in ('/','/app.js','/style.css','/tdf-logo.svg') and method in ('GET','HEAD'):
   f=STATIC/({'/':'index.html','/tdf-logo.svg':'tdf-logo.svg'}.get(path,path[1:]));content=f.read_bytes();ctype=mimetypes.guess_type(str(f))[0] or 'application/octet-stream';self.respond(200,content,ctype,{'Content-Security-Policy':"default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; media-src 'self'; connect-src 'self'; frame-ancestors https://www.tdfrecords.net https://tdfrecords.net; base-uri 'none'; form-action 'self'"});return
  if path=='/health' and method=='GET':
   with s.db() as d:h=d.execute('SELECT at FROM heartbeat WHERE name="worker"').fetchone()
   self.respond(200,{'service':'tdf-campaign-platform','version':cfg.get('VERSION','development'),'worker_healthy':bool(h and time.time()-h['at']<45)});return
  if path.startswith('/oauth/callback/') and method=='GET':
   q=urllib.parse.parse_qs(url.query)
   if q.get('error'):raise Denied('El proveedor no autorizó la conexión.',409)
   self.server.providers.callback(path.rsplit('/',1)[1],q.get('state',[''])[0],q.get('code',[''])[0],self.actor());self.respond(303,extra={'Location':PREFIX+'/'});return
  if re.fullmatch('/media/[0-9a-f]{32}',path) and method in ('GET','HEAD'):
   aid=path.rsplit('/',1)[1];q=urllib.parse.parse_qs(url.query);expires=q.get('expires',['0'])[0];sig=q.get('signature',[''])[0]
   expected=hmac.new(cfg['MEDIA_KEY'].encode(),(aid+':'+expires).encode(),hashlib.sha256).hexdigest()
   if not expires.isdigit() or int(expires)<time.time() or int(expires)>time.time()+7300 or not hmac.compare_digest(sig,expected):raise Denied('Enlace vencido.',403)
   with s.db() as d:a=d.execute('SELECT * FROM assets WHERE id=? AND status="ready"',(aid,)).fetchone()
   if not a:raise Denied('Archivo no encontrado.',404)
   return self.send_asset(a)
  actor=self.actor(method not in ('GET','HEAD'))
  if path=='/session' and method=='GET':self.respond(200,{'user':actor,'usage':s.stats(actor),'accounts':s.account_list(actor),'capabilities':self.server.providers.capabilities()});return
  if path=='/tdf/review':
   if method=='GET':
    rows=s.tdf_review(actor)
    for row in rows:
     if row['asset']:row['preview_url']=signed_media_url(cfg,row['asset'])
    self.respond(200,rows);return
   if method=='POST':
    data=self.read_json();self.respond(200,s.tdf_review(actor,data.get('piece'),data.get('version')));return
  if path=='/campaigns':
   if method=='GET':self.respond(200,s.list(actor));return
   if method=='POST':self.respond(201,{'id':s.create(actor,self.read_json())});return
  if path.startswith('/oauth/start/') and method=='POST':
   data=self.read_json();self.respond(200,{'url':self.server.providers.start(actor,path.rsplit('/',1)[1],data.get('scope','own'))});return
  m=re.fullmatch('/accounts/([0-9a-f]{32})',path)
  if m and method=='DELETE':
   with s.db(True) as d:
    a=d.execute('SELECT * FROM accounts WHERE id=?',(m[1],)).fetchone()
    if not a or a['owner']!=actor['id'] or (a['scope']=='tdf' and not actor['admin']):raise Denied('Cuenta no encontrada.',404)
    d.execute('UPDATE accounts SET revoked=1,credentials="revoked" WHERE id=?',(m[1],));s.audit(d,actor['id'],'account.revoke',subject=m[1])
   self.respond(204);return
  if path=='/notifications/preferences':
   if method=='GET':
    with s.db() as d:row=d.execute('SELECT * FROM notification_preferences WHERE owner=?',(actor['id'],)).fetchone();self.respond(200,dict(row) if row else {'channels':['in_app']});return
   if method=='POST':
    data=self.read_json();email=str(data.get('email','')).strip();channels=data.get('channels',['in_app'])
    if email and not re.fullmatch(r'[^\s@]+@[^\s@]+\.[^\s@]+',email):raise Denied('Correo inválido.')
    if not isinstance(channels,list) or any(c not in ('in_app','email') for c in channels):raise Denied('Canal no disponible: habilita primero su integración.')
    with s.db(True) as d:d.execute('INSERT INTO notification_preferences(owner,email,channels) VALUES(?,?,?) ON CONFLICT(owner) DO UPDATE SET email=excluded.email,channels=excluded.channels',(actor['id'],email,json.dumps(list(set(channels+['in_app'])))))
    self.respond(200,{'saved':True});return
  m=re.fullmatch('/campaigns/([0-9a-f]{32})(?:/(.*))?',path)
  if m:
   cid,action=m[1],m[2]
   if not action and method=='GET':self.respond(200,s.detail(actor,cid));return
   if action=='members' and method=='POST':s.member(actor,cid,self.read_json());self.respond(204);return
   if action=='approve' and method=='POST':s.approve(actor,cid,self.read_json());self.respond(200,s.detail(actor,cid));return
   if action=='youtube-checks' and method=='POST':s.youtube_checks(actor,cid,self.read_json());self.respond(200,s.detail(actor,cid));return
   if action=='status' and method=='POST':s.status(actor,cid,self.read_json().get('status'));self.respond(200,s.detail(actor,cid));return
   if action=='pieces' and method=='POST':self.respond(201,{'id':s.piece(actor,cid,self.read_json())});return
   if action and action.startswith('pieces/') and method=='PUT':self.respond(200,{'id':s.piece(actor,cid,self.read_json(),action.split('/')[1])});return
   if action=='uploads' and method=='POST':
    data=self.read_json();aid=s.reserve_asset(actor,cid,str(data.get('name','archivo')),data.get('size'),data.get('original') is True);self.respond(201,{'id':aid,'offset':0,'chunk_size':4*1024*1024});return
   if action=='plan' and method=='POST':
    from planning import queue_plan
    self.respond(201,queue_plan(s,actor,cid,self.read_json()));return
   if action=='render' and method=='POST':
    from media import queue_render
    self.respond(201,queue_render(s,actor,cid,self.read_json()));return
   if action=='export' and method=='GET':self.respond(200,s.detail(actor,cid),extra={'Content-Disposition':'attachment; filename="campana.json"'});return
  m=re.fullmatch('/uploads/([0-9a-f]{32})',path)
  if m:
   with s.db(True) as d:
    a=d.execute('SELECT * FROM assets WHERE id=?',(m[1],)).fetchone()
    if not a:raise Denied('Archivo no encontrado.',404)
    s.access(d,actor,a['campaign'],'edit')
    if method=='GET':self.respond(200,{'offset':a['uploaded'],'size':a['reserved'],'status':a['status']});return
    if method=='PUT':
     if a['status']!='uploading':raise Denied('Carga ya completada.',409)
     try:n=int(self.headers.get('Content-Length','0'));offset=int(self.headers.get('Upload-Offset','-1'))
     except ValueError:raise Denied('Carga inválida.')
     if offset!=a['uploaded'] or n<=0 or n>4*1024*1024 or offset+n>a['reserved']:raise Denied('Offset o tamaño de fragmento inválido.',409)
     target=Path(cfg['STATE_DIR'])/'assets'/a['path'];target.parent.mkdir(exist_ok=True)
     # Reserved quota is committed before creating bytes; truncate only uncommitted tail of this upload.
     with target.open('r+b' if target.exists() else 'w+b') as f:
      f.truncate(offset);f.seek(offset);remaining=n
      while remaining:
       chunk=self.rfile.read(min(65536,remaining))
       if not chunk:raise Denied('Carga interrumpida; reanuda desde el último offset confirmado.',409)
       f.write(chunk);remaining-=len(chunk)
      f.flush();os.fsync(f.fileno())
     completed=offset+n==a['reserved'];digest=None
     if completed:
      h=hashlib.sha256()
      with target.open('rb') as f:
       for block in iter(lambda:f.read(1024*1024),b''):h.update(block)
      digest=h.hexdigest()
     d.execute('UPDATE assets SET uploaded=?,size=?,sha256=?,status=? WHERE id=?',(offset+n,offset+n,digest,'ready' if completed else 'uploading',a['id']));self.respond(200,{'offset':offset+n,'status':'ready' if completed else 'uploading','sha256':digest});return
  m=re.fullmatch('/assets/([0-9a-f]{32})',path)
  if m and method=='DELETE':
   data=self.read_json()
   with s.db(True) as d:
    a=d.execute('SELECT * FROM assets WHERE id=? AND status!="deleted"',(m[1],)).fetchone()
    if not a:raise Denied('Archivo no encontrado.',404)
    c=s.access(d,actor,a['campaign'],'manage')
    if a['original'] and data.get('delete_original') is not True:raise Denied('Confirma explícitamente la eliminación de este original.')
    if d.execute('SELECT 1 FROM pieces WHERE campaign=? AND asset=?',(a['campaign'],a['id'])).fetchone():raise Denied('Este archivo forma parte de una publicación registrada. Conserva el historial y retíralo primero de las piezas que corresponda.',409)
    for job in d.execute('SELECT payload FROM jobs WHERE campaign=? AND state IN ("queued","running")',(a['campaign'],)):
     if json.loads(job['payload']).get('source')==a['id']:raise Denied('El archivo está en uso por un trabajo pendiente.',409)
    for row in d.execute('SELECT settings FROM pieces WHERE campaign=?',(a['campaign'],)):
     if a['id'] in json.loads(row['settings']).get('children',[]):raise Denied('El archivo forma parte de un carrusel.',409)
    target=Path(cfg['STATE_DIR'])/'assets'/a['path']
    if target.exists():target.unlink()
    d.execute('UPDATE assets SET status="deleted",reserved=0 WHERE id=?',(a['id'],));s.audit(d,actor['id'],'asset.delete',a['campaign'],a['id'],{'name':a['name'],'sha256':a['sha256'],'original':bool(a['original'])})
   self.respond(204);return
  if m and method in ('GET','HEAD'):
   with s.db() as d:
    a=d.execute('SELECT * FROM assets WHERE id=? AND status="ready"',(m[1],)).fetchone()
    if not a:raise Denied('Archivo no encontrado.',404)
    s.access(d,actor,a['campaign'])
   return self.send_asset(a)
  raise Denied('Ruta no encontrada.',404)
 def send_asset(self,a):
  target=Path(self.server.config['STATE_DIR'])/'assets'/a['path'];size=target.stat().st_size;start=0;end=size-1;status=200;raw=self.headers.get('Range')
  if raw:
   m=re.fullmatch(r'bytes=(\d+)-(\d*)',raw)
   if not m:raise Denied('Rango inválido.',416)
   start=int(m[1]);end=min(int(m[2]) if m[2] else end,end)
   if start>end:raise Denied('Rango inválido.',416)
   status=206
  safe_type=mimetypes.guess_type(a['name'])[0] or 'application/octet-stream'
  if safe_type not in ('video/mp4','video/webm','image/jpeg','image/png','image/webp','audio/mpeg','audio/mp4','audio/wav','audio/x-wav','text/vtt','application/pdf'):safe_type='application/octet-stream'
  self.send_response(status);self.send_header('Content-Security-Policy',"sandbox; default-src 'none'");self.send_header('Content-Type',safe_type);self.send_header('Content-Length',str(end-start+1));self.send_header('Accept-Ranges','bytes');self.send_header('X-Content-Type-Options','nosniff');self.send_header('Cache-Control','private, max-age=0');self.send_header('Content-Disposition',('attachment' if safe_type=='application/octet-stream' else 'inline')+"; filename*=UTF-8''"+urllib.parse.quote(a['name']))
  if status==206:self.send_header('Content-Range',f'bytes {start}-{end}/{size}')
  self.end_headers()
  if self.command=='HEAD':return
  with target.open('rb') as f:
   f.seek(start);remaining=end-start+1
   while remaining:
    chunk=f.read(min(65536,remaining))
    if not chunk:break
    self.wfile.write(chunk);remaining-=len(chunk)

class Server(http.server.ThreadingHTTPServer):
 daemon_threads=True
 def __init__(self,address,store,providers,config):self.store=store;self.providers=providers;self.config=config;super().__init__(address,Handler)
def main():
 store,providers,config=make_runtime();Server((config.get('BIND','127.0.0.1'),config.get('PORT',8791)),store,providers,config).serve_forever()
if __name__=='__main__':main()
