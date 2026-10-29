"""Durable campaign authority. SQLite WAL: one host, multiple processes, transactional claims."""
import contextlib, datetime, hashlib, json, os, shutil, sqlite3, time, uuid, urllib.parse
from pathlib import Path
from zoneinfo import ZoneInfo

PLANS = {
 'free': {'price':0,'storage':5_000_000_000,'active':1,'render':900},
 'artist':{'price':9,'storage':25_000_000_000,'active':3,'render':3600},
 'pro':{'price':19,'storage':100_000_000_000,'active':10,'render':10800},
 'team':{'price':49,'storage':500_000_000_000,'active':25,'render':36000}}
PLATFORMS={'youtube','instagram','facebook','tiktok'}
FORMATS={'youtube':{'video','short','community','comment','collaboration'},'instagram':{'reel','story','carousel','post','message'},'facebook':{'video','reel','story','carousel','post','message'},'tiktok':{'video','photo'}}
ROLES={'manager':{'read','edit','approve','manage'},'editor':{'read','edit'},'reviewer':{'read','approve'},'viewer':{'read'}}
def uid():return uuid.uuid4().hex
def destination_url(value):
 value=str(value or '').strip()
 if value and (urllib.parse.urlsplit(value).scheme!='https' or not urllib.parse.urlsplit(value).hostname):raise Denied('El destino debe ser un enlace HTTPS válido.')
 return value
class Denied(Exception):
 def __init__(self,message,status=400):super().__init__(message);self.status=status
class Store:
 def __init__(self,path):
  self.path=Path(path);self.path.parent.mkdir(parents=True,exist_ok=True)
  with self.db() as db:db.executescript(Path(__file__).with_name('schema.sql').read_text())
 @contextlib.contextmanager
 def db(self,write=False):
  db=sqlite3.connect(self.path,timeout=15,isolation_level=None);db.row_factory=sqlite3.Row
  db.execute('PRAGMA foreign_keys=ON');db.execute('PRAGMA journal_mode=WAL');db.execute('PRAGMA busy_timeout=15000')
  try:
   if write:db.execute('BEGIN IMMEDIATE')
   yield db
   if write:db.commit()
  except Exception:
   if write:db.rollback()
   raise
  finally:db.close()
 def user(self,actor):
  with self.db(True) as d:d.execute('INSERT INTO users(id,display_name,created_at) VALUES(?,?,?) ON CONFLICT(id) DO UPDATE SET display_name=excluded.display_name',(actor['id'],actor['name'],time.time()))
 def audit(self,d,actor,action,campaign=None,subject=None,details=None):
  d.execute('INSERT INTO audit(campaign,actor,action,subject,at,details) VALUES(?,?,?,?,?,?)',(campaign,actor,action,subject,time.time(),json.dumps(details or {},ensure_ascii=False)))
 def access(self,d,actor,campaign,ability='read'):
  c=d.execute('SELECT * FROM campaigns WHERE id=?',(campaign,)).fetchone()
  if not c:raise Denied('Campaña no encontrada.',404)
  if c['owner']==actor['id']:return c
  m=d.execute('SELECT role FROM members WHERE campaign=? AND user_id=?',(campaign,actor['id'])).fetchone()
  # Admin status does not grant blanket access to another user's campaigns.
  if not m or ability not in ROLES[m['role']]:raise Denied('Campaña no encontrada.',404)
  return c
 def notice(self,d,owner,message,campaign=None,severity='warning'):
  # Do not flood the same unresolved incident.
  if d.execute('SELECT 1 FROM notices WHERE owner=? AND message=? AND created_at>?',(owner,message,time.time()-3600)).fetchone():return
  d.execute('INSERT INTO notices(id,owner,campaign,severity,message,created_at) VALUES(?,?,?,?,?,?)',(uid(),owner,campaign,severity,message,time.time()))
 def create(self,actor,data):
  name=str(data.get('name','')).strip();objective=str(data.get('objective','')).strip()
  if not name or len(name)>200 or not objective or len(objective)>5000:raise Denied('Nombre y objetivo son obligatorios.')
  zone=data.get('timezone','America/Guayaquil')
  try:ZoneInfo(zone)
  except Exception:raise Denied('Zona horaria inválida.')
  ident=uid();now=time.time()
  with self.db(True) as d:
   d.execute('INSERT INTO campaigns(id,owner,name,objective,timezone,destination,created_at,updated_at) VALUES(?,?,?,?,?,?,?,?)',(ident,actor['id'],name,objective,zone,destination_url(data.get('destination','')),now,now));self.audit(d,actor['id'],'campaign.create',ident)
  return ident
 def list(self,actor):
  with self.db() as d:return [dict(r) for r in d.execute('SELECT DISTINCT c.* FROM campaigns c LEFT JOIN members m ON m.campaign=c.id WHERE c.owner=? OR m.user_id=? ORDER BY c.updated_at DESC',(actor['id'],actor['id']))]
 def detail(self,actor,ident):
  with self.db() as d:
   c=dict(self.access(d,actor,ident));c['pieces']=[dict(r) for r in d.execute('SELECT * FROM pieces WHERE campaign=? ORDER BY due_at',(ident,))];c['assets']=[dict(r) for r in d.execute('SELECT id,name,size,reserved,uploaded,status,sha256,original FROM assets WHERE campaign=?',(ident,))];c['members']=[dict(r) for r in d.execute('SELECT user_id,role FROM members WHERE campaign=?',(ident,))];c['audit']=[dict(r) for r in d.execute('SELECT * FROM audit WHERE campaign=? ORDER BY at DESC LIMIT 100',(ident,))];c['metrics']=[{**dict(r),'data':json.loads(r['data'])} for r in d.execute('SELECT * FROM metrics WHERE campaign=? ORDER BY at DESC LIMIT 100',(ident,))];return c
 def member(self,actor,ident,data):
  target=data.get('user_id');role=data.get('role')
  if not isinstance(target,int) or target<=0 or role not in ROLES:raise Denied('Usuario y rol inválidos.')
  with self.db(True) as d:
   c=self.access(d,actor,ident,'manage')
   if target==c['owner']:raise Denied('El propietario conserva sus permisos.')
   if not d.execute('SELECT 1 FROM users WHERE id=?',(target,)).fetchone():raise Denied('El usuario debe abrir Campañas antes de ser invitado.')
   d.execute('INSERT INTO members VALUES(?,?,?) ON CONFLICT(campaign,user_id) DO UPDATE SET role=excluded.role',(ident,target,role));self.audit(d,actor['id'],'member.set',ident,str(target),{'role':role})
 def piece(self,actor,cid,data,pid=None):
  if data.get('platform') not in FORMATS or data.get('format') not in FORMATS[data['platform']]:raise Denied('Formato o plataforma inválidos.')
  due=float(data.get('due_at',0));late=int(data.get('late_seconds',3600))
  if due<=0 or not 0<=late<=86400:raise Denied('Fecha o tolerancia inválida.')
  if len(str(data.get('copy','')))>20000:raise Denied('Texto demasiado largo.')
  with self.db(True) as d:
   c=self.access(d,actor,cid,'edit')
   if c['status'] in ('cancelled','completed'):raise Denied('Campaña cerrada.',409)
   account=data.get('account') or None
   if account:
    a=d.execute('SELECT * FROM accounts WHERE id=? AND revoked=0',(account,)).fetchone()
    if not a or a['platform']!=data['platform'] or (a['owner']!=c['owner'] and a['scope']!='tdf'):raise Denied('Cuenta de destino no disponible.',403)
   asset=data.get('asset') or None
   if asset and not d.execute('SELECT 1 FROM assets WHERE id=? AND campaign=? AND status="ready"',(asset,cid)).fetchone():raise Denied('Archivo no disponible en esta campaña.')
   settings=data.get('settings',{})
   if not isinstance(settings,dict):raise Denied('Ajustes inválidos.')
   settings=dict(settings);settings.pop('youtube_checks_reviewed',None)
   values=(str(data.get('name','Pieza'))[:200],data['platform'],data['format'],account,str(data.get('copy','')),asset,destination_url(data.get('destination',c['destination'])),due,late,data.get('recipient'),json.dumps(settings))
   if pid:
    old=d.execute('SELECT * FROM pieces WHERE id=? AND campaign=?',(pid,cid)).fetchone()
    if not old:raise Denied('Pieza no encontrada.',404)
    if old['status'] in ('publishing','processing','published','unknown'):raise Denied('La pieza ya tiene un efecto externo; requiere una acción de edición independiente.',409)
    d.execute('UPDATE pieces SET name=?,platform=?,format=?,account=?,copy=?,asset=?,destination=?,due_at=?,late_seconds=?,recipient=?,settings=?,version=version+1,approved_version=NULL,tdf_approval_version=NULL,dm_approval_version=NULL,status="draft" WHERE id=?',values+(pid,));d.execute('UPDATE jobs SET state="cancelled" WHERE piece=? AND state="queued"',(pid,))
   else:
    pid=uid();d.execute('INSERT INTO pieces(name,platform,format,account,copy,asset,destination,due_at,late_seconds,recipient,settings,id,campaign,created_at) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?)',values+(pid,cid,time.time()))
   d.execute('UPDATE campaigns SET version=version+1,approved_version=NULL,updated_at=? WHERE id=?',(time.time(),cid));self.audit(d,actor['id'],'piece.edit',cid,pid)
  return pid
 def approve(self,actor,cid,data):
  with self.db(True) as d:
   c=self.access(d,actor,cid,'approve')
   if c['status'] in ('cancelled','completed'):raise Denied('Campaña cerrada.',409)
   if data.get('version')!=c['version']:raise Denied('La campaña cambió; revisa la versión actual.',409)
   if data.get('rights_confirmed') is not True:raise Denied('Confirma que tienes los derechos de publicación.')
   for p in d.execute('SELECT * FROM pieces WHERE campaign=? AND status NOT IN ("published","cancelled","publishing","processing","unknown") AND external_id IS NULL',(cid,)).fetchall():
    a=d.execute('SELECT scope FROM accounts WHERE id=?',(p['account'],)).fetchone() if p['account'] else None
    if a and a['scope']=='tdf' and actor.get('admin') and data.get('tdf_approve') is True:d.execute('UPDATE pieces SET tdf_approval_version=version WHERE id=?',(p['id'],))
    if p['format']=='message' and data.get('messages_approve') is True:d.execute('UPDATE pieces SET dm_approval_version=version WHERE id=?',(p['id'],))
    d.execute('UPDATE pieces SET approved_version=version,status="approved" WHERE id=?',(p['id'],))
   d.execute('UPDATE campaigns SET approved_version=version,rights_confirmed=1,status=CASE WHEN status="active" THEN "active" ELSE "approved" END WHERE id=?',(cid,));self.audit(d,actor['id'],'campaign.approve',cid,details={'version':c['version'],'tdf':bool(actor.get('admin') and data.get('tdf_approve')),'messages':data.get('messages_approve') is True})
 def youtube_checks(self,actor,cid,data):
  with self.db(True) as d:
   c=self.access(d,actor,cid,'approve');p=d.execute('SELECT * FROM pieces WHERE id=? AND campaign=?',(data.get('piece'),cid)).fetchone()
   if not p or p['platform']!='youtube' or p['format'] not in ('video','short') or not p['external_id']:raise Denied('Selecciona el video privado ya subido.',409)
   a=d.execute('SELECT * FROM accounts WHERE id=? AND revoked=0',(p['account'],)).fetchone()
   if not a or (a['scope']=='tdf' and not actor.get('admin')):raise Denied('Esta revisión requiere al administrador del canal.',403)
   if c['approved_version']!=c['version'] or p['approved_version']!=p['version'] or data.get('version')!=p['version'] or data.get('video_id')!=p['external_id']:raise Denied('La versión cambió. Vuelve a revisar el video exacto.',409)
   if p['status'] not in ('processing','blocked') or data.get('checks_passed') is not True:raise Denied('Confirma las comprobaciones de Studio sobre este video privado.',409)
   settings=json.loads(p['settings']);settings['youtube_checks_reviewed']=True
   d.execute('UPDATE pieces SET settings=?,status="processing" WHERE id=?',(json.dumps(settings),p['id']))
   d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?) ON CONFLICT(piece,kind) DO UPDATE SET state=CASE WHEN jobs.state="blocked" THEN "queued" ELSE jobs.state END,due_at=excluded.due_at',(uid(),cid,p['id'],'verify_youtube','{}',time.time()))
   self.audit(d,actor['id'],'youtube.studio_checks',cid,p['id'],{'video_id':p['external_id'],'version':p['version'],'reviewer_confirmation':True})
 def status(self,actor,cid,status):
  if status not in ('active','paused','cancelled','completed'):raise Denied('Estado inválido.')
  with self.db(True) as d:
   c=self.access(d,actor,cid,'manage')
   if c['status'] in ('cancelled','completed'):raise Denied('Estado terminal.',409)
   if status=='active':
    if c['approved_version']!=c['version'] or not c['rights_confirmed']:raise Denied('Aprueba la versión actual antes de ejecutar.',409)
    u=d.execute('SELECT * FROM users WHERE id=?',(c['owner'],)).fetchone();cap=PLANS[u['plan']]['active']
    n=d.execute('SELECT count(*) FROM campaigns WHERE owner=? AND status="active" AND id!=?',(c['owner'],cid)).fetchone()[0]
    if n>=cap:raise Denied('Límite de campañas activas alcanzado.',409)
    for p in d.execute('SELECT * FROM pieces WHERE campaign=? AND status="approved"',(cid,)).fetchall():
     # Existing jobs preserve their identity and provider state.
     d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?) ON CONFLICT(piece,kind) DO UPDATE SET due_at=excluded.due_at,state=CASE WHEN jobs.state IN ("cancelled","blocked") AND (SELECT external_id FROM pieces WHERE id=jobs.piece) IS NULL THEN "queued" ELSE jobs.state END',(uid(),cid,p['id'],'publish','{}',p['due_at']))
   d.execute('UPDATE campaigns SET status=?,updated_at=? WHERE id=?',(status,time.time(),cid))
   if status in ('cancelled','completed'):
    d.execute('UPDATE jobs SET state="cancelled" WHERE campaign=? AND state="queued"',(cid,));d.execute('UPDATE pieces SET status="cancelled" WHERE campaign=? AND status IN ("draft","approved","blocked")',(cid,))
   self.audit(d,actor['id'],'campaign.'+status,cid)
 def account_list(self,actor):
  with self.db() as d:return [dict(r) for r in d.execute('SELECT id,platform,external_id,handle,scope,permissions,expires_at,revoked FROM accounts WHERE owner=? OR scope="tdf"',(actor['id'],))]
 def stats(self,actor):
  with self.db() as d:
   u=dict(d.execute('SELECT * FROM users WHERE id=?',(actor['id'],)).fetchone());limit=u['storage_limit'] or PLANS[u['plan']]['storage'];used=d.execute('SELECT coalesce(sum(reserved),0) FROM assets WHERE owner=?',(actor['id'],)).fetchone()[0];return {'plan':u['plan'],'used_bytes':used,'limit_bytes':limit,'plans':PLANS,'prices_status':'propuesta; sin cobros automáticos','notices':[dict(r) for r in d.execute('SELECT * FROM notices WHERE owner=? ORDER BY created_at DESC LIMIT 100',(actor['id'],))]}
 def reserve_asset(self,actor,cid,name,size,original=False):
  if not isinstance(size,int) or size<=0:raise Denied('Tamaño inválido.')
  aid=uid()
  with self.db(True) as d:
   c=self.access(d,actor,cid,'edit');u=d.execute('SELECT * FROM users WHERE id=?',(c['owner'],)).fetchone();used=d.execute('SELECT coalesce(sum(reserved),0) FROM assets WHERE owner=?',(c['owner'],)).fetchone()[0];limit=u['storage_limit'] or PLANS[u['plan']]['storage']
   if used+size>limit:raise Denied('La carga supera tu espacio disponible.',409)
   pending=d.execute('SELECT coalesce(sum(max(reserved-uploaded,0)),0) FROM assets WHERE status="uploading"').fetchone()[0]
   if shutil.disk_usage(self.path.parent).free < size+pending+2_000_000_000:raise Denied('El servidor no tiene capacidad libre suficiente para esta carga. Los originales existentes se conservan.',409)
   d.execute('INSERT INTO assets(id,owner,campaign,name,path,reserved,original,created_at) VALUES(?,?,?,?,?,?,?,?)',(aid,c['owner'],cid,Path(name).name[:200],aid,size,int(original),time.time()))
   if used+size>=limit*.9:self.notice(d,c['owner'],'El almacenamiento reservado supera el90% de tu cuota.',cid)
  return aid
 def claim(self,now=None):
  now=time.time() if now is None else now
  with self.db(True) as d:
   # A crash after the provider boundary is uncertain, never an automatic replay.
   for j in d.execute('SELECT * FROM jobs WHERE state="running" AND claimed_at<?',(now-600,)).fetchall():
    if j['kind']=='publish':
     d.execute('UPDATE jobs SET state="unknown",error="worker_interrupted_after_claim" WHERE id=?',(j['id'],));d.execute('UPDATE pieces SET status="unknown" WHERE id=?',(j['piece'],))
    else:d.execute('UPDATE jobs SET state="queued" WHERE id=?',(j['id'],))
   j=d.execute('SELECT j.* FROM jobs j LEFT JOIN campaigns c ON c.id=j.campaign WHERE j.state="queued" AND j.due_at<=? AND ((j.kind="heartbeat" OR j.kind="metrics" OR j.kind LIKE "metrics_%") OR (j.kind="verify_youtube" AND c.status="active") OR (j.kind IN ("render","plan") AND c.status IN ("draft","approved","paused","active")) OR (j.kind="publish" AND c.status="active")) ORDER BY j.due_at LIMIT 1',(now,)).fetchone()
   if not j:return None
   d.execute('UPDATE jobs SET state="running",claimed_at=?,attempts=attempts+1 WHERE id=?',(now,j['id']));return dict(j)
 def execution_guard(self,d,p,now=None):
  now=time.time() if now is None else now;c=d.execute('SELECT * FROM campaigns WHERE id=?',(p['campaign'],)).fetchone()
  if c['status']!='active' or c['approved_version']!=c['version'] or p['approved_version']!=p['version']:raise Denied('Aprobación o ejecución no vigente.',409)
  if now>p['due_at']+p['late_seconds']:raise Denied('Venció la tolerancia de publicación. Revisa la fecha.',409)
  if p['format']=='message' and p['dm_approval_version']!=p['version']:raise Denied('El mensaje privado requiere aprobación separada.',403)
  a=d.execute('SELECT * FROM accounts WHERE id=? AND revoked=0',(p['account'],)).fetchone()
  if not a:raise Denied('Conecta una cuenta autorizada para esta publicación.',409)
  if a['owner']!=c['owner'] and a['scope']!='tdf':raise Denied('Cuenta de otra persona.',403)
  if a['scope']=='tdf' and p['tdf_approval_version']!=p['version']:raise Denied('Falta aprobación de un administrador de TDF.',403)
  if a['platform'] not in ('youtube','tiktok') and a['expires_at'] and a['expires_at']<=now:raise Denied('Conexión vencida. Reconecta tu cuenta.',409)
  return dict(a)
 def tdf_review(self,actor,piece=None,version=None):
  if not actor.get('admin'):raise Denied('Solo administradores de TDF.',403)
  with self.db(True) as d:
   rows=d.execute('SELECT p.*,c.name AS campaign_name,c.timezone AS campaign_timezone,a.handle AS account_handle,a.external_id AS account_external_id FROM pieces p JOIN campaigns c ON c.id=p.campaign JOIN accounts a ON a.id=p.account WHERE a.scope="tdf" AND p.approved_version=p.version AND p.status="approved" AND p.tdf_approval_version IS NOT p.version').fetchall()
   if piece:
    row=next((r for r in rows if r['id']==piece),None)
    if not row or row['version']!=version:raise Denied('Solicitud no encontrada o versión distinta.',409)
    d.execute('UPDATE pieces SET tdf_approval_version=version WHERE id=?',(piece,));self.audit(d,actor['id'],'tdf.piece.approve',row['campaign'],piece,{'version':version});return {'approved':True}
   return [dict(r) for r in rows]
