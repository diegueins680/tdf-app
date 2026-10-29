"""Bounded, server-owned FFmpeg jobs; original media is never overwritten."""
import datetime, hashlib, json, os, subprocess, time
from pathlib import Path
from domain import Denied, PLANS, uid

def queue_render(store,actor,cid,data):
 source=data.get('source');segments=data.get('segments');month=datetime.datetime.now(datetime.timezone.utc).strftime('%Y-%m')
 if not isinstance(segments,list) or not 1<=len(segments)<=12:raise Denied('Define entre1y12 fragmentos.')
 normalized=[]
 for segment in segments:
  start=float(segment.get('start',-1));duration=float(segment.get('duration',0))
  if start<0 or not 1<=duration<=180:raise Denied('Fragmento inválido: duración1–180segundos.')
  normalized.append({'start':start,'duration':duration,'label':str(segment.get('label','Fragmento'))[:120]})
 seconds=sum(x['duration'] for x in normalized)
 with store.db(True) as d:
  c=store.access(d,actor,cid,'edit');a=d.execute('SELECT * FROM assets WHERE id=? AND campaign=? AND status="ready"',(source,cid)).fetchone()
  if not a:raise Denied('Selecciona un máster de esta campaña.')
  u=d.execute('SELECT * FROM users WHERE id=?',(c['owner'],)).fetchone();used=d.execute('SELECT render_seconds FROM usage WHERE owner=? AND month=?',(c['owner'],month)).fetchone()
  if seconds+(used[0] if used else 0)>PLANS[u['plan']]['render']:raise Denied('Se superó el procesamiento disponible para este mes.',409)
  d.execute('INSERT INTO usage VALUES(?,?,?) ON CONFLICT(owner,month) DO UPDATE SET render_seconds=render_seconds+excluded.render_seconds',(c['owner'],month,seconds));ident=uid();payload={'source':source,'segments':normalized,'charged_seconds':seconds,'owner':c['owner'],'month':month};d.execute('INSERT INTO jobs(id,campaign,kind,payload,due_at) VALUES(?,?,?,?,?)',(ident,cid,'render',json.dumps(payload),time.time()));store.audit(d,actor['id'],'render.queue',cid,ident,{'seconds':seconds})
 return {'job':ident,'reserved_render_seconds':seconds}

def render(store,config,j):
 payload=json.loads(j['payload']);outputs=[];assets=Path(config['STATE_DIR'])/'assets'
 with store.db() as d:source=dict(d.execute('SELECT * FROM assets WHERE id=?',(payload['source'],)).fetchone());campaign=dict(d.execute('SELECT * FROM campaigns WHERE id=?',(j['campaign'],)).fetchone())
 filename=assets/source['path']
 try:probe=json.loads(subprocess.run(['ffprobe','-v','error','-show_format','-show_streams','-of','json',str(filename)],check=True,capture_output=True,timeout=30).stdout)
 except (OSError,ValueError,subprocess.SubprocessError):raise Denied('No se pudo analizar el máster. Revisa el formato.') from None
 total=float(probe['format']['duration'])
 for segment in payload['segments']:
  if segment['start']+segment['duration']>total+.05:raise Denied('El fragmento excede la duración del máster.')
 # Reserve disk for every derivative before rendering. Conservative ceiling8Mb/s+audio.
 actor={'id':payload['owner'],'name':'Procesamiento de campaña','admin':False}
 for n,seg in enumerate(payload['segments'],1):
  maximum=int(seg['duration']*1_100_000)+2_000_000;aid=store.reserve_asset(actor,j['campaign'],f'clip-{n:02d}.mp4',maximum);target=assets/aid;partial=assets/(aid+'.partial.mp4')
  try:
   subprocess.run(['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-filter_threads','1','-filter_complex_threads','1','-threads','1','-ss',str(seg['start']),'-threads','1','-i',str(filename),'-t',str(seg['duration']),'-vf','scale=1080:1080:force_original_aspect_ratio=decrease,pad=1080:1920:(ow-iw)/2:(oh-ih)/2:color=0x160f12','-c:v','libx264','-preset','veryfast','-crf','21','-maxrate','8M','-bufsize','16M','-threads','1','-r','30','-c:a','aac','-b:a','192k','-movflags','+faststart','-fs',str(maximum),str(partial)],check=True,capture_output=True,timeout=max(120,int(seg['duration']*20)))
   # Add native TDF logo whenever the campaign mentions TDF; keep original musical timing.
   if 'tdf' in campaign['name'].lower() or 'tdf' in campaign['objective'].lower():
    branded=assets/(aid+'.branded.mp4');logo=Path(config['LOGO_PNG'])
    subprocess.run(['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-filter_threads','1','-filter_complex_threads','1','-i',str(partial),'-i',str(logo),'-filter_complex','[1:v]scale=350:-1[logo];[0:v][logo]overlay=70:90','-c:v','libx264','-preset','veryfast','-crf','21','-threads','1','-c:a','copy','-movflags','+faststart','-fs',str(maximum),str(branded)],check=True,capture_output=True,timeout=max(120,int(seg['duration']*20)));os.replace(branded,target);partial.unlink()
   else:os.replace(partial,target)
   check=json.loads(subprocess.run(['ffprobe','-v','error','-show_format','-of','json',str(target)],check=True,capture_output=True,timeout=30).stdout)
   if abs(float(check['format']['duration'])-seg['duration'])>.12:raise Denied('Exportación incompleta. Se conservó para inspección; no se aprobó.')
   size=target.stat().st_size;h=hashlib.sha256(target.read_bytes()).hexdigest()
   with store.db(True) as d:d.execute('UPDATE assets SET size=?,uploaded=?,reserved=?,sha256=?,status="ready" WHERE id=?',(size,size,size,h,aid));store.audit(d,None,'render.completed',j['campaign'],aid,{'source':source['id'],'start':seg['start'],'duration':seg['duration'],'review_required':True})
   bind_rendered_clip(store,j,seg,aid)
   outputs.append(aid)
  except (OSError,subprocess.SubprocessError):raise Denied('La exportación falló. Archivos parciales retenidos; no se publicaron.') from None
 return {'assets':outputs,'review_required':True,'source_unchanged':True}

def bind_rendered_clip(store,job,segment,asset):
 """Attach only to this plan's unapproved draft; approvals cannot silently gain media."""
 with store.db(True) as d:
  for row in d.execute('SELECT id,settings,status,approved_version FROM pieces WHERE campaign=? AND name=?',(job['campaign'],segment['label'])).fetchall():
   if json.loads(row['settings']).get('render_job')!=job['id']:continue
   if row['status']!='draft' or row['approved_version'] is not None:
    owner=d.execute('SELECT owner FROM campaigns WHERE id=?',(job['campaign'],)).fetchone()[0];store.notice(d,owner,'Un fragmento terminó después de la revisión. Asígnalo a una nueva versión y vuelve a aprobar.',job['campaign']);continue
   d.execute('UPDATE pieces SET asset=?,version=version+1 WHERE id=?',(asset,row['id']))
   d.execute('UPDATE campaigns SET version=version+1,approved_version=NULL,updated_at=? WHERE id=?',(time.time(),job['campaign']))
   store.audit(d,None,'render.bind_draft',job['campaign'],row['id'],{'asset':asset,'render_job':job['id']})
