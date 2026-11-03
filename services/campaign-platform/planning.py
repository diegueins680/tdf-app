"""Deterministic launch draft from verified facts. Selection is proposed, never auto-approved."""
import datetime,json,re,subprocess,time
from pathlib import Path
from zoneinfo import ZoneInfo
from domain import Denied,uid

def queue_plan(store,actor,cid,data):
 with store.db(True) as d:
  campaign=store.access(d,actor,cid,'edit');source=d.execute('SELECT * FROM assets WHERE id=? AND campaign=? AND status="ready"',(data.get('source'),cid)).fetchone()
  if not source:raise Denied('Selecciona el máster terminado de esta campaña.')
  if d.execute('SELECT 1 FROM jobs WHERE campaign=? AND kind="plan" AND state IN ("queued","running")',(cid,)).fetchone():raise Denied('Ya se está preparando un plan para este máster.',409)
  facts=data.get('facts',{})
  if not isinstance(facts,dict):raise Denied('Datos de campaña inválidos.')
  for key in ('artist','recording_date','credits'):
   if len(str(facts.get(key,'')))>12000:raise Denied('Datos demasiado largos.')
  try:
   start_date=datetime.datetime.fromisoformat(data['start_at'])
   if start_date.tzinfo is None:raise ValueError('offset required')
   start=start_date.timestamp()
  except (KeyError,ValueError):raise Denied('Define una fecha de inicio con zona horaria.')
  ident=uid();payload={'source':source['id'],'owner':campaign['owner'],'start_at':start,'facts':facts};d.execute('INSERT INTO jobs(id,campaign,kind,payload,due_at) VALUES(?,?,?,?,?)',(ident,cid,'plan',json.dumps(payload),time.time()));store.audit(d,actor['id'],'plan.queue',cid,ident)
 return {'job':ident,'review_required':True}

def plan(store,config,job):
 data=json.loads(job['payload']);actor={'id':data['owner'],'name':'Preparación de campaña','admin':False};cid=job['campaign'];root=Path(config['STATE_DIR']);facts=data['facts']
 with store.db() as d:
  source=dict(d.execute('SELECT * FROM assets WHERE id=?',(data['source'],)).fetchone());campaign=dict(d.execute('SELECT * FROM campaigns WHERE id=?',(cid,)).fetchone())
 media=root/'assets'/source['path'];probe=json.loads(subprocess.run(['ffprobe','-v','error','-show_format','-of','json',str(media)],capture_output=True,check=True,timeout=30).stdout);duration=float(probe['format']['duration'])
 if duration<30:raise Denied('El máster es demasiado breve para la plantilla de seis fragmentos. Crea una pieza individual.')
 # Decode audio only, at8kHz, then select the strongest RMS window in each sixth.
 analysis=root/'assets'/(uid()+'.rms.txt')
 command=['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-i',str(media),'-vn','-ac','1','-ar','8000','-af','asetnsamples=n=8000,astats=metadata=1:reset=1,ametadata=print:key=lavfi.astats.Overall.RMS_level:file='+str(analysis),'-f','null','-']
 subprocess.run(command,check=True,capture_output=True,timeout=300)
 levels=[];at=0
 for line in analysis.read_text().splitlines():
  match=re.search(r'pts_time:([\d.]+)',line)
  if match:at=float(match[1])
  elif 'lavfi.astats.Overall.RMS_level=' in line:
   try:levels.append((at,float(line.split('=')[-1])))
   except ValueError:pass
 if len(levels)<6:raise Denied('No se pudo proponer una selección musical fiable. Selecciona los intervalos manualmente.')
 chosen=[];clip_duration=min(26,max(10,duration/12));segment=duration/6
 for i in range(6):
  lo=max(2,i*segment);hi=min(duration-clip_duration-2,(i+1)*segment)
  candidates=[x for x in levels if lo<=x[0]<=hi]
  start=max(candidates,key=lambda x:x[1])[0] if candidates else max(0,min(lo,duration-clip_duration));chosen.append({'start':round(start,3),'duration':round(clip_duration,3),'label':'CL'+str(i+1).zfill(2)})
 from media import queue_render
 rendered=queue_render(store,actor,cid,{'source':source['id'],'segments':chosen})
 artist=str(facts.get('artist') or campaign['name']);credits=str(facts.get('credits','')).strip();date=str(facts.get('recording_date','')).strip();context=('Grabada el '+date+'. ') if date else '';suffix=('\n\n'+credits) if credits else '';destination=campaign['destination']
 angles=['La interpretación de cerca','El diálogo entre músicos','El trabajo detrás de la sesión'];hooks=['Escucha de cerca.','Sigue el cambio.','Vuelve a este momento.','Una razón para quedarte.','Dale espacio a la música.','La sesión continúa.'];piece_ids=[]
 schedule=[(0,'youtube','video','Sesión completa'),(0,'instagram','story','S01'),(0,'instagram','story','S02'),(0,'instagram','story','S03'),(0,'youtube','community','Lanzamiento'),(1,'instagram','reel','CL01'),(2,'instagram','story','S04'),(3,'youtube','short','CL02'),(4,'instagram','carousel','Carrusel'),(5,'instagram','story','S05'),(6,'instagram','reel','CL03'),(7,'youtube','community','Momento destacado'),(8,'instagram','story','S06'),(9,'tiktok','video','CL04'),(10,'instagram','story','S07'),(11,'youtube','short','CL05'),(12,'instagram','story','S08'),(13,'instagram','reel','CL06'),(13,'youtube','community','Vuelve a la sesión')]
 # Existing master publication may be imported manually; this creates a review draft, never a duplicate live post.
 for day,platform,fmt,name in schedule:
  index=int(name[-2:])-1 if name.startswith('CL') else 0;hook=hooks[index%6] if name.startswith('CL') else 'Quédate con la música.';copy=f'{hook} {artist}. {context}Mira la sesión completa.'
  if destination:copy+='\n\nSesión completa: '+destination
  if platform=='instagram' and fmt in ('reel','carousel'):copy+='\nMira la sesión completa en el enlace de la bio. [Verificar ese destino antes de aprobar.]'
  if platform=='youtube' and fmt=='short':copy+='\nAbre el video relacionado, una vez configurado y verificado.'
  if platform=='tiktok':copy+='\nConfigura y verifica la ruta real desde tu perfil antes de publicar.'
  piece=store.piece(actor,cid,{'name':name,'platform':platform,'format':fmt,'copy':copy+suffix,'asset':source['id'] if name=='Sesión completa' else None,'destination':destination,'due_at':(datetime.datetime.fromtimestamp(data['start_at'],ZoneInfo(campaign['timezone']))+datetime.timedelta(days=day)).timestamp(),'settings':{'draft_from_template':True,'selection_review_required':True,'render_job':rendered['job'] if name.startswith('CL') else None}});piece_ids.append(piece)
 from graphics import generate
 graphics=generate(store,config,cid,actor,source['id'],artist,chosen)
 with store.db(True) as d:
  for name,aid in graphics.items():
   if name.startswith('S'):d.execute('UPDATE pieces SET asset=? WHERE campaign=? AND name=? AND status="draft"',(aid,cid,name))
  carousel=[aid for name,aid in graphics.items() if name.startswith('CAR')]
  d.execute('UPDATE pieces SET asset=?,settings=? WHERE campaign=? AND format="carousel" AND status="draft"',(carousel[0],json.dumps({'children':carousel,'review_required':True}),cid))
 report={'idea':'Quédate con la música','audience':'Hipótesis: personas que conectan con la interpretación observada y con las audiencias verificadas del artista. Ajustar con datos reales.','angles':angles,'selection':chosen,'selection_basis':'Picos de energía RMS en seis partes del máster. No equivale a evaluación musical ni garantiza frases completas; revisar entradas, salidas y encuadres.','pieces':piece_ids,'render_job':rendered['job'],'rights_review_required':True,'captions':'Pendientes de transcripción y revisión; no se inventaron letras.','graphics':graphics,'metrics_checkpoints_hours':[24,72,168,336],'approved':False}
 with store.db(True) as d:store.audit(d,None,'plan.generated',cid,job['id'],report);store.notice(d,campaign['owner'],'Plan y selección propuestos. Revisa la música, completa los gráficos, conecta las cuentas y confirma los destinos antes de aprobar.',cid,'info')
 return report
