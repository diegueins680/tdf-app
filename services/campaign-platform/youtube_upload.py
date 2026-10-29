"""Resumable private YouTube upload, followed by a separate durable HD gate."""
import json, time, urllib.error, urllib.parse, urllib.request
from pathlib import Path
from providers import NoRedirect, ProviderError, request
from domain import Denied, uid

def upload(providers,p,a,token):
 settings=json.loads(p['settings']);title=str(settings.get('title') or p['name'])[:100]
 with providers.store.db() as d:
  asset=d.execute('SELECT * FROM assets WHERE id=? AND campaign=? AND status="ready"',(p['asset'],p['campaign'])).fetchone();session=d.execute('SELECT payload FROM provider_sessions WHERE piece=?',(p['id'],)).fetchone()
 if not asset:raise Denied('Selecciona el video final revisado.')
 source=Path(providers.config['STATE_DIR'])/'assets'/asset['path'];size=source.stat().st_size
 metadata={'snippet':{'title':title,'description':p['copy'],'categoryId':'10','defaultLanguage':settings.get('language','es-419')},'status':{'privacyStatus':'private','selfDeclaredMadeForKids':settings.get('made_for_kids',False),'containsSyntheticMedia':settings.get('contains_synthetic_media',False)}}
 if session:
  session=providers.vault.open(session[0]);url=session['url'];offset=session.get('offset',0)
 else:
  r,headers=request('https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status',token,metadata,headers={'X-Upload-Content-Length':str(size),'X-Upload-Content-Type':'video/mp4'});url=headers.get('Location') or headers.get('location');offset=0
  parsed=urllib.parse.urlsplit(url or '')
  if parsed.scheme!='https' or parsed.hostname!='www.googleapis.com':raise ProviderError('youtube_missing_resumable_url',True)
  with providers.store.db(True) as d:d.execute('INSERT INTO provider_sessions VALUES(?,?,?)',(p['id'],providers.vault.seal({'url':url,'offset':0}),time.time()))
 # Each 8MiB chunk is server-owned and checkpointed. An uncertain final response must reconcile, not start a second upload.
 with source.open('rb') as f:
  while offset<size:
   providers.effect_guard(p)
   f.seek(offset);chunk=f.read(8*1024*1024);end=offset+len(chunk)-1;req=urllib.request.Request(url,data=chunk,method='PUT',headers={'Authorization':'Bearer '+token,'Content-Type':'video/mp4','Content-Range':f'bytes {offset}-{end}/{size}','Content-Length':str(len(chunk))})
   try:
    with urllib.request.build_opener(NoRedirect).open(req,timeout=120) as response:
     result=json.loads(response.read(1_000_000));video=result['id'];offset=size
   except urllib.error.HTTPError as e:
    if e.code==308:
     acknowledged=e.headers.get('Range','')
     if not acknowledged.startswith('bytes=0-'):raise ProviderError('youtube_invalid_upload_range',True) from None
     offset=int(acknowledged.split('-')[-1])+1
    else:raise ProviderError('youtube_chunk_http_'+str(e.code),True) from None
   except (OSError,ValueError):raise ProviderError('youtube_chunk_outcome_unknown',True) from None
   with providers.store.db(True) as d:d.execute('UPDATE provider_sessions SET payload=?,updated_at=? WHERE piece=?',(providers.vault.seal({'url':url,'offset':offset}),time.time(),p['id']))
 with providers.store.db(True) as d:
  d.execute('INSERT INTO jobs(id,campaign,piece,kind,payload,due_at) VALUES(?,?,?,?,?,?) ON CONFLICT(piece,kind) DO NOTHING',(uid(),p['campaign'],p['id'],'verify_youtube','{}',time.time()+30))
 return {'external_id':video,'url':'https://www.youtube.com/watch?v='+video,'state':'processing'}

def verify(providers,p,a):
 token=providers.token(a);q=urllib.parse.urlencode({'part':'status,processingDetails,contentDetails','id':p['external_id']});data,_=request('https://www.googleapis.com/youtube/v3/videos?'+q,token)
 if not data.get('items'):raise Denied('No se pudo verificar el video subido; requiere revisión.')
 video=data['items'][0];status=video.get('status',{});processing=video.get('processingDetails',{}).get('processingStatus')
 if status.get('uploadStatus') in ('failed','rejected','deleted') or processing in ('failed','terminated'):raise Denied('YouTube rechazó o no pudo procesar el video. Revisa Studio; el video sigue privado.')
 if processing!='succeeded' or video.get('contentDetails',{}).get('definition')!='hd':return {'state':'waiting_for_hd'}
 # The public API does not expose Studio's complete copyright/checks report.
 settings=json.loads(p['settings'])
 if settings.get('youtube_checks_reviewed') is not True:raise Denied('HD disponible. Revisa derechos y restricciones en YouTube Studio y aprueba su publicación; la API no expone todas esas comprobaciones.')
 providers.effect_guard(p)
 result,_=request('https://www.googleapis.com/youtube/v3/videos?part=status',token,{'id':p['external_id'],'status':{**{k:v for k,v in status.items() if k in ('license','embeddable','publicStatsViewable','selfDeclaredMadeForKids','containsSyntheticMedia')},'privacyStatus':settings.get('visibility','public')}},method='PUT')
 return {'state':'published' if result['status']['privacyStatus']=='public' else 'private','external_id':p['external_id'],'url':'https://www.youtube.com/watch?v='+p['external_id']}
