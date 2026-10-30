"""Official provider adapters. Credentials never leave the server or enter receipts."""
import base64, hashlib, json, os, time, urllib.error, urllib.parse, urllib.request
from cryptography.fernet import Fernet
from domain import Denied, uid

class ProviderError(Exception):
 def __init__(self,code,uncertain=False):super().__init__(code);self.uncertain=uncertain
class Vault:
 def __init__(self,key):self.box=Fernet(key)
 def seal(self,value):return self.box.encrypt(json.dumps(value).encode()).decode()
 def open(self,value):return json.loads(self.box.decrypt(value.encode()))
class NoRedirect(urllib.request.HTTPRedirectHandler):
 def redirect_request(self,*args):return None

def request(url,token=None,data=None,method=None,headers=None):
 """Fixed provider HTTPS endpoints only; never follow credential-bearing redirects."""
 u=urllib.parse.urlsplit(url)
 if u.scheme!='https' or u.hostname not in {'graph.facebook.com','graph.instagram.com','rupload.facebook.com','www.googleapis.com','youtubeanalytics.googleapis.com','oauth2.googleapis.com','open.tiktokapis.com'} or u.username or u.password:raise ProviderError('provider_destination_refused')
 h={'Accept':'application/json',**(headers or {})}
 if token:h['Authorization']='Bearer '+token
 if isinstance(data,dict):data=json.dumps(data).encode();h['Content-Type']='application/json'
 req=urllib.request.Request(url,data=data,headers=h,method=method)
 try:
  with urllib.request.build_opener(NoRedirect).open(req,timeout=45) as r:return json.loads(r.read(2_000_000) or b'{}'),dict(r.headers)
 except urllib.error.HTTPError as e:
  # Do not include token-containing URLs or raw provider responses in logs.
  raise ProviderError('provider_http_'+str(e.code),e.code>=500) from None
 except (OSError,ValueError):raise ProviderError('provider_outcome_unknown',True) from None

class Providers:
 def __init__(self,store,vault,config):self.store=store;self.vault=vault;self.config=config;self.graph='https://graph.facebook.com/'+config.get('META_API_VERSION','v23.0')
 def capabilities(self):
  return {'youtube':{'video':'automatic_after_oauth_and_studio_checks','short':'automatic_after_oauth_and_studio_checks','community':'assisted','comment':'automatic_after_oauth','collaboration':'assisted','related_video':'assisted','end_screen':'assisted'},'instagram':{'reel':'automatic_after_oauth','post':'automatic_after_oauth','carousel':'automatic_after_oauth','story':'automatic_if_eligible_without_link_sticker','story_link_sticker':'assisted','message':'assisted_with_separate_approval'},'facebook':{'post':'connector_ready_pending_page_authorization_and_live_validation','video':'connector_ready_pending_page_authorization_and_live_validation','reel':'connector_ready_pending_page_authorization_and_live_validation','story':'assisted','carousel':'connector_ready_pending_page_authorization_and_live_validation','message':'assisted_with_separate_approval'},'tiktok':{'video':'tiktok_connector_pending_audit_and_verified_domain','photo':'tiktok_connector_pending_audit_and_verified_domain'},'oauth_configured':{p:bool(self.config.get(k)) for p,k in [('youtube','GOOGLE_CLIENT_ID'),('instagram','FACEBOOK_APP_ID'),('facebook','FACEBOOK_APP_ID'),('tiktok','TIKTOK_CLIENT_KEY')]}}
 def known_tdf_identity(self,platform,external_id):
  from tiktok_environment import mode
  if mode(self.config)=='sandbox' and platform=='tiktok':return True # Callback verifies the configured username with TikTok before storage.
  return external_id in self.config.get('TDF_ACCOUNT_IDS',{}).get(platform,[])
 def start(self,actor,platform,scope):
  from tiktok_environment import connection
  connection(self.config,actor,platform,scope)
  if platform not in ('youtube','instagram','facebook','tiktok','drive'):raise Denied('Plataforma inválida.')
  if platform=='tiktok' and not self.config.get('TIKTOK_CLIENT_SECRET'):raise Denied('Falta configurar la aplicación de TikTok en el servidor.',409)
  if platform=='drive' and (not actor.get('admin') or scope!='tdf'):raise Denied('El respaldo de TDF requiere un administrador y una conexión de TDF.',403)
  if scope=='tdf' and not actor['admin']:raise Denied('Solo un administrador puede conectar canales de TDF.',403)
  if scope not in ('own','tdf'):raise Denied('Ámbito inválido.')
  state=uid()+uid();verifier=base64.urlsafe_b64encode(os.urandom(48)).decode().rstrip('=');redirect=self.config['PUBLIC_BASE']+'/oauth/callback/'+platform
  conf={'youtube':('https://accounts.google.com/o/oauth2/v2/auth','GOOGLE_CLIENT_ID','https://www.googleapis.com/auth/youtube https://www.googleapis.com/auth/yt-analytics.readonly'),'drive':('https://accounts.google.com/o/oauth2/v2/auth','GOOGLE_CLIENT_ID','https://www.googleapis.com/auth/drive.file'),'instagram':('https://www.facebook.com/'+self.config.get('META_API_VERSION','v23.0')+'/dialog/oauth','FACEBOOK_APP_ID','pages_show_list,pages_read_engagement,instagram_basic,instagram_content_publish,instagram_manage_insights'),'facebook':('https://www.facebook.com/'+self.config.get('META_API_VERSION','v23.0')+'/dialog/oauth','FACEBOOK_APP_ID','pages_show_list,pages_read_engagement,pages_manage_posts'),'tiktok':('https://www.tiktok.com/v2/auth/authorize/','TIKTOK_CLIENT_KEY','user.info.basic,video.publish')}
  base,key,scopes=conf[platform]
  if not self.config.get(key):raise Denied('Falta configurar la aplicación de '+platform+' en el servidor.',409)
  with self.store.db(True) as d:d.execute('INSERT INTO oauth_states VALUES(?,?,?,?,?,?,0)',(state,actor['id'],platform,scope,self.vault.seal({'verifier':verifier}),time.time()+600))
  q={'client_key' if platform=='tiktok' else 'client_id':self.config[key],'redirect_uri':redirect,'response_type':'code','scope':scopes,'state':state}
  if platform in ('youtube','drive'):q.update(access_type='offline',prompt='consent',code_challenge=base64.urlsafe_b64encode(hashlib.sha256(verifier.encode()).digest()).decode().rstrip('='),code_challenge_method='S256')
  return base+'?'+urllib.parse.urlencode(q)
 def callback(self,platform,state,code,actor):
  with self.store.db(True) as d:
   row=d.execute('SELECT * FROM oauth_states WHERE state=? AND platform=? AND used=0 AND expires_at>?',(state,platform,time.time())).fetchone()
   if not row:raise Denied('Autorización inválida o vencida.',403)
   if row['owner']!=actor['id'] or (row['scope']=='tdf' and not actor.get('admin')):raise Denied('La autorización no pertenece a esta sesión o ya no tienes permiso administrativo.',403)
   from tiktok_environment import connection
   connection(self.config,actor,platform,row['scope'])
   d.execute('UPDATE oauth_states SET used=1 WHERE state=?',(state,));row=dict(row)
  redirect=self.config['PUBLIC_BASE']+'/oauth/callback/'+platform
  if platform in ('youtube','drive'):
   body={'client_id':self.config['GOOGLE_CLIENT_ID'],'client_secret':self.config['GOOGLE_CLIENT_SECRET'],'code':code,'redirect_uri':redirect,'grant_type':'authorization_code','code_verifier':self.vault.open(row['verifier'])['verifier']};token,_=request('https://oauth2.googleapis.com/token',data=urllib.parse.urlencode(body).encode(),headers={'Content-Type':'application/x-www-form-urlencoded'})
   if platform=='youtube':
    info,_=request('https://www.googleapis.com/youtube/v3/channels?part=id,snippet&mine=true',token['access_token']);candidates=[{'id':i['id'],'handle':i['snippet']['title'],'platform':'youtube','token':token} for i in info.get('items',[])]
   else:
    info,_=request('https://www.googleapis.com/drive/v3/about?fields=user(permissionId,emailAddress,displayName),storageQuota',token['access_token']);user=info.get('user',{});candidates=[{'id':user['permissionId'],'handle':user.get('emailAddress') or user.get('displayName','Google Drive'),'platform':'drive','token':token}] if user.get('permissionId') else []
    if user.get('emailAddress','').casefold()!=self.config.get('BACKUP_GOOGLE_EMAIL','tdfestudiodegrabacion@gmail.com').casefold():raise Denied('Selecciona la cuenta de Google Drive de TDF configurada para el respaldo.',403)
  elif platform in ('facebook','instagram'):
   q=urllib.parse.urlencode({'client_id':self.config['FACEBOOK_APP_ID'],'client_secret':self.config['FACEBOOK_APP_SECRET'],'redirect_uri':redirect,'code':code});token,_=request(self.graph+'/oauth/access_token?'+q);grants,_=request(self.graph+'/me/permissions',token['access_token']);granted=[x['permission'] for x in grants.get('data',[]) if x.get('status')=='granted'];info,_=request(self.graph+'/me/accounts?fields=id,name,access_token,instagram_business_account{id,username}',token['access_token']);candidates=[]
   for page in info.get('data',[]):
    if platform=='facebook':candidates.append({'id':page['id'],'handle':page['name'],'platform':platform,'token':{'access_token':page['access_token']}})
    elif page.get('instagram_business_account'):candidates.append({'id':page['instagram_business_account']['id'],'handle':page['instagram_business_account'].get('username','Instagram'),'platform':platform,'token':{'access_token':page['access_token']}})
   for candidate in candidates:candidate['permissions']=granted
  else:
   body={'client_key':self.config['TIKTOK_CLIENT_KEY'],'client_secret':self.config['TIKTOK_CLIENT_SECRET'],'code':code,'grant_type':'authorization_code','redirect_uri':redirect};token,_=request('https://open.tiktokapis.com/v2/oauth/token/',data=urllib.parse.urlencode(body).encode(),headers={'Content-Type':'application/x-www-form-urlencoded'});info,_=request('https://open.tiktokapis.com/v2/user/info/?fields=open_id,display_name',token['access_token']);u=info['data']['user'];candidates=[{'id':u['open_id'],'handle':u['display_name'],'platform':platform,'token':token}]
   from tiktok_environment import mode,binding
   if mode(self.config)=='sandbox':
    if 'video.publish' not in token.get('scope','').split(','):raise Denied('La cuenta de prueba no autorizó la publicación de TikTok.',403)
    creator,_=request('https://open.tiktokapis.com/v2/post/publish/creator_info/query/',token['access_token'],{},method='POST')
    if creator.get('error',{}).get('code')!='ok' or creator.get('data',{}).get('creator_username')!=self.config['TIKTOK_SANDBOX_USERNAME']:raise Denied('La cuenta no coincide con el usuario autorizado para el sandbox.',403)
   token['_tdf_tiktok_client_binding']=binding(self.config)
  if platform!='drive':
   # Provider grants can include unrelated assets already connected to this app.
   # A TDF OAuth connection must never turn those assets into shared TDF channels.
   candidates=[c for c in candidates if self.known_tdf_identity(platform,c['id']) == (row['scope']=='tdf')]
  if not candidates:raise Denied('No se encontró una cuenta elegible para este ámbito. Los canales de TDF deben figurar en su registro oficial del servidor.',409)
  with self.store.db(True) as d:
   for c in candidates:
    if self.known_tdf_identity(platform,c['id']) and row['scope']!='tdf':raise Denied('Esta identidad corresponde a un canal oficial de TDF. Debe conectarla un administrador como canal de TDF.',403)
    old=d.execute('SELECT owner,scope FROM accounts WHERE platform=? AND external_id=?',(platform,c['id'])).fetchone()
    if old and (old['owner']!=row['owner'] or old['scope']!=row['scope']):raise Denied('Esta cuenta ya pertenece a otra conexión. Solicita una transferencia administrativa.',409)
    expiry=time.time()+c['token']['expires_in'] if c['token'].get('expires_in') else None
    d.execute('INSERT INTO accounts(id,owner,platform,external_id,handle,scope,credentials,permissions,expires_at) VALUES(?,?,?,?,?,?,?,?,?) ON CONFLICT(platform,external_id) DO UPDATE SET credentials=excluded.credentials,permissions=excluded.permissions,expires_at=excluded.expires_at,revoked=0,handle=excluded.handle',(uid(),row['owner'],platform,c['id'],c['handle'],row['scope'],self.vault.seal(c['token']),json.dumps(c.get('permissions',c['token'].get('scope','requested_in_oauth'))),expiry));self.store.audit(d,row['owner'],'account.connect',subject=c['id'],details={'platform':platform,'scope':row['scope']})
 def token(self,account):
  if account['platform']=='tiktok':
   from tiktok_token import refresh
   return refresh(self,account)
  token=self.vault.open(account['credentials'])
  if account['platform'] in ('youtube','drive') and token.get('refresh_token'):
   # Refresh on the server. Do not require the user's browser to stay open.
   body={'grant_type':'refresh_token','refresh_token':token['refresh_token'],'client_id':self.config['GOOGLE_CLIENT_ID'],'client_secret':self.config['GOOGLE_CLIENT_SECRET']};fresh,_=request('https://oauth2.googleapis.com/token',data=urllib.parse.urlencode(body).encode(),headers={'Content-Type':'application/x-www-form-urlencoded'});token.update(fresh)
   with self.store.db(True) as d:d.execute('UPDATE accounts SET credentials=?,expires_at=? WHERE id=?',(self.vault.seal(token),time.time()+fresh.get('expires_in',3600),account['id']))
  return token['access_token']
 def effect_guard(self,p):
  with self.store.db() as d:
   current=d.execute('SELECT * FROM pieces WHERE id=?',(p['id'],)).fetchone()
   if not current or current['version']!=p['version']:raise Denied('La pieza cambió durante el procesamiento.',409)
   account=self.store.execution_guard(d,current)
   if self.known_tdf_identity(account['platform'],account['external_id']) and account['scope']!='tdf':raise Denied('Un canal oficial de TDF requiere su conexión y aprobación administrativa.',403)
 def publish(self,p,a,media_url=None):
  from tiktok_environment import mode
  if mode(self.config)=='sandbox' and p['platform']!='tiktok':raise Denied('Este entorno solo permite pruebas privadas de TikTok.',403)
  self.effect_guard(p)
  settings=json.loads(p['settings']);token=self.token(a);platform=p['platform']
  if platform=='instagram':
   if p['format'] not in ('reel','post','carousel','story') or (p['format']=='story' and p['destination']):raise Denied('Esta pieza requiere publicación asistida para conservar su enlace o función nativa.',409)
   body={'caption':p['copy']}
   if p['format']=='reel':body.update(media_type='REELS',video_url=media_url,share_to_feed=True)
   elif p['format']=='story':body.update(media_type='STORIES',image_url=media_url)
   elif p['format']=='carousel':
    children=settings.get('children',[])
    if not isinstance(children,list) or not 2<=len(children)<=10:raise Denied('Selecciona entre2y10 archivos para el carrusel.')
    from server import signed_media_url
    child_ids=[]
    for aid in children:
     with self.store.db() as d:child=d.execute('SELECT * FROM assets WHERE id=? AND campaign=? AND status="ready"',(aid,p['campaign'])).fetchone()
     if not child:raise Denied('Un archivo del carrusel no pertenece a esta campaña.')
     child_body={'is_carousel_item':True,'image_url':signed_media_url(self.config,aid)}
     if child['name'].lower().endswith('.mp4'):child_body={'is_carousel_item':True,'media_type':'VIDEO','video_url':signed_media_url(self.config,aid)}
     created,_=request(self.graph+'/'+a['external_id']+'/media',token,child_body);child_ids.append(created['id'])
    body.update(media_type='CAROUSEL',children=child_ids)
   else:body['image_url']=media_url
   if not media_url and p['format']!='carousel':raise Denied('Falta el archivo revisado.')
   container,_=request(self.graph+'/'+a['external_id']+'/media',token,body);ident=container['id']
   # Save provider container before polling: later recovery must reconcile it.
   with self.store.db(True) as d:d.execute('UPDATE pieces SET external_id=?,status="processing" WHERE id=?',(ident,p['id']))
   for _ in range(10):
    status,_=request(self.graph+'/'+ident+'?fields=status_code',token)
    if status.get('status_code')=='FINISHED':break
    if status.get('status_code') in ('ERROR','EXPIRED'):raise ProviderError('instagram_container_failed')
    time.sleep(30)
   else:raise ProviderError('instagram_container_processing_requires_reconciliation',True)
   self.effect_guard(p)
   result,_=request(self.graph+'/'+a['external_id']+'/media_publish',token,{'creation_id':ident});media,_=request(self.graph+'/'+result['id']+'?fields=id,permalink',token);return {'external_id':result['id'],'url':media.get('permalink'),'state':'published'}
  if platform=='youtube' and p['format'] in ('video','short'):
   from youtube_upload import upload
   return upload(self,p,a,token)
  if platform=='tiktok':
   from tiktok import publish
   return publish(self,p,a,media_url)
  if platform=='facebook':
   from facebook import publish
   return publish(self,p,a,token,media_url)
  if platform=='youtube' and p['format']=='comment':
   video=settings.get('video_id')
   if not video:raise Denied('Selecciona el video de destino.')
   self.effect_guard(p)
   r,_=request('https://www.googleapis.com/youtube/v3/commentThreads?part=snippet',token,{'snippet':{'videoId':video,'topLevelComment':{'snippet':{'textOriginal':p['copy']}}}});return {'external_id':r['id'],'url':'https://www.youtube.com/watch?v='+video+'&lc='+r['snippet']['topLevelComment']['id'],'state':'published'}
  # Unsupported formats are explicit assisted tasks; never masquerade as success.
  raise Denied('Formato pendiente de integración o permisos: descarga la pieza y completa el paso asistido.',409)
 def metrics(self,p):
  if p['platform']!='youtube':return {'status':'unavailable','reason':'Métricas de esta plataforma pendientes de conexión e integración.','metrics':None}
  video=p['external_id']
  if not video:return {'status':'unavailable','reason':'Sin identificador externo.','metrics':None}
  with self.store.db() as d:a=d.execute('SELECT * FROM accounts WHERE id=? AND revoked=0',(p['account'],)).fetchone()
  token=self.token(dict(a)) if a else None
  key=self.config.get('YOUTUBE_API_KEY');url='https://www.googleapis.com/youtube/v3/videos?'+urllib.parse.urlencode({'part':'statistics,status,snippet','id':video,**({'key':key} if key else {})})
  data,_=request(url,token)
  if not data.get('items'):return {'status':'unavailable','reason':'Video no disponible para esta consulta.','metrics':None}
  item=data['items'][0];result={'status':'partial','public_statistics':item.get('statistics',{}),'retention':None,'watch_time':None,'attributed_subscribers':None,'reason':'Falta autorización YouTube Analytics o datos todavía no disponibles.','qa_views_may_be_included':True,'thumbnail_impressions':None,'thumbnail_ctr':None,'reach_status':'Revisar/exportar desde Studio o integrar Reporting API; no se confunden con impresiones publicitarias.','report_errors':{}}
  if a:
   try:start,today=youtube_report_dates(item.get('snippet',{}).get('publishedAt'))
   except ProviderError as error:
    result['report_errors']['date_range']=str(error);return result
   result['analytics_date_range']={'start':start,'end':today,'timezone':'America/Los_Angeles','granularity':'calendar_day','note':'Provider calendar dates; collection checkpoint does not imply all metrics cover the complete interval.'}
   base={'ids':'channel==MINE','startDate':start,'endDate':today,'filters':'video=='+video}
   queries={'analytics':{'metrics':'views,estimatedMinutesWatched,averageViewDuration,averageViewPercentage,subscribersGained,subscribersLost'},'traffic_sources':{'dimensions':'insightTrafficSourceType','metrics':'views,estimatedMinutesWatched'},'retention':{'dimensions':'elapsedVideoTimeRatio','metrics':'audienceWatchRatio,relativeRetentionPerformance'}}
   for name,query in queries.items():
    try:
     report,_=request('https://youtubeanalytics.googleapis.com/v2/reports?'+urllib.parse.urlencode({**base,**query}),token);result[name]={'status':'available' if report.get('rows') else 'pending','report':report}
     if name=='analytics':
      result['status']='available' if report.get('rows') else 'pending'
      if report.get('rows'):
       values=dict(zip([x['name'] for x in report['columnHeaders']],report['rows'][0]));result.update(watch_time=values.get('estimatedMinutesWatched'),watch_time_unit='minutes',average_view_seconds=values.get('averageViewDuration'),average_view_percentage=values.get('averageViewPercentage'),attributed_subscribers=values.get('subscribersGained'),subscribers_lost=values.get('subscribersLost'))
     result['reason']='Datos de API con posible retraso. Suscriptores del video corresponden a su página de reproducción; no implican atribución causal a piezas promocionales.'
    except ProviderError as error:result['report_errors'][name]=str(error)
  return result

def datetime_date(offset=0):
 import datetime
 return (datetime.datetime.now(datetime.timezone.utc)+datetime.timedelta(days=offset)).date().isoformat()

def youtube_report_dates(published_at,now=None):
 import datetime
 from zoneinfo import ZoneInfo
 try:
  published=datetime.datetime.fromisoformat(published_at.replace('Z','+00:00'))
  if published.tzinfo is None:raise ValueError('timezone required')
  current=now or datetime.datetime.fromtimestamp(time.time(),datetime.timezone.utc)
  if current.tzinfo is None:raise ValueError('timezone required')
  zone=ZoneInfo('America/Los_Angeles');start=published.astimezone(zone).date();end=current.astimezone(zone).date()
  if start>end:raise ValueError('future publication')
  return start.isoformat(),end.isoformat()
 except (AttributeError,TypeError,ValueError,OverflowError):raise ProviderError('youtube_publication_time_unavailable_for_analytics') from None
