import hashlib, json, logging, os, time, threading
from pathlib import Path
from domain import Store, Denied, uid
from providers import Providers, ProviderError, Vault

log=logging.getLogger('campaign-worker')
ACTIVE_JOB=None
def run_one(store,providers,media_url):
 global ACTIVE_JOB
 j=store.claim()
 if not j:return False
 ACTIVE_JOB=j['id']
 try:
  if j['kind']=='heartbeat':result={'status':'server_owned_job_completed','at':time.time()}
  elif j['kind']=='plan':
   from planning import plan
   result=plan(store,providers.config,j)
  elif j['kind']=='render':
   from media import render
   result=render(store,providers.config,j)
  elif j['kind']=='metrics' or j['kind'].startswith('metrics_'):
   with store.db() as d:p=dict(d.execute('SELECT * FROM pieces WHERE id=?',(j['piece'],)).fetchone())
   result=providers.metrics(p)
   with store.db(True) as d:d.execute('INSERT INTO metrics VALUES(?,?,?,?,?)',(uid(),p['campaign'],p['id'],time.time(),json.dumps(result)))
  elif j['kind'] in ('verify_youtube','verify_facebook','verify_tiktok'):
   if j['kind']=='verify_youtube':from youtube_upload import verify
   elif j['kind']=='verify_facebook':from facebook import verify
   else:from tiktok import verify
   with store.db() as d:
    p=dict(d.execute('SELECT * FROM pieces WHERE id=?',(j['piece'],)).fetchone())
    if j['kind']=='verify_youtube':a=store.execution_guard(d,p)
    else:
     a=d.execute('SELECT * FROM accounts WHERE id=? AND revoked=0',(p['account'],)).fetchone()
     if not a:raise Denied('La conexión de publicación ya no está disponible.',409)
     a=dict(a)
   result=verify(providers,p,a)
   if result['state'] in ('waiting_for_hd','waiting_for_facebook','waiting_for_tiktok'):
    with store.db(True) as d:d.execute('UPDATE jobs SET state="queued",due_at=? WHERE id=?',(time.time()+60,j['id']))
    ACTIVE_JOB=None
    return True
   with store.db(True) as d:d.execute('UPDATE pieces SET status=?,public_url=? WHERE id=?',(result['state'],result['url'],p['id']))
  elif j['kind']=='publish':
   with store.db(True) as d:
    p=dict(d.execute('SELECT * FROM pieces WHERE id=?',(j['piece'],)).fetchone());a=store.execution_guard(d,p);d.execute('UPDATE pieces SET status="publishing" WHERE id=?',(p['id'],))
   result=providers.publish(p,a,media_url(p['asset']) if p['asset'] else None)
   with store.db(True) as d:
    d.execute('UPDATE pieces SET status=?,external_id=?,public_url=?,verified_at=? WHERE id=?',(result['state'],result['external_id'],result.get('url'),None,p['id']));store.audit(d,None,'provider.publish',p['campaign'],p['id'],{'external_id':result['external_id'],'url':result.get('url'),'verification':'provider response; public playback not yet independently checked'})
  else:raise Denied('Tipo de trabajo no implementado: requiere revisión.')
  with store.db(True) as d:d.execute('UPDATE jobs SET state="completed",finished_at=?,payload=? WHERE id=?',(time.time(),json.dumps(result),j['id']))
 except Denied as e:
  with store.db(True) as d:
   d.execute('UPDATE jobs SET state="blocked",error=? WHERE id=?',(str(e),j['id']));d.execute('UPDATE pieces SET status="blocked" WHERE id=? AND status!="published"',(j['piece'],));c=d.execute('SELECT owner FROM campaigns WHERE id=?',(j['campaign'],)).fetchone()
   if c:store.notice(d,c['owner'],str(e),j['campaign'])
 except ProviderError as e:
  state=('queued' if j['attempts']<3 else 'failed') if j['kind']=='verify_tiktok' else ('unknown' if e.uncertain or j['kind'] in ('publish','verify_facebook') else ('queued' if j['attempts']<3 else 'failed'))
  with store.db(True) as d:
   d.execute('UPDATE jobs SET state=?,error=?,due_at=? WHERE id=?',(state,str(e),time.time()+60*2**j['attempts'],j['id']));
   if j['kind'] in ('publish','verify_facebook'):d.execute('UPDATE pieces SET status="unknown" WHERE id=?',(j['piece'],))
   c=d.execute('SELECT owner FROM campaigns WHERE id=?',(j['campaign'],)).fetchone()
   if c:store.notice(d,c['owner'],'No se confirmó una acción. Revisar antes de reintentar: '+str(e),j['campaign'],'error')
 except Exception:
  # Fail closed, with no provider payloads or secrets in logs.
  with store.db(True) as d:
   d.execute('UPDATE jobs SET state="unknown",error="internal_error_requires_review" WHERE id=?',(j['id'],))
   if j['kind'] in ('publish','verify_facebook','verify_youtube','verify_tiktok'):d.execute('UPDATE pieces SET status="unknown" WHERE id=? AND status!="published"',(j['piece'],))
   c=d.execute('SELECT owner FROM campaigns WHERE id=?',(j['campaign'],)).fetchone()
   if c:store.notice(d,c['owner'],'Una operación se detuvo sin confirmar su resultado. Revisa el registro antes de repetirla.',j['campaign'],'error')
  log.error('job_failed id=%s kind=%s',j['id'],j['kind'])
 ACTIVE_JOB=None
 return True

def heartbeat_loop(store,stop):
 while not stop.is_set():
  try:
   with store.db(True) as d:
    d.execute('INSERT INTO heartbeat VALUES(?,?,?) ON CONFLICT(name) DO UPDATE SET at=excluded.at,details=excluded.details',('worker',time.time(),json.dumps({'pid':os.getpid(),'active_job':ACTIVE_JOB})))
    if ACTIVE_JOB:d.execute('UPDATE jobs SET claimed_at=? WHERE id=? AND state="running"',(time.time(),ACTIVE_JOB))
  except Exception:log.error('heartbeat_failed')
  stop.wait(10)

def main():
 from server import make_runtime, signed_media_url
 store,providers,config=make_runtime()
 stop=threading.Event();threading.Thread(target=heartbeat_loop,args=(store,stop),daemon=True).start()
 while True:
  with store.db(True) as d:d.execute('INSERT INTO heartbeat VALUES(?,?,?) ON CONFLICT(name) DO UPDATE SET at=excluded.at,details=excluded.details',('worker',time.time(),json.dumps({'pid':os.getpid()})))
  from notifications import tick
  tick(store,config)
  if not run_one(store,providers,lambda a:signed_media_url(config,a)):time.sleep(5)
if __name__=='__main__':logging.basicConfig(level=logging.INFO);main()
