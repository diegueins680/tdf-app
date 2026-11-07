"""Isolated real-media qualification. Never uses live accounts or publishes."""
import hashlib,json,os,pathlib,resource,sqlite3,subprocess,time
from domain import Store,uid
from providers import Providers,Vault
from cryptography.fernet import Fernet
from planning import queue_plan,plan
from media import render
code_root=pathlib.Path(__file__).resolve().parent;code_manifest={f.name:hashlib.sha256(f.read_bytes()).hexdigest() for f in code_root.glob('*.py')}
root=pathlib.Path('/var/lib/tdf-campaign-platform/media-qualification-'+time.strftime('%Y%m%dT%H%M%S'));root.mkdir();(root/'assets').mkdir();s=Store(root/'qualification.sqlite3');actor={'id':900001,'name':'Isolated media QA','admin':False};s.user(actor);cid=s.create(actor,{'name':'TDF media qualification','objective':'Revisar render sin publicaciones','destination':'https://youtu.be/uRNyIMLiqU8'})
source=root/'source.mp4';subprocess.run(['ffmpeg','-nostdin','-hide_banner','-loglevel','error','-ss','40','-i','/opt/tdf-campaign-platform/import/master.mp4','-t','40','-c','copy',str(source)],check=True,capture_output=True,timeout=60);source_hash=hashlib.sha256(source.read_bytes()).hexdigest();aid=s.reserve_asset(actor,cid,'source.mp4',source.stat().st_size,True);os.link(source,root/'assets'/aid)
with s.db(True) as d:d.execute('UPDATE assets SET path=?,size=reserved,uploaded=reserved,status="ready",sha256=? WHERE id=?',(aid,source_hash,aid))
config={'STATE_DIR':str(root),'LOGO_PNG':'/opt/tdf-campaign-platform/tdf-logo.png','PUBLIC_BASE':'https://api.tdfrecords.net/campaign-platform'};queue_plan(s,actor,cid,{'source':aid,'start_at':'2030-01-01T18:30:00-05:00','facts':{'artist':'Roj(o) — prueba privada'}})
with s.db() as d:job=dict(d.execute('SELECT * FROM jobs WHERE kind="plan"').fetchone())
started=time.monotonic();result=plan(s,config,job)
with s.db() as d:job=dict(d.execute('SELECT * FROM jobs WHERE kind="render"').fetchone())
rendered=render(s,config,job)
checks=[]
with s.db() as d:assets=d.execute('SELECT * FROM assets WHERE id!=?',(aid,)).fetchall();live_publications=d.execute('SELECT count(*) FROM jobs WHERE kind="publish"').fetchone()[0]
for a in assets:
 path=root/'assets'/a['id'];info=json.loads(subprocess.run(['ffprobe','-v','error','-show_streams','-show_format','-of','json',str(path)],check=True,capture_output=True,timeout=20).stdout);video=next(x for x in info['streams'] if x['codec_type']=='video');expected=(1080,1920) if a['name'].startswith(('S','clip')) else (1080,1350);actual=(video['width'],video['height']);assert actual==expected,(a['name'],actual,expected);checks.append({'name':a['name'],'width':actual[0],'height':actual[1],'bytes':a['size'],'sha256':a['sha256']})
assert len(checks)==20 and live_publications==0;assert hashlib.sha256(source.read_bytes()).hexdigest()==source_hash
assert code_manifest=={f.name:hashlib.sha256(f.read_bytes()).hexdigest() for f in code_root.glob('*.py')}
r={'source_manifest':code_manifest,'result':'PASS','elapsed_seconds':time.monotonic()-started,'cpu_seconds_children':resource.getrusage(resource.RUSAGE_CHILDREN).ru_utime+resource.getrusage(resource.RUSAGE_CHILDREN).ru_stime,'assets':checks,'source_preserved':True,'publication_jobs':0,'scope':'Actual FFmpeg audio analysis,6clips,8Stories,6carousel graphics. Draft selection requires human musical review. No external accounts or publications.','output_directory':str(root)};(root/'result.json').write_text(json.dumps(r,indent=2));print(json.dumps(r));
