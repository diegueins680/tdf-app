import subprocess,pathlib,os,re,datetime,json
P=pathlib.Path(__file__).parent;root=P/'events';db='tdf_event_audit_20261001_'+str(os.getpid());assert re.fullmatch('tdf_event_audit_20261001_[0-9]+',db)
bin='/usr/local/opt/postgresql@16/bin/'
env={k:v for k,v in os.environ.items() if not k.startswith('PG')}
connection=['-h','/tmp','-p','5432','-U','diegosaa']
subprocess.run([bin+'createdb',*connection,db],env=env,check=True)
try:
 script=(root/'scripts/test-event-operations-http.sh').read_text()
 paths=re.findall(r'apply_sql "\$repo_root/([^"\n]+)"',script)
 assert len(paths)==11,paths
 for path in paths:
  print('Apply unchanged HTTP fixture/migration:',path,flush=True)
  subprocess.run([bin+'psql',*connection,'-X','-v','ON_ERROR_STOP=1','-d',db],input=(root/path).read_text(),text=True,env=env,check=True,stdout=subprocess.DEVNULL)
 env.update(EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST='1',EVENT_OPERATIONS_TEST_DSN='host=/tmp port=5432 user=diegosaa dbname='+db)
 subprocess.run(['sh',str(root/'scripts/run-event-operations-http-harness.sh')],cwd=root/'tdf-hq',env=env,check=True)
 print('PASS unchanged event HTTP harness on isolated native PostgreSQL16; Docker runner not claimed',flush=True)
finally:
 subprocess.run([bin+'dropdb',*connection,db],env=env,check=True)
