import pathlib,subprocess,tempfile,socket,os,re,shlex,json
P=pathlib.Path(__file__).parent;W=P/'events-editorial-repair-20261003';PG=pathlib.Path('/usr/local/opt/postgresql@16/bin')
data=pathlib.Path(tempfile.mkdtemp(prefix='lifecycle-review-pg-',dir=P))
with socket.socket() as s:s.bind(('127.0.0.1',0));port=s.getsockname()[1]
env={k:v for k,v in os.environ.items() if not k.startswith('PG')};env['PATH']=str(PG)+':'+env['PATH']
def run(args,**kw):return subprocess.run([str(a) for a in args],env=env,check=True,**kw)
started=False
try:
 run([PG/'initdb','-D',data,'-U','postgres','-A','trust','--no-locale','-E','UTF8'],stdout=subprocess.DEVNULL)
 run([PG/'pg_ctl','-D',data,'-l',data/'server.log','-o',f'-h 127.0.0.1 -p {port} -k {data}','-w','start']);started=True
 source=(W/'scripts/test-event-operations-api-migration.sh').read_text();body=source[source.index('json_field() {'):]
 old=P/'lifecycle-before.sql';old.write_text(subprocess.check_output(['git','show','HEAD:tdf-hq/sql/2026-09-14_event_operations_api.sql'],cwd=W,text=True))
 outcomes=[]
 for label,migration in [('before',old),('fixed',W/'tdf-hq/sql/2026-09-14_event_operations_api.sql')]:
  db='lifecycle_'+label;run([PG/'createdb','-h','127.0.0.1','-p',port,'-U','postgres',db]);results=pathlib.Path(tempfile.mkdtemp(prefix='lifecycle-'+label+'-results-',dir=P))
  psql=[PG/'psql','-h','127.0.0.1','-p',port,'-U','postgres','-d',db,'-X','-v','ON_ERROR_STOP=1'];cmd=shlex.join([str(x) for x in psql])
  pre='set -eu\n'+''.join(k+'='+shlex.quote(str(v))+'\n' for k,v in dict(repo_root=W,result_dir=results,test_database=db,foundation_migration=W/'tdf-hq/sql/2026-09-14_event_operations_foundation.sql',api_migration=migration,api_rollback=W/'tdf-hq/sql/2026-09-14_event_operations_api_rollback.sql',fixture_sql=W/'tdf-hq/test/integration/event_operations_foundation_fixture.sql').items())
  pre+='psql_exec() { '+cmd+' "$@"; }\napply_sql() { psql_exec < "$1" >/dev/null; }\n'
  script=P/('lifecycle-native-'+label+'.sh');script.write_text(pre+body)
  with (P/('lifecycle-native-'+label+'.log')).open('w') as log:r=subprocess.run(['sh',str(script)],env=env,cwd=W,stdout=log,stderr=subprocess.STDOUT)
  text=(P/('lifecycle-native-'+label+'.log')).read_text();outcomes.append({'label':label,'exit_code':r.returncode,'results':str(results)})
  if label=='before':assert r.returncode!=0 and 'Replay test backend did not reach Lock: transition_flag_second' in text,text[-2500:]
  else:assert r.returncode==0,text[-3500:]
  run(psql+['-c',"SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname=current_database() AND pid<>pg_backend_pid()"],stdout=subprocess.DEVNULL)
  print('VERIFIED',label,'exit',r.returncode,flush=True)
 (P/'lifecycle-native-verification.json').write_text(json.dumps({'outcomes':outcomes,'result':'PASS','old_defect_reproduced':True,'both_orders_all_three_isolations_pass':True},indent=2))
 db='lifecycle_http';run([PG/'createdb','-h','127.0.0.1','-p',port,'-U','postgres',db]);psql=[PG/'psql','-h','127.0.0.1','-p',port,'-U','postgres','-d',db,'-X','-v','ON_ERROR_STOP=1']
 script=(W/'scripts/test-event-operations-http.sh').read_text();paths=re.findall(r'apply_sql "\$repo_root/([^"\n]+)"',script);assert len(paths)==11
 for path in paths:run(psql+['-f',W/path],stdout=subprocess.DEVNULL)
 env.update(EVENT_OPERATIONS_DISPOSABLE_HTTP_TEST='1',EVENT_OPERATIONS_TEST_DSN=f'host=127.0.0.1 port={port} user=postgres dbname={db}')
 run(['sh',W/'scripts/run-event-operations-http-harness.sh'],cwd=W/'tdf-hq')
 print('PASS lifecycle concurrency and production-auth HTTP transaction rollback harness',flush=True)
finally:
 if started:run([PG/'pg_ctl','-D',data,'-m','immediate','-w','stop'])
 print('Owned PostgreSQL cluster stopped and retained:',data,flush=True)
