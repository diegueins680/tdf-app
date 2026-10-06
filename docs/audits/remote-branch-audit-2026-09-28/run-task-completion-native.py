import pathlib, subprocess, tempfile, socket, os, shlex, json
P=pathlib.Path(__file__).parent; W=P/'events-editorial-repair-20261003'; PG=pathlib.Path('/usr/local/opt/postgresql@16/bin')
source=(W/'scripts/test-event-task-completion-migration.sh').read_text()
data=pathlib.Path(tempfile.mkdtemp(prefix='task-completion-review-pg-',dir=P))
with socket.socket() as sock: sock.bind(('127.0.0.1',0)); port=sock.getsockname()[1]
env={k:v for k,v in os.environ.items() if not k.startswith('PG')}; env['PATH']=str(PG)+':'+env['PATH']
def run(args,**kw): return subprocess.run([str(x) for x in args],env=env,check=True,**kw)
started=False
try:
 run([PG/'initdb','-D',data,'-U','postgres','-A','trust','--no-locale','-E','UTF8'],stdout=subprocess.DEVNULL)
 run([PG/'pg_ctl','-D',data,'-l',data/'server.log','-o',f'-h 127.0.0.1 -p {port} -k {data}','-w','start']); started=True
 label=os.environ.get('COMPLETION_LABEL','before'); db='completion_'+label
 run([PG/'createdb','-h','127.0.0.1','-p',port,'-U','postgres',db])
 results=pathlib.Path(tempfile.mkdtemp(prefix='task-completion-'+label+'-results-',dir=P))
 psql=[PG/'psql','-h','127.0.0.1','-p',port,'-U','postgres','-d',db,'-X','-v','ON_ERROR_STOP=1']
 pre='set -eux\n'+''.join(k+'='+shlex.quote(str(v))+'\n' for k,v in dict(repo_root=W,test_logs=results).items())
 pre+='sql() { PGOPTIONS="-c client_min_messages=warning -c statement_timeout=25000" '+shlex.join([str(x) for x in psql])+' "$@"; }\napply() { sql < "$repo_root/$1" >/dev/null; }\n'
 script=P/('task-completion-native-'+label+'.sh'); script.write_text(pre+source[source.index('wait_backend() {'):])
 log=P/('task-completion-native-'+label+'.log')
 with log.open('w') as out: result=subprocess.run(['sh',str(script)],env=env,cwd=W,stdout=out,stderr=subprocess.STDOUT)
 evidence={'label':label,'exit_code':result.returncode,'cluster':str(data),'log':str(log),'results':str(results)}
 (P/('task-completion-native-'+label+'.json')).write_text(json.dumps(evidence,indent=2));print(json.dumps(evidence),flush=True)
finally:
 if started: run([PG/'pg_ctl','-D',data,'-m','immediate','-w','stop'])
