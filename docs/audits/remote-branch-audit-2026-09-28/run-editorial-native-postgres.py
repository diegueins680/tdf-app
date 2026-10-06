import datetime, json, os, pathlib, socket, subprocess, tempfile
P=pathlib.Path(__file__).parent
W=P/'editorial-repair-20261003'
PG=pathlib.Path('/usr/local/opt/postgresql@16/bin')
binary=W/'tdf-hq/.stack-work/dist/x86_64-osx/ghc-9.10.3/build/tdf-hq-test/tdf-hq-test'
assert binary.is_file(), 'Build the prescribed Stack test executable first'
data=pathlib.Path(tempfile.mkdtemp(prefix='editorial-postgres-',dir=P))
with socket.socket() as sock:
 sock.bind(('127.0.0.1',0));port=sock.getsockname()[1]
def run(args,**kw):
 print('RUN',args[0],flush=True)
 return subprocess.run([str(a) for a in args],check=True,**kw)
started=False
try:
 run([PG/'initdb','-D',data,'-U','postgres','-A','trust','--no-locale','-E','UTF8'])
 run([PG/'pg_ctl','-D',data,'-l',data/'server.log','-o',f'-h 127.0.0.1 -p {port} -k {data}','-w','start']);started=True
 run([PG/'createdb','-h','127.0.0.1','-p',str(port),'-U','postgres','editorial_invitation_test'])
 env=dict(os.environ,TDF_INVITATION_TEST_DATABASE_URL=f'host=127.0.0.1 port={port} user=postgres dbname=editorial_invitation_test')
 for match in ['actual PostgreSQL visibility predicate','invitation']:
  print('MATCH',match,flush=True)
  run([binary,'--match='+match,'--fail-on=empty'],cwd=W/'tdf-hq',env=env)
 print('PASS: actual PostgreSQL visibility predicate and invitation concurrency suites',flush=True)
finally:
 if started:run([PG/'pg_ctl','-D',data,'-m','immediate','-w','stop'])
 print('Isolated database retained stopped at',data,flush=True)
