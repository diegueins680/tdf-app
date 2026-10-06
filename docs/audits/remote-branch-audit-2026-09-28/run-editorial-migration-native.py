import pathlib, socket, subprocess, tempfile
P=pathlib.Path(__file__).parent
PG=pathlib.Path('/usr/local/opt/postgresql@16/bin')
data=pathlib.Path(tempfile.mkdtemp(prefix='editorial-migration-pg-',dir=P))
with socket.socket() as sock:
 sock.bind(('127.0.0.1',0));port=sock.getsockname()[1]
def run(args):return subprocess.run([str(a) for a in args],check=True)
started=False
try:
 run([PG/'initdb','-D',data,'-U','postgres','-A','trust','--no-locale','-E','UTF8'])
 run([PG/'pg_ctl','-D',data,'-l',data/'server.log','-o',f'-h 127.0.0.1 -p {port} -k {data}','-w','start']);started=True
 run([PG/'psql','-h','127.0.0.1','-p',str(port),'-U','postgres','-d','postgres','-X','-v','ON_ERROR_STOP=1','-f',P/'editorial-migration-native.sql'])
finally:
 if started:run([PG/'pg_ctl','-D',data,'-m','immediate','-w','stop'])
 print('Owned database retained stopped:',data,flush=True)
