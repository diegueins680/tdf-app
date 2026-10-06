import os, pathlib, socket, subprocess, tempfile, json
P=pathlib.Path(__file__).parent; W=P/'editorial-repair-20261003'
PG=pathlib.Path('/usr/local/opt/postgresql@16/bin')
data=pathlib.Path(tempfile.mkdtemp(prefix='editorial-schema-pg-',dir=P))
with socket.socket() as sock:
 sock.bind(('127.0.0.1',0));port=sock.getsockname()[1]
def run(args,**kw):
 print('RUN',str(args[0]),flush=True)
 return subprocess.run([str(a) for a in args],check=True,**kw)
started=False
try:
 run([PG/'initdb','-D',data,'-U','postgres','-A','trust','--no-locale','-E','UTF8'])
 run([PG/'pg_ctl','-D',data,'-l',data/'server.log','-o',f'-h 127.0.0.1 -p {port} -k {data}','-w','start']);started=True
 run([PG/'createdb','-h','127.0.0.1','-p',str(port),'-U','postgres','editorial_schema_test'])
 psql=[PG/'psql','-h','127.0.0.1','-p',str(port),'-U','postgres','-d','editorial_schema_test','-X','-v','ON_ERROR_STOP=1']
 def apply(path):
  print('APPLY',path,flush=True);run(psql+['-f',path])
 batch=P/'editorial-production-batch.sql';verification=P/'editorial-production-verification.sql'
 with batch.open('w') as f:run(['node','scripts/render-production-migration-batch.mjs'],cwd=W,stdout=f)
 with verification.open('w') as f:run(['node','scripts/render-production-schema-verification.mjs'],cwd=W,stdout=f)
 for path in ['scripts/__tests__/fixtures/production-schema-20260814.sql','scripts/__tests__/fixtures/catalog-production-source-fixture.sql']:apply(W/path)
 apply(batch);apply(verification)
 apply(W/'tdf-hq/test/integration/directory_event_privacy_composition_postgres.sql')
 count=subprocess.check_output([str(x) for x in psql+['-qAtc','SELECT count(*) FROM public.tdf_schema_migration']],text=True).strip()
 assert count=='160',count
 apply(batch);apply(verification)
 for older in ['2026-09-07_directory_event_visibility_and_favorite_evidence','2026-09-09_music_directory_suppressed_event_privacy']:
  apply(W/('tdf-hq/sql/'+older+'.sql'))
  failed=subprocess.run([str(x) for x in psql+['-f',verification]])
  assert failed.returncode!=0,'Historical privacy defect incorrectly passed gate'
  for _ in range(2):
   apply(W/'tdf-hq/sql/2026-09-17_directory_event_privacy_composition.sql')
   apply(W/'tdf-hq/sql/2026-10-03_discovery_ownership_metadata_boundary.sql')
  apply(verification);apply(W/'tdf-hq/test/integration/directory_event_privacy_composition_postgres.sql')
 print('PASS: 160 registered migrations, apply/reapply, schema gate and both historical privacy orders with ownership/visibility/suppression regressions',flush=True)
finally:
 if started:run([PG/'pg_ctl','-D',data,'-m','immediate','-w','stop'])
 print('Owned schema database retained stopped:',data,flush=True)
