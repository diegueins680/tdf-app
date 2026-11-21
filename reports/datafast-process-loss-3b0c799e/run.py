from pathlib import Path
import socket,secrets,tempfile,subprocess,shutil
base=Path.cwd(); repo=base/'pg-replay-src'; rt=base/'replay-tools/root'; b=rt/'usr/lib/postgresql/17/bin'; share=rt/'usr/share/postgresql/17'
with tempfile.TemporaryDirectory(prefix='payment-replay-') as tmp:
 p=Path(tmp);pw=p/'pw';password=secrets.token_hex(24);pw.write_text(password);pw.chmod(0o600)
 with socket.socket() as s:s.bind(('127.0.0.1',0));port=s.getsockname()[1]
 env=dict(PATH=':'.join(map(str,[b,Path(shutil.which('node')).parent,Path(shutil.which('git')).parent,'/usr/bin','/bin'])),PGPASSWORD=password,PGHOST='127.0.0.1',PGPORT=str(port),PGUSER='synthetic',LANG='C.UTF-8')
 def run(*args):subprocess.run(list(map(str,args)),env=env,check=True)
 run(b/'initdb','-D',p/'data','-L',share,'-U','synthetic','--pwfile',pw,'--auth-local=reject','--auth-host=scram-sha-256','--data-checksums')
 with (p/'data/postgresql.conf').open('a') as f:f.write(f"\nlisten_addresses='127.0.0.1'\nport={port}\nunix_socket_directories=''\n")
 run(b/'pg_ctl','-D',p/'data','-l',p/'server.log','-w','start')
 try:
  run(b/'createdb','tdf_provider_retry_test')
  url=f'postgresql://synthetic@127.0.0.1:{port}/tdf_provider_retry_test'
  run('sh',repo/'scripts/load-payment-test-schema.sh',url)
  run(b/'psql',url,'-X','-q','-v','ON_ERROR_STOP=1','-f',repo/'scripts/__tests__/fixtures/provider-domain-fixtures.sql')
  env.update(TDF_PROVIDER_RETRY_DATABASE_URL=url,TDF_RECOVERY_RESTORE_LAB='synthetic-loopback-only')
  binary=base/'durable-lab/probe'
  def counts():
   return subprocess.check_output([str(b/'psql'),url,'-XAt','-c',"select count(*) filter(where paid_minor>0),coalesce(sum(paid_minor),0) from commerce_checkout_session where id in(select checkout_id from commerce_payment_attempt where provider='datafast')"],env=env,text=True).strip()
  run(binary,'origin',p/'pending.dump')
  assert counts()=='1|12515',counts()
  print('After original process exited: 1 paid checkout, 12515 minor units',flush=True)
  run(b/'pg_restore','--clean','--if-exists','--exit-on-error','--no-owner','--dbname',url,p/'pending.dump')
  assert counts()=='0|0',counts()
  run(binary,'recover')
  assert counts()=='0|0',counts()
  print('After restore and fresh recovery process: 0 paid checkouts, 0 applied minor units',flush=True)
 finally:run(b/'pg_ctl','-D',p/'data','-m','fast','-w','stop')
