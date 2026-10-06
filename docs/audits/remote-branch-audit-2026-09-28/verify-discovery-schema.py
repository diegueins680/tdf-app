import pathlib,subprocess,os,tempfile,socket,json,datetime
P=pathlib.Path(__file__).parent;cwd=P/'discovery';pg=pathlib.Path('/usr/local/opt/postgresql@16/bin')
root=subprocess.check_output(['stack','path','--local-install-root'],cwd=cwd/'tdf-hq',text=True).strip()
binary=pathlib.Path(root)/'bin/tdf-hq-exe';assert binary.is_file()
tmp=pathlib.Path(tempfile.mkdtemp(prefix='tdf-audit-discovery-schema-',dir='/private/tmp'))
def freeport():
 with socket.socket() as s:s.bind(('127.0.0.1',0));return s.getsockname()[1]
dbport=freeport();httpport=freeport();env={**os.environ,'LC_ALL':'C'}
record={'fixture':str(tmp),'binary':str(binary),'head':subprocess.check_output(['git','rev-parse','HEAD'],cwd=cwd,text=True).strip(),'started':datetime.datetime.now(datetime.timezone.utc).isoformat()}
with open(P/'discovery-schema-rehearsal.log','w') as log:
 subprocess.run([str(pg/'initdb'),'-D',str(tmp/'data'),'--locale=C','--encoding=UTF8','-A','trust','-U','postgres'],env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
 subprocess.run([str(pg/'pg_ctl'),'-D',str(tmp/'data'),'-o',f'-h 127.0.0.1 -k {tmp} -p {dbport}','-l',str(tmp/'postgres.log'),'-w','start'],env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
 try:
  subprocess.run([str(pg/'createdb'),'-h','127.0.0.1','-p',str(dbport),'-U','postgres','audit_schema'],env=env,check=True)
  env.update(TDF_AUTOMIG_TEST_DATABASE_URL=f'postgresql://postgres@127.0.0.1:{dbport}/audit_schema',TDF_AUTOMIG_SERVER_BIN=str(binary),TDF_AUTOMIG_SERVER_PORT=str(httpport),GITHUB_SHA=record['head'])
  r=subprocess.run(['bash','scripts/test-automatic-migrations-production-schema.sh'],cwd=cwd,env=env,stdout=log,stderr=subprocess.STDOUT)
  record['exit_code']=r.returncode
 finally:
  subprocess.run([str(pg/'pg_ctl'),'-D',str(tmp/'data'),'-m','immediate','-w','stop'],env=env,stdout=log,stderr=subprocess.STDOUT,check=True)
record['finished']=datetime.datetime.now(datetime.timezone.utc).isoformat()
(P/'discovery-schema-result.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(record),flush=True)
raise SystemExit(record['exit_code'])
