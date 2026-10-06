import sys,subprocess,shlex,json
sys.path.insert(0,'/private/tmp/tdf-branch-audit-20260928/cutover/scripts')
import production_access as a
code=a.REMOTE.replace("sys.stderr.write('Verified production read-only access failed\\n')", "import traceback; sys.stderr.write(json.dumps({'type': type(sys.exception()).__name__, 'lines': [f.lineno for f in traceback.extract_tb(sys.exception().__traceback__)]}) + '\\n')")
mode=sys.argv[1] if len(sys.argv)>1 else 'metadata'
sql=subprocess.check_output(['node','/private/tmp/tdf-branch-audit-20260928/cutover/scripts/production-catalog-inventory.mjs','--dry-run'],text=True).removesuffix('\n') if mode=='inventory' else None
r=subprocess.run(a.connection_args()+['python3 -c '+shlex.quote(code)+' '+mode],input=sql,capture_output=True,text=True,timeout=210)
print('exit',r.returncode)
print(r.stderr)
if r.returncode==0 and mode=='metadata':
 x=json.loads(r.stdout);print(json.dumps({k:x[k] for k in ('provider','sshServerAddress','version')}))
