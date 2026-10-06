import sys,subprocess,shlex,json,datetime
from pathlib import Path
sys.path.insert(0,'/private/tmp/tdf-branch-audit-20260928/cutover/scripts')
import production_access as a
sql="SELECT c.relname FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND NOT has_table_privilege(current_user,c.oid,'SELECT') ORDER BY c.relname"
args=['docker','exec','-e','PGOPTIONS=-c default_transaction_read_only=on','tdf-production-db-1','psql','-X','-v','ON_ERROR_STOP=1','-qAt','-U','tdf_catalog_inventory','-d','tdf_hq','-c',sql]
r=subprocess.run(a.connection_args()+[shlex.join(args)],capture_output=True,text=True,timeout=60)
assert r.returncode==0,'Read-only schema coverage query failed'
x={'capturedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'missingSelectTables':r.stdout.splitlines()}
Path('/private/tmp/tdf-branch-audit-20260928/cutover-reader-new-tables.json').write_text(json.dumps(x,indent=2)+'\n');print(json.dumps(x))
