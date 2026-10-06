import sys,subprocess,shlex,json,datetime
from pathlib import Path
import collect
P=collect.ROOT
sys.path.insert(0,str(P/'cutover/scripts'))
import production_access as a
proof=json.load(open(P/'cutover-new-table-source-proof.json'));expected=sorted(proof['tables']);sha=proof['commit']
head=json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+'/git/ref/heads/main'],env=collect.ENV,text=True))['object']['sha'];assert head==sha
before=json.loads(a.remote('metadata'));assert before['version']['commit']==sha
literals=','.join("'"+x+"'" for x in expected)
relations=','.join('public."'+x+'"' for x in expected)
expected_array='ARRAY['+literals+']::text[]'
sql="""\set ON_ERROR_STOP on
BEGIN;
SET LOCAL lock_timeout='2s';
DO $guard$ BEGIN
 IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname='tdf_catalog_inventory' AND NOT rolsuper AND NOT rolcreatedb AND NOT rolcreaterole AND NOT rolinherit AND NOT rolreplication AND NOT rolbypassrls) OR has_schema_privilege('tdf_catalog_inventory','public','CREATE') THEN RAISE EXCEPTION 'Unexpected catalog-reader privileges'; END IF;
 IF (SELECT array_agg(c.relname::text ORDER BY c.relname) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND NOT has_table_privilege('tdf_catalog_inventory',c.oid,'SELECT')) IS DISTINCT FROM """+expected_array+""" THEN RAISE EXCEPTION 'Concurrent schema or grants changed'; END IF;
END $guard$;
GRANT SELECT ON TABLE """+relations+""" TO tdf_catalog_inventory;
DO $guard$ BEGIN
 IF EXISTS (SELECT 1 FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND (NOT has_table_privilege('tdf_catalog_inventory',c.oid,'SELECT') OR has_table_privilege('tdf_catalog_inventory',c.oid,'INSERT,UPDATE,DELETE,TRUNCATE,TRIGGER,REFERENCES'))) THEN RAISE EXCEPTION 'Reader coverage or privilege validation failed'; END IF;
END $guard$;
COMMIT;
SELECT json_build_object('readableTables', count(*) FILTER (WHERE has_table_privilege('tdf_catalog_inventory',c.oid,'SELECT')), 'writableTables', count(*) FILTER (WHERE has_table_privilege('tdf_catalog_inventory',c.oid,'INSERT,UPDATE,DELETE,TRUNCATE')), 'schemaCreate',has_schema_privilege('tdf_catalog_inventory','public','CREATE')) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p');
"""
(P/'catalog-reviewed-tables-grant.sql').write_text(sql)
args=['docker','exec','-i','tdf-production-db-1','psql','-X','-v','ON_ERROR_STOP=1','-qAt','-U','postgres','-d','tdf_hq']
r=subprocess.run(a.connection_args()+[shlex.join(args)],input=sql,capture_output=True,text=True,timeout=60)
assert r.returncode==0,'Reviewed grant transaction failed: '+r.stderr
result=json.loads(r.stdout.strip());assert result['writableTables']==0 and result['schemaCreate']==False
result.update({'capturedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'productionCommit':sha,'grantedTables':expected,'applicationDataChanged':False})
(P/'catalog-reviewed-tables-grant-result.json').write_text(json.dumps(result,indent=2)+'\n')
with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':result['capturedAt'],'action':'extended_catalog_reader_select_to_reviewed_new_tables','sha':sha,'tables':expected,'evidence':'catalog-reviewed-tables-grant-result.json','no_application_data_changed':True})+'\n')
print(json.dumps(result))
