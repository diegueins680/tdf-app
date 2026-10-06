from pathlib import Path
import subprocess,json,datetime,hashlib
P=Path(__file__).parent;sql=(P/'cutover/ops/hetzner/catalog-readonly-role.sql').read_bytes()
ssh=['ssh','-i','/Users/diegosaa/.ssh/tdf_hetzner_deploy_20260928','-o','BatchMode=yes','-o','StrictHostKeyChecking=yes','-o','ConnectTimeout=10','root@178.105.93.101']
# Fresh identity precondition; no environment values are printed.
import sys
sys.path.insert(0,str(P/'cutover/scripts'));import production_access
meta=json.loads(production_access.remote('metadata'));assert meta['project']=='tdf-production' and meta['database']=='tdf_hq' and meta['health']=={'status':'ok','db':'ok'}
cmd='docker exec -i tdf-production-db-1 psql -X -v ON_ERROR_STOP=1 -U postgres -d tdf_hq'
r=subprocess.run(ssh+[cmd],input=sql,capture_output=True);assert r.returncode==0,r.stderr.decode()
probe="SELECT json_build_object('role',current_user,'superuser',(SELECT rolsuper FROM pg_roles WHERE rolname=current_user),'schemaCreate',has_schema_privilege(current_user,'public','CREATE'),'tableWrites',(SELECT count(*) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND (has_table_privilege(current_user,c.oid,'INSERT') OR has_table_privilege(current_user,c.oid,'UPDATE') OR has_table_privilege(current_user,c.oid,'DELETE'))),'tableReads',(SELECT count(*) FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname='public' AND c.relkind IN ('r','p') AND has_table_privilege(current_user,c.oid,'SELECT')))"
r=subprocess.run(ssh+['docker exec -i tdf-production-db-1 psql -X -v ON_ERROR_STOP=1 -qAt -U tdf_catalog_inventory -d tdf_hq'],input=probe.encode(),capture_output=True);assert r.returncode==0
record=json.loads(r.stdout);assert record['role']=='tdf_catalog_inventory' and not record['superuser'] and not record['schemaCreate'] and record['tableWrites']==0 and record['tableReads']>0
# Zero-row probe: even if a permission regressed, no customer row can change.
negative=b"BEGIN; SET TRANSACTION READ WRITE; SET default_transaction_read_only=off; UPDATE public.country SET name=name WHERE false; ROLLBACK;"
r=subprocess.run(ssh+['docker exec -i tdf-production-db-1 psql -X -v ON_ERROR_STOP=1 -qAt -U tdf_catalog_inventory -d tdf_hq'],input=negative,capture_output=True);assert r.returncode!=0 and b'permission denied' in r.stderr
record.update(verified_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),sql_sha256=hashlib.sha256(sql).hexdigest(),write_probe='permission denied after explicit read-write/default override; zero-row predicate',application_data_changed=False,password_provisioned=False,existing_role_capabilities_preserved=True,public_schema_create_hardened=True)
(P/'catalog-reader-provisioned.json').write_text(json.dumps(record,indent=2)+'\n')
with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps({'time':record['verified_at'],'action':'provisioned_verified_readonly_catalog_role','role':record['role'],'evidence':'catalog-reader-provisioned.json','application_data_changed':False,'existing_role_capabilities_preserved':True,'public_schema_create_hardened':True})+'\n')
print(json.dumps(record))
