import pathlib,json,hashlib,ast,subprocess
folder=pathlib.Path(__file__).parent
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
canonical=(folder/'canonical-inspect-runtime.py').read_text();collector=(folder/'collect.py').read_text();query=(folder/'query-spec.json').read_text();provenance=json.loads((folder/'source-provenance.json').read_text())
assert hashlib.sha256(canonical.encode()).hexdigest()==provenance['canonicalSha256']
payload="import types,json\ncanonical=types.ModuleType('canonical_runtime_inventory')\nexec(compile("+repr(canonical)+",'canonical-inspect-runtime.py','exec'),canonical.__dict__)\nqueries=json.loads("+repr(query)+")\nnamespace={'__name__':'__main__','CANONICAL':canonical,'QUERY_SPECS':queries,'CANONICAL_SOURCE_SHA256':"+repr(provenance['canonicalSha256'])+"}\nexec(compile("+repr(collector)+",'collect.py','exec'),namespace)\n"
(folder/'remote-payload.py').write_text(payload)
for name in ['collect.py','canonical-inspect-runtime.py','prepare-spec.py','test_collect.py','build-review.py','remote-payload.py']:
 ast.parse((folder/name).read_text(),filename=name)
subprocess.run(['node','--check',str(folder/'launch.mjs')],check=True,capture_output=True)
log=(folder/'self-test.log').read_text();assert 'Ran 13 tests' in log and log.strip().endswith('OK')
module=ast.parse(canonical);storage=next(node.value.value for node in module.body if isinstance(node,ast.Assign) and any(isinstance(t,ast.Name) and t.id=='STORAGE_SQL' for t in node.targets))
files=['canonical-inspect-runtime.py','collect.py','query-spec.json','fixed-queries.sql','source-provenance.json','launch.mjs','remote-payload.py','test_collect.py','self-test.log','README.md','prepare-spec.py','build-review.py']
manifest={'schemaVersion':1,'status':'PREPARED_NOT_EXECUTED','networkExecuted':False,'rootReviewRequired':True,'sourceProvenance':provenance,'files':{name:sha(folder/name) for name in files},'fixedAggregateFamilies':9,'newInventoryRoleSqlStatements':11,'allNewQueryStatementsHaveReadOnlyRoleAnd2sWrapper':True,'privilegedIdentityException':{'function':'unchanged canonical.verify_storage','sqlSha256':hashlib.sha256(storage.encode()).hexdigest(),'role':'postgres','newBusinessQueriesAllowedUnderPostgres':False},'localValidation':{'pythonAstSyntax':'PASS all six Python sources including bundled payload','nodeSyntax':'PASS launch.mjs','unitTests':13,'unitResult':'PASS fake/pure tests only','postgresSqlExecution':'NOT_RUN','productionExecution':'NOT_RUN'},'limits':['Local validation does not establish PostgreSQL grammar or live permissions/data.','Counts remain UNOBSERVED until an explicitly reviewed execution.','Do not run role provisioning,DDL,grants or privileged catalog fallback.']}
(folder/'review-manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
for p in folder.iterdir():
 if p.is_file():p.chmod(0o600)
print(json.dumps({'status':'PREPARED_NOT_EXECUTED','folder':str(folder),'reviewManifestSha256':sha(folder/'review-manifest.json'),'canonicalSourceSha256':provenance['canonicalSha256'],'fixedIdentitySqlSha256':manifest['privilegedIdentityException']['sqlSha256'],'localTests':13,'sshExecuted':False}))
