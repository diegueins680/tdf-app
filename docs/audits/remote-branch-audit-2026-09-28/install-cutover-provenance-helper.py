import collect,pathlib,json,subprocess,hashlib,datetime,os,tempfile,stat,plistlib
P=collect.ROOT;W=P/'cutover-provenance-followup-20261003';dest=pathlib.Path('/Users/diegosaa/GitHub/tdf-app/tmp/mail-deliverability/production_access.py');old='8a23c52aadf95f61c918900d2f5a4583e33759ee9166a5c28a51ba53ef0bf681';head='ce1a71fe241ccc05b6114088fb36dd919974dc87'
def digest(p):return hashlib.sha256(p.read_bytes()).hexdigest()
# Require successful live proof of the new access contract before installing it.
assert json.loads((P/'cutover-provenance-latest-live-attempt.json').read_text())['exit_code']==0
assert (P/'cutover-provenance-live-catalog.json').exists()
mail=json.loads((P/'cutover-provenance-mail-live/latest.json').read_text());assert mail['mailboxReadSucceeded'] and not mail['errors'],mail['errors']
pr=json.loads(subprocess.check_output(['/usr/local/bin/gh','api',collect.REPO+'/pulls/468'],env=collect.ENV,text=True));assert pr['head']['sha']==head and pr['state']=='open'
assert digest(dest)==old
plist=pathlib.Path('/Users/diegosaa/Library/LaunchAgents/net.tdfrecords.mail-deliverability.plist');plist_hash=digest(plist)
monitor=dest.with_name('monitor.py');assert digest(monitor)==digest(W/'scripts/mail-deliverability-monitor.py')
backup=P/'installed-production-access-before-ce1.py';assert not backup.exists();backup.write_bytes(dest.read_bytes())
new=(W/'scripts/production_access.py').read_bytes();expected=hashlib.sha256(new).hexdigest()
with tempfile.NamedTemporaryFile(dir=dest.parent,prefix='.production-access-',suffix='.py',delete=False) as out:
 temp=pathlib.Path(out.name);out.write(new);out.flush();os.fsync(out.fileno())
os.chmod(temp,stat.S_IMODE(dest.stat().st_mode));assert digest(dest)==old
os.replace(temp,dest);assert digest(dest)==expected and digest(plist)==plist_hash
entry={'time':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'installed_verified_database_image_guard','pr':468,'sha':head,'path':str(dest),'before_sha256':old,'sha256':expected,'launchagent_unchanged':True,'live_metadata_catalog_and_mail_passed':True}
(P/'cutover-provenance-installed-helper.json').write_text(json.dumps(entry,indent=2))
with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps(entry)+'\n')
args=plistlib.loads(plist.read_bytes())['ProgramArguments']
with (P/'cutover-provenance-installed-mail.log').open('w') as log:r=subprocess.run(args,cwd=dest.parent,stdout=log,stderr=subprocess.STDOUT)
report=json.loads((dest.parent/'reports/latest.json').read_text())
result={'timestamp':datetime.datetime.now(datetime.timezone.utc).isoformat(),'exit_code':r.returncode,'mailboxReadSucceeded':report['mailboxReadSucceeded'],'aggregate_report_count':len(report['aggregateReports']),'errors':report['errors'],'no_mail_sent':True,'helper_sha256':digest(dest),'launchagent_unchanged':digest(plist)==plist_hash}
(P/'cutover-provenance-installed-mail-verified.json').write_text(json.dumps(result,indent=2));assert r.returncode==0 and result['mailboxReadSucceeded'] and not result['errors'];print(json.dumps(result))
