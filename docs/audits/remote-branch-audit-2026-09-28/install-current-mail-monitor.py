from pathlib import Path
import hashlib,json,subprocess,os,datetime,plistlib
P=Path(__file__).parent;C=P/'cutover';D=Path('/Users/diegosaa/GitHub/tdf-app/tmp/mail-deliverability');plist=Path('/Users/diegosaa/Library/LaunchAgents/net.tdfrecords.mail-deliverability.plist');original=plist.read_bytes();job=plistlib.loads(original)
assert job['ProgramArguments'][1]==str(D/'monitor.py')
prior=subprocess.check_output(['git','show','4031ed4a2:scripts/mail-deliverability-monitor.py'],cwd=C)
assert (D/'monitor.py').read_bytes()==prior,'Installed monitor changed concurrently; preserve it'
assert not (D/'production_access.py').exists(),'Unexpected existing helper; preserve it'
(P/'installed-mail-monitor-before.py').write_bytes(prior)
files=[('production_access.py','production_access.py'),('mail-deliverability-monitor.py','monitor.py')]
rows=[]
for source,target in files:
 data=(C/'scripts'/source).read_bytes();dest=D/target;temp=D/(target+'.audit-new')
 with temp.open('xb') as f:f.write(data)
 os.chmod(temp,0o600)
 if target=='monitor.py':assert dest.read_bytes()==prior
 else:assert not dest.exists()
 os.replace(temp,dest);assert dest.read_bytes()==data
 rows.append({'path':str(dest),'sha256':hashlib.sha256(data).hexdigest()})
assert plist.read_bytes()==original
record={'timestamp':datetime.datetime.now(datetime.timezone.utc).isoformat(),'action':'installed_current_readonly_mail_monitor','files':rows,'launchagent_unchanged':True,'prior_monitor_sha256':hashlib.sha256(prior).hexdigest()}
(P/'installed-mail-monitor-update.json').write_text(json.dumps(record,indent=2)+'\n')
with (P/'mutations.jsonl').open('a') as f:f.write(json.dumps(record)+'\n')
print('Verified installed monitor/helper hashes; existing LaunchAgent unchanged')
