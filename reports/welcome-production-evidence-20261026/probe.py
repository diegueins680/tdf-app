from pathlib import Path
import subprocess,json,re,sys,datetime,hashlib
expected=sys.argv[1];assert re.fullmatch('[a-f0-9]{40}',expected)
base='https://www.tdfrecords.net'
def get(url):
 r=subprocess.run(['curl','--silent','--show-error','--max-time','20','--max-filesize','5000000','-H','Cache-Control: no-cache','-w','\n%{http_code}',url],capture_output=True,timeout=25);assert r.returncode==0,'Transport failed';body,status=r.stdout.rsplit(b'\n',1);return int(status),body
status,html=get(base+'/');assert status==200;entries=re.findall(r'<script[^>]+src="(/assets/[^\"]+\.js)"',html.decode());assert entries
status,data=get(base+entries[0]);assert status==200
r={'observedAt':datetime.datetime.now(datetime.timezone.utc).isoformat(),'expectedMerge':expected,'entry':entries[0],'entrySha256':hashlib.sha256(data).hexdigest(),'embeddedCommits':sorted(set(re.findall(r'(?<![a-f0-9])[a-f0-9]{40}(?![a-f0-9])',data.decode()))),'expectedPublished':expected.encode() in data,'scope':'Public read-only HTML/entry; not browser behavior or backend deployment.'}
p=Path(__file__).parent/'publication-probes.json';old=json.loads(p.read_text()) if p.exists() else [];old.append(r);p.write_text(json.dumps(old,indent=2)+'\n');print(json.dumps(r))
