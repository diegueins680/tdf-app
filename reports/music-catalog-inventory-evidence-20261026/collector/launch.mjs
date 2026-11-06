// PRIVATE PREPARATION. Do not execute before root reviews the manifest digest.
import {readFileSync,writeFileSync} from 'node:fs';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
const folder=path.dirname(fileURLToPath(import.meta.url));
const sha=b=>createHash('sha256').update(b).digest('hex');
if(process.argv.length!==8 || process.argv[2]!=='--review-sha256' || process.argv[4]!=='--identity-file' || process.argv[6]!=='--output'){
  throw new Error('Usage: node launch.mjs --review-sha256 REVIEWED_MANIFEST_SHA --identity-file PRIVATE_KEY --output NEW_PRIVATE_JSON');
}
const expected=process.argv[3];
const identity=path.resolve(process.argv[5]);
const output=path.resolve(process.argv[7]);
const manifestPath=path.join(folder,'review-manifest.json');
function verify(){
 const raw=readFileSync(manifestPath);
 if(!/^[a-f0-9]{64}$/.test(expected)||sha(raw)!==expected)throw new Error('Unreviewed manifest');
 const m=JSON.parse(raw);
 for(const [name,digest] of Object.entries(m.files)){
  if(!/^[A-Za-z0-9_.-]+$/.test(name)||sha(readFileSync(path.join(folder,name)))!==digest)throw new Error('Review source changed');
 }
 if(m.networkExecuted!==false||m.rootReviewRequired!==true)throw new Error('Invalid preparation boundary');
 return m;
}
try{
 const manifest=verify();
 const payload=readFileSync(path.join(folder,'remote-payload.py'));
 const startedAt=new Date().toISOString();
 // Exact canonical SSH host and host-key/identity flags; no shell command string.
 const raw=execFileSync('ssh',['-F','/dev/null','-o','BatchMode=yes','-o','IdentitiesOnly=yes',
  '-o','StrictHostKeyChecking=yes','-o','ConnectTimeout=10','-i',identity,
  'root@178.105.93.101','python3','-'],{input:payload,timeout:180000,maxBuffer:1024*1024,stdio:['pipe','pipe','pipe']});
 const snapshot=JSON.parse(raw);
 if(snapshot.schemaVersion!==1||snapshot.inventoryRole!=='tdf_catalog_inventory'||
   !['OBSERVED','PARTIAL_OR_UNOBSERVED'].includes(snapshot.status))throw new Error('Output boundary failed');
 verify();
 const receipt={startedAt,observedAt:new Date().toISOString(),reviewedManifestSha256:expected,
   sourceProvenance:manifest.sourceProvenance,remotePayloadSha256:manifest.files['remote-payload.py'],snapshot};
 writeFileSync(output,JSON.stringify(receipt,null,2)+'\n',{flag:'wx',mode:0o600});
 console.log(JSON.stringify({receipt:output,status:snapshot.status,observedFamilies:snapshot.aggregateFamiliesObserved,
  unobservedFamilies:snapshot.aggregateFamiliesUnobserved,productionWrites:false}));
}catch{
 console.error('Read-only aggregate collection or reviewed-source boundary failed; no raw runtime output printed.');
 process.exitCode=1;
}
