import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {readFileSync,writeFileSync} from 'node:fs';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {resolve} from 'node:path';
import {mkdirSync} from 'node:fs';
const base='https://1ad9172b.tdf-app.pages.dev';
const source='0b46cc5fb4ac63884f32c8170c18dd75418ceb26';
const root=process.env.TDF_QUITO_SOURCE_ROOT || fileURLToPath(new URL('../../',import.meta.url));
const dir=process.env.TDF_QUITO_OUTPUT || resolve(root,'artifacts/quito-suggestions-preview');
mkdirSync(dir,{recursive:true});
const config=fileURLToPath(new URL('playwright.config.mjs',import.meta.url));
async function attest(){
 const response=await fetch(base+'/reservar');assert.equal(response.status,200);
 const html=await response.text();const match=html.match(/<script[^>]*type="module"[^>]*src="([^"]+)"/);assert.ok(match);
 const url=new URL(match[1],base);assert.equal(url.origin,base);
 const asset=await fetch(url);assert.equal(asset.status,200);const bytes=Buffer.from(await asset.arrayBuffer());assert.ok(bytes.toString().includes(source));
 return {observedAt:new Date().toISOString(),base,source,entry:url.pathname,sha256:createHash('sha256').update(bytes).digest('hex')};
}
const before=await attest();writeFileSync(dir+'/attestation-before.json',JSON.stringify(before,null,2)+'\n');
const result=spawnSync(process.execPath,[root+'/node_modules/@playwright/test/cli.js','test','--config='+config,'--max-failures=1'],{cwd:root,env:{...process.env,TDF_QUITO_SOURCE_ROOT:root,TDF_QUITO_OUTPUT:dir},stdio:'inherit'});
const after=await attest();assert.equal(after.entry,before.entry);assert.equal(after.sha256,before.sha256);writeFileSync(dir+'/attestation-after.json',JSON.stringify(after,null,2)+'\n');
assert.equal(result.status,0,'preview browser suite failed');const report=JSON.parse(readFileSync(dir+'/results.json','utf8'));assert.equal(report.stats.expected,4);assert.equal(report.stats.unexpected,0);assert.equal(report.stats.skipped,0);
writeFileSync(dir+'/verification.json',JSON.stringify({before,after,stats:report.stats,scope:'Actual preview documents/assets; every business/session API synthetic. External traffic aborted, no actual booking/payment. Not canonical production or physical devices.'},null,2)+'\n');console.log(JSON.stringify({passed:report.stats.expected,source,entry:before.entry,sha256:before.sha256}));
