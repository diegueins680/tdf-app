import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {writeFileSync,mkdirSync,readFileSync} from 'node:fs';
import {spawnSync} from 'node:child_process';
const base=process.argv[2];assert.match(base,/^https:\/\/[a-f0-9]+\.tdf-app\.pages\.dev$/);
const source='8bdafbeb059cf4a53ce1a07580479cd1957b882d';
const root='/private/tmp/tdf-studio-booking-catalog-worktree-20261026';
const dir='/private/tmp/tdf-studio-booking-catalog-20261026/hosted';mkdirSync(dir,{recursive:true});
async function attest(){const html=await fetch(base+'/reservar');assert.equal(html.status,200);const text=await html.text();const entry=text.match(/<script[^>]+src="([^"]+)"/)[1];const r=await fetch(new URL(entry,base));assert.equal(r.status,200);const body=Buffer.from(await r.arrayBuffer());assert.ok(body.toString().includes(source));return {observedAt:new Date().toISOString(),base,source,entry,sha256:createHash('sha256').update(body).digest('hex')};}
const before=await attest();writeFileSync(dir+'/before.json',JSON.stringify(before,null,2));
const config=`import base from '${root}/playwright.config.mjs';
export default {...base,testDir:'${root}/e2e/web',projects:base.projects.filter(p=>['chromium-desktop','chromium-phone'].includes(p.name)),webServer:undefined,outputDir:'${dir}/test-results',reporter:[['line'],['json',{outputFile:'${dir}/results.json'}]],use:{...base.use,baseURL:${JSON.stringify(base)},serviceWorkers:'block'}};
`;
writeFileSync(dir+'/playwright.config.mjs',config);
const result=spawnSync(process.execPath,[root+'/node_modules/@playwright/test/cli.js','test','--config='+dir+'/playwright.config.mjs','booking-catalog-recovery.spec.mjs'],{cwd:root,env:{...process.env,PATH:'/private/tmp/tdf-node22-20261026/node-v22.14.0-darwin-x64/bin:'+process.env.PATH},stdio:'inherit'});
const after=await attest();assert.equal(after.sha256,before.sha256);
const report={before,after,exitCode:result.status,scope:'Actual immutable Cloudflare preview artifact; all business/session APIs synthetic, all external traffic and business writes blocked. No production or persistence claim.'};
if(result.status===0){const tests=JSON.parse(readFileSync(dir+'/results.json'));assert.equal(tests.stats.expected,4);assert.equal(tests.stats.unexpected,0);report.stats=tests.stats;}
writeFileSync(dir+'/receipt.json',JSON.stringify(report,null,2));process.exitCode=result.status??1;
