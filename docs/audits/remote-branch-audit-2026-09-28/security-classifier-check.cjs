const assert = require('node:assert/strict');
const fs = require('node:fs');
const old = require('./security-validation/node_modules/ip-address-vulnerable');
const fixed = require('./security-validation/node_modules/ip-address');
const rows = [];
for (const [value, method] of [['fe80:1::1', 'isLinkLocal'], ['febf:ffff::1', 'isLinkLocal'], ['64:ff9b:1:7f00:0:100::', 'isPrivate'], ['64:ff9b:1::7f00:1', 'isPrivate']]) {
 const before = new old.Address6(value)[method]();
 const after = new fixed.Address6(value)[method]();
 assert.equal(before, false); assert.equal(after, true);
 rows.push({ value, method, before, after });
}
for (const value of ['2001:4860:4860::8888', '2606:4700:4700::1111']) {
 const ip = new fixed.Address6(value); assert.equal(ip.isPrivate(), false); assert.equal(ip.isLinkLocal(), false);
 rows.push({ value, publicControl: 'pass' });
}
const report = { verified_at: new Date().toISOString(), previous: '10.3.1', patched: '10.7.2', result: 'PASS', cases: rows,
 sources: ['https://github.com/advisories/GHSA-2vr4-cq9g-pvrc','https://github.com/advisories/GHSA-rpw4-54j3-4h4q'] };
fs.writeFileSync(__dirname + '/security-classifier-check.json', JSON.stringify(report, null, 2) + '\n');
console.log('PASS: four vulnerable-before/fixed-after address cases and two public controls');
