import fs from 'node:fs/promises';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import ts from 'typescript';
import { build } from 'esbuild';

const root = process.cwd();
const out = path.resolve(process.argv[2] ?? 'docs/ux-ui-audit/2026-09-17');
await fs.mkdir(out, { recursive: true });
const read = (file) => fs.readFile(path.join(root, file), 'utf8');
const sha = (cwd) => execFileSync('git', ['rev-parse', 'HEAD'], { cwd, encoding: 'utf8' }).trim();
const csv = (rows) => rows.map(row => row.map(value => `"${String(value ?? '').replaceAll('"', '""')}"`).join(',')).join('\n') + '\n';
const files = (cwd, args) => execFileSync('git', ['ls-files', ...args], { cwd, encoding: 'utf8' }).trim().split('\n').filter(Boolean);
const sourceRoutes = [];
for (const file of ['tdf-hq-ui/src/routes/publicRoutes.tsx', 'tdf-hq-ui/src/routes/protectedRoutes.tsx']) {
  const source = ts.createSourceFile(file, await read(file), ts.ScriptTarget.Latest, true, ts.ScriptKind.TSX);
  const walk = (node) => {
    if (ts.isJsxSelfClosingElement(node) || ts.isJsxOpeningElement(node)) {
      if (node.tagName.getText(source) === 'Route') {
        const attrs = node.attributes.properties;
        const attr = attrs.find(a => ts.isJsxAttribute(a) && a.name.getText(source) === 'path');
        sourceRoutes.push({ path: attr?.initializer?.getText(source) ?? '(pathless/index wrapper)', source: file, line: source.getLineAndCharacterOfPosition(node.getStart()).line + 1, declaration: node.getText(source) });
      }
    }
    ts.forEachChild(node, walk);
  };
  walk(source);
}
const bundle = await build({ entryPoints: ['tdf-hq-ui/src/features/featureRegistry.ts'], bundle: true, write: false, platform: 'node', format: 'esm', logLevel: 'silent' });
const registry = await import('data:text/javascript;base64,' + Buffer.from(bundle.outputFiles[0].text).toString('base64'));
const backendAuth = await read('tdf-hq/src/TDF/Auth.hs');
const roleModules = [...backendAuth.matchAll(/^modulesForRole\s+(\w+)\s*=\s*(.+)$/gm)].map(([,role, expression]) => ({role, modules: [...expression.matchAll(/Module(\w+)/g)].map(([,name]) => name.toLowerCase())}));
const sessions = [{ id: 'anonymous', authenticated: false, roles: [], modules: [] }, { id: 'authenticated-no-grants', authenticated: true, roles: [], modules: [] }];
for (let i=0;i<roleModules.length;i++) {
  const first = roleModules[i];
  sessions.push({id:first.role,authenticated:true,roles:[first.role],modules:first.modules});
  for(let j=i+1;j<roleModules.length;j++) {
    const second=roleModules[j];
    sessions.push({id:[first.role,second.role].join('+'),authenticated:true,roles:[first.role,second.role],modules:[...new Set([...first.modules,...second.modules])]});
  }
}
const decisions=sessions.map(session=>({session:session.id, roles:session.roles, modules:session.modules, actions:registry.featureRegistry.map(feature=>({feature:feature.id, decisions:Object.fromEntries([...new Set([feature.routeAction,...Object.keys(feature.permissions)])].map(action=>[action, registry.evaluateFeatureAccess(feature,session,action).state]))}))}));
const groups = new Map();
for (const row of decisions) {
  const signature = JSON.stringify(row.actions);
  if (!groups.has(signature)) groups.set(signature, { sessions: [], actions: row.actions });
  groups.get(signature).sessions.push({ id: row.session, roles: row.roles, modules: row.modules });
}
await fs.writeFile(path.join(out,'role-access.json'), JSON.stringify({warning:'Executable CLIENT policy with seed-derived module grants and flags off; live canonical database grants and record authorization must be verified separately. All single roles and pairs, not every larger role set.',roleModules,equivalentPolicyGroups:[...groups.values()]}));
const states='loading;empty;validation;denied;expired-session;cancel;slow;interrupted;retry;recovery;return;deep-link;partial-progress';
const rows=[['surface','route-or-file','source','access-definition','devices','languages','required-states','evidence-status']];
for(const feature of registry.featureRegistry) rows.push(['registered-feature',feature.webRoute??feature.id,'tdf-hq/assets/feature-registry.json',JSON.stringify({auth:feature.requiredAuth,permissions:feature.permissions,scope:feature.recordScope,flag:feature.featureFlag,mobile:feature.mobilePresentation}),'desktop;tablet;phone;native-where-mapped','es;en',states,'inventoried;runtime coverage pending unless evidence register names exact journey']);
for(const route of sourceRoutes) rows.push(['route-declaration',route.path,route.source+':'+route.line,route.declaration,'desktop;tablet;phone','es;en',states,'inventory;parent guard and nested path require manual review']);
for(const file of files(path.join(root,'tdf-mobile'),['app']).filter(f=>/\.(tsx|jsx)$/.test(f))) rows.push(['native-route',file,'tdf-mobile/'+file,'Expo Router;layout and API guards','iOS;Android','es;en',states,'inventory;native runtime pending']);
for(const file of files(root,['tdf-hq-ui/public','public']).filter(f=>/\.html$/.test(f))) rows.push(['static-page',file,file,'public;file-specific','desktop;tablet;phone','es;en',states,'inventory;runtime pending']);
await fs.writeFile(path.join(out,'coverage.csv'),csv(rows));
await fs.writeFile(path.join(out,'inventory.json'),JSON.stringify({rootSha:sha(root),mobileSha:sha(path.join(root,'tdf-mobile')),features:registry.featureRegistry.length,routeDeclarations:sourceRoutes.length,roleCount:roleModules.length,roleCombinations:sessions.length,surfaces:rows.length-1,sourceRoutes},null,2)+'\n');
console.log(JSON.stringify({features:registry.featureRegistry.length,routeDeclarations:sourceRoutes.length,roles:roleModules.length,roleCombinations:sessions.length,surfaces:rows.length-1}));
