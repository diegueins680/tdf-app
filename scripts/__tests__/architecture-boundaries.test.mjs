import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';
import ts from 'typescript';

import {
  checkLayerRatchet,
  checkSizeRatchet,
  checkSystem,
  collectModules,
  findLayerViolations,
  haskellImports,
  layerResolver,
  typescriptRuntimeImports,
} from '../check-architecture-boundaries.mjs';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..');

function fixture(files) {
  const root = mkdtempSync(join(tmpdir(), 'tdf-architecture-'));
  for (const [path, body] of Object.entries(files)) {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), body);
  }
  return root;
}

function withFixture(files, run) {
  const root = fixture(files);
  try {
    return run(root);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
}

const haskellSystem = (overrides = {}) => ({
  language: 'haskell',
  sourceRoots: ['src'],
  layers: [
    { name: 'persistence', match: ['\\.Models$'] },
    { name: 'domain', default: true },
    { name: 'http-delivery', match: ['\\.Server$'] },
  ],
  knownViolations: [],
  ...overrides,
});

const webSystem = (overrides = {}) => ({
  language: 'typescript',
  sourceRoots: ['src'],
  exclude: ['\\.test\\.tsx?$'],
  layers: [
    { name: 'utils', match: ['^utils/'] },
    { name: 'api', match: ['^api/'] },
    { name: 'app-services', default: true },
    { name: 'pages', match: ['^pages/'] },
  ],
  knownViolations: [],
  ...overrides,
});

test('Haskell imports cover qualified, safe, package and source forms only at line start', () => {
  const source = [
    'module A where',
    'import TDF.Models',
    'import qualified TDF.DTO as DTO',
    'import           TDF.Config (loadConfig)',
    'import safe qualified "pkg" TDF.Auth as Auth',
    'import {-# SOURCE #-} TDF.Cycle',
    '  import TDF.Indented',
    '-- import TDF.Commented',
    'x = "import TDF.InString"',
  ].join('\n');
  assert.deepEqual(haskellImports(source), ['TDF.Auth', 'TDF.Config', 'TDF.Cycle', 'TDF.DTO', 'TDF.Models']);
});

test('TypeScript runtime imports exclude erased type-only imports', () => {
  const source = [
    "import type { A } from './types';",
    "import { type B, type C } from './moreTypes';",
    "import { type D, run } from './mixed';",
    "import Default from './default';",
    "import * as everything from './namespace';",
    "import './sideEffect';",
    "export { helper } from './reexport';",
    "export type { Shape } from './typeReexport';",
    "const lazy = () => import('./lazy');",
    "const text = \"import x from './inString'\";",
  ].join('\n');
  assert.deepEqual(typescriptRuntimeImports(ts, 'module.ts', source), [
    './default', './lazy', './mixed', './namespace', './reexport', './sideEffect',
  ]);
});

test('a layer needs exactly one default and unique names', () => {
  assert.throws(() => layerResolver({ layers: [{ name: 'a', match: ['^a'] }] }), /default/);
  assert.throws(() => layerResolver({ layers: [{ name: 'a', default: true }, { name: 'a', match: ['^b'] }] }), /unique/);
});

test('a lower Haskell layer importing a higher one is a violation; the reverse is not', () => {
  withFixture({
    'src/TDF/Models.hs': 'module TDF.Models where\n',
    'src/TDF/Worker.hs': 'module TDF.Worker where\nimport TDF.Models\nimport TDF.Trials.Server\nimport Data.Text (Text)\n',
    'src/TDF/Trials/Server.hs': 'module TDF.Trials.Server where\nimport TDF.Worker\nimport TDF.Models\n',
  }, (root) => {
    const system = haskellSystem();
    const modules = collectModules(root, system, null);
    assert.deepEqual(findLayerViolations(system, modules), [
      { edge: 'TDF.Worker -> TDF.Trials.Server', detail: 'domain must not import http-delivery' },
    ]);
  });
});

test('the ratchet rejects new violations and resolved ones that are still listed', () => {
  withFixture({
    'src/TDF/Worker.hs': 'import TDF.Trials.Server\n',
    'src/TDF/Trials/Server.hs': 'module TDF.Trials.Server where\n',
  }, (root) => {
    const unlisted = haskellSystem();
    assert.match(checkLayerRatchet(unlisted, collectModules(root, unlisted, null)).problems.join('\n'),
      /New layer violation: TDF\.Worker -> TDF\.Trials\.Server/);

    const listed = haskellSystem({ knownViolations: ['TDF.Worker -> TDF.Trials.Server'] });
    assert.deepEqual(checkLayerRatchet(listed, collectModules(root, listed, null)).problems, []);

    const stale = haskellSystem({ knownViolations: ['TDF.Gone -> TDF.Trials.Server', 'TDF.Worker -> TDF.Trials.Server'] });
    assert.match(checkLayerRatchet(stale, collectModules(root, stale, null)).problems.join('\n'),
      /Resolved violation is still listed.*TDF\.Gone -> TDF\.Trials\.Server/);

    const unsorted = haskellSystem({ knownViolations: ['TDF.Worker -> TDF.Trials.Server', 'TDF.A -> TDF.B'] });
    assert.match(checkLayerRatchet(unsorted, collectModules(root, unsorted, null)).problems.join('\n'), /must be sorted/);
  });
});

test('TypeScript layering resolves extensions and index files, and ignores tests and type imports', () => {
  withFixture({
    'src/utils/money.ts': "import { post } from '../api/client';\nimport type { Order } from '../api/orders';\nexport const x = post;\n",
    'src/utils/money.test.ts': "import '../pages/Home';\n",
    'src/api/client.ts': "import { logout } from '../session';\nexport const post = logout;\n",
    'src/api/orders.ts': 'export interface Order { id: number }\n',
    'src/session/index.tsx': "import { post } from '../api/client';\nexport const logout = post;\n",
    'src/pages/Home.tsx': "import { x } from '../utils/money';\nimport React from 'react';\nexport default x;\n",
  }, (root) => {
    const system = webSystem();
    const modules = collectModules(root, system, ts);
    assert.equal(modules.has('utils/money.test.ts'), false);
    assert.deepEqual(findLayerViolations(system, modules).map((violation) => violation.edge), [
      'api/client.ts -> session/index.tsx',
      'utils/money.ts -> api/client.ts',
    ]);
  });
});

test('size ratchet: unlisted oversized files, growth, stale and slack entries all fail', () => {
  const body = (lines) => 'x\n'.repeat(lines);
  withFixture({
    'src/TDF/Small.hs': body(10),
    'src/TDF/Big.hs': body(60),
    'src/TDF/Huge.hs': body(200),
  }, (root) => {
    const run = (oversized) => {
      const system = haskellSystem({ sizeBudget: { maxLines: 50, headroom: 20, oversized } });
      return checkSizeRatchet(system, collectModules(root, system, null)).problems.join('\n');
    };
    assert.equal(run({ 'src/TDF/Big.hs': 70, 'src/TDF/Huge.hs': 200 }), '');
    assert.match(run({ 'src/TDF/Huge.hs': 200 }), /Big\.hs has 60 lines; the limit for a module is 50/);
    assert.match(run({ 'src/TDF/Big.hs': 59, 'src/TDF/Huge.hs': 200 }), /Big\.hs grew to 60 lines; its recorded ceiling is 59/);
    assert.match(run({ 'src/TDF/Big.hs': 70, 'src/TDF/Huge.hs': 300 }), /Huge\.hs shrank to 200 lines; lower its ceiling from 300 to at most 220/);
    assert.match(run({ 'src/TDF/Big.hs': 70, 'src/TDF/Huge.hs': 200, 'src/TDF/Small.hs': 50 }), /Small\.hs is within the 50-line limit/);
    assert.match(run({ 'src/TDF/Big.hs': 70, 'src/TDF/Huge.hs': 200, 'src/TDF/Deleted.hs': 90 }), /no longer exists: src\/TDF\/Deleted\.hs/);
  });
});

test('an empty system is a configuration error, not a pass', () => {
  withFixture({ 'src/readme.txt': '' }, (root) => {
    assert.throws(() => checkSystem(root, 'backend', haskellSystem(), null), /no source modules/);
  });
});

test('the committed boundaries hold for this repository', () => {
  const config = JSON.parse(readFileSync(join(REPO_ROOT, 'docs/architecture/module-boundaries.json'), 'utf8'));
  assert.deepEqual(Object.keys(config.systems).sort(), ['backend', 'web']);
  for (const [name, system] of Object.entries(config.systems)) {
    const result = checkSystem(REPO_ROOT, name, system, ts);
    assert.deepEqual(result.problems, [], `${name} boundaries`);
    assert.ok(result.moduleCount > 100, `${name} must scan the real source tree`);
  }
});

test('http delivery is only reachable from delivery and the composition root', () => {
  const config = JSON.parse(readFileSync(join(REPO_ROOT, 'docs/architecture/module-boundaries.json'), 'utf8'));
  const layerOf = layerResolver(config.systems.backend);
  assert.equal(layerOf('TDF.Server').name, 'http-delivery');
  assert.equal(layerOf('TDF.ServerAdmin').name, 'http-delivery');
  assert.equal(layerOf('TDF.Server.Catalog').name, 'http-delivery');
  assert.equal(layerOf('TDF.Trials.Server').name, 'http-delivery');
  assert.equal(layerOf('TDF.Services.EventDiscovery').name, 'domain');
  assert.equal(layerOf('TDF.Cron').name, 'domain');
  assert.equal(layerOf('TDF.App.Boot').name, 'composition');
  assert.equal(layerOf('Main').name, 'composition');
  assert.equal(layerOf('TDF.Models.SocialEventsModels').name, 'persistence');
  assert.equal(layerOf('TDF.API.Types').name, 'transfer-types');
  assert.equal(layerOf('TDF.API.Admin').name, 'http-contract');
});
