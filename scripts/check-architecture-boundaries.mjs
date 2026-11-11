#!/usr/bin/env node
// Architecture fitness check (ADR 0133).
//
// Each system in docs/architecture/module-boundaries.json declares ordered layers,
// lowest first. A module may import its own layer and lower ones. Existing debt is
// listed explicitly and can only shrink: an unlisted violation fails, and so does a
// listed one that no longer exists. Oversized files follow the same ratchet.

import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs';
import { dirname, extname, join, posix, relative, resolve, sep } from 'node:path';
import process from 'node:process';
import { fileURLToPath, pathToFileURL } from 'node:url';

const DEFAULT_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const DEFAULT_CONFIG = 'docs/architecture/module-boundaries.json';

function toPosix(pathname) {
  return pathname.split(sep).join('/');
}

function walkFiles(directory) {
  if (!existsSync(directory)) return [];
  return readdirSync(directory, { withFileTypes: true })
    .sort((left, right) => (left.name < right.name ? -1 : left.name > right.name ? 1 : 0))
    .flatMap((entry) => {
      const pathname = join(directory, entry.name);
      if (entry.isDirectory()) return entry.name === 'node_modules' ? [] : walkFiles(pathname);
      return entry.isFile() ? [pathname] : [];
    });
}

function countLines(text) {
  if (text.length === 0) return 0;
  const newlines = text.split('\n').length - 1;
  return text.endsWith('\n') ? newlines : newlines + 1;
}

// ---------------------------------------------------------------------------
// Import extraction
// ---------------------------------------------------------------------------

const HASKELL_IMPORT = /^import\s+(?:\{-#\s*SOURCE\s*#-\}\s*)?(?:safe\s+)?(?:qualified\s+)?(?:"[^"\n]*"\s+)?([A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)/gm;

export function haskellImports(source) {
  return [...new Set([...source.matchAll(HASKELL_IMPORT)].map((match) => match[1]))].sort();
}

// `import type { A } from 'm'` is erased. `import { type A } from 'm'` is not:
// with verbatimModuleSyntax (set in tdf-hq-ui/tsconfig.json) the compiler keeps
// it as `import {} from 'm'`, so the module is still loaded and is still an edge.
function importClauseIsTypeOnly(clause) {
  return Boolean(clause && clause.isTypeOnly);
}

// Runtime module specifiers only: a whole-declaration type-only import or export
// is erased by the compiler and cannot create a load-order or bundle dependency.
export function typescriptRuntimeImports(ts, fileName, source) {
  const kind = fileName.endsWith('.tsx') ? ts.ScriptKind.TSX : ts.ScriptKind.TS;
  const file = ts.createSourceFile(fileName, source, ts.ScriptTarget.Latest, false, kind);
  const specifiers = new Set();
  const visit = (node) => {
    if (ts.isImportDeclaration(node)) {
      if (!importClauseIsTypeOnly(node.importClause) && ts.isStringLiteral(node.moduleSpecifier)) {
        specifiers.add(node.moduleSpecifier.text);
      }
      return;
    }
    if (ts.isExportDeclaration(node)) {
      if (node.moduleSpecifier && !node.isTypeOnly && ts.isStringLiteral(node.moduleSpecifier)) {
        specifiers.add(node.moduleSpecifier.text);
      }
      return;
    }
    if (ts.isCallExpression(node)
      && node.expression.kind === ts.SyntaxKind.ImportKeyword
      && node.arguments.length === 1
      && ts.isStringLiteralLike(node.arguments[0])) {
      specifiers.add(node.arguments[0].text);
    }
    ts.forEachChild(node, visit);
  };
  visit(file);
  return [...specifiers].sort();
}

// ---------------------------------------------------------------------------
// Module graphs
// ---------------------------------------------------------------------------

function compileAll(patterns) {
  return (patterns ?? []).map((pattern) => new RegExp(pattern));
}

function matchesAny(patterns, value) {
  return patterns.some((pattern) => pattern.test(value));
}

function collectHaskellModules(root, system) {
  const modules = new Map();
  for (const sourceRoot of system.sourceRoots) {
    const absoluteRoot = resolve(root, sourceRoot);
    for (const pathname of walkFiles(absoluteRoot)) {
      if (extname(pathname) !== '.hs') continue;
      const name = toPosix(relative(absoluteRoot, pathname)).slice(0, -'.hs'.length).split('/').join('.');
      const source = readFileSync(pathname, 'utf8');
      modules.set(name, {
        name,
        path: toPosix(relative(root, pathname)),
        lines: countLines(source),
        rawImports: haskellImports(source),
      });
    }
  }
  for (const module of modules.values()) {
    module.imports = module.rawImports.filter((name) => modules.has(name));
    delete module.rawImports;
  }
  return modules;
}

const TYPESCRIPT_SOURCE = /\.tsx?$/;

function resolveTypescriptTarget(known, fromName, specifier) {
  if (!specifier.startsWith('.')) return null;
  const base = posix.normalize(posix.join(posix.dirname(fromName), specifier));
  if (base.startsWith('..')) return null;
  const candidates = [base, `${base}.ts`, `${base}.tsx`, `${base}/index.ts`, `${base}/index.tsx`];
  return candidates.find((candidate) => known.has(candidate)) ?? null;
}

function collectTypescriptModules(root, system, ts) {
  const excluded = compileAll(system.exclude);
  const modules = new Map();
  const sources = new Map();
  for (const sourceRoot of system.sourceRoots) {
    const absoluteRoot = resolve(root, sourceRoot);
    for (const pathname of walkFiles(absoluteRoot)) {
      if (!TYPESCRIPT_SOURCE.test(pathname) || pathname.endsWith('.d.ts')) continue;
      const name = toPosix(relative(absoluteRoot, pathname));
      if (matchesAny(excluded, name)) continue;
      const source = readFileSync(pathname, 'utf8');
      sources.set(name, source);
      modules.set(name, { name, path: toPosix(relative(root, pathname)), lines: countLines(source) });
    }
  }
  const skipImports = compileAll(system.skipImportScan);
  for (const module of modules.values()) {
    const specifiers = matchesAny(skipImports, module.name)
      ? []
      : typescriptRuntimeImports(ts, module.name, sources.get(module.name));
    module.imports = [...new Set(specifiers
      .map((specifier) => resolveTypescriptTarget(modules, module.name, specifier))
      .filter((target) => target !== null && target !== module.name))].sort();
  }
  return modules;
}

export function collectModules(root, system, ts) {
  if (system.language === 'haskell') return collectHaskellModules(root, system);
  if (system.language === 'typescript') {
    if (!ts) throw new Error('The typescript package is required to scan a typescript system');
    return collectTypescriptModules(root, system, ts);
  }
  throw new Error(`Unsupported language: ${system.language}`);
}

// ---------------------------------------------------------------------------
// Rules
// ---------------------------------------------------------------------------

export function layerResolver(system) {
  const layers = system.layers.map((layer, rank) => ({
    name: layer.name,
    rank,
    patterns: compileAll(layer.match),
    isDefault: layer.default === true,
  }));
  const defaults = layers.filter((layer) => layer.isDefault);
  if (defaults.length !== 1) throw new Error('Exactly one layer must be marked as the default');
  if (new Set(layers.map((layer) => layer.name)).size !== layers.length) {
    throw new Error('Layer names must be unique');
  }
  return (moduleName) => layers.find((layer) => matchesAny(layer.patterns, moduleName)) ?? defaults[0];
}

export function findLayerViolations(system, modules) {
  const layerOf = layerResolver(system);
  const violations = [];
  for (const module of modules.values()) {
    const from = layerOf(module.name);
    for (const imported of module.imports) {
      const to = layerOf(imported);
      if (to.rank > from.rank) {
        violations.push({
          edge: `${module.name} -> ${imported}`,
          detail: `${from.name} must not import ${to.name}`,
        });
      }
    }
  }
  return violations.sort((left, right) => (left.edge < right.edge ? -1 : left.edge > right.edge ? 1 : 0));
}

export function checkLayerRatchet(system, modules) {
  const violations = findLayerViolations(system, modules);
  const known = system.knownViolations ?? [];
  const problems = [];
  if (new Set(known).size !== known.length) problems.push('knownViolations contains duplicate entries');
  if (known.join('\n') !== [...known].sort().join('\n')) problems.push('knownViolations must be sorted');
  const knownSet = new Set(known);
  const currentSet = new Set(violations.map((violation) => violation.edge));
  for (const violation of violations) {
    if (!knownSet.has(violation.edge)) {
      problems.push(`New layer violation: ${violation.edge} (${violation.detail})`);
    }
  }
  for (const edge of known) {
    if (!currentSet.has(edge)) {
      problems.push(`Resolved violation is still listed; delete it from knownViolations: ${edge}`);
    }
  }
  return { violations, problems };
}

export function checkSizeRatchet(system, modules) {
  const budget = system.sizeBudget;
  if (!budget) return { oversized: [], problems: [] };
  const exempt = compileAll(budget.exempt);
  const caps = budget.oversized ?? {};
  const problems = [];
  const oversized = [];
  const byPath = new Map([...modules.values()].map((module) => [module.path, module]));
  for (const module of byPath.values()) {
    if (matchesAny(exempt, module.name)) continue;
    const cap = caps[module.path];
    if (module.lines > budget.maxLines) oversized.push({ path: module.path, lines: module.lines });
    if (cap === undefined) {
      if (module.lines > budget.maxLines) {
        problems.push(`${module.path} has ${module.lines} lines; the limit for a module is ${budget.maxLines}. Split it instead of listing it.`);
      }
    } else if (module.lines > cap) {
      problems.push(`${module.path} grew to ${module.lines} lines; its recorded ceiling is ${cap}. Put new code in a focused module.`);
    } else if (module.lines <= budget.maxLines) {
      problems.push(`${module.path} is within the ${budget.maxLines}-line limit; delete its sizeBudget.oversized entry.`);
    } else if (cap - module.lines > budget.headroom) {
      problems.push(`${module.path} shrank to ${module.lines} lines; lower its ceiling from ${cap} to at most ${module.lines + budget.headroom}.`);
    }
  }
  for (const path of Object.keys(caps)) {
    if (!byPath.has(path)) problems.push(`sizeBudget.oversized lists a file that no longer exists: ${path}`);
  }
  oversized.sort((left, right) => right.lines - left.lines || (left.path < right.path ? -1 : 1));
  return { oversized, problems };
}

export function checkSystem(root, name, system, ts) {
  const modules = collectModules(root, system, ts);
  if (modules.size === 0) throw new Error(`System ${name} has no source modules under ${system.sourceRoots.join(', ')}`);
  const layers = checkLayerRatchet(system, modules);
  const sizes = checkSizeRatchet(system, modules);
  return {
    name,
    moduleCount: modules.size,
    violations: layers.violations,
    oversized: sizes.oversized,
    problems: [...layers.problems, ...sizes.problems],
  };
}

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  let root = DEFAULT_ROOT;
  let config = DEFAULT_CONFIG;
  let report = false;
  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--root') root = resolve(argv[++index]);
    else if (arg === '--config') config = argv[++index];
    else if (arg === '--report') report = true;
    else if (arg === '--help') {
      process.stdout.write('Usage: node scripts/check-architecture-boundaries.mjs [--root PATH] [--config PATH] [--report]\n');
      process.exit(0);
    } else throw new Error(`Unknown argument: ${arg}`);
  }
  return [root, config, report];
}

async function loadTypescript(config) {
  const needed = Object.values(config.systems).some((system) => system.language === 'typescript');
  if (!needed) return null;
  return (await import('typescript')).default;
}

async function main() {
  const [root, configPath, report] = parseArgs(process.argv.slice(2));
  const config = JSON.parse(readFileSync(resolve(root, configPath), 'utf8'));
  if (!statSync(root).isDirectory()) throw new Error(`Not a directory: ${root}`);
  const ts = await loadTypescript(config);
  const results = Object.entries(config.systems).map(([name, system]) => checkSystem(root, name, system, ts));
  if (report) {
    process.stdout.write(`${JSON.stringify(results, null, 2)}\n`);
    return;
  }
  let failed = false;
  for (const result of results) {
    process.stdout.write(`${result.name}: ${result.moduleCount} modules, ${result.violations.length} listed layer violations, ${result.oversized.length} oversized files\n`);
    for (const problem of result.problems) {
      failed = true;
      process.stderr.write(`  ✗ ${problem}\n`);
    }
  }
  if (failed) {
    process.stderr.write(`Architecture boundaries failed. See docs/adr/0133-enforced-module-boundaries.md.\n`);
    process.exit(1);
  }
  process.stdout.write('Architecture boundaries hold.\n');
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  main().catch((error) => {
    process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
    process.exit(1);
  });
}
