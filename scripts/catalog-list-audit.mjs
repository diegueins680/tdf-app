#!/usr/bin/env node

import { createHash } from 'node:crypto';
import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync, readdirSync, realpathSync, statSync, writeFileSync } from 'node:fs';
import { extname, relative, resolve, sep } from 'node:path';
import process from 'node:process';
import ts from 'typescript';

const SOURCE_ROOTS = [
  'tdf-hq/app',
  'tdf-hq/.env.example',
  'tdf-hq/config',
  'tdf-hq/db',
  'tdf-hq/docs/openapi',
  'tdf-hq/sql',
  'tdf-hq/src',
  'tdf-hq/test',
  'tdf-hq-ui/src',
  'tdf-hq-ui/.env.example',
  'tdf-mobile/app',
  'tdf-mobile/src',
  'tdf-mobile/.env.example',
  'scripts',
  '.github/workflows',
];

const SCANNED_EXTENSIONS = new Set([
  '.cjs',
  '.env',
  '.hs',
  '.js',
  '.jsx',
  '.json',
  '.mjs',
  '.sql',
  '.ts',
  '.tsx',
  '.yaml',
  '.yml',
]);

const SKIPPED_SEGMENTS = new Set([
  '.git',
  '.stack-work',
  'build',
  'coverage',
  'dist',
  'node_modules',
]);

const DOMAIN_NAME_PATTERN = /(?:action|asset|attendance|booking|capabilit|categor|channel|city|cms|collection|content|countr|currenc|ddex|deal|event|feature|genre|grant|identifier|instrument|integration|language|locale|menu|method|module|navigation|option|package|payment|permission|plan|platform|pricing|provider|recording|refund|release|resource|role|room|schema|seed|service|session|slug|stage|state|status|subdivision|tag|territor|transition|type|unit|workflow)/i;
const TECHNICAL_NAME_PATTERN = /(?:algorithm|breakpoint|cors|error|header|http|mime|port|protocol|regex|route param|signal|token|transport)/i;
const GOVERNED_NAME_PATTERN = /(?:countr|currenc|ddex|external|identifier|iso|language|locale|payment|platform|provider|subdivision|territor)/i;
const SECURITY_NAME_PATTERN = /(?:action|capabilit|grant|module|permission|role|security)/i;

function parseArgs(argv) {
  const options = {
    root: process.cwd(),
    format: 'json',
    output: null,
    failOnUnreviewed: false,
    decisions: null,
  };

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--root') options.root = argv[++index];
    else if (arg === '--format') options.format = argv[++index];
    else if (arg === '--output') options.output = argv[++index];
    else if (arg === '--decisions') options.decisions = argv[++index];
    else if (arg === '--fail-on-unreviewed') options.failOnUnreviewed = true;
    else if (arg === '--help') {
      process.stdout.write(
        'Usage: node scripts/catalog-list-audit.mjs [--root PATH] [--format json|csv] ' +
          '[--output PATH] [--decisions PATH] [--fail-on-unreviewed]\n',
      );
      process.exit(0);
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }
  return options;
}

function walk(pathname) {
  if (!existsSync(pathname)) return [];
  const stat = statSync(pathname);
  if (stat.isFile()) return [pathname];
  return readdirSync(pathname, { withFileTypes: true }).flatMap((entry) => {
    if (SKIPPED_SEGMENTS.has(entry.name)) return [];
    return walk(resolve(pathname, entry.name));
  });
}

function repositoryTrackedFiles(root) {
  const rootResult = spawnSync('git', ['-C', root, 'rev-parse', '--show-toplevel'], {
    encoding: 'utf8',
  });
  if (
    rootResult.status !== 0 ||
    realpathSync(resolve(rootResult.stdout.trim())) !== realpathSync(root)
  ) {
    return null;
  }

  const filesResult = spawnSync(
    'git',
    ['-C', root, '-c', 'core.quotePath=false', 'ls-files', '--cached', '--recurse-submodules', '-z'],
    { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 },
  );
  if (filesResult.status !== 0) return null;
  return new Set(filesResult.stdout.split('\0').filter(Boolean));
}

function sourceKind(file) {
  if (/(?:^|\/)(?:test|tests|__tests__|fixtures)(?:\/|$)|\.(?:spec|test)\./i.test(file)) return 'test';
  if (/\/api\/generated\//.test(file)) return 'generated-client';
  if (/\/(?:sql|db\/migrations)\//.test(file)) return 'migration';
  if (/\/docs\/openapi\//.test(file)) return 'openapi';
  if (/\/(?:config|\.github\/workflows)\//.test(file)) return 'configuration';
  return 'production';
}

function surface(file) {
  if (file.startsWith('tdf-mobile/')) return 'mobile';
  if (file.startsWith('tdf-hq-ui/')) return 'web';
  if (file.startsWith('tdf-hq/')) return 'backend';
  return 'automation';
}

function domainFor(file, name) {
  const context = `${file} ${name}`.toLowerCase();
  if (/(?:auth|role|permission|module|capabilit|security)/.test(context)) return 'security-authorization';
  if (/(?:cms|records|release|recording|ddex|catalog)/.test(context)) return 'music-catalog-cms';
  if (/(?:locale|language|currency|countr|city|territor|international)/.test(context)) return 'international-reference-data';
  if (/(?:service|booking|room|session|pipeline)/.test(context)) return 'studio-services-booking';
  if (/(?:event|ticket|venue|logistics)/.test(context)) return 'events-ticketing';
  if (/(?:inventory|asset|stock|checkout|maintenance)/.test(context)) return 'inventory-operations';
  if (/(?:payment|invoice|receipt|stripe|paypal|datafast|refund)/.test(context)) return 'payments-finance';
  if (/(?:course|class|teacher|student|trial|academy)/.test(context)) return 'academy-courses';
  if (/(?:fan|social|reaction|instagram|facebook|whatsapp)/.test(context)) return 'fan-social';
  if (/(?:nav|menu|route|feature)/.test(context)) return 'product-navigation';
  if (/(?:campaign|promotion|marketing)/.test(context)) return 'campaigns-promotion';
  return 'cross-cutting-or-technical';
}

function recommendation(name, values) {
  const context = `${name} ${values.join(' ')}`;
  if (SECURITY_NAME_PATTERN.test(context)) return 'security-system-registry';
  if (GOVERNED_NAME_PATTERN.test(context)) return 'governed-reference-data';
  if (TECHNICAL_NAME_PATTERN.test(context)) return 'genuine-technical-constant';
  return 'dynamic-business-catalog';
}

function normalizeValue(value) {
  return String(value).replace(/\s+/g, ' ').trim().slice(0, 240);
}

function fingerprint(candidate) {
  return createHash('sha256')
    .update(
      [
        candidate.file,
        candidate.kind,
        candidate.name,
        ...candidate.values,
      ].join('\u0000'),
    )
    .digest('hex')
    .slice(0, 20);
}

function makeCandidate(file, kind, name, line, rawValues, metadata = {}) {
  const values = [...new Set(rawValues.map(normalizeValue).filter(Boolean))];
  const candidate = {
    id: '',
    file,
    sourceKind: sourceKind(file),
    surface: surface(file),
    domain: domainFor(file, name || ''),
    kind,
    name: name || '<anonymous>',
    line,
    values,
    valueCount: values.length,
    recommendedClassification: recommendation(name || '', values),
    ...metadata,
  };
  candidate.id = fingerprint(candidate);
  return candidate;
}

function canonicalValue(value) {
  return normalizeValue(value)
    .normalize('NFKD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLocaleLowerCase('en-US')
    .replace(/[^a-z0-9]+/g, ' ')
    .trim();
}

function valueSet(candidate) {
  return new Set(candidate.values.map(canonicalValue).filter(Boolean));
}

function jaccard(left, right) {
  if (left.size === 0 || right.size === 0) return 0;
  let intersection = 0;
  for (const value of left) if (right.has(value)) intersection += 1;
  return intersection / (left.size + right.size - intersection);
}

const COMMON_CONSUMER_VALUES = new Set([
  'active',
  'all',
  'cancelled',
  'completed',
  'default',
  'draft',
  'error',
  'failed',
  'false',
  'none',
  'pending',
  'published',
  'success',
  'true',
]);

function consumerNeedles(candidate) {
  const needles = [];
  if (
    candidate.name !== '<anonymous>' &&
    candidate.name !== 'JSX value attributes' &&
    candidate.name.length >= 4
  ) {
    needles.push(candidate.name);
  }
  for (const value of candidate.values) {
    const normalized = canonicalValue(value);
    if (
      value.length >= 5 &&
      value.length <= 80 &&
      !value.startsWith('{') &&
      !COMMON_CONSUMER_VALUES.has(normalized)
    ) {
      needles.push(`'${value}'`, `"${value}"`);
    }
    if (needles.length >= 12) break;
  }
  return [...new Set(needles)];
}

function attachConsumers(candidate, fileTexts) {
  const needles = consumerNeedles(candidate);
  const consumers = [...fileTexts.entries()]
    .filter(([file, text]) => file !== candidate.file && needles.some((needle) => text.includes(needle)))
    .map(([file]) => file);
  const bySurface = Object.fromEntries(
    ['backend', 'web', 'mobile', 'automation'].map((candidateSurface) => [
      candidateSurface,
      consumers.filter((file) => surface(file) === candidateSurface).slice(0, 50),
    ]),
  );
  return {
    consumerCount: consumers.length,
    backendConsumers: bySurface.backend,
    webConsumers: bySurface.web,
    mobileConsumers: bySurface.mobile,
    automationConsumers: bySurface.automation,
    apiAndIntegrationConsumers: consumers
      .filter((file) => /(?:\/api\/|openapi|generated|ddex|stripe|paypal|datafast|instagram|facebook|whatsapp)/i.test(file))
      .slice(0, 50),
  };
}

function lineOf(sourceFile, node) {
  return sourceFile.getLineAndCharacterOfPosition(node.getStart(sourceFile)).line + 1;
}

function literalText(node) {
  if (ts.isStringLiteralLike(node) || ts.isNumericLiteral(node)) return node.text;
  if (node.kind === ts.SyntaxKind.TrueKeyword) return 'true';
  if (node.kind === ts.SyntaxKind.FalseKeyword) return 'false';
  if (node.kind === ts.SyntaxKind.NullKeyword) return 'null';
  if (ts.isPrefixUnaryExpression(node) && ts.isNumericLiteral(node.operand)) {
    return `${node.operator === ts.SyntaxKind.MinusToken ? '-' : ''}${node.operand.text}`;
  }
  return null;
}

function propertyName(node) {
  if (!node) return null;
  if (ts.isIdentifier(node) || ts.isStringLiteralLike(node) || ts.isNumericLiteral(node)) return node.text;
  return null;
}

function declaredName(node) {
  let current = node.parent;
  while (current) {
    if (ts.isVariableDeclaration(current) && ts.isIdentifier(current.name)) return current.name.text;
    if (ts.isPropertyAssignment(current)) return propertyName(current.name) ?? '<property>';
    if (ts.isTypeAliasDeclaration(current) || ts.isEnumDeclaration(current)) return current.name.text;
    if (ts.isCallExpression(current)) {
      const callText = current.expression.getText();
      if (/^(?:z\.)?enum$/.test(callText)) return callText;
    }
    if (ts.isSourceFile(current) || ts.isFunctionLike(current)) break;
    current = current.parent;
  }
  return '<anonymous>';
}

function objectSummary(node) {
  if (!ts.isObjectLiteralExpression(node)) return null;
  const pairs = [];
  for (const property of node.properties) {
    if (!ts.isPropertyAssignment(property)) continue;
    const key = propertyName(property.name);
    if (!key) continue;
    const value = literalText(property.initializer);
    pairs.push(value == null ? key : `${key}=${value}`);
  }
  return pairs.length > 0 ? `{${pairs.join(', ')}}` : null;
}

function arrayValues(node) {
  const values = [];
  for (const element of node.elements) {
    const literal = literalText(element);
    if (literal != null) values.push(literal);
    else {
      const summary = objectSummary(element);
      if (summary) values.push(summary);
    }
  }
  return values;
}

function scanTypeScript(file, _absolutePath, text) {
  const scriptKind = /\.tsx?$/.test(file) ? ts.ScriptKind.TSX : ts.ScriptKind.JSX;
  const sourceFile = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true, scriptKind);
  const candidates = [];

  function visit(node) {
    if (ts.isArrayLiteralExpression(node)) {
      const name = declaredName(node);
      const values = arrayValues(node);
      const hasDomainName = name !== '<anonymous>' && DOMAIN_NAME_PATTERN.test(name);
      const hasOptionObjectShape = values.some(
        (value) => /^\{/.test(value) && /(?:^|[, {])(?:code|id|label|name|slug|status|value)=/i.test(value),
      );
      const isProductSource = sourceKind(file) === 'production';
      if (values.length >= 2 && (hasDomainName || (isProductSource && hasOptionObjectShape))) {
        candidates.push(makeCandidate(file, 'array', name, lineOf(sourceFile, node), values));
      }
    }

    if (ts.isEnumDeclaration(node) && node.members.length >= 2) {
      const values = node.members.map((member) => {
        const key = propertyName(member.name) ?? member.name.getText(sourceFile);
        const value = member.initializer ? literalText(member.initializer) : null;
        return value == null ? key : `${key}=${value}`;
      });
      candidates.push(makeCandidate(file, 'typescript-enum', node.name.text, lineOf(sourceFile, node), values));
    }

    if (ts.isTypeAliasDeclaration(node) && ts.isUnionTypeNode(node.type)) {
      const values = node.type.types
        .filter(ts.isLiteralTypeNode)
        .map((type) => literalText(type.literal))
        .filter((value) => value != null);
      if (values.length >= 2) {
        candidates.push(makeCandidate(file, 'literal-union', node.name.text, lineOf(sourceFile, node), values));
      }
    }

    if (
      ts.isVariableDeclaration(node) &&
      ts.isIdentifier(node.name) &&
      node.initializer &&
      ts.isObjectLiteralExpression(node.initializer)
    ) {
      const name = node.name.text;
      const values = node.initializer.properties
        .map((property) => (ts.isPropertyAssignment(property) ? propertyName(property.name) : null))
        .filter((value) => value != null);
      if (values.length >= 2 && DOMAIN_NAME_PATTERN.test(name)) {
        candidates.push(makeCandidate(file, 'object-registry', name, lineOf(sourceFile, node), values));
      }
    }

    if (ts.isSwitchStatement(node)) {
      const values = node.caseBlock.clauses
        .filter(ts.isCaseClause)
        .map((clause) => literalText(clause.expression))
        .filter((value) => value != null);
      if (values.length >= 2) {
        candidates.push(
          makeCandidate(
            file,
            'switch-cases',
            node.expression.getText(sourceFile).slice(0, 100),
            lineOf(sourceFile, node),
            values,
          ),
        );
      }
    }

    ts.forEachChild(node, visit);
  }
  visit(sourceFile);

  const jsxValues = [];
  function visitJsx(node) {
    if (ts.isJsxAttribute(node) && node.name.text === 'value') {
      let value = null;
      if (node.initializer && ts.isStringLiteral(node.initializer)) value = node.initializer.text;
      if (
        node.initializer &&
        ts.isJsxExpression(node.initializer) &&
        node.initializer.expression
      ) {
        value = literalText(node.initializer.expression);
      }
      if (value != null) jsxValues.push({ line: lineOf(sourceFile, node), value });
    }
    ts.forEachChild(node, visitJsx);
  }
  visitJsx(sourceFile);
  const uniqueJsxValues = [...new Set(jsxValues.map(({ value }) => value))];
  if (uniqueJsxValues.length >= 2) {
    candidates.push(
      makeCandidate(
        file,
        'jsx-option-values',
        'JSX value attributes',
        jsxValues[0].line,
        uniqueJsxValues,
      ),
    );
  }

  return candidates;
}

function splitHaskellConstructors(body) {
  return body
    .replace(/--[^\n]*/g, '')
    .split('|')
    .map((value) => value.trim().match(/^([A-Z][A-Za-z0-9_']*)/)?.[1])
    .filter(Boolean);
}

function lineAt(text, offset) {
  return text.slice(0, offset).split('\n').length;
}

function quotedValues(body) {
  return [...body.matchAll(/"([^"\\]*(?:\\.[^"\\]*)*)"/g)].map((match) => match[1]);
}

function scanHaskell(file, text) {
  const candidates = [];
  const dataPattern = /^data\s+([A-Z][A-Za-z0-9_']*)[^=\n]*=([\s\S]*?)(?=\n(?:derivePersistField|instance\s|data\s|newtype\s|type\s|[A-Za-z][A-Za-z0-9_']*\s*::|share\s|$))/gm;
  for (const match of text.matchAll(dataPattern)) {
    const values = splitHaskellConstructors(match[2]);
    if (values.length >= 2) {
      candidates.push(makeCandidate(file, 'haskell-sum-type', match[1], lineAt(text, match.index), values));
    }
  }

  const listPattern = /^([a-z][A-Za-z0-9_']*)[^\n=]*=\s*(?:Set\.)?fromList\s*\[([\s\S]{0,5000}?)\]|^([a-z][A-Za-z0-9_']*)[^\n=]*=\s*\[([\s\S]{0,5000}?)\]/gm;
  for (const match of text.matchAll(listPattern)) {
    const name = match[1] ?? match[3];
    const body = match[2] ?? match[4] ?? '';
    const strings = quotedValues(body);
    const constructors = body
      .split(',')
      .map((value) => value.trim().match(/^([A-Z][A-Za-z0-9_']*)$/)?.[1])
      .filter(Boolean);
    const values = strings.length >= 2 ? strings : constructors;
    if (values.length >= 2 && DOMAIN_NAME_PATTERN.test(name)) {
      candidates.push(makeCandidate(file, 'haskell-list', name, lineAt(text, match.index), values));
    }
  }
  return candidates;
}

function scanSql(file, text) {
  const candidates = [];
  const checkPattern = /CHECK\s*\([^)]*?\bIN\s*\(([^)]*)\)[^)]*\)/gims;
  for (const match of text.matchAll(checkPattern)) {
    const values = quotedValues(match[1].replace(/'/g, '"'));
    if (values.length >= 2) {
      candidates.push(makeCandidate(file, 'sql-check-values', 'CHECK IN', lineAt(text, match.index), values));
    }
  }

  const enumPattern = /CREATE\s+TYPE\s+([\w."]+)\s+AS\s+ENUM\s*\(([^)]*)\)/gims;
  for (const match of text.matchAll(enumPattern)) {
    const values = quotedValues(match[2].replace(/'/g, '"'));
    if (values.length >= 2) {
      candidates.push(makeCandidate(file, 'sql-enum', match[1], lineAt(text, match.index), values));
    }
  }
  return candidates;
}

function scanYaml(file, text) {
  const candidates = [];
  const lines = text.split('\n');
  for (let index = 0; index < lines.length; index += 1) {
    const match = lines[index].match(/^(\s*)enum:\s*(?:\[(.*)\])?\s*$/);
    if (!match) continue;
    const indent = match[1].length;
    const values = [];
    if (match[2]) {
      values.push(...match[2].split(',').map((value) => value.trim().replace(/^['"]|['"]$/g, '')));
    } else {
      for (let cursor = index + 1; cursor < lines.length; cursor += 1) {
        const valueMatch = lines[cursor].match(/^(\s*)-\s+(.+?)\s*$/);
        if (!valueMatch || valueMatch[1].length <= indent) break;
        values.push(valueMatch[2].replace(/^['"]|['"]$/g, ''));
      }
    }
    if (values.length >= 2) {
      const nearbyName = lines
        .slice(Math.max(0, index - 4), index)
        .reverse()
        .map((line) => line.match(/^\s*([A-Za-z0-9_.-]+):\s*$/)?.[1])
        .find(Boolean);
      candidates.push(makeCandidate(file, 'openapi-enum', nearbyName ?? 'enum', index + 1, values));
    }
  }
  return candidates;
}

function scanEnvironment(file, text) {
  const candidates = [];
  for (const match of text.matchAll(/^([A-Z][A-Z0-9_]*(?:SUPPORTED|ALLOWED|ENABLED|CURRENC|LOCALE|COUNTR|LANGUAGE)[A-Z0-9_]*)=(.+)$/gm)) {
    const values = match[2]
      .replace(/^['"]|['"]$/g, '')
      .split(',')
      .map((value) => value.trim())
      .filter(Boolean);
    if (values.length >= 2) {
      candidates.push(makeCandidate(file, 'environment-list', match[1], lineAt(text, match.index), values));
    }
  }
  return candidates;
}

function scanJson(file, text) {
  let document;
  try {
    document = JSON.parse(text);
  } catch {
    return [];
  }

  const candidates = [];
  const lineForKey = (key) => {
    const matchIndex = text.indexOf(`"${key}"`);
    return matchIndex < 0 ? 1 : lineAt(text, matchIndex);
  };
  const renderJsonValue = (value) => {
    if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
      return String(value);
    }
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      const fields = ['code', 'id', 'label', 'name', 'slug', 'status', 'value']
        .filter((key) => Object.hasOwn(value, key))
        .map((key) => `${key}=${String(value[key])}`);
      return fields.length > 0 ? `{${fields.join(',')}}` : null;
    }
    return null;
  };

  const visit = (value, path = []) => {
    const name = path.join('.') || '<root>';
    if (Array.isArray(value)) {
      const values = value.map(renderJsonValue).filter((entry) => entry != null);
      const hasOptionShape = values.some((entry) => entry.startsWith('{'));
      if (values.length >= 2 && (DOMAIN_NAME_PATTERN.test(name) || hasOptionShape)) {
        candidates.push(
          makeCandidate(file, 'json-array', name, lineForKey(path.at(-1) ?? ''), values),
        );
      }
      value.forEach((entry, index) => visit(entry, [...path, String(index)]));
      return;
    }
    if (!value || typeof value !== 'object') return;

    const keys = Object.keys(value);
    if (keys.length >= 2 && DOMAIN_NAME_PATTERN.test(name)) {
      candidates.push(
        makeCandidate(file, 'json-object-registry', name, lineForKey(path.at(-1) ?? ''), keys),
      );
    }
    for (const [key, entry] of Object.entries(value)) visit(entry, [...path, key]);
  };

  visit(document);
  return candidates;
}

function scanFile(root, absolutePath) {
  const file = relative(root, absolutePath).split(sep).join('/');
  const extension = extname(file).toLowerCase();
  if (!SCANNED_EXTENSIONS.has(extension) && !/\.env(?:\.|$)/.test(file)) return [];
  const text = readFileSync(absolutePath, 'utf8');
  if (['.ts', '.tsx', '.js', '.jsx', '.mjs', '.cjs'].includes(extension)) {
    return scanTypeScript(file, absolutePath, text);
  }
  if (extension === '.hs') return scanHaskell(file, text);
  if (extension === '.json') return scanJson(file, text);
  if (extension === '.sql') return scanSql(file, text);
  if (extension === '.yaml' || extension === '.yml') return scanYaml(file, text);
  return scanEnvironment(file, text);
}

function csvCell(value) {
  const text = Array.isArray(value) ? value.join(' | ') : String(value ?? '');
  return `"${text.replaceAll('"', '""')}"`;
}

function asCsv(candidates) {
  const fields = [
    'id',
    'file',
    'line',
    'sourceKind',
    'surface',
    'domain',
    'kind',
    'name',
    'valueCount',
    'values',
    'recommendedClassification',
    'consumerCount',
    'backendConsumers',
    'webConsumers',
    'mobileConsumers',
    'apiAndIntegrationConsumers',
    'exactDuplicateIds',
    'similarCandidateIds',
    'decision',
    'disposition',
    'specializedModel',
    'priority',
    'risk',
    'justification',
  ];
  return [
    fields.join(','),
    ...candidates.map((candidate) => fields.map((field) => csvCell(candidate[field])).join(',')),
  ].join('\n') + '\n';
}

function loadDecisions(pathname, root) {
  if (!pathname) return new Map();
  const absolutePath = resolve(root, pathname);
  if (!existsSync(absolutePath)) throw new Error(`Decision file not found: ${absolutePath}`);
  const parsed = JSON.parse(readFileSync(absolutePath, 'utf8'));
  const decisions = Array.isArray(parsed) ? parsed : parsed.decisions;
  if (!Array.isArray(decisions)) throw new Error('Decision file must contain an array or { decisions: [] }.');
  const allowedClassifications = new Set([
    'dynamic-business-catalog',
    'governed-reference-data',
    'security-system-registry',
    'genuine-technical-constant',
  ]);
  const result = new Map();
  for (const [index, decision] of decisions.entries()) {
    if (!decision || typeof decision !== 'object') {
      throw new Error(`Decision ${index} must be an object.`);
    }
    if (typeof decision.id !== 'string' || decision.id.length === 0) {
      throw new Error(`Decision ${index} is missing a stable candidate id.`);
    }
    if (result.has(decision.id)) throw new Error(`Duplicate decision id: ${decision.id}`);
    if (!allowedClassifications.has(decision.classification)) {
      throw new Error(`Decision ${decision.id} has an invalid classification.`);
    }
    if (decision.reviewed !== true) {
      throw new Error(`Decision ${decision.id} must be explicitly marked reviewed.`);
    }
    if (typeof decision.disposition !== 'string' || decision.disposition.trim().length === 0) {
      throw new Error(`Decision ${decision.id} is missing a disposition.`);
    }
    if (typeof decision.specializedModel !== 'string' || decision.specializedModel.trim().length === 0) {
      throw new Error(`Decision ${decision.id} is missing a specialized model or allowlist target.`);
    }
    if (typeof decision.justification !== 'string' || decision.justification.trim().length < 24) {
      throw new Error(`Decision ${decision.id} is missing a substantive justification.`);
    }
    if (
      decision.classification === 'genuine-technical-constant' &&
      decision.specializedModel !== 'technical_constant_allowlist'
    ) {
      throw new Error(`Technical constant ${decision.id} is not on the explicit technical allowlist.`);
    }
    result.set(decision.id, decision);
  }
  return result;
}

function main() {
  const options = parseArgs(process.argv.slice(2));
  const root = resolve(options.root);
  const trackedFiles = repositoryTrackedFiles(root);
  const files = SOURCE_ROOTS
    .flatMap((sourceRoot) => walk(resolve(root, sourceRoot)))
    .filter((absolutePath) => {
      if (!trackedFiles) return true;
      const file = relative(root, absolutePath).split(sep).join('/');
      return trackedFiles.has(file);
    });
  const uniqueFiles = [...new Set(files)].sort();
  const fileTexts = new Map(
    uniqueFiles.map((absolutePath) => [
      relative(root, absolutePath).split(sep).join('/'),
      readFileSync(absolutePath, 'utf8'),
    ]),
  );
  const decisions = loadDecisions(options.decisions, root);
  const discoveredCandidates = uniqueFiles
    .filter((file) => !file.endsWith('/scripts/catalog-list-audit.mjs'))
    .flatMap((file) => scanFile(root, file));
  const candidatesById = new Map();
  for (const candidate of discoveredCandidates) {
    const existing = candidatesById.get(candidate.id);
    if (existing) {
      existing.lines = [...new Set([...existing.lines, candidate.line])].sort((left, right) => left - right);
      existing.line = existing.lines[0];
    } else {
      candidatesById.set(candidate.id, { ...candidate, lines: [candidate.line] });
    }
  }
  const candidates = [...candidatesById.values()]
    .map((candidate) => {
      const review = decisions.get(candidate.id);
      return {
        ...candidate,
        ...attachConsumers(candidate, fileTexts),
        decision: review?.classification ?? 'unreviewed',
        disposition: review?.disposition ?? '',
        specializedModel: review?.specializedModel ?? '',
        priority: review?.priority ?? '',
        risk: review?.risk ?? '',
        justification: review?.justification ?? '',
      };
    })
    .sort((left, right) =>
      left.file.localeCompare(right.file) || left.line - right.line || left.kind.localeCompare(right.kind),
    );

  const exactGroups = new Map();
  for (const candidate of candidates) {
    const signature = [...valueSet(candidate)].sort().join('\u0000');
    if (!signature) continue;
    const group = exactGroups.get(signature) ?? [];
    group.push(candidate.id);
    exactGroups.set(signature, group);
  }
  const sets = new Map(candidates.map((candidate) => [candidate.id, valueSet(candidate)]));
  for (const candidate of candidates) {
    const candidateSet = sets.get(candidate.id);
    const signature = [...candidateSet].sort().join('\u0000');
    candidate.exactDuplicateIds = (exactGroups.get(signature) ?? []).filter((id) => id !== candidate.id);
    candidate.similarCandidateIds = candidates
      .filter(
        (other) =>
          other.id !== candidate.id &&
          other.domain === candidate.domain &&
          (other.surface !== candidate.surface || other.file !== candidate.file) &&
          jaccard(candidateSet, sets.get(other.id)) >= 0.5,
      )
      .sort(
        (left, right) =>
          jaccard(candidateSet, sets.get(right.id)) - jaccard(candidateSet, sets.get(left.id)),
      )
      .slice(0, 20)
      .map(({ id }) => id);
  }

  const observedForms = new Map();
  for (const candidate of candidates) {
    for (const value of candidate.values) {
      if (value.startsWith('{')) continue;
      const canonical = canonicalValue(value);
      if (!canonical) continue;
      const forms = observedForms.get(canonical) ?? new Map();
      const ids = forms.get(value) ?? new Set();
      ids.add(candidate.id);
      forms.set(value, ids);
      observedForms.set(canonical, forms);
    }
  }
  const normalizedVariants = [...observedForms.entries()]
    .filter(([, forms]) => forms.size > 1)
    .map(([canonical, forms]) => ({
      canonical,
      forms: [...forms.entries()].map(([value, ids]) => ({ value, candidateIds: [...ids] })),
    }))
    .sort((left, right) => left.canonical.localeCompare(right.canonical));

  const byClassification = Object.fromEntries(
    [...new Set(candidates.map(({ recommendedClassification }) => recommendedClassification))]
      .sort()
      .map((classification) => [
        classification,
        candidates.filter(({ recommendedClassification }) => recommendedClassification === classification).length,
      ]),
  );
  const result = {
    schemaVersion: 1,
    generatedAt: new Date().toISOString(),
    repositoryRoot: '.',
    filesScanned: uniqueFiles.length,
    candidateCount: candidates.length,
    byClassification,
    exactDuplicateGroupCount: [...exactGroups.values()].filter((ids) => ids.length > 1).length,
    normalizedVariantGroupCount: normalizedVariants.length,
    normalizedVariants,
    candidates,
  };
  const rendered = options.format === 'csv' ? asCsv(candidates) : `${JSON.stringify(result, null, 2)}\n`;
  if (options.output) {
    const outputPath = resolve(root, options.output);
    writeFileSync(outputPath, rendered);
  } else {
    process.stdout.write(rendered);
  }

  if (options.failOnUnreviewed) {
    const unreviewed = candidates.filter(({ decision }) => decision === 'unreviewed');
    const staleDecisions = [...decisions.keys()].filter((id) => !candidates.some((candidate) => candidate.id === id));
    if (unreviewed.length > 0 || staleDecisions.length > 0) {
      process.stderr.write(
        `Catalog list audit failed: ${unreviewed.length} unreviewed candidate(s), ` +
          `${staleDecisions.length} stale decision(s).\n`,
      );
      if (unreviewed.length > 0) {
        process.stderr.write(`Unreviewed candidate ids: ${unreviewed.map(({ id }) => id).join(', ')}\n`);
      }
      if (staleDecisions.length > 0) {
        process.stderr.write(`Stale decision ids: ${staleDecisions.join(', ')}\n`);
      }
      process.exitCode = 1;
    }
  }
}

main();
