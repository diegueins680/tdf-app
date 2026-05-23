import fs from 'node:fs/promises';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';

const execFileAsync = promisify(execFile);

const SOURCE_FILE_PATTERN = /\.[jt]sx?$/;
const HASKELL_FILE_PATTERN = /\.hs$/;
const TEST_FILE_PATTERN = /(?:^|\/)(?:__tests__|__mocks__)(?:\/|$)|\.(?:test|spec)\.[jt]sx?$/;

const SKIPPED_SEGMENTS = [
  `${path.sep}node_modules${path.sep}`,
  `${path.sep}dist${path.sep}`,
  `${path.sep}build${path.sep}`,
  `${path.sep}coverage${path.sep}`,
  `${path.sep}.stack-work${path.sep}`,
  `${path.sep}.cabal${path.sep}`,
];

async function listTrackedFiles(repoRoot) {
  const { stdout } = await execFileAsync('git', ['ls-files'], {
    cwd: repoRoot,
    maxBuffer: 10 * 1024 * 1024,
  });
  return stdout
    .split('\n')
    .map((value) => value.trim())
    .filter(Boolean)
    .map((relativePath) => path.join(repoRoot, relativePath));
}

function shouldScanFile(filePath) {
  if (SKIPPED_SEGMENTS.some((segment) => filePath.includes(segment))) {
    return false;
  }
  return SOURCE_FILE_PATTERN.test(filePath) || HASKELL_FILE_PATTERN.test(filePath);
}

function lineNumberAt(source, index) {
  return source.slice(0, index).split('\n').length;
}

function compactSnippet(value, maxLen = 120) {
  const trimmed = value.replace(/\s+/g, ' ').trim();
  return trimmed.length > maxLen ? `${trimmed.slice(0, maxLen)}...` : trimmed;
}

function scoreImportance(severity, reach = 1, criticality = 1) {
  const severityRank = { critical: 100, error: 50, warning: 20, info: 5 };
  const base = severityRank[severity] ?? 5;
  return base * Math.max(1, reach) * Math.max(1, criticality);
}

// ─── JS/TS Formal Methods Audits ───

function auditJsTsFormal(source, filePath, isTest) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. Missing explicit invariant comments on stateful classes/objects
  const classPattern = /\bclass\s+\w+/g;
  for (const match of source.matchAll(classPattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 8).join('\n');
    if (!/\binvariant\b/i.test(surrounding) && !/@invariant/i.test(surrounding)) {
      findings.push({
        rule: 'missing-invariant-docs',
        severity: 'info',
        file: filePath,
        line,
        message: 'Stateful class lacks documented invariants; consider explicit contracts.',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('info', 1, 1),
      });
    }
  }

  // 2. Functions with many boolean parameters (boolean trap)
  const funcPattern = /(?:function\s*\w*|\(\w+\s*=>|\w+\s*[:=]\s*\(.*?)\)/g;
  for (const match of source.matchAll(funcPattern)) {
    const signature = match[0];
    const boolCount = (signature.match(/\b(boolean|Boolean)\b/g) ?? []).length;
    if (boolCount >= 3) {
      const line = lineNumberAt(source, match.index);
      findings.push({
        rule: 'boolean-trap',
        severity: 'warning',
        file: filePath,
        line,
        message: 'Function signature has multiple boolean parameters; prefer an options object or enum.',
        snippet: compactSnippet(signature),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 3. Missing precondition checks on exported functions
  const exportFuncPattern = /\bexport\s+(?:async\s+)?function\s+(\w+)\s*\(([^)]*)\)/g;
  for (const match of source.matchAll(exportFuncPattern)) {
    const line = lineNumberAt(source, match.index);
    const funcName = match[1];
    const params = match[2];
    const hasParams = params.trim().length > 0;
    const surrounding = lines.slice(Math.max(0, line - 1), line + 10).join('\n');
    const hasPreconditionCheck = /\b(throw|if\s*\(|assert|invariant|precondition)\b/.test(surrounding);
    const hasDocs = /\/\*\*|\s*\/\/.*(?:param|require|precondition)/i.test(surrounding);
    if (hasParams && !hasPreconditionCheck && !hasDocs && !isTest) {
      findings.push({
        rule: 'missing-precondition',
        severity: 'info',
        file: filePath,
        line,
        message: `Exported function "${funcName}" lacks explicit precondition checks or contract documentation.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('info', 1, 1),
      });
    }
  }

  // 4. State mutations without validation (only flag setState with object literals)
  const stateSetPattern = /\bsetState\s*\(\s*\{/g;
  for (const match of source.matchAll(stateSetPattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 2).join('\n');
    if (!/\b(if|validate|check|assert|invariant|guard|ensure)\b/.test(surrounding)) {
      findings.push({
        rule: 'unvalidated-state-mutation',
        severity: 'warning',
        file: filePath,
        line,
        message: 'State mutation lacks explicit validation; consider guarding against invalid transitions.',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 5. Missing exhaustiveness in switch-like patterns (object literal maps)
  const objMapPattern = /const\s+(\w+)\s*=\s*\{/g;
  for (const match of source.matchAll(objMapPattern)) {
    const mapName = match[1];
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 15).join('\n');
    if (/\bdefault\s*:/.test(surrounding)) continue;
    if (/\b(?:status|state|mode|type|kind|action)\b/i.test(mapName)) {
      findings.push({
        rule: 'missing-default-case',
        severity: 'warning',
        file: filePath,
        line,
        message: `Lookup map "${mapName}" may be missing a default/fallback case; add exhaustive handling.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 6. Magic numbers
  const magicNumberPattern = /[^\w.](\d{2,})(?![\w.])/g;
  const allowedNumbers = new Set(['0', '1', '2', '10', '100', '200', '404', '500', '1000']);
  for (const match of source.matchAll(magicNumberPattern)) {
    const num = match[1];
    if (allowedNumbers.has(num)) continue;
    const line = lineNumberAt(source, match.index);
    const snippet = lines[line - 1] ?? '';
    // Skip array indices, regex quantifiers, dates, hex
    if (/\[\d+\]/.test(snippet)) continue;
    if (/0x[0-9a-fA-F]+/.test(snippet)) continue;
    findings.push({
      rule: 'magic-number',
      severity: 'info',
      file: filePath,
      line,
      message: `Magic number "${num}" should be a named constant with semantic meaning.`,
      snippet: compactSnippet(snippet),
      importance: scoreImportance('info', 1, 0.5),
    });
  }

  // 7. Deeply nested conditionals (cyclomatic complexity signal)
  let maxDepth = 0;
  let maxDepthLine = 1;
  let currentDepth = 0;
  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (/\b(if|while|for|switch)\b/.test(trimmed) && /\{/.test(trimmed)) {
      currentDepth++;
      if (currentDepth > maxDepth) {
        maxDepth = currentDepth;
        maxDepthLine = i + 1;
      }
    }
    if (/\}/.test(trimmed) && !/\{/.test(trimmed)) {
      currentDepth = Math.max(0, currentDepth - 1);
    }
  }
  if (maxDepth >= 5) {
    findings.push({
      rule: 'deep-nesting',
      severity: 'warning',
      file: filePath,
      line: maxDepthLine,
      message: `Deeply nested control flow (depth ${maxDepth}); consider early returns or decomposition.`,
      snippet: compactSnippet(lines[maxDepthLine - 1] ?? ''),
      importance: scoreImportance('warning', 1, 1.5),
    });
  }

  // 8. Functions too long
  const funcStartPattern = /\b(?:function\s+\w+|\w+\s*[:=]\s*(?:async\s+)?function|\w+\s*[:=]\s*\(|\(\w+\)\s*=>)\s*\{/g;
  for (const match of source.matchAll(funcStartPattern)) {
    const startIdx = match.index;
    const startLine = lineNumberAt(source, startIdx);
    let braceDepth = 1;
    let j = startIdx + match[0].length;
    while (j < source.length && braceDepth > 0) {
      if (source[j] === '{') braceDepth++;
      if (source[j] === '}') braceDepth--;
      j++;
    }
    const endLine = lineNumberAt(source, j);
    const lineCount = endLine - startLine + 1;
    if (lineCount > 80) {
      findings.push({
        rule: 'long-function',
        severity: 'warning',
        file: filePath,
        line: startLine,
        message: `Function spans ${lineCount} lines; consider decomposition for testability and clarity.`,
        snippet: compactSnippet(lines[startLine - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1),
      });
    }
  }

  // 9. Missing error boundaries in React components
  const componentPattern = /\b(?:function|const)\s+(\w+)[^\{]*\{[^\}]*\breturn\s*\(/g;
  for (const match of source.matchAll(componentPattern)) {
    const compName = match[1];
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 15).join('\n');
    if (/\b(ErrorBoundary|componentDidCatch|getDerivedStateFromError)\b/.test(surrounding)) continue;
    if (/^[A-Z]/.test(compName)) {
      findings.push({
        rule: 'missing-error-boundary',
        severity: 'info',
        file: filePath,
        line,
        message: `React component "${compName}" has no error boundary; consider wrapping critical subtrees.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  // 10. Side-effecting functions without documentation
  const voidFuncPattern = /\b(?:function|const)\s+(\w+)\s*\([^)]*\)\s*(?::\s*void\b)?\s*\{/g;
  for (const match of source.matchAll(voidFuncPattern)) {
    const funcName = match[1];
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 8).join('\n');
    if (/\b(save|delete|update|create|mutate|set|post|put|patch|send|emit|dispatch)\b/i.test(funcName)) {
      if (!/\b(?:side.effect|mutates|writes|modifies)\b/i.test(surrounding) && !/\/\*\*/.test(surrounding)) {
        findings.push({
          rule: 'undocumented-side-effect',
          severity: 'info',
          file: filePath,
          line,
          message: `Side-effecting function "${funcName}" lacks documentation of its mutations.`,
          snippet: compactSnippet(lines[line - 1] ?? ''),
          importance: scoreImportance('info', 1, 0.5),
        });
      }
    }
  }

  return findings;
}

// ─── Haskell Formal Methods Audits ───

function auditHaskellFormal(source, filePath) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. Missing explicit module exports (hiding internal details)
  const modulePattern = /\bmodule\s+(\w+)/g;
  for (const match of source.matchAll(modulePattern)) {
    const line = lineNumberAt(source, match.index);
    const snippet = lines[line - 1] ?? '';
    if (!/\(/.test(snippet) || /\bwhere\b/.test(snippet)) {
      findings.push({
        rule: 'implicit-module-exports',
        severity: 'warning',
        file: filePath,
        line,
        message: 'Module uses implicit exports; prefer an explicit export list to enforce invariants.',
        snippet: compactSnippet(snippet),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 2. Missing QuickCheck / property tests for exported functions
  const exportedFuncPattern = /^\s*(\w+)\s*::/gm;
  const testFilePattern = /\bSpec\b|\bTest\b|\bProperties\b/;
  const hasTests = testFilePattern.test(filePath) || /\bprop_/.test(source);
  if (!hasTests) {
    let exportedCount = 0;
    for (const match of source.matchAll(exportedFuncPattern)) {
      exportedCount++;
    }
    if (exportedCount > 5) {
      findings.push({
        rule: 'missing-property-tests',
        severity: 'info',
        file: filePath,
        line: 1,
        message: 'Module has many exported functions but no visible property-based tests; consider QuickCheck invariants.',
        snippet: '',
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  // 3. IO without bracket / withResource
  const ioPattern = /\b(openFile|connect|acquire|lock|with)\b/gi;
  for (const match of source.matchAll(ioPattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 3).join('\n');
    if (!/\bbracket\b|\bwith\w+\b|\bfinally\b/.test(surrounding)) {
      findings.push({
        rule: 'unbracketed-resource',
        severity: 'warning',
        file: filePath,
        line,
        message: 'Resource acquisition may lack guaranteed cleanup; prefer bracket or with-pattern.',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  return findings;
}

// ─── Collect & Summarize ───

function compareFindings(left, right) {
  return (
    (right.importance ?? 0) - (left.importance ?? 0) ||
    left.file.localeCompare(right.file) ||
    left.line - right.line
  );
}

export function summarizeFormalFindings(findings) {
  return findings.reduce(
    (summary, finding) => {
      summary.total += 1;
      summary.totalImportance += finding.importance ?? 0;
      if (finding.severity === 'critical') summary.critical += 1;
      if (finding.severity === 'error') summary.errors += 1;
      if (finding.severity === 'warning') summary.warnings += 1;
      if (finding.severity === 'info') summary.info += 1;
      return summary;
    },
    { total: 0, critical: 0, errors: 0, warnings: 0, info: 0, totalImportance: 0 },
  );
}

export async function collectFormalFindings(repoRoot) {
  const files = await listTrackedFiles(repoRoot);
  const allFindings = [];

  for (const filePath of files) {
    if (!shouldScanFile(filePath)) continue;

    const stats = await fs.stat(filePath);
    if (stats.size > 500_000) continue;

    const source = await fs.readFile(filePath, 'utf8');
    const isTest = TEST_FILE_PATTERN.test(filePath);

    if (HASKELL_FILE_PATTERN.test(filePath)) {
      allFindings.push(...auditHaskellFormal(source, filePath));
    } else if (SOURCE_FILE_PATTERN.test(filePath)) {
      allFindings.push(...auditJsTsFormal(source, filePath, isTest));
    }
  }

  return allFindings.sort(compareFindings);
}
