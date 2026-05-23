import fs from 'node:fs/promises';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';

const execFileAsync = promisify(execFile);

const SOURCE_FILE_PATTERN = /\.[jt]sx?$/;
const HASKELL_FILE_PATTERN = /\.hs$/;
const SQL_FILE_PATTERN = /\.sql$/;
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
  return (
    SOURCE_FILE_PATTERN.test(filePath) ||
    HASKELL_FILE_PATTERN.test(filePath) ||
    SQL_FILE_PATTERN.test(filePath)
  );
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

// ─── JS/TS Logical Audits ───

function auditJsTsLogical(source, filePath, isTest) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. Loose equality (== / !=) instead of strict
  const looseEqPattern = /(?<![=!])==(?!=)/g;
  for (const match of source.matchAll(looseEqPattern)) {
    const line = lineNumberAt(source, match.index);
    const snippet = lines[line - 1] ?? '';
    if (/\btypeof\b/.test(snippet)) continue; // typeof is safe with ==
    findings.push({
      rule: 'loose-equality',
      severity: 'warning',
      file: filePath,
      line,
      message: 'Loose equality (==) detected; prefer strict equality (===) for predictability.',
      snippet: compactSnippet(snippet),
      importance: scoreImportance('warning', 1, isTest ? 0.5 : 1.5),
    });
  }

  // 2. Assignment in condition
  const assignInCondPattern = /\b(if|while|for)\s*\([^)]*(?<![=!<>])=(?!=|>)[^)]*\)/g;
  for (const match of source.matchAll(assignInCondPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'assignment-in-condition',
      severity: 'error',
      file: filePath,
      line,
      message: 'Assignment inside a conditional expression is a common source of bugs.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 3. Empty catch block
  const emptyCatchPattern = /catch\s*\([^)]*\)\s*\{\s*\}/g;
  for (const match of source.matchAll(emptyCatchPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'empty-catch',
      severity: 'error',
      file: filePath,
      line,
      message: 'Empty catch block silently swallows errors; log or re-throw.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 4. JSON.parse without try/catch
  const jsonParsePattern = /(?<!\bcatch\b.*)\bJSON\.parse\s*\(/g;
  for (const match of source.matchAll(jsonParsePattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 3), line + 2).join('\n');
    if (/\btry\b/.test(surrounding) && /\bcatch\b/.test(surrounding)) continue;
    findings.push({
      rule: 'unprotected-json-parse',
      severity: 'error',
      file: filePath,
      line,
      message: 'JSON.parse without try/catch can crash on malformed input.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 5. debugger statement
  const debuggerPattern = /\bdebugger\b/g;
  for (const match of source.matchAll(debuggerPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'debugger-statement',
      severity: 'warning',
      file: filePath,
      line,
      message: 'debugger statement should not be present in production code.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('warning', 1, 1),
    });
  }

  // 6. console.log in non-test source
  if (!isTest) {
    const consolePattern = /\bconsole\.(log|warn|error|debug)\s*\(/g;
    for (const match of source.matchAll(consolePattern)) {
      const line = lineNumberAt(source, match.index);
      const snippet = lines[line - 1] ?? '';
      if (/\berror\b/.test(snippet) && /\bconsole\.error\b/.test(snippet)) {
        // Allow console.error for actual error reporting
        continue;
      }
      findings.push({
        rule: 'console-usage',
        severity: 'info',
        file: filePath,
        line,
        message: 'Console statement in production code; consider structured logging.',
        snippet: compactSnippet(snippet),
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  // 7. eval usage
  const evalPattern = /\beval\s*\(/g;
  for (const match of source.matchAll(evalPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'eval-usage',
      severity: 'critical',
      file: filePath,
      line,
      message: 'eval() is dangerous and should be avoided.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('critical', 1, 3),
    });
  }

  // 8. parseInt without radix
  const parseIntPattern = /\bparseInt\s*\(\s*[^,)]+\s*\)/g;
  for (const match of source.matchAll(parseIntPattern)) {
    const line = lineNumberAt(source, match.index);
    const snippet = lines[line - 1] ?? '';
    if (/parseInt\s*\([^,]+,\s*\d+\s*\)/.test(snippet)) continue;
    findings.push({
      rule: 'parseint-missing-radix',
      severity: 'warning',
      file: filePath,
      line,
      message: 'parseInt without explicit radix can produce surprising results.',
      snippet: compactSnippet(snippet),
      importance: scoreImportance('warning', 1, 1),
    });
  }

  // 9. Promise without catch or await
  const promisePattern = /\bnew\s+Promise\s*\(/g;
  for (const match of source.matchAll(promisePattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 2), line + 3).join('\n');
    if (/\.catch\b/.test(surrounding) || /\bawait\b/.test(surrounding)) continue;
    findings.push({
      rule: 'unhandled-promise',
      severity: 'error',
      file: filePath,
      line,
      message: 'Promise created without .catch() or await; unhandled rejections may crash.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 10. Missing break in switch
  const switchPattern = /switch\s*\([^)]*\)\s*\{/g;
  for (const match of source.matchAll(switchPattern)) {
    const startIndex = match.index;
    let braceDepth = 1;
    let i = source.indexOf('{', startIndex) + 1;
    const switchBody = [];
    while (i < source.length && braceDepth > 0) {
      if (source[i] === '{') braceDepth++;
      if (source[i] === '}') braceDepth--;
      if (braceDepth > 0) switchBody.push(source[i]);
      i++;
    }
    const body = switchBody.join('');
    const cases = body.split(/case\s+/);
    for (const c of cases) {
      if (!c.trim()) continue;
      const hasBreak = /\bbreak\b/.test(c);
      const hasReturn = /\breturn\b/.test(c);
      const hasThrow = /\bthrow\b/.test(c);
      if (!hasBreak && !hasReturn && !hasThrow) {
        // Find line number roughly
        const caseIndex = source.indexOf('case ' + c.slice(0, 20), startIndex);
        const line = caseIndex > 0 ? lineNumberAt(source, caseIndex) : lineNumberAt(source, startIndex);
        findings.push({
          rule: 'missing-switch-break',
          severity: 'warning',
          file: filePath,
          line,
          message: 'Switch case may be missing break, return, or throw; risk of fall-through bugs.',
          snippet: compactSnippet(c.split('\n')[0] ?? ''),
          importance: scoreImportance('warning', 1, 1.5),
        });
      }
    }
  }

  // 11. Async function without await
  const asyncFuncPattern = /\basync\s+function\b/g;
  for (const match of source.matchAll(asyncFuncPattern)) {
    const line = lineNumberAt(source, match.index);
    // Find the function body
    const bodyStart = source.indexOf('{', match.index);
    if (bodyStart < 0) continue;
    let depth = 1;
    let j = bodyStart + 1;
    const bodyChars = [];
    while (j < source.length && depth > 0) {
      if (source[j] === '{') depth++;
      if (source[j] === '}') depth--;
      if (depth > 0) bodyChars.push(source[j]);
      j++;
    }
    const funcBody = bodyChars.join('');
    if (!/\bawait\b/.test(funcBody) && !/\.then\b/.test(funcBody)) {
      findings.push({
        rule: 'async-without-await',
        severity: 'info',
        file: filePath,
        line,
        message: 'Async function contains no await or .then; consider whether async is necessary.',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  // 12. Potentially unreachable code after return/throw
  const returnPattern = /\breturn\b[^;]*;[ \t]*\n[ \t]*/g;
  for (const match of source.matchAll(returnPattern)) {
    const line = lineNumberAt(source, match.index);
    const nextLine = lines[line] ?? '';
    const trimmed = nextLine.trim();
    if (
      trimmed &&
      !/^[\}\]\);,]*$/.test(trimmed) &&
      !/^(\/\/|\/\*|\*|#)/.test(trimmed) &&
      !/^else\b/.test(trimmed) &&
      !/^catch\b/.test(trimmed) &&
      !/^finally\b/.test(trimmed)
    ) {
      findings.push({
        rule: 'potentially-unreachable-code',
        severity: 'warning',
        file: filePath,
        line: line + 1,
        message: 'Code after return may be unreachable; review control flow.',
        snippet: compactSnippet(nextLine),
        importance: scoreImportance('warning', 1, 1),
      });
    }
  }

  // 13. Variable shadowing (simple heuristic)
  const letConstPattern = /\b(let|const)\s+(\w+)/g;
  const declaredVars = new Set();
  for (const match of source.matchAll(letConstPattern)) {
    const varName = match[2];
    if (declaredVars.has(varName)) {
      const line = lineNumberAt(source, match.index);
      findings.push({
        rule: 'variable-shadowing',
        severity: 'warning',
        file: filePath,
        line,
        message: `Variable "${varName}" may shadow a previous declaration; review scope.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1),
      });
    }
    declaredVars.add(varName);
  }

  return findings;
}

// ─── Haskell Logical Audits ───

function auditHaskellLogical(source, filePath) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. Incomplete pattern matches (no exhaustive match)
  const casePattern = /\bcase\b/g;
  for (const match of source.matchAll(casePattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 5).join('\n');
    if (!/_->/.test(surrounding) && !/otherwise\b/.test(surrounding)) {
      findings.push({
        rule: 'incomplete-pattern-match',
        severity: 'error',
        file: filePath,
        line,
        message: 'case expression may lack an exhaustive catch-all pattern (_ or otherwise).',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('error', 1, 2),
      });
    }
  }

  // 2. head/tail/fromJust usage (partial functions)
  const partialFuncPattern = /\b(head|tail|init|last|fromJust|fromLeft|fromRight)\b/g;
  for (const match of source.matchAll(partialFuncPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'partial-function',
      severity: 'warning',
      file: filePath,
      line,
      message: `Partial function "${match[1]}" can crash; prefer total alternatives.`,
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('warning', 1, 1.5),
    });
  }

  // 3. error/undefined usage
  const errorPattern = /\b(error|undefined)\s+["']/g;
  for (const match of source.matchAll(errorPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'runtime-error-call',
      severity: 'error',
      file: filePath,
      line,
      message: 'Direct call to error/undefined will crash at runtime; use Maybe/Either.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 4. Missing type signature on top-level binding
  const topLevelPattern = /^([a-z_]\w*)\s+(::|=)/gm;
  const bindingsWithSigs = new Set();
  for (const match of source.matchAll(topLevelPattern)) {
    if (match[2] === '::') {
      bindingsWithSigs.add(match[1]);
    }
  }
  const topLevelEqPattern = /^([a-z_]\w*)\s*=/gm;
  for (const match of source.matchAll(topLevelEqPattern)) {
    const name = match[1];
    if (!bindingsWithSigs.has(name)) {
      const line = lineNumberAt(source, match.index);
      findings.push({
        rule: 'missing-type-signature',
        severity: 'info',
        file: filePath,
        line,
        message: `Top-level binding "${name}" lacks an explicit type signature.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  return findings;
}

// ─── SQL Logical Audits ───

function auditSqlLogical(source, filePath) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. SELECT * in migrations
  const selectStarPattern = /\bSELECT\s+\*/gi;
  for (const match of source.matchAll(selectStarPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'select-star',
      severity: 'warning',
      file: filePath,
      line,
      message: 'SELECT * is fragile; explicitly list required columns.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('warning', 1, 1),
    });
  }

  // 2. Missing WHERE on DELETE/UPDATE (per-statement, bounded)
  const statements = source.split(/;\s*\n/);
  for (const stmt of statements) {
    const trimmed = stmt.trim();
    // Only flag actual DML statements, not triggers, policies, or comments
    if (!/^(DELETE|UPDATE)\b/i.test(trimmed)) continue;
    if (/\bWHERE\b/i.test(stmt)) continue;
    const firstLine = stmt.split('\n')[0] ?? stmt;
    const lineIndex = source.indexOf(firstLine);
    const line = lineIndex >= 0 ? lineNumberAt(source, lineIndex) : 1;
    findings.push({
      rule: 'missing-where-clause',
      severity: 'critical',
      file: filePath,
      line,
      message: 'DELETE or UPDATE without a WHERE clause affects every row.',
      snippet: compactSnippet(firstLine),
      importance: scoreImportance('critical', 1, 3),
    });
  }

  return findings;
}

// ─── File Walking ───

async function walkSourceFiles(rootDir) {
  const entries = await fs.readdir(rootDir, { withFileTypes: true });
  const files = [];
  for (const entry of entries) {
    const fullPath = path.join(rootDir, entry.name);
    if (entry.isDirectory()) {
      files.push(...(await walkSourceFiles(fullPath)));
      continue;
    }
    if (shouldScanFile(fullPath)) {
      files.push(fullPath);
    }
  }
  return files;
}

function compareFindings(left, right) {
  return (
    (right.importance ?? 0) - (left.importance ?? 0) ||
    left.file.localeCompare(right.file) ||
    left.line - right.line
  );
}

export function summarizeLogicalFindings(findings) {
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

export async function collectLogicalFindings(repoRoot) {
  const files = await listTrackedFiles(repoRoot);
  const allFindings = [];

  for (const filePath of files) {
    if (!shouldScanFile(filePath)) continue;

    const stats = await fs.stat(filePath);
    if (stats.size > 500_000) continue;

    const source = await fs.readFile(filePath, 'utf8');
    const isTest = TEST_FILE_PATTERN.test(filePath);

    if (HASKELL_FILE_PATTERN.test(filePath)) {
      allFindings.push(...auditHaskellLogical(source, filePath));
    } else if (SQL_FILE_PATTERN.test(filePath)) {
      allFindings.push(...auditSqlLogical(source, filePath));
    } else if (SOURCE_FILE_PATTERN.test(filePath)) {
      allFindings.push(...auditJsTsLogical(source, filePath, isTest));
    }
  }

  return allFindings.sort(compareFindings);
}
