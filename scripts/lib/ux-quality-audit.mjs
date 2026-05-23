import fs from 'node:fs/promises';
import path from 'node:path';

const SOURCE_FILE_PATTERN = /\.[jt]sx?$/;
const TEST_FILE_PATTERN = /(?:^|\/)(?:__tests__|__mocks__)(?:\/|$)|\.(?:test|spec)\.[jt]sx?$/;

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

function collectOpeningTags(source, tagName) {
  const needle = `<${tagName}`;
  const tags = [];
  let cursor = 0;
  let quote = null;
  let lineComment = false;
  let blockComment = false;

  while (cursor < source.length) {
    const character = source[cursor];
    const next = source[cursor + 1];
    const previous = source[cursor - 1];

    if (lineComment) {
      if (character === '\n') lineComment = false;
      cursor += 1;
      continue;
    }
    if (blockComment) {
      if (character === '*' && next === '/') blockComment = false;
      cursor += 1;
      continue;
    }
    if (quote) {
      if (character === quote && previous !== '\\') quote = null;
      cursor += 1;
      continue;
    }
    if (character === '/' && next === '/') {
      lineComment = true;
      cursor += 2;
      continue;
    }
    if (character === '/' && next === '*') {
      blockComment = true;
      cursor += 2;
      continue;
    }
    if (character === '"' || character === "'" || character === '`') {
      quote = character;
      cursor += 1;
      continue;
    }
    if (source.startsWith(needle, cursor)) {
      const end = source.indexOf('>', cursor);
      if (end > 0) {
        tags.push({ tag: source.slice(cursor, end + 1), index: cursor });
        cursor = end + 1;
        continue;
      }
    }
    cursor += 1;
  }
  return tags;
}

function countNestingDepth(source) {
  let maxDepth = 0;
  let currentDepth = 0;
  let inString = false;
  let stringChar = null;
  let inComment = false;
  let blockComment = false;

  for (let i = 0; i < source.length; i++) {
    const ch = source[i];
    const next = source[i + 1];

    if (inComment) {
      if (blockComment && ch === '*' && next === '/') {
        blockComment = false;
        inComment = false;
        i++;
      } else if (!blockComment && ch === '\n') {
        inComment = false;
      }
      continue;
    }

    if (inString) {
      if (ch === stringChar && source[i - 1] !== '\\') {
        inString = false;
      }
      continue;
    }

    if (ch === '/' && next === '/') {
      inComment = true;
      i++;
      continue;
    }
    if (ch === '/' && next === '*') {
      inComment = true;
      blockComment = true;
      i++;
      continue;
    }
    if (ch === '"' || ch === "'" || ch === '`') {
      inString = true;
      stringChar = ch;
      continue;
    }

    if (ch === '<' && !/\s/.test(next) && next !== '/') {
      currentDepth++;
      maxDepth = Math.max(maxDepth, currentDepth);
    }
    if (ch === '<' && next === '/') {
      currentDepth = Math.max(0, currentDepth - 1);
    }
    if (ch === '/' && next === '>') {
      currentDepth = Math.max(0, currentDepth - 1);
    }
  }
  return maxDepth;
}

export function auditUxSource(source, filePath) {
  const findings = [];
  const lines = source.split(/\r?\n/);

  // 1. Component with too many props (>8) — complexity / not minimal
  const componentPattern = /(?:function|const)\s+(\w+)[^\{]*\{/g;
  for (const match of source.matchAll(componentPattern)) {
    const compName = match[1];
    if (!/^[A-Z]/.test(compName)) continue;
    const startIdx = match.index;
    const parenOpen = source.indexOf('(', startIdx);
    const parenClose = source.indexOf(')', parenOpen);
    if (parenOpen < 0 || parenClose < 0) continue;
    const params = source.slice(parenOpen + 1, parenClose);
    const propCount = params.split(',').filter((p) => p.trim()).length;
    if (propCount > 8) {
      const line = lineNumberAt(source, startIdx);
      findings.push({
        rule: 'too-many-props',
        severity: 'warning',
        file: filePath,
        line,
        message: `Component "${compName}" has ${propCount} props; consider composition or context for simplicity.`,
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 2. Deep JSX nesting (>10 levels) — clutter / not minimal
  const jsxDepth = countNestingDepth(source);
  if (jsxDepth > 10) {
    findings.push({
      rule: 'deep-jsx-nesting',
      severity: 'warning',
      file: filePath,
      line: 1,
      message: `JSX nesting depth is ${jsxDepth}; extract sub-components for clarity and minimalism.`,
      snippet: '',
      importance: scoreImportance('warning', 1, 1.5),
    });
  }

  // 3. Missing loading state
  const hasLoading = /\b(loading|skeleton|spinner|placeholder|Suspense|fallback)\b/i.test(source);
  const hasDataFetch = /\b(useQuery|useEffect|fetch|axios|api|getData)\b/.test(source);
  if (hasDataFetch && !hasLoading) {
    findings.push({
      rule: 'missing-loading-state',
      severity: 'error',
      file: filePath,
      line: 1,
      message: 'Data-fetching component lacks a visible loading state; users see blank or stale content.',
      snippet: '',
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 4. Missing empty state
  const hasEmpty = /\b(empty|no.?results|nothing|zero)\b/i.test(source);
  const hasListRender = /\b\.map\s*\(/.test(source);
  if (hasListRender && !hasEmpty) {
    findings.push({
      rule: 'missing-empty-state',
      severity: 'warning',
      file: filePath,
      line: 1,
      message: 'List rendering lacks an empty state; users see nothing when there are no items.',
      snippet: '',
      importance: scoreImportance('warning', 1, 1.5),
    });
  }

  // 5. Form without validation feedback
  const formPattern = /\b<form\b/gi;
  for (const match of source.matchAll(formPattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 15).join('\n');
    if (!/\b(error|invalid|helperText|validation|FormHelperText)\b/i.test(surrounding)) {
      findings.push({
        rule: 'missing-form-feedback',
        severity: 'error',
        file: filePath,
        line,
        message: 'Form lacks visible validation feedback; users cannot correct mistakes intuitively.',
        snippet: compactSnippet(lines[line - 1] ?? ''),
        importance: scoreImportance('error', 1, 2),
      });
    }
  }

  // 6. Button without visual feedback states
  const buttonTags = collectOpeningTags(source, 'Button');
  for (const tag of buttonTags) {
    const tagText = tag.tag;
    const line = lineNumberAt(source, tag.index);
    const hasDisabled = /\bdisabled\b/.test(tagText);
    const hasLoading = /\bloading\b/.test(tagText);
    if (!hasDisabled && !hasLoading) {
      findings.push({
        rule: 'missing-button-feedback',
        severity: 'warning',
        file: filePath,
        line,
        message: 'Button lacks disabled/loading state; users may trigger duplicate actions.',
        snippet: compactSnippet(tagText),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 7. Overly complex conditional rendering (>4 branches)
  const ternaryPattern = /\?\s*<[^>]+>\s*:\s*<[^>]+>/g;
  for (const match of source.matchAll(ternaryPattern)) {
    const line = lineNumberAt(source, match.index);
    const snippet = lines[line - 1] ?? '';
    const ternaryCount = (snippet.match(/\?/g) ?? []).length;
    if (ternaryCount >= 3) {
      findings.push({
        rule: 'complex-conditional-render',
        severity: 'warning',
        file: filePath,
        line,
        message: 'Deeply nested conditional rendering is hard to follow; extract named sub-components.',
        snippet: compactSnippet(snippet),
        importance: scoreImportance('warning', 1, 1.5),
      });
    }
  }

  // 8. Too many useState hooks (>5) — state sprawl
  const stateCount = (source.match(/\buseState\b/g) ?? []).length;
  if (stateCount > 5) {
    findings.push({
      rule: 'state-sprawl',
      severity: 'warning',
      file: filePath,
      line: 1,
      message: `Component uses ${stateCount} useState hooks; consider useReducer or context for cleaner state.`,
      snippet: '',
      importance: scoreImportance('warning', 1, 1.5),
    });
  }

  // 9. Missing keyboard interaction (only for non-button, non-link elements)
  const clickablePattern = /\bonClick\s*=/g;
  for (const match of source.matchAll(clickablePattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 2), line + 2).join('\n');
    // Skip if keyboard support is already present
    if (/\b(onKeyDown|onKeyPress|role\s*=\s*["']button["']|tabIndex\s*=)/.test(surrounding)) continue;
    // Skip native interactive elements
    if (/\b<\s*(button|a\s|input|select|textarea|label)\b/i.test(surrounding)) continue;
    // Skip if it's a Button component
    if (/\b<\s*(Button|IconButton|MenuItem|ListItemButton|Tab)\b/.test(surrounding)) continue;
    findings.push({
      rule: 'missing-keyboard-interaction',
      severity: 'error',
      file: filePath,
      line,
      message: 'Clickable element lacks keyboard support; not accessible or intuitive for keyboard users.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('error', 1, 2),
    });
  }

  // 10. Overly long JSX expression on one line (>200 chars)
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    if (line.length > 200 && /<[A-Z]/.test(line)) {
      findings.push({
        rule: 'long-jsx-line',
        severity: 'info',
        file: filePath,
        line: i + 1,
        message: 'Very long JSX line; break into multiple lines for readability and maintainability.',
        snippet: compactSnippet(line),
        importance: scoreImportance('info', 1, 0.5),
      });
    }
  }

  // 11. Hardcoded user-facing text (i18n issue + engagement)
  const hardcodedTextPattern = />\s*([A-Z][a-zA-Z\s]{10,}[a-z])\s*</g;
  for (const match of source.matchAll(hardcodedTextPattern)) {
    const line = lineNumberAt(source, match.index);
    findings.push({
      rule: 'hardcoded-user-text',
      severity: 'info',
      file: filePath,
      line,
      message: 'Hardcoded user-facing text; consider i18n for engagement and accessibility.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('info', 1, 0.5),
    });
  }

  // 12. Missing focus management after action
  const actionPattern = /\b(onSubmit|onClick|handleSubmit|handleSave)\b/g;
  for (const match of source.matchAll(actionPattern)) {
    const line = lineNumberAt(source, match.index);
    const surrounding = lines.slice(Math.max(0, line - 1), line + 8).join('\n');
    if (/\b(focus|scrollIntoView|autoFocus)\b/.test(surrounding)) continue;
    findings.push({
      rule: 'missing-focus-management',
      severity: 'warning',
      file: filePath,
      line,
      message: 'Action handler lacks focus management; screen-reader and keyboard users lose context.',
      snippet: compactSnippet(lines[line - 1] ?? ''),
      importance: scoreImportance('warning', 1, 1),
    });
  }

  return findings;
}

async function walkSourceFiles(rootDir) {
  const entries = await fs.readdir(rootDir, { withFileTypes: true });
  const files = [];
  for (const entry of entries) {
    const fullPath = path.join(rootDir, entry.name);
    if (entry.isDirectory()) {
      files.push(...(await walkSourceFiles(fullPath)));
      continue;
    }
    if (SOURCE_FILE_PATTERN.test(entry.name) && !TEST_FILE_PATTERN.test(fullPath)) {
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

export function summarizeUxFindings(findings) {
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

export async function collectUxFindings(rootDir) {
  const files = await walkSourceFiles(rootDir);
  const findings = [];
  for (const filePath of files) {
    const source = await fs.readFile(filePath, 'utf8');
    findings.push(...auditUxSource(source, filePath));
  }
  return findings.sort(compareFindings);
}
