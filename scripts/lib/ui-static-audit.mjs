import fs from 'node:fs/promises';
import path from 'node:path';

const SOURCE_FILE_PATTERN = /\.[jt]sx?$/;
const TEST_FILE_PATTERN = /(?:^|\/)(?:__tests__|__mocks__)(?:\/|$)|\.(?:test|spec)\.[jt]sx?$/;

function lineNumberAt(source, index) {
  return source.slice(0, index).split('\n').length;
}

function compactSnippet(value) {
  return value.replace(/\s+/g, ' ').trim();
}

function isTagBoundary(character) {
  return character == null || /\s|\/|>/.test(character);
}

function collectOpeningTags(source, tagName) {
  const needle = `<${tagName}`;
  const tags = [];
  let cursor = 0;

  while (cursor < source.length) {
    const start = source.indexOf(needle, cursor);
    if (start === -1) break;

    const boundary = source[start + needle.length];
    if (!isTagBoundary(boundary)) {
      cursor = start + needle.length;
      continue;
    }

    let index = start + needle.length;
    let quote = null;
    let braceDepth = 0;

    while (index < source.length) {
      const character = source[index];
      const previous = source[index - 1];

      if (quote) {
        if (character === quote && previous !== '\\') {
          quote = null;
        }
        index += 1;
        continue;
      }

      if (character === '"' || character === '\'' || character === '`') {
        quote = character;
        index += 1;
        continue;
      }

      if (character === '{') {
        braceDepth += 1;
        index += 1;
        continue;
      }

      if (character === '}') {
        braceDepth = Math.max(0, braceDepth - 1);
        index += 1;
        continue;
      }

      if (character === '>' && braceDepth === 0) {
        tags.push({
          tag: source.slice(start, index + 1),
          index: start,
        });
        cursor = index + 1;
        break;
      }

      index += 1;
    }

    if (index >= source.length) {
      break;
    }
  }

  return tags;
}

export function auditUiSource(source, filePath) {
  const findings = [];
  const rules = [
    {
      name: 'icon-button-label',
      severity: 'error',
      tags: collectOpeningTags(source, 'IconButton'),
      passes: (tag) => /\baria-label\s*=/.test(tag) || /\baria-labelledby\s*=/.test(tag),
      message: 'IconButton is missing an explicit accessible label.',
    },
    {
      name: 'text-field-label',
      severity: 'warning',
      tags: collectOpeningTags(source, 'TextField'),
      passes: (tag) =>
        /\blabel\s*=/.test(tag) ||
        /\baria-label\s*=/.test(tag) ||
        /\baria-labelledby\s*=/.test(tag),
      message: 'TextField is missing a visible or programmatic label.',
    },
    {
      name: 'image-alt',
      severity: 'error',
      tags: collectOpeningTags(source, 'img'),
      passes: (tag) => /\balt\s*=/.test(tag) || /\brole\s*=\s*["']presentation["']/.test(tag),
      message: 'Image tag is missing alternative text.',
    },
  ];

  for (const rule of rules) {
    for (const match of rule.tags) {
      const tag = match.tag;
      if (rule.passes(tag)) continue;
      const index = match.index ?? 0;
      findings.push({
        rule: rule.name,
        severity: rule.severity,
        file: filePath,
        line: lineNumberAt(source, index),
        message: rule.message,
        snippet: compactSnippet(tag),
      });
    }
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

    if (!SOURCE_FILE_PATTERN.test(entry.name)) continue;
    if (TEST_FILE_PATTERN.test(fullPath)) continue;
    files.push(fullPath);
  }

  return files;
}

function compareFindings(left, right) {
  const severityRank = { error: 0, warning: 1, info: 2 };
  return (
    (severityRank[left.severity] ?? 99) - (severityRank[right.severity] ?? 99) ||
    left.file.localeCompare(right.file) ||
    left.line - right.line
  );
}

export function summarizeUiFindings(findings) {
  return findings.reduce(
    (summary, finding) => {
      summary.total += 1;
      if (finding.severity === 'error') summary.errors += 1;
      if (finding.severity === 'warning') summary.warnings += 1;
      if (finding.severity === 'info') summary.info += 1;
      return summary;
    },
    { total: 0, errors: 0, warnings: 0, info: 0 },
  );
}

export async function collectUiFindings(rootDir) {
  const files = await walkSourceFiles(rootDir);
  const findings = [];

  for (const filePath of files) {
    const source = await fs.readFile(filePath, 'utf8');
    findings.push(...auditUiSource(source, filePath));
  }

  return findings.sort(compareFindings);
}
