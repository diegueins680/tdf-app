#!/usr/bin/env node
import path from 'node:path';
import process from 'node:process';
import { parseArgs } from 'node:util';

import {
  collectFailingFormalFindings,
  collectFormalFindings,
  formatFormalFinding,
  summarizeFormalFindings,
} from './lib/formal-methods-audit.mjs';

const VALID_FAIL_THRESHOLDS = new Set(['none', 'critical', 'error', 'warning', 'info']);

function parsePositiveInteger(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function relativeFinding(repoRoot, finding) {
  return {
    ...finding,
    file: path.relative(repoRoot, finding.file),
  };
}

async function main() {
  const { values } = parseArgs({
    options: {
      json: { type: 'boolean' },
      'fail-on': { type: 'string', default: 'error' },
      'max-findings': { type: 'string', default: '25' },
    },
  });

  const failOn = values['fail-on'];
  if (!VALID_FAIL_THRESHOLDS.has(failOn)) {
    throw new Error(`Unsupported --fail-on value "${failOn}". Use one of: ${[...VALID_FAIL_THRESHOLDS].join(', ')}.`);
  }

  const repoRoot = process.cwd();
  const maxFindings = parsePositiveInteger(values['max-findings'], 25);
  const findings = await collectFormalFindings(repoRoot);
  const failingFindings = collectFailingFormalFindings(findings, failOn);
  const summary = summarizeFormalFindings(findings);
  const report = {
    ok: failingFindings.length === 0,
    tool: 'builtin-formal-methods-audit',
    generatedAt: new Date().toISOString(),
    failOn,
    summary,
    findings: findings.slice(0, maxFindings).map((finding) => relativeFinding(repoRoot, finding)),
    failingFindings: failingFindings.map((finding) => relativeFinding(repoRoot, finding)),
  };

  if (values.json) {
    console.log(JSON.stringify(report, null, 2));
  } else {
    console.log(
      [
        `Formal methods audit ${report.ok ? 'passed' : 'failed'}.`,
        `Findings: ${summary.total} total, ${summary.critical} critical, ${summary.errors} errors, ${summary.warnings} warnings, ${summary.info} info.`,
        `Fail threshold: ${failOn}.`,
      ].join('\n'),
    );

    if (findings.length > 0) {
      console.log(`\nTop ${Math.min(maxFindings, findings.length)} findings:`);
      for (const finding of findings.slice(0, maxFindings)) {
        console.log(`- ${formatFormalFinding(finding, repoRoot)}`);
      }
    }
  }

  if (!report.ok) {
    process.exit(1);
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
