#!/usr/bin/env node
import path from 'node:path';
import process from 'node:process';
import { parseArgs } from 'node:util';
import { collectUiFindings, summarizeUiFindings } from './lib/ui-static-audit.mjs';

async function main() {
  const { values } = parseArgs({
    options: {
      root: { type: 'string' },
      json: { type: 'boolean' },
      'exit-zero': { type: 'boolean' },
    },
  });

  const rootDir = values.root ? path.resolve(values.root) : path.resolve('tdf-hq-ui/src');
  const findings = await collectUiFindings(rootDir);
  const report = {
    ok: findings.length === 0,
    findings,
    summary: summarizeUiFindings(findings),
    tool: 'builtin-static-ui-audit',
    root: rootDir,
    generatedAt: new Date().toISOString(),
  };

  if (values.json) {
    console.log(JSON.stringify(report, null, 2));
  } else if (findings.length === 0) {
    console.log(`No static UI findings in ${rootDir}`);
  } else {
    console.log(
      `Static UI audit found ${report.summary.total} issue(s) in ${rootDir} ` +
        `(${report.summary.errors} errors, ${report.summary.warnings} warnings).`,
    );
    for (const finding of findings) {
      console.log(`- [${finding.severity}] ${finding.file}:${finding.line} ${finding.message}`);
    }
  }

  if (!report.ok && !values['exit-zero']) {
    process.exit(1);
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
