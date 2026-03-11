#!/usr/bin/env node
import process from 'node:process';
import { parseArgs } from 'node:util';
import { verifyImprovementLoopModel } from './lib/continuous-improvement-loop.mjs';

function main() {
  const { values } = parseArgs({
    options: {
      json: { type: 'boolean' },
    },
  });

  const report = {
    ...verifyImprovementLoopModel(),
    tool: 'builtin-loop-model-check',
    generatedAt: new Date().toISOString(),
  };

  if (values.json) {
    console.log(JSON.stringify(report, null, 2));
  } else if (report.ok) {
    console.log('Continuous improvement loop model verified.');
  } else {
    console.log('Continuous improvement loop model failed verification:');
    for (const finding of report.findings) {
      console.log(`- ${finding}`);
    }
  }

  if (!report.ok) {
    process.exit(1);
  }
}

main();
