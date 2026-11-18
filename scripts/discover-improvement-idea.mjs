#!/usr/bin/env node
import fs from 'node:fs/promises';
import process from 'node:process';
import { parseArgs } from 'node:util';
import { discoverImprovementIdea } from './lib/discovery.mjs';

async function main() {
  const { values } = parseArgs({
    options: {
      json: { type: 'boolean' },
    },
  });

  // The loop lists files that already have a pending pull request so they are skipped.
  let excludedFiles = [];
  const excludedFilesPath = process.env.CONTINUOUS_LOOP_EXCLUDED_TARGETS_FILE;
  if (excludedFilesPath) {
    const parsed = JSON.parse(await fs.readFile(excludedFilesPath, 'utf8'));
    excludedFiles = Array.isArray(parsed) ? parsed : [];
  }

  const idea = await discoverImprovementIdea(process.cwd(), { excludedFiles });
  if (values.json) {
    console.log(JSON.stringify(idea, null, 2));
    return;
  }

  process.stdout.write(idea.markdown);
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
