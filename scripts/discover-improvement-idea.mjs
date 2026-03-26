#!/usr/bin/env node
import process from 'node:process';
import { parseArgs } from 'node:util';
import { discoverImprovementIdea } from './lib/discovery.mjs';

async function main() {
  const { values } = parseArgs({
    options: {
      json: { type: 'boolean' },
    },
  });

  const idea = await discoverImprovementIdea(process.cwd());
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
