#!/usr/bin/env node
import { stdin, stdout, stderr, exit } from 'node:process';
import { issueSriInvoice } from './lib/sri-invoice.mjs';

async function readStdin() {
  const chunks = [];
  for await (const chunk of stdin) {
    chunks.push(chunk);
  }
  return Buffer.concat(chunks).toString('utf8').trim();
}

async function main() {
  try {
    const raw = await readStdin();
    if (!raw) {
      throw new Error('Expected invoice payload JSON on stdin');
    }
    const payload = JSON.parse(raw);
    const result = await issueSriInvoice(payload);
    stdout.write(`${JSON.stringify(result)}\n`);
  } catch (error) {
    stderr.write(`${error instanceof Error ? error.message : String(error)}\n`);
    exit(1);
  }
}

main();
