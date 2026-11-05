import { readFileSync, writeFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { sealEventReport } from '../functions/_shared/event-report-crypto.mjs';

// Inputs contain private content/capability. Never print them, including parse errors.
try {
  const args = process.argv.slice(2);
  if (args.length !== 6 || args[0] !== '--input' || args[2] !== '--token-file' || args[4] !== '--output') {
    throw new Error('Usage: seal-event-report.mjs --input PRIVATE_JSON --token-file PRIVATE_LINK_JSON --output MODULE');
  }
  const input = resolve(args[1]), tokenFile = resolve(args[3]), output = resolve(args[5]);
  if (input === output || tokenFile === output) throw new Error('Output must be separate');
  const report = JSON.parse(readFileSync(input, 'utf8'));
  const { token } = JSON.parse(readFileSync(tokenFile, 'utf8'));
  const envelope = await sealEventReport(report, token);
  const module = '// Ciphertext only. The separately delivered capability is the decryption key.\n'
    + 'export const sealedEventReport = Object.freeze(' + JSON.stringify(envelope, null, 2) + ');\n';
  if (module.includes(token)) throw new Error('Capability disclosure');
  writeFileSync(output, module);
  console.log(JSON.stringify({ sealed: true, version: envelope.version, tokenSha256: envelope.tokenSha256 }));
} catch {
  console.error('Could not seal report. Validate the private input, capability and output paths.');
  process.exitCode = 1;
}
