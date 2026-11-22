import { execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, readFileSync, writeFileSync, rmSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const jar = process.env['TLA2TOOLS_JAR'];
if (!jar) throw new Error('Set TLA2TOOLS_JAR to the reviewed TLC distribution.');
const model = path.join(root, 'formal/event-operations/EventAgreement.tla');
const baseline = readFileSync(path.join(root, 'formal/event-operations/EventAgreement.cfg'), 'utf8');
const results = [];
mkdirSync(path.join(root, 'tmp'), { recursive: true });
for (const fault of [null, 'BreakRecipient', 'BreakVersion', 'BreakFrozen', 'BreakAnnex', 'BreakDuplicate']) {
  const directory = mkdtempSync(path.join(root, 'tmp', 'event-agreement-tlc-'));
  try {
    const config = path.join(directory, 'EventAgreement.cfg');
    writeFileSync(path.join(directory, 'EventAgreement.tla'), readFileSync(model));
    writeFileSync(config, fault ? baseline.replace(`${fault} = FALSE`, `${fault} = TRUE`) : baseline);
    let output = ''; let status = 0;
    try { output = execFileSync(process.env['JAVA_BIN'] ?? 'java', ['-XX:+UseSerialGC', '-Xmx512m', `-Djava.io.tmpdir=${directory}`, '-cp', path.resolve(jar), 'tlc2.TLC', '-workers', '1', '-metadir', 'states', '-config', 'EventAgreement.cfg', 'EventAgreement'], { cwd: directory, encoding: 'utf8', timeout: 60000 }); }
    catch (error) { output = String(error.stdout ?? '') + String(error.stderr ?? ''); status = error.status ?? -1; }
    if (fault ? !(status !== 0 && /Invariant (FrozenEvidence|ApprovalBeforeAcceptance) is violated/.test(output) && /State 1:/.test(output)) : !(status === 0 && output.includes('No error has been found'))) {
      throw new Error(`TLC did not establish the expected result for ${fault ?? 'baseline'}:\n${output}`);
    }
    results.push({ configuration: fault ?? 'baseline', status: fault ? 'counterexample-found' : 'bounded-invariants-pass', summary: output.split('\n').filter((line) => /states generated|distinct states|depth of/.test(line)) });
  } finally { rmSync(directory, { recursive: true, force: true }); }
}
process.stdout.write(JSON.stringify({ model: 'EventAgreement', scope: 'two agreement versions, two plan versions, three actor classes; no cryptographic or implementation refinement proof', results }, null, 2) + '\n');
