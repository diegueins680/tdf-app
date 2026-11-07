import { spawnSync } from 'node:child_process';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { sha256 } from './lib/verification-evidence.mjs';
import { alloyControls, validateAlloyCoverage, mutateAlloy, admitAlloyResult } from './lib/alloy-controls.mjs';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
if (process.argv.length !== 4 || process.argv[2] !== '--output') {
  throw new Error('Usage: node scripts/verify-alloy-controls.mjs --output NEW_DIRECTORY');
}
const output = path.resolve(process.argv[3]);
mkdirSync(output); // Never reuse a previous solver receipt.
const result = { schemaVersion: 1, status: 'running', executions: [],
  scope: 'Original finite command scopes; structural consistency and mutation sensitivity only, not implementation refinement or liveness.' };
const save = () => writeFileSync(path.join(output, 'result.json'), JSON.stringify(result, null, 2) + '\n');
save();
try {
  const jar = process.env.ALLOY_JAR;
  if (!jar || sha256(readFileSync(jar)) !== '6b8c1cb5bc93bedfc7c61435c4e1ab6e688a242dc702a394628d9a9801edb78d') {
    throw new Error('ALLOY_JAR must be the pinned Alloy 6.2.0 release');
  }
  result.jarSha256 = sha256(readFileSync(jar));
  for (const { model, controls } of alloyControls) {
    const source = readFileSync(path.join(root, 'formal/event-operations', model), 'utf8');
    const commands = validateAlloyCoverage(source, controls);
    const cases = [
      ...commands.map(command => ({ id: command.name, command, expected: command.type === 'run' ? 'SAT' : 'UNSAT', source, kind: command.type === 'run' ? 'scenario' : 'assertion' })),
      ...controls.map(control => ({ id: control[0], command: commands.find(c => c.name === control[1]), expected: 'SAT', source: mutateAlloy(source, control), kind: 'negative' })),
    ];
    for (const item of cases) {
      const directory = path.join(output, `${model}-${item.kind}-${item.id}`);
      mkdirSync(directory);
      const modelPath = path.join(directory, model);
      writeFileSync(modelPath, item.source, { flag: 'wx' });
      const args = ['-jar', jar, 'exec', '-c', item.command.name, '-s', 'sat4j', '-t', 'none', '-o', path.join(directory, 'solver'), modelPath];
      const execution = spawnSync(process.env.JAVA_BIN || 'java', args,
        { encoding: 'utf8', timeout: 120_000, maxBuffer: 8 * 1024 * 1024 });
      const log = `${execution.stdout || ''}${execution.stderr || ''}`;
      writeFileSync(path.join(directory, 'solver.log'), log);
      const record = { model, kind: item.kind, id: item.id, command: item.command, expected: item.expected,
        sourceSha256: sha256(source), checkedSourceSha256: sha256(item.source),
        exitCode: execution.status, signal: execution.signal, status: 'failed',
        logSha256: sha256(log), artifact: path.basename(directory) };
      result.executions.push(record);
      save();
      if (execution.error || execution.signal || execution.status !== 0) throw new Error(`Alloy execution failed: ${model}/${item.id}`);
      const receiptBytes = readFileSync(path.join(directory, 'solver/receipt.json'));
      const receipt = JSON.parse(receiptBytes);
      admitAlloyResult({ ...execution, log, receipt, command: item.command, expected: item.expected });
      record.receiptSha256 = sha256(receiptBytes);
      record.scope = receipt.commands[item.command.name].source;
      record.status = 'passed';
      save();
      console.log(`TDF_ALLOY_RESULT ${item.kind} ${model} ${item.id} ${item.expected}`);
    }
    if (readFileSync(path.join(root, 'formal/event-operations', model), 'utf8') !== source) throw new Error('Alloy source changed during verification');
  }
  result.status = 'bounded-model-checks-passed';
} catch (error) {
  result.status = 'failed'; result.error = error.message; process.exitCode = 1;
  console.error(error.message);
} finally { save(); }
