import { pathToFileURL } from 'node:url';

const scopeNames = ['repo', 'ui', 'mobile', 'backend', 'contracts', 'migrations'];

function record(value, label) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`Missing or invalid ${label}`);
  }
  return value;
}

function flag(value, label) {
  if (value !== 'true' && value !== 'false') throw new Error(`Invalid selection flag: ${label}`);
  return value === 'true';
}

export function admitSelectedJobs(gate, needs, nativeAndroidSelected) {
  record(needs, 'dependency context');
  const changes = record(needs.changes, 'changes dependency');
  if (changes.result !== 'success') throw new Error('Path classification did not succeed');
  const outputs = record(changes.outputs, 'selection outputs');
  if (Object.keys(outputs).some(name => !scopeNames.includes(name))) {
    throw new Error('Unknown selection output requires reviewed admission mapping');
  }
  const selected = Object.fromEntries(scopeNames.map(name => [name, flag(outputs[name], name)]));
  const plans = {
    quality: [
      ['repo-quality', selected.repo],
      ['ui-quality', selected.ui],
      ['persona-web-e2e', selected.ui || selected.repo],
      ['native-music-e2e', selected.ui || selected.repo],
      ['mobile-quality', selected.mobile],
      ['backend-quality', selected.backend],
      ['native-android-e2e', gate === 'quality' ? flag(nativeAndroidSelected, 'native Android') : false],
    ],
    'api-contracts': [['api-contract-tests', selected.contracts]],
    'production-migrations': [['migration-tests', selected.migrations]],
  };
  if (!Object.hasOwn(plans, gate)) throw new Error('Unknown CI admission gate');
  const dependencies = new Set(['changes', ...plans[gate].map(([name]) => name)]);
  if (Object.keys(needs).some(name => !dependencies.has(name))) {
    throw new Error('Unknown dependency requires reviewed admission mapping');
  }
  const results = [];
  for (const [name, required] of plans[gate]) {
    const { result } = record(needs[name], name);
    if (result !== 'success' && !(result === 'skipped' && !required)) {
      throw new Error(`CI dependency did not satisfy admission: ${name}`);
    }
    results.push({ name, selected: required, result });
  }
  return results;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  try {
    if (process.argv.length !== 3) throw new Error('Specify one CI admission gate');
    const results = admitSelectedJobs(process.argv[2], JSON.parse(process.env.CI_ADMISSION_NEEDS ?? ''),
      process.env.CI_ADMISSION_NATIVE_ANDROID);
    process.stdout.write(`${JSON.stringify({ gate: process.argv[2], results })}\n`);
  } catch (error) {
    // Do not print the dependency JSON: it may contain unrelated job outputs.
    process.stderr.write(`${error instanceof SyntaxError ? 'Invalid dependency JSON' : error.message}\n`);
    process.exitCode = 1;
  }
}
