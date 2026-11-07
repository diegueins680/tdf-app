from pathlib import Path
import subprocess, json, hashlib, os
w = Path('/private/tmp/tdf-music-worker-completion-20261029')
out = Path('/private/tmp/tdf-worker-completion-final-bound-controls-20261029')
out.mkdir()
p = w / 'scripts/lib/test-child-completion.mjs'
correct = p.read_bytes()
test = (w / 'scripts/__tests__/test-child-completion.test.mjs').read_bytes()
env = os.environ.copy()
env['PATH'] = '/Users/macpro/.nvm/versions/node/v24.13.1/bin:' + env['PATH']
rows = []
original_context = '''        const error = new Error(`Worker did not stop within ${timeoutMs / 1000}s\nWorker completion snapshot: ${JSON.stringify(snapshot)}`);
        error.workerSnapshot = snapshot;'''.replace('s\nWorker', 's\\nWorker')
generic_context = '        const error = new Error(`Worker did not stop within ${timeoutMs / 1000}s`);'
mutations = [
 ('omit-exit-observation', "  child.once('exit', () => { exitedAt = performance.now(); });", '', 'exited parent', 'exited-awaiting-close'),
 ('complete-at-exit', "    child.once('close', (status, signal) => {", "    child.once('exit', (status, signal) => {", 'exited parent', 'Missing expected rejection'),
 ('discard-timeout-context', original_context, generic_context, 'a live child timeout', 'Worker completion snapshot:')]
for label, old, new, pattern, expected in mutations:
    try:
        source = correct.decode()
        assert source.count(old) == 1, label
        mutant = source.replace(old, new, 1)
        p.write_text(mutant)
        with (out / (label + '.txt')).open('w') as log:
            run = subprocess.run(['node', '--test', '--test-name-pattern=' + pattern, 'scripts/__tests__/test-child-completion.test.mjs'], cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT, timeout=20)
        result = (out / (label + '.txt')).read_text()
        assert run.returncode == 1 and expected in result and 'fail 1' in result, (label, run.returncode, result[-1000:])
        rows.append({'label': label, 'actualExit': 1, 'expectedFailure': expected, 'mutantSha256': hashlib.sha256(mutant.encode()).hexdigest(), 'selectedRealProcessCasesFailed': 1})
        print(label + ' actualexit1/one intended real-process failure', flush=True)
    finally:
        p.write_bytes(correct)
with (out / 'correct-unit.txt').open('w') as log:
    run = subprocess.run(['node', '--test', 'scripts/__tests__/test-child-completion.test.mjs'], cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT, timeout=20)
assert run.returncode == 0 and 'pass 6' in (out / 'correct-unit.txt').read_text()
assert p.read_bytes() == correct and (w / 'scripts/__tests__/test-child-completion.test.mjs').read_bytes() == test
for name, data in [('correct-library.mjs', correct), ('correct-unit.mjs', test)]: (out / name).write_bytes(data)
(out / 'controls.json').write_text(json.dumps({'correctLibrarySha256': hashlib.sha256(correct).hexdigest(), 'correctTestsSha256': hashlib.sha256(test).hexdigest(), 'positiveActualExit': 0, 'positiveCases': 6, 'mutations': rows, 'exactCorrectLibraryRestored': True, 'scope': 'Real child lifecycle and CLI/structured context sensitivity; no hosted worker rootcause or proof.'}, indent=2) + '\n')
print('Final frozen6/0, three actual intended mutant failures; source restored.', flush=True)
