from pathlib import Path
import subprocess, os, json, hashlib
w = Path('/private/tmp/tdf-music-worker-completion-20261029')
c = Path('/private/tmp/tdf-correctness-cycle-20261028')
o = Path('/private/tmp/tdf-worker-completion-source-20261029')
base = 'e302dc7fa76ac1c20e4e95cb946c82019ee17bb2'
env = os.environ.copy()
env['PATH'] = '/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin'
env['PYTHONDONTWRITEBYTECODE'] = '1'
def git(*args): return subprocess.check_output(['git', '-C', str(w), *args])
def sha(data): return hashlib.sha256(data).hexdigest()
def run(label, args):
    with (o / (label + '.txt')).open('w') as log:
        result = subprocess.run(args, cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT)
    assert result.returncode == 0, (label, result.returncode)
    print(label + ' actualexit0', flush=True)
assert git('rev-parse', 'HEAD').decode().strip() == base
controls = json.loads(Path('/private/tmp/tdf-worker-completion-final-bound-controls-20261029/controls.json').read_text())
assert controls['positiveActualExit'] == 0 and controls['positiveCases'] == 6 and controls['exactCorrectLibraryRestored']
assert controls['correctLibrarySha256'] == sha((w / 'scripts/lib/test-child-completion.mjs').read_bytes())
assert controls['correctTestsSha256'] == sha((w / 'scripts/__tests__/test-child-completion.test.mjs').read_bytes())
runtime = json.loads(Path('/private/tmp/tdf-worker-concurrency-completion-20261029/execution.json').read_text())
assert runtime['actualExit'] == 0 and runtime['sourceUnchanged'] and runtime['postgresStopExit'] == 0
core = ['scripts/test-music-release-worker-runtime.mjs', 'scripts/run-music-release-worker-once.sh', 'scripts/lib/music-test-database.mjs', 'scripts/lib/test-child-completion.mjs']
for p in core: assert runtime['sourceSha256'][p] == sha((w / p).read_bytes()), p
frozen = {p: sha((w / p).read_bytes()) for p in core + ['scripts/__tests__/test-child-completion.test.mjs', 'formal/system/requirements.json', 'formal/system/music-worker-completion-observation.md', 'scripts/quality-repo.sh']}
made, scans = [], []
try:
    for name in ['node_modules', 'tdf-hq-ui/node_modules']:
        p, target = w / name, c / name
        assert not (p.exists() or p.is_symlink())
        p.symlink_to(target, target_is_directory=True)
        made.append((p, target))
    # Independent canonical scans write only private outputs, then copy their
    # completed results before running any checks consuming tracked reports.
    for label, fmt in [('strict-catalog-json', 'json'), ('strict-catalog-csv', 'csv')]:
        log = (o / (label + '.txt')).open('w')
        args = ['node', 'scripts/catalog-list-audit.mjs', '--decisions', 'docs/catalog-persistence/catalog-list-decisions.json', '--fail-on-unreviewed', '--format', fmt, '--output', str(o / ('fresh-catalog.' + fmt))]
        proc = subprocess.Popen(args, cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT)
        scans.append((label, proc, log))
    results = []
    for label, proc, log in scans:
        result = proc.wait()
        log.close()
        results.append(result)
        print(label + ' actualexit' + str(result), flush=True)
    assert results == [0, 0]
    for fmt, destination in [('json', 'static-list-inventory.json'), ('csv', 'list-consumer-matrix.csv')]:
        (w / 'docs/catalog-persistence/reports' / destination).write_bytes((o / ('fresh-catalog.' + fmt)).read_bytes())
    for label, script in [('inventory-final', 'specification-inventory'), ('conformance-final', 'specification-conformance')]:
        run(label, ['python3', 'scripts/' + script + '.py'])
        run(label + '-check', ['python3', 'scripts/' + script + '.py', '--check'])
    run('root-quality', ['bash', 'scripts/quality-repo.sh'])
    run('catalog-unit', ['node', '--test', 'scripts/__tests__/catalog-list-audit.test.mjs'])
    run('diff-check', ['git', 'diff', '--check'])
finally:
    for label, proc, log in scans:
        if proc.poll() is None:
            proc.terminate()
            proc.wait(timeout=10)
        log.close()
    for p, target in reversed(made):
        assert p.is_symlink() and p.resolve() == target.resolve()
        p.unlink()
for p, digest in frozen.items(): assert sha((w / p).read_bytes()) == digest, p
assert (w / 'docs/catalog-persistence/catalog-list-decisions.json').read_bytes() == git('show', base + ':docs/catalog-persistence/catalog-list-decisions.json')
old = json.loads(git('show', base + ':formal/system/requirements.json'))['requirements']
new = json.loads((w / 'formal/system/requirements.json').read_text())['requirements']
assert len(old) == 156 and len(new) == 157 and new[:156] == old
for p in ['tdf-hq', 'tdf-hq-ui', '.github', 'ops', 'package.json', 'package-lock.json', 'tdf-mobile']:
    assert not git('diff', '--name-only', '--', p).strip(), p
qualification = {'baseline': base, 'sourceHashes': frozen, 'all156ExistingRequirementObjectsUnchanged': True,
 'newRequirement': 'SYS-WORKER-COMPLETION-001', 'globalRequirements': 157,
 'whole1352HistoricalCatalogReviewBytesUnchanged': True, 'canonicalJSONAndCSVStrictScans': 'actualexit0/1352candidates',
 'rootQuality': 'PASS; existing platform skips retained', 'actualCatalogUnitCases': 8,
 'unitCases': 6, 'intendedMutantFailures': 3, 'actualMusicConcurrencyScenarios': 7,
 'runtimeBinding': 'Four executed runtime/core files exact final; later unit-only finally/container-bound changes qualified by final6/3controls.',
 'productUIBackendSchemaDependenciesWorkflowsUnchanged': True,
 'ownedAliasesRemoved': True, 'syntheticPGStopped': True,
 'scope': 'TESTED bounded synthetic observations; hosted worker cause UNKNOWN; no full backend/provider/liveness proof or integration claim.'}
(o / 'qualification.json').write_text(json.dumps(qualification, indent=2) + '\n')
print('Worker qualification complete; final frozen source, strict catalog, root quality and actual8catalog tests.', flush=True)
