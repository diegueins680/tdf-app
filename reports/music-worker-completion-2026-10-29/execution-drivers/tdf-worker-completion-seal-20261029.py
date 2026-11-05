from pathlib import Path
import subprocess, os, json, hashlib
w = Path('/private/tmp/tdf-music-worker-completion-20261029')
o = Path('/private/tmp/tdf-worker-completion-source-20261029')
base = 'e302dc7fa76ac1c20e4e95cb946c82019ee17bb2'
env = os.environ.copy()
env['PATH'] = '/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin'
env['PYTHONDONTWRITEBYTECODE'] = '1'
def git(*args, root=w): return subprocess.check_output(['git', '-C', str(root), *args])
def sha(data): return hashlib.sha256(data).hexdigest()
def run(label, args):
    with (o / (label + '.txt')).open('w') as log:
        result = subprocess.run(args, cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT)
    assert result.returncode == 0, (label, result.returncode)
    print(label + ' actualexit0', flush=True)
q = json.loads((o / 'qualification.json').read_text())
assert q['baseline'] == base and git('rev-parse', 'HEAD').decode().strip() == base
for p, digest in q['sourceHashes'].items(): assert sha((w / p).read_bytes()) == digest, p
paths = ['formal/system/inventory.json', 'formal/system/requirements.json', 'formal/system/traceability.json', 'formal/system/music-worker-completion-observation.md', 'scripts/lib/test-child-completion.mjs', 'scripts/__tests__/test-child-completion.test.mjs', 'scripts/test-music-release-worker-runtime.mjs', 'scripts/quality-repo.sh', 'docs/catalog-persistence/reports/static-list-inventory.json', 'docs/catalog-persistence/reports/list-consumer-matrix.csv']
actual = set(git('diff', '--name-only').decode().splitlines()) | set(git('ls-files', '--others', '--exclude-standard').decode().splitlines())
assert actual <= set(paths), actual - set(paths)
for path in set(paths) - actual:
    assert (w / path).read_bytes() == git('show', base + ':' + path), path
(o / 'unchanged-allowed-inputs.json').write_text(json.dumps({'unchangedAllowedPaths': sorted(set(paths) - actual), 'scope':'Unchanged canonical CSV is byte-exact baseline, not an omitted correction.'}, indent=2) + '\n')
run('source-add', ['git', 'add', '--', *paths])
run('source-commit', ['git', 'commit', '-m', 'test(music): preserve lifecycle evidence on worker timeouts'])
source = git('rev-parse', 'HEAD').decode().strip()
assert not git('status', '--porcelain').strip()
(o / 'source-C.json').write_text(json.dumps({'baseline': base, 'sourceCommit': source, 'productWorkerUnchanged': True}, indent=2) + '\n')
run('assurance-committed', ['python3', 'scripts/verification-report.py', '--output', str(o / 'assurance-committed')])
run('source-admission', ['python3', 'scripts/check-new-specification-surfaces.py', '--base', base, '--head', source])
s = json.loads((o / 'assurance-committed/sources.json').read_text())
assert s['revision'] == source and not s['worktreeDirty']
files = s['files']
mobile = git('rev-parse', source + ':tdf-mobile').decode().strip()
assert git('rev-parse', 'HEAD', root=w / 'tdf-mobile').decode().strip() == mobile
for root, revision, prefix in [(w, source, ''), (w / 'tdf-mobile', mobile, 'tdf-mobile/')]:
    selected = [(p, p[len(prefix):]) for p in sorted(files) if p.startswith('tdf-mobile/') == bool(prefix)]
    blobs = subprocess.check_output(['git', '-C', str(root), 'cat-file', '--batch'], input=('\n'.join(revision + ':' + local for _, local in selected) + '\n').encode())
    offset = 0
    for full, local in selected:
        end = blobs.index(b'\n', offset)
        header = blobs[offset:end].split()
        assert header[1] == b'blob'
        length = int(header[2]); start = end + 1
        assert sha(blobs[start:start + length]) == files[full], full
        offset = start + length + 1
    assert offset == len(blobs)
old = json.loads(git('show', base + ':formal/system/requirements.json'))['requirements']
new = json.loads(git('show', source + ':formal/system/requirements.json'))['requirements']
assert len(new) == 157 and len(old) == 156 and new[:156] == old
assert git('show', source + ':docs/catalog-persistence/catalog-list-decisions.json') == git('show', base + ':docs/catalog-persistence/catalog-list-decisions.json')
for p in ['tdf-hq', 'tdf-hq-ui', '.github', 'ops', 'package.json', 'package-lock.json', 'tdf-mobile']:
    assert git('rev-parse', source + ':' + p) == git('rev-parse', base + ':' + p), p
contract = 'formal/system/music-worker-completion-observation.md'
contract_hash = sha(git('show', source + ':' + contract))
inventory = json.loads((w / 'formal/system/inventory.json').read_text())
def visit(v):
    if isinstance(v, dict):
        if v.get('path') == contract: assert v['sha256'] == contract_hash
        for child in v.values(): visit(child)
    elif isinstance(v, list):
        for child in v: visit(child)
visit(inventory)
seal = {'baseline': base, 'sourceCommit': source, 'registeredInputs': len(files), 'sourceDigest': s['digest'], 'allRegisteredGitAndPinnedMobileHashesVerified': True,
 'contractSha256': contract_hash, 'all156ExistingRequirementObjectsUnchanged': True, 'newWholeRequirement': 'SYS-WORKER-COMPLETION-001',
 'wholeHistorical1352ReviewFileBytesUnchanged': True, 'currentStrictGeneratedJSONAndCSV': True,
 'localNewUnitCases': 6, 'intendedMutantFailures': 3, 'actualMusicConcurrencyScenarios': 7, 'actualCatalogCases': 8, 'rootQuality': 'PASS',
 'runtimeQualification': 'Executed core files exact sourceC; later unit-only finally/outerbounds changes have separate exact final6case/3mutant receipts.',
 'globalAssurance': '157whole/314implementation-refinement UNPROVEN;11existingconditionalSMT;releaseBLOCKED',
 'productWorkerUIBackendSchemaDependenciesWorkflowsUnchanged': True,
 'hostedWorkerFailureCause': 'UNKNOWN; diagnostics correction does not claim shutdown repair', 'currentMainComposition': 'Source qualified against statedE302 baseline; recheck latest main before integration',
 'cleanSource': not bool(git('status', '--porcelain').strip())}
assert seal['cleanSource']
(o / 'source-seal.json').write_text(json.dumps(seal, indent=2) + '\n')
print(json.dumps(seal, indent=2), flush=True)
