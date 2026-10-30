from pathlib import Path
import subprocess, os, json, hashlib
w = Path('/private/tmp/tdf-music-worker-completion-20261029')
o = Path('/private/tmp/tdf-worker-completion-source-20261029')
own = '4ac12ee2f9a4b2e1b7fd33ae01ac2b0adfb8b630'
main = '6197b87326a437350dca239ee64902e78da6964c'
c = Path('/private/tmp/tdf-correctness-cycle-20261028')
env = os.environ.copy()
env['PATH'] = '/private/tmp/tdf-correctness-bin-20261028:/private/tmp/tdf-event-report-venv-20261027/bin:/Users/macpro/.nvm/versions/node/v24.13.1/bin:/usr/local/bin:/usr/bin:/bin'
env['PYTHONDONTWRITEBYTECODE'] = '1'
def git(*args, root=w): return subprocess.check_output(['git', '-C', str(root), *args])
def sha(data): return hashlib.sha256(data).hexdigest()
def run(label, args):
    with (o / (label + '.txt')).open('w') as log:
        r = subprocess.run(args, cwd=w, env=env, stdout=log, stderr=subprocess.STDOUT)
    assert r.returncode == 0, (label, r.returncode)
    print(label + ' actualexit0', flush=True)
conflicts = {'formal/system/inventory.json','formal/system/traceability.json','docs/catalog-persistence/reports/static-list-inventory.json'}
assert git('rev-parse','HEAD').decode().strip()==own
assert git('rev-parse','MERGE_HEAD').decode().strip()==main
assert set(git('diff','--name-only','--diff-filter=U').decode().splitlines())=={'formal/system/inventory.json','formal/system/traceability.json','docs/catalog-persistence/reports/static-list-inventory.json'}
for fmt,dest in [('json','static-list-inventory.json'),('csv','list-consumer-matrix.csv')]:
    assert (w/'docs/catalog-persistence/reports'/dest).read_bytes()==(o/('composed-catalog.'+fmt)).read_bytes()
assert 'pass 8' in (o/'composed-catalog-unit.txt').read_text()
run('resume-inventory-check',['python3','scripts/specification-inventory.py','--check'])
run('resume-conformance-check',['python3','scripts/specification-conformance.py','--check'])
run('composed-source-diff-check', ['git', 'diff', '--check', '--', ':(exclude)reports/**', ':(exclude)formal/system/evidence/**'])
assert not git('diff', '--name-only', own, '--', 'scripts').strip()
incoming = json.loads(git('show', main + ':formal/system/requirements.json'))['requirements']
combined = json.loads((w / 'formal/system/requirements.json').read_text())['requirements']
original = json.loads(git('show', own + ':formal/system/requirements.json'))['requirements']
by_id = {r['id']: r for r in combined}
assert len(incoming) == 156 and len(combined) == 157
assert all(by_id[r['id']] == r for r in incoming)
assert by_id['SYS-WORKER-COMPLETION-001'] == next(r for r in original if r['id'] == 'SYS-WORKER-COMPLETION-001')
(o / 'incoming-requirement-preservation.json').write_text(json.dumps({'all156IncomingMainObjectsPreserved':True,'ownNewRequirementExact':True,'incomingChangeFromOldComponent':{'BOOK-SERVICE-PICKER-001':['source','verification']},'scope':'Preserve protectedmain booking references rather than overwrite them with older component baseline.'}, indent=2) + '\n')
run('resolve-generated-conflicts', ['git', 'add', '--', *sorted(conflicts), 'docs/catalog-persistence/reports/list-consumer-matrix.csv'])
assert not git('diff', '--name-only', '--diff-filter=U').strip()
run('source-merge-commit', ['git', 'commit', '-m', 'Merge protected booking correction into worker diagnostics'])
source = git('rev-parse', 'HEAD').decode().strip()
assert git('show', '-s', '--format=%P', source).decode().split() == [own, main]
assert not git('status', '--porcelain').strip()
run('assurance-composed', ['python3', 'scripts/verification-report.py', '--output', str(o / 'assurance-composed')])
run('current-main-admission', ['python3', 'scripts/check-new-specification-surfaces.py', '--base', main, '--head', source])
s = json.loads((o / 'assurance-composed/sources.json').read_text())
assert s['revision'] == source and not s['worktreeDirty']
files = s['files']; oldfiles = json.loads((o / 'assurance-committed/sources.json').read_text())['files']
assert set(files) == set(oldfiles)
assert {p for p in files if files[p] != oldfiles[p]} == {'e2e/web/booking-catalog-recovery.spec.mjs','formal/system/requirements.json','tdf-hq-ui/src/pages/PublicBookingPage.tsx'}
mobile = git('rev-parse', source + ':tdf-mobile').decode().strip()
assert git('rev-parse', 'HEAD', root=w / 'tdf-mobile').decode().strip() == mobile
for root, rev, prefix in [(w, source, ''), (w / 'tdf-mobile', mobile, 'tdf-mobile/')]:
    selected = [(p, p[len(prefix):]) for p in sorted(files) if p.startswith('tdf-mobile/') == bool(prefix)]
    blobs = subprocess.check_output(['git', '-C', str(root), 'cat-file', '--batch'], input=('\n'.join(rev + ':' + local for _, local in selected) + '\n').encode())
    offset = 0
    for full, local in selected:
        end = blobs.index(b'\n', offset); header = blobs[offset:end].split(); assert header[1] == b'blob'
        n = int(header[2]); start = end + 1; assert sha(blobs[start:start + n]) == files[full], full
        offset = start + n + 1
    assert offset == len(blobs)
for p in ['tdf-hq', 'tdf-hq-ui', '.github', 'ops', 'package.json', 'package-lock.json', 'tdf-mobile']:
    assert git('rev-parse', source + ':' + p) == git('rev-parse', main + ':' + p), p
assert git('rev-parse', source + ':scripts') == git('rev-parse', own + ':scripts')
contract_paths = ['formal/system/music-worker-completion-observation.md','formal/system/booking-tooltip-viewport.md']
contract_hashes = {p:sha(git('show',source+':'+p)) for p in contract_paths}
assert git('show',source+':'+contract_paths[0]) == git('show',own+':'+contract_paths[0])
assert git('show',source+':'+contract_paths[1]) == git('show',main+':'+contract_paths[1])
inventory = json.loads(git('show',source+':formal/system/inventory.json'))
def matches(v,path):
    if isinstance(v,dict): return ([v] if v.get('path')==path else []) + [r for c in v.values() for r in matches(c,path)]
    if isinstance(v,list): return [r for c in v for r in matches(c,path)]
    return []
for p,h in contract_hashes.items():
    rows=matches(inventory,p); assert rows and all(r.get('sha256')==h for r in rows)
seal = {'baselineMain': main, 'qualifiedComponentCommit': own, 'sourceCommit': source,
 'mergeParents': [own, main], 'registeredInputs': len(files), 'sourceDigest': s['digest'], 'allGitAndPinnedMobileHashesVerified': True, 'selectedContractGitHashesSeparatelyBound': contract_hashes, 'sourceFingerprintMarkdownLimit':'Markdown is outside registered source set; selected contracts are separately bound to Git and discovery hashes.',
 'regeneratedConflicts': sorted(conflicts), 'strictCanonicalJSONAndCSV': 'PASS1352', 'composedCatalogTests': 8,
 'allOwnRuntimeUnitQualitySourcesExactQualifiedC': True, 'allProductUIBackendWorkflowsExactProtectedMain': True,
 'all156IncomingMainObjectsPlusOwnRequirementPreserved': True, 'historical1352ReviewFile': 'unchanged',
 'verificationComposition': 'OwnC rootQ/6unit/3mutant/7actualconcurrency plus protectedmain CI643persona; current merged indices/strictcatalog/admission/assurance/Gitbinding rechecked. No single full composed backend serving run claimed.',
 'assurance': 'TESTED scoped observation;157whole/314implementation-refinementUNPROVEN/11existingconditionalSMT/releaseBLOCKED',
 'hostedNewWorkerSource': 'PENDING; underlying workerstopcauseUNKNOWN', 'deployment': 'UNVERIFIED',
 'clean': not bool(git('status', '--porcelain').strip())}
assert seal['clean']
(o / 'composed-source-seal.json').write_text(json.dumps(seal, indent=2) + '\n')
print(json.dumps(seal, indent=2), flush=True)
