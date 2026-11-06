#!/usr/bin/env python3
"""Offline archive verification only: no SSH, Docker or SQL execution."""
import ast
import hashlib
import json
import pathlib
import subprocess
import sys

HERE=pathlib.Path(__file__).parent
COLLECTOR=HERE/'collector'
REVIEW='cf359cf1f9a704b052e17cf5c2e423e66aa346a440a6de094dff751bd38fb046'
OBSERVATION='57164d673611d3af3eaadda84384b93319bb953e386c167d0ab6064172e59295'
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
assert sha(COLLECTOR/'review-manifest.json')==REVIEW
review=json.loads((COLLECTOR/'review-manifest.json').read_text())
for name,digest in review['files'].items():
    assert pathlib.Path(name).name==name
    assert sha(COLLECTOR/name)==digest,name
assert sha(HERE/'observation.json')==OBSERVATION
observation=json.loads((HERE/'observation.json').read_text())
assert observation['reviewedManifestSha256']==REVIEW
assert observation['remotePayloadSha256']==review['files']['remote-payload.py']
snapshot=observation['snapshot']
assert snapshot['status']=='OBSERVED'
assert snapshot['aggregateFamiliesObserved']==9 and snapshot['aggregateFamiliesUnobserved']==0
assert all(v is True for v in snapshot['inventorySessionIdentity']['counts'].values())
assert all(v=='select_available' for v in snapshot['relationCapabilities']['counts'].values())
assert snapshot['aggregates']['legacy_artist_release_reference_totals']['counts']=={'total':65,'spotifyReferencePresent':8,'youtubeReferencePresent':0}
for name,result in snapshot['aggregates'].items():
    assert result['status']=='OBSERVED'
    if name=='legacy_artist_release_reference_totals':
        continue
    def zeros(value):
        if isinstance(value,dict):
            return all(zeros(v) for v in value.values())
        return type(value) is int and value==0
    assert zeros(result['counts']),name
for path in COLLECTOR.glob('*.py'):
    ast.parse(path.read_text(),filename=path.name)
subprocess.run(['node','--check',str(COLLECTOR/'launch.mjs')],check=True)
# The reviewed suite uses mocked processes/containers and pure command builders.
subprocess.run([sys.executable,'-B',str(COLLECTOR/'test_collect.py')],check=True)
print(json.dumps({'status':'PASS_OFFLINE_ONLY','reviewedManifestSha256':REVIEW,
    'observedLiveAggregateFamiliesArchived':9,'fakePureLocalTests':13,
    'pythonAstSyntax':'PASS','javascriptSyntax':'PASS',
    'sshDockerSqlOrStorageExecutedByVerifier':False}))
