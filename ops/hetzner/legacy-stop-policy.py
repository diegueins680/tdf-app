#!/usr/bin/env python3
"""Durable exact-identity policy binding; no stop or recovery authority.

Qualification digests are references, not proofs or approvals. The future
restricted coordinator must authenticate them and establish all live boundaries.
Ordinary writer/abort adapters reject plans using this exceptional contract.
"""
import importlib.util
import json
import os
from pathlib import Path

spec=importlib.util.spec_from_file_location('legacy_original',Path(__file__).with_name('original-deployment-admission.py'))
o=importlib.util.module_from_spec(spec);spec.loader.exec_module(o)
j=o.abort.j
require,canonical,sha=j.require,j.canonical,j.sha
NAME='legacy-stop-policy.json'
IMAGE='diegueins680/tdf-hq@sha256:38e6264b82db2d81a5b51c3a78740b6a305538b4cdae8d53ced067ccbb1e8fe0'
REVISION='645f56fcc44f81609fbfd0e03d683b40376ce77a'
KEYS={'schemaVersion','kind','api','runtimeConfigurationSha256','legacySourceRevision',
      'qualificationSourceRevision','stopEvidenceSha256','captureEvidenceSha256',
      'recoveryEvidenceSha256','restrictionPolicySha256'}


def validate(policy,plan,original):
    j.validate_plan(plan)
    require(plan.get('schemaVersion')==2 and isinstance(policy,dict) and set(policy)==KEYS
            and type(policy['schemaVersion']) is int and policy['schemaVersion']==1
            and policy['kind']=='legacy-sigint-645f' and policy['legacySourceRevision']==REVISION)
    for key in ('runtimeConfigurationSha256','stopEvidenceSha256','captureEvidenceSha256',
                'recoveryEvidenceSha256','restrictionPolicySha256'):
        require(j.hash_value(policy[key]))
    require(j.hash_value(policy['qualificationSourceRevision'],40))
    require(sha(canonical(policy))==plan['legacyStopPolicyHash'])
    require(original['planHash']==sha(canonical(plan)))
    saved=original['originalDeployment'];binding=policy['api']
    require(isinstance(binding,dict) and set(binding)=={'containerId','image','imageId'}
            and j.hash_value(binding['containerId']) and binding['image']==IMAGE
            and isinstance(binding['imageId'],str) and binding['imageId'].startswith('sha256:') and j.hash_value(binding['imageId'][7:])
            and binding==saved['expected']['api'])
    api=saved['containers']['api']
    require(api['Id']==binding['containerId'] and api['Image']==binding['imageId']
            and api['Config']['Image']==IMAGE)
    env={}
    for line in api['Config']['Env']:
        key,sep,value=line.partition('=');require(sep and key not in env);env[key]=value
    require(env.get('SOURCE_COMMIT')==REVISION and env.get('GIT_SHA')==REVISION)
    require(policy['runtimeConfigurationSha256']==saved['runtimeConfigurationSha256']==plan['runtimeHash'])
    return {'schemaVersion':1,'policySha256':plan['legacyStopPolicyHash'],
            'qualificationReferencesVerified':False,'stopAuthorized':False,
            'httpDrainVerified':False,'externalOutcomesKnown':False}


def read(directory,plan,original):
    with j.files.directory(str(directory),private=True) as parent:
        raw,_=o.abort.read_file(parent,NAME,maximum=j.MAX_RECORD)
    envelope=json.loads(raw)
    require(canonical(envelope)==raw and isinstance(envelope,dict)
            and set(envelope)=={'schemaVersion','releaseNonce','planHash','originalAdmissionSha256','policy'}
            and type(envelope['schemaVersion']) is int and envelope['schemaVersion']==1
            and envelope['releaseNonce']==original['releaseNonce']
            and envelope['planHash']==original['planHash']==sha(canonical(plan))
            and envelope['originalAdmissionSha256']==sha(canonical(original)))
    validate(envelope['policy'],plan,original)
    return envelope['policy']


def prepare(journal,directory,policy):
    """Publish exclusively before any maintenance intent; never adopt leftovers."""
    journal.guard();records=journal.records();require(len(records)==1)
    plan=records[0]['event']['plan']
    original=o.read_prepared(directory,records[0]['releaseNonce'],records[0]['planHash'])
    result=validate(policy,plan,original)
    with j.files.directory(str(directory),private=True) as parent:
        a,b=os.fstat(parent),os.fstat(journal.directory_fd)
        require((a.st_dev,a.st_ino)!=(b.st_dev,b.st_ino))
        o.abort.publish(parent,NAME,{'schemaVersion':1,'releaseNonce':original['releaseNonce'],
                                    'planHash':original['planHash'],
                                    'originalAdmissionSha256':sha(canonical(original)),
                                    'policy':policy})
    require(read(directory,plan,original)==policy and journal.records()==records
            and o.read_prepared(directory,records[0]['releaseNonce'],records[0]['planHash'])==original)
    return result
