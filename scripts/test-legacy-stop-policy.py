#!/usr/bin/env python3
"""Real journal/publication checks; legacy host and qualification are synthetic."""
import copy
import importlib.util
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('legacy_policy',ROOT/'ops/hetzner/legacy-stop-policy.py')
m=importlib.util.module_from_spec(spec);spec.loader.exec_module(m)
j=m.j
BINDING={'containerId':'1'*64,'image':m.IMAGE,'imageId':'sha256:'+'2'*64}
POLICY={'schemaVersion':1,'kind':'legacy-sigint-645f','api':BINDING,
        'runtimeConfigurationSha256':'3'*64,'legacySourceRevision':m.REVISION,
        'qualificationSourceRevision':'4'*40,'stopEvidenceSha256':'5'*64,
        'captureEvidenceSha256':'6'*64,'recoveryEvidenceSha256':'7'*64,'restrictionPolicySha256':'8'*64}
BASE={k:('sha256:'+'a'*64 if k.endswith('Image') else 'a'*(40 if k.endswith('Revision') else 64)) for k in j.PLAN_KEYS}
PLAN={**BASE,'runtimeHash':POLICY['runtimeConfigurationSha256'],'schemaVersion':2,'legacyStopPolicyHash':j.sha(j.canonical(POLICY))}
HOST={'machineId':'1'*32,'bootId':'11111111-1111-1111-1111-111111111111'}
SAVED={'runtimeConfigurationSha256':PLAN['runtimeHash'],'expected':{'api':BINDING},
       'containers':{'api':{'Id':BINDING['containerId'],'Image':BINDING['imageId'],
         'Config':{'Image':m.IMAGE,'Env':['SOURCE_COMMIT='+m.REVISION,'GIT_SHA='+m.REVISION]}}}}


class PolicyTests(unittest.TestCase):
    def setUp(self):
        self.temp=tempfile.TemporaryDirectory();self.addCleanup(self.temp.cleanup)
        self.root=Path(self.temp.name).resolve();self.control=self.root/'journal';self.control.mkdir(mode=0o700)
        self.directory=self.root/'prepared';self.directory.mkdir(mode=0o700)
        with j.open_journal(self.control) as current,patch.object(m.o,'observe',return_value=SAVED),patch.object(m.o.abort,'boot_identity',return_value=HOST):
            current.initialize(PLAN,'b'*32)
            m.o.prepare(current,self.directory,{}, {})
            self.original=m.o.read_prepared(self.directory,'b'*32,j.sha(j.canonical(PLAN)))

    def test_durable_binding_is_not_permission_or_qualification(self):
        with j.open_journal(self.control) as current:
            receipt=m.prepare(current,self.directory,POLICY)
            self.assertFalse(receipt['stopAuthorized']);self.assertFalse(receipt['qualificationReferencesVerified'])
            self.assertFalse(receipt['httpDrainVerified']);self.assertFalse(receipt['externalOutcomesKnown'])
            self.assertEqual(len(current.records()),1)
        self.assertEqual(m.read(self.directory,PLAN,self.original),POLICY)

    def test_version_one_is_unchanged_and_version_two_is_closed_shape(self):
        j.validate_plan(BASE);j.validate_plan(PLAN)
        for changed in ({**PLAN,'schemaVersion':True},{**PLAN,'schemaVersion':3},
                        {**PLAN,'legacyStopPolicyHash':True},{**BASE,'schemaVersion':2},
                        {**PLAN,'allowUnclean':True},{**BASE,'legacyStopPolicyHash':'a'*64}):
            with self.assertRaises(ValueError):j.validate_plan(changed)

    def test_changed_policy_cannot_reuse_plan_hash(self):
        for field in ('stopEvidenceSha256','captureEvidenceSha256','recoveryEvidenceSha256','restrictionPolicySha256'):
            with self.assertRaises(ValueError):m.validate({**POLICY,field:'f'*64},PLAN,self.original)

    def test_rehashed_policy_still_requires_exact_prepared_identity(self):
        for field,value in (('containerId','f'*64),('imageId','sha256:'+'f'*64),('image','elsewhere/image@sha256:'+'f'*64)):
            policy=copy.deepcopy(POLICY);policy['api'][field]=value
            plan={**PLAN,'legacyStopPolicyHash':j.sha(j.canonical(policy))}
            original={**self.original,'planHash':j.sha(j.canonical(plan))}
            with self.assertRaises(ValueError):m.validate(policy,plan,original)
        for field,value in (('legacySourceRevision','f'*40),('runtimeConfigurationSha256','f'*64),('kind','allow-any-unclean-stop')):
            policy={**POLICY,field:value};plan={**PLAN,'legacyStopPolicyHash':j.sha(j.canonical(policy))}
            original={**self.original,'planHash':j.sha(j.canonical(plan))}
            with self.assertRaises(ValueError):m.validate(policy,plan,original)

    def test_wrong_or_duplicate_runtime_revision_rejects(self):
        for env in (['SOURCE_COMMIT='+m.REVISION,'GIT_SHA='+'f'*40],
                    ['SOURCE_COMMIT='+m.REVISION,'GIT_SHA='+m.REVISION,'GIT_SHA='+m.REVISION]):
            original=copy.deepcopy(self.original);original['originalDeployment']['containers']['api']['Config']['Env']=env
            with self.assertRaises(ValueError):m.validate(POLICY,PLAN,original)

    def test_cannot_publish_after_maintenance_or_adopt_existing_artifact(self):
        with j.open_journal(self.control) as current:
            m.prepare(current,self.directory,POLICY)
            with self.assertRaises(FileExistsError):m.prepare(current,self.directory,POLICY)
            current.perform('maintenance','c'*64,lambda c:{**c,'evidenceHash':'d'*64})
            with self.assertRaises(ValueError):m.prepare(current,self.directory,POLICY)
        path=self.directory/m.NAME;path.write_bytes(path.read_bytes()+b' ')
        with self.assertRaises(ValueError):m.read(self.directory,PLAN,self.original)

    def test_saved_policy_cannot_move_to_another_release_or_original_admission(self):
        with j.open_journal(self.control) as current:m.prepare(current,self.directory,POLICY)
        for changed in ({**self.original,'releaseNonce':'f'*32},
                        {**self.original,'host':{**HOST,'bootId':'22222222-2222-2222-2222-222222222222'}}):
            with self.assertRaises(ValueError):m.read(self.directory,PLAN,changed)

    def test_publication_failure_preserves_pending_file_without_stage_effect(self):
        with j.open_journal(self.control) as current:
            with patch.object(m.o.abort.os,'fsync',side_effect=OSError('synthetic durability failure')):
                with self.assertRaises(OSError):m.prepare(current,self.directory,POLICY)
            self.assertEqual(len(current.records()),1)
            self.assertTrue((self.directory/(m.NAME+'.pending')).exists())
            self.assertFalse((self.directory/m.NAME).exists())
            with self.assertRaises(FileExistsError):m.prepare(current,self.directory,POLICY)

    def test_ordinary_fence_denies_version_two_before_observation_or_commands(self):
        with j.open_journal(self.control) as current:
            fence=m.o.fence.WriterFence(current,{},PLAN['runtimeHash'],{})
            with patch.object(m.o.fence.sources,'observe') as observe,patch.object(m.o.fence,'execute') as execute:
                with self.assertRaises(ValueError):fence.maintenance()
                observe.assert_not_called();execute.assert_not_called()
                self.assertEqual(len(current.records()),1)

    def test_ordinary_abort_cannot_expose_unrestricted_reboot_for_new_plan(self):
        with m.o.abort.open_abort(self.control) as current:
            with self.assertRaises(ValueError):current.latch(self.original,j.sha(j.canonical(self.original)))
        self.assertFalse((self.control/'abort').exists())


if __name__=='__main__':unittest.main()
