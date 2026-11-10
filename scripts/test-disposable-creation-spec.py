#!/usr/bin/env python3
"""Admission reconstruction tests; Docker/boot effects are outside this fixture."""
import copy
import importlib.util
import shutil
import tempfile
from pathlib import Path
from types import SimpleNamespace
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent

def load(name,path):
    spec=importlib.util.spec_from_file_location(name,ROOT/path)
    value=importlib.util.module_from_spec(spec);spec.loader.exec_module(value);return value

s=load('disposable_specs','ops/hetzner/disposable-creation-spec.py')
p=load('physical_spec_fixture','scripts/test-physical-postgres-recovery.py')
c=load('canary_spec_fixture','scripts/test-isolated-application-canary.py')
ORIGINALS={'db':c.SOURCE,'api':'6'*64,'edge':'7'*64}
REGIONAL={'SUPPORTED_LOCALES':'en,es','DEFAULT_LOCALE':'es','SUPPORTED_CURRENCIES':'USD','DEFAULT_CURRENCY':'USD'}


def identity(path):
    return {'path':str(path),'device':1,'inode':int(s.sha(str(path).encode())[:8],16),
            'uid':0,'gid':0,'mode':0o700}


class CreationSpecTests(unittest.TestCase):
    def setUp(self):
        self.enterContext(patch.object(s.original,'directory_identity',side_effect=identity))
        self.physical=s.physical.PhysicalClone(c.SOURCE,'pgvector/pgvector@sha256:'+'2'*64,
            'sha256:'+'2'*64,c.NONCE,c.DIRECTORY,'12345')
        self.physical.prepared_manifest={}
        self.application=s.canary.Canary(s.physical.restore,SimpleNamespace(source=c.SOURCE,target=c.DB,nonce=c.NONCE),
            c.DIRECTORY,c.IMAGE,c.REV)
        self.application.image_id=c.IMAGE_ID
        self.application.regional_configuration=REGIONAL.copy()
        env={**s.canary.ENVIRONMENT,**REGIONAL}
        self.application.runtime_command=['env','-i',*[key+'='+value for key,value in env.items()],'/app/production-entrypoint.sh']
        self.specs={role:s.snapshot(role,obj,ORIGINALS) for role,obj in
            [('physical-database',self.physical),('application-canary',self.application)]}
        database=p.PhysicalRecoveryTests.inspection(SimpleNamespace(clone=self.physical,data=self.physical.data))
        database.update(Id=c.DB,Name='/tdf-audit-restore-'+c.NONCE)
        application=c.container();application['Name']='/'+self.application.name
        application['Config']['Cmd']=self.application.runtime_command
        self.containers={'physical-database':database,'application-canary':application}

    def test_exact_both_roles_reconstruct_same_fixed_commands_and_admit(self):
        for role,value in self.specs.items():
            self.assertEqual(s.admit(value,self.containers[role],ORIGINALS),self.containers[role]['Id'])
            self.assertNotIn('command',value)

    def test_absent_dependency_does_not_trigger_inspection_or_start(self):
        with patch.object(s.canary.subprocess,'run') as run:
            self.assertEqual(s.admit(self.specs['application-canary'],self.containers['application-canary'],ORIGINALS),c.APP)
        run.assert_not_called()

    def test_original_containers_are_never_admitted_as_disposables(self):
        for original in ORIGINALS.values():
            for role,value in self.specs.items():
                row=copy.deepcopy(self.containers[role]);row['Id']=original
                with self.subTest(role=role,original=original),self.assertRaises(ValueError):s.admit(value,row,ORIGINALS)

    def test_original_api_or_edge_cannot_be_the_source_database(self):
        for source in [ORIGINALS['api'],ORIGINALS['edge']]:
            for value in self.specs.values():
                wrong=copy.deepcopy(value);wrong['sourceContainer']=source
                with self.assertRaises(ValueError):s.reconstruct(wrong,ORIGINALS)

    def test_schema_role_hash_and_directory_rebinding_are_rejected(self):
        for value in self.specs.values():
            for key,replacement in [('schemaVersion',True),('schemaVersion',2),('role','unknown'),
                    ('nonce','e'*32),('admissionPolicySha256','0'*64),('commandSha256','0'*64),('directory','/tmp/other'),('name','adopted')]:
                wrong=copy.deepcopy(value);wrong[key]=replacement
                with self.subTest(key=key),self.assertRaises(ValueError):s.reconstruct(wrong,ORIGINALS)
            wrong=copy.deepcopy(value);wrong['command']=['docker','rm','--force',c.SOURCE]
            with self.assertRaises(ValueError):s.reconstruct(wrong,ORIGINALS)

    def test_directory_replacement_and_identity_loss_reject(self):
        for role,value in self.specs.items():
            wrong=copy.deepcopy(value);wrong['directoryIdentities'][1]['inode']+=1
            with self.assertRaises(ValueError):s.admit(wrong,self.containers[role],ORIGINALS)
            wrong=copy.deepcopy(value);wrong['directoryIdentities'].pop()
            with self.assertRaises(ValueError):s.reconstruct(wrong,ORIGINALS)

    def test_same_label_but_different_image_command_mount_or_policy_rejects(self):
        for role,value in self.specs.items():
            for mutate in [lambda row:row.update(Name='/other'),lambda row:row.update(Image='sha256:'+'0'*64),
                    lambda row:row['Config'].update(Cmd=['arbitrary']),
                    lambda row:row['HostConfig'].update(NetworkMode='host'),
                    lambda row:row['HostConfig'].update(AutoRemove=True),
                    lambda row:row['Mounts'][0].update(Source='/opt/tdf/production')]:
                row=copy.deepcopy(self.containers[role]);mutate(row)
                with self.subTest(role=role),self.assertRaises(ValueError):s.admit(value,row,ORIGINALS)

    def test_regional_environment_cannot_introduce_credentials_or_changed_defaults(self):
        for wrong in [{**REGIONAL,'PAYPAL_CLIENT_SECRET':'synthetic'},
                      {**REGIONAL,'DEFAULT_LOCALE':'fr'},
                      {**REGIONAL,'SUPPORTED_LOCALES':'es\nTOKEN=bad'},
                      {**REGIONAL,'SUPPORTED_CURRENCIES':'USD,USD'}]:
            with self.assertRaises(ValueError):s.regional_configuration(wrong)

    def test_pre_creation_snapshot_rejects_already_dispatched_objects(self):
        for role,obj in [('physical-database',self.physical),('application-canary',self.application)]:
            obj.creation_attempted=True
            with self.assertRaises(ValueError):s.snapshot(role,obj,ORIGINALS)

    def test_policy_replaced_after_import_cannot_label_loaded_validators_as_new(self):
        with tempfile.TemporaryDirectory() as directory:
            copied=Path(directory)
            for source in (ROOT/'ops/hetzner').glob('*.py'):
                shutil.copyfile(source,copied/source.name)
            imported=load('changed_policy_fixture',copied/'disposable-creation-spec.py')
            initial=imported.policy_digest()
            changed=copied/'physical-postgres-recovery.py'
            changed.write_bytes(changed.read_bytes()+b'\n# synthetic replacement after import\n')
            with self.assertRaises(ValueError):imported.policy_digest()
            self.assertEqual(imported.INITIAL_POLICY_DIGEST,initial)

    def test_bad_original_inventory_rejected(self):
        for originals in [list(ORIGINALS.values()),{}, {**ORIGINALS,'other':'9'*64},
                          {**ORIGINALS,'api':c.SOURCE},{**ORIGINALS,'api':None}]:
            with self.assertRaises(ValueError):s.reconstruct(self.specs['physical-database'],originals)


if __name__=='__main__':unittest.main(verbosity=2)
