#!/usr/bin/env python3
"""Drift controls for a trusted, separately qualified firewall policy."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('ufw_admission',ROOT/'ops/hetzner/ufw-recovery-admission.py')
u=importlib.util.module_from_spec(spec);spec.loader.exec_module(u)


class AdmissionTests(unittest.TestCase):
    def setUp(self):
        def row(path):return {'path':path,'sha256':'a'*64,'mode':0o640,'uid':0,'bytes':20}
        self.snapshot={'schemaVersion':1,'version':'0.36.2-6','backend':{'iptables':'nf_tables'},
            'binaries':[{'name':'iptables','entry':'/usr/sbin/iptables','resolved':row('/usr/sbin/xtables-nft-multi')}],
            'implementation':[row('/usr/lib/ufw/ufw-init')],
            'configuration':[row('/etc/ufw/before.init'),row('/etc/default/ufw')],
            'unit':{'ActiveState':'active','DropInPaths':''},'unitFiles':[row('/usr/lib/systemd/system/ufw.service')]}
        self.policy={'schemaVersion':1,'qualification':{'sourceRevision':'b'*40,
            'packetEvidenceSha256':'c'*64,'rebootEvidenceSha256':'d'*64},'snapshot':copy.deepcopy(self.snapshot)}

    def test_match_is_identity_evidence_not_complete_host_admission(self):
        result=u.admit(self.snapshot,self.policy)
        self.assertTrue(result['ufwIdentityMatchesReviewedQualification'])
        self.assertFalse(result['hostBypassAdmissionVerified'])

    def test_changed_rules_hooks_implementation_and_unit_rejected(self):
        for category in ('configuration','implementation','unitFiles'):
            for field,value in (('sha256','e'*64),('mode',0o750),('uid',1000),('path','/changed')):
                with self.subTest(category=category,field=field):
                    changed=copy.deepcopy(self.snapshot);changed[category][0][field]=value
                    with self.assertRaises(ValueError):u.admit(changed,self.policy)

    def test_new_config_or_unit_dropin_rejected(self):
        for category in ('configuration','unitFiles'):
            changed=copy.deepcopy(self.snapshot);changed[category].append({'path':'/new'})
            with self.assertRaises(ValueError):u.admit(changed,self.policy)

    def test_changed_selected_backend_or_binary_rejected(self):
        for mutate in (lambda s:s['backend'].update(iptables='legacy'),
                       lambda s:s['binaries'][0].update(entry='/new/iptables'),
                       lambda s:s['binaries'][0]['resolved'].update(sha256='e'*64)):
            changed=copy.deepcopy(self.snapshot);mutate(changed)
            with self.assertRaises(ValueError):u.admit(changed,self.policy)

    def test_unqualified_version_or_unit_state_rejected(self):
        for mutate in (lambda s:s.update(version='unqualified'),
                       lambda s:s['unit'].update(ActiveState='inactive'),
                       lambda s:s['unit'].update(DropInPaths='/new.conf')):
            changed=copy.deepcopy(self.snapshot);mutate(changed)
            with self.assertRaises(ValueError):u.admit(changed,self.policy)

    def test_missing_or_malformed_evidence_rejected(self):
        for key in self.policy['qualification']:
            bad=copy.deepcopy(self.policy);del bad['qualification'][key]
            with self.assertRaises(ValueError):u.admit(self.snapshot,bad)
            bad=copy.deepcopy(self.policy);bad['qualification'][key]='not-an-evidence-hash'
            with self.assertRaises(ValueError):u.admit(self.snapshot,bad)

    def test_empty_snapshot_cannot_become_qualification(self):
        self.policy['snapshot']={}
        with self.assertRaises(ValueError):u.admit({},self.policy)

    def test_change_between_live_observations_rejected(self):
        changed=copy.deepcopy(self.snapshot);changed['configuration'][0]['mode']=0o750
        with patch.object(u,'observe',side_effect=[self.snapshot,changed]):
            with self.assertRaises(ValueError):u.observe_qualified(self.policy)


if __name__=='__main__':unittest.main()
