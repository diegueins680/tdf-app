#!/usr/bin/env python3
"""Drift controls for a trusted, separately qualified firewall policy."""
import copy
import importlib.util
import itertools
from pathlib import Path
import subprocess
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
            'unit':{'ActiveState':'active','DropInPaths':''},'unitFiles':[row('/usr/lib/systemd/system/ufw.service')],
            'settings':{'enabled':True,'ipv6':True,'manageBuiltins':False},
            'kernelRules':{'ipv4':{'state':'hooked'},'ipv6':{'state':'hooked'}}}
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

    def test_active_service_is_not_enabled_filtering(self):
        changed=copy.deepcopy(self.snapshot)
        changed['settings']['enabled']=False
        policy=copy.deepcopy(self.policy);policy['snapshot']=changed
        with self.assertRaises(ValueError):u.admit(changed,policy)
        for row in changed['kernelRules'].values():row['state']='absent'
        self.assertFalse(u.admit(changed,policy)['hostBypassAdmissionVerified'])

    def test_partial_filter_or_unsafe_loader_defaults_rejected_even_in_policy(self):
        for mutate in (lambda s:s['settings'].update(ipv6=False),
                       lambda s:s['settings'].update(manageBuiltins=True),
                       lambda s:s['kernelRules']['ipv6'].update(state='partial')):
            changed=copy.deepcopy(self.snapshot);mutate(changed)
            policy=copy.deepcopy(self.policy);policy['snapshot']=changed
            with self.assertRaises(ValueError):u.admit(changed,policy)

    def test_kernel_presence_requires_hooks_not_just_chain_names(self):
        with patch.object(u,'run',return_value='-N ufw-before-input\n-A ufw-before-input -j ACCEPT'):
            self.assertEqual(u.kernel_rules('iptables')['state'],'partial')
        with patch.object(u,'run',return_value='-P INPUT ACCEPT\n-N DOCKER'):
            self.assertEqual(u.kernel_rules('iptables')['state'],'absent')
        rules='\n'.join('-A '+chain+' -j ufw-before-'+chain.lower() for chain in ('INPUT','OUTPUT','FORWARD'))
        with patch.object(u,'run',return_value=rules):
            self.assertEqual(u.kernel_rules('iptables')['state'],'hooked')

    def test_conditional_jump_drift_is_observed_but_not_full_hook(self):
        with patch.object(u,'run',return_value='-A INPUT -p tcp -j ufw-before-input'):
            row=u.kernel_rules('iptables');self.assertEqual(row['state'],'partial')
        with patch.object(u,'run',return_value=''):
            self.assertNotEqual(row['rulesSha256'],u.kernel_rules('iptables')['rulesSha256'])

    def test_literal_settings_follow_shell_values_without_executing_shell(self):
        for raw,expected in ((b'ENABLED=yes\n',True),(b'ENABLED="no"\n',False),
                             (b"# comment\nENABLED='YES'\nIPT_MODULES=\"\"\n",True)):
            with patch.object(u,'fingerprint',return_value=({'sha256':'test'},raw)):
                self.assertEqual(u.configured_setting('/unused','ENABLED',{'sha256':'test'}),expected)

    def test_overrides_and_unsupported_shell_syntax_are_rejected(self):
        for raw in (b'ENABLED=yes\nENABLED="no"',b'ENABLED=no\nexport ENABLED=yes',
                    b'enabled=yes',b'ENABLED=yes\nif true; then ENABLED=no; fi',
                    b'ENABLED=$(echo yes)',b'ENABLED=yes; ENABLED=no',
                    b'ENABLED=yes\nOTHER=`command`',b'ENABLED=yes\n. /extra/config',
                    b'ENABLED=yes\nOTHER=one\nOTHER=two',b'ENABLED=yes\r\n',
                    b'ENABLED=yes\v',b'ENABLED=yes\f',b'ENABLED=yes\x85',
                    '\u00a0ENABLED=yes'.encode(),'ENABLED=yes\u2028'.encode()):
            with self.subTest(raw=raw),patch.object(u,'fingerprint',return_value=({'sha256':'test'},raw)):
                with self.assertRaises(ValueError):u.configured_setting('/unused','ENABLED',{'sha256':'test'})

    def test_accepted_fixed_domain_settings_agree_with_actual_shell(self):
        # Only these fixed synthetic literals are executed, never a host config.
        # This bounded domain covers quotes, case, whitespace and CRLF controls.
        accepted=0
        for prefix,quote,value,ending in itertools.product(('', ' ', '\t'),('',"'",'"'),
                ('yes','YES','no','NO'),('', '\n', ' \t\n', '\r\n')):
            source=prefix+'ENABLED='+quote+value+quote+ending
            with patch.object(u,'fingerprint',return_value=({'sha256':'test'},source.encode())):
                try:parsed=u.configured_setting('/unused','ENABLED',{'sha256':'test'})
                except ValueError:continue
            result=subprocess.run(['/bin/sh','-c',source+'\nprintf %s "$ENABLED"'],
                    env={'PATH':'/usr/bin:/bin'},capture_output=True,timeout=2)
            self.assertEqual(result.returncode,0)
            self.assertEqual(parsed,result.stdout in (b'yes',b'YES'),repr(source))
            accepted+=1
        self.assertEqual(accepted,108)


if __name__=='__main__':unittest.main()
