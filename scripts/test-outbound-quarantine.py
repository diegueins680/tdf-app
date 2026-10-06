#!/usr/bin/env python3
"""Pure policy admission controls; real packet/reboot checks are a separate gate."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('quarantine', ROOT/'ops/hetzner/outbound-quarantine.py')
q = importlib.util.module_from_spec(spec)
spec.loader.exec_module(q)
BRIDGES = ['br-0123456789ab', 'br-123456789abc']


class PolicyTests(unittest.TestCase):
    def fixture(self):
        return {'nftables': copy.deepcopy(q.objects(BRIDGES))}

    def test_exact_policy_and_kernel_handles(self):
        value = self.fixture()
        value['nftables'].insert(0, {'metainfo': {'version': 'test'}})
        for index, row in enumerate(value['nftables'][1:]):
            next(iter(row.values()))['handle'] = index + 1
        self.assertFalse(q.admit(value, BRIDGES)['bootOrderingVerified'])

    def test_missing_drop_rejected(self):
        value = self.fixture()
        value['nftables'].pop()
        with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_established_outbound_permit_rejected(self):
        value = self.fixture()
        value['nftables'][-2]['rule']['expr'][1] = {
            'match': {'op': '==', 'left': {'ct': {'key': 'state'}}, 'right': 'established'}}
        with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_ipv4_only_policy_rejected(self):
        value = self.fixture()
        value['nftables'][0]['table']['family'] = 'ip'
        with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_cross_bridge_permit_rejected(self):
        value = self.fixture()
        row = next(r['rule'] for r in value['nftables'] if r.get('rule', {}).get('expr', [{}])[0] == q.match('iifname', BRIDGES[0]))
        row['expr'][1] = q.match('oifname', BRIDGES[1])
        with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_added_accept_and_reordering_rejected(self):
        for reverse in (False, True):
            value = self.fixture()
            if reverse: value['nftables'][-2:] = reversed(value['nftables'][-2:])
            else: value['nftables'].append({'rule': {'family': 'inet', 'table': q.TABLE,
                         'chain': 'routed', 'expr': [{'accept': None}]}})
            with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_unknown_policy_fields_rejected(self):
        value = self.fixture()
        value['nftables'][1]['chain']['flags'] = ['offload']
        with self.assertRaises(ValueError): q.admit(value, BRIDGES)

    def test_unsafe_or_ambiguous_bridge_input_rejected(self):
        for value in ([], ['eth0'], ['docker0'], ['br-*'], BRIDGES * 2, list(reversed(BRIDGES)),
                      ['br-0123456789ab; flush ruleset'], ['br-' + 'a' * 12] * 5):
            with self.assertRaises(ValueError): q.program(value)

    def test_existing_changed_policy_is_not_repaired(self):
        config = {'bridges': BRIDGES}
        with patch.object(q,'persistent_configuration',return_value=config), \
             patch.object(q,'run',return_value=q.canonical({'nftables':[{'table':{'name':q.TABLE}}]})), \
             patch.object(q,'observe',side_effect=ValueError('changed')), \
             patch.object(q,'load_absent') as load:
            with self.assertRaises(ValueError):q.enforce()
            load.assert_not_called()

    def test_boot_loads_absent_policy(self):
        config = {'bridges': BRIDGES}
        with patch.object(q,'persistent_configuration',return_value=config), \
             patch.object(q,'run',return_value=q.canonical({'nftables':[]})), \
             patch.object(q,'load_absent',return_value={'loaded':True}) as load:
            self.assertEqual(q.enforce(),{'loaded':True})
            load.assert_called_once_with(BRIDGES)

    def test_configuration_change_during_enforcement_denies(self):
        with patch.object(q,'persistent_configuration',side_effect=[{'bridges':BRIDGES},{'bridges':BRIDGES[:1]}]), \
             patch.object(q,'run',return_value=q.canonical({'nftables':[]})), \
             patch.object(q,'load_absent',return_value={}):
            with self.assertRaises(ValueError):q.enforce()


if __name__ == '__main__': unittest.main()
