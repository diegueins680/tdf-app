#!/usr/bin/env python3
"""Mutation checks for the deliberately narrow BPF admission subset."""
import copy
import hashlib
import importlib.util
from pathlib import Path
import struct
import unittest
from unittest.mock import patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('bpf_admission',ROOT/'ops/hetzner/bpf-recovery-admission.py')
b=importlib.util.module_from_spec(spec);spec.loader.exec_module(b)


def refresh(program):
    raw=b''.join(struct.pack('<BBhi',*row) for row in program['instructions'])
    program['translatedBytes']=len(raw);program['translatedSha256']=hashlib.sha256(raw).hexdigest()


def program(kind,pid,code,maps=(),calls=()):
    row={'type':kind,'id':pid,'mapIds':list(maps),'resolvedCalls':list(calls),'instructions':code,
         'createdByUid':0,'ifindex':0,'netnsDevice':0,'netnsInode':0,'name':'diagnostic-only'}
    refresh(row);return row


def fixture():
    pure=program(15,1,[[97,18,0,0],[84,2,0,65535],[183,0,0,1],[149,0,0,0]])
    hid=program(26,2,[[121,18,0,0],[97,35,0,0],[24,18,0,7],
                     [0,0,0,0],[133,0,0,12],[183,0,0,0],[149,0,0,0]],(7,))
    address=program(8,3,[[191,22,0,0],[105,103,180,0],[180,8,0,0],[85,7,14,8],
              [191,97,0,0],[180,2,0,16],[191,163,0,0],[7,3,0,-4],
              [180,4,0,4],[133,0,0,100],[24,17,0,9],[0,0,0,0],
              [191,162,0,0],[7,2,0,-8],[98,2,0,32],[133,0,0,200],
              [21,0,1,0],[68,8,0,1],[68,8,0,2],[183,0,0,1],[85,8,1,2],
              [183,0,0,0],[149,0,0,0]],(9,),
              ({'index':9,'immediate':100,'symbols':['bpf_skb_load_bytes']},
               {'index':15,'immediate':200,'symbols':['trie_lookup_elem']}))
    return {'schemaVersion':1,'enumerationComplete':True,'kernelRelease':'qualified-kernel',
            'vmlinuxBtfSha256':'a'*64,'programs':[pure,hid,address],
            'maps':{'7':{'type':3,'id':7,'keySize':4,'valueSize':4,'maxEntries':1024,
                          'flags':0,'slotsRead':1024,'populatedSlots':[]},
                    '9':{'type':11,'id':9,'keySize':8,'valueSize':8,'maxEntries':1,'flags':1}},
            'links':[{'type':2,'id':1,'programId':2,'attachType':26,'targetObjectId':1,'targetBtfId':500,
                      'targetBtf':{'kind':12,'name':'__hid_bpf_tail_call'}}],
            'btfObjects':{'1':{'id':1,'name':'vmlinux','kernel':1,'sha256':'a'*64}}}


class AdmissionTests(unittest.TestCase):
    def setUp(self):self.snapshot=fixture()

    def rejects(self,mutate):
        changed=copy.deepcopy(self.snapshot);mutate(changed)
        for p in changed['programs']:refresh(p)
        with self.assertRaises(ValueError):b.check_inventory(changed)

    def test_qualified_shapes_pass_without_host_isolation_claim(self):
        q={'kernelRelease':'qualified-kernel','vmlinuxBtfSha256':'a'*64,
           'sourceRevision':'b'*40,'inspectionEvidenceSha256':'c'*64}
        result=b.admit(self.snapshot,q)
        self.assertEqual(result['counts'],{'pureVerdict':1,'addressVerdict':1,'hidEmptyTailCall':1})
        self.assertFalse(result['hostBypassAdmissionVerified'])

    def test_names_do_not_establish_authority(self):
        for p in self.snapshot['programs']:p['name']='unrelated-name'
        b.check_inventory(self.snapshot)

    def test_store_helper_kfunc_and_two_slot_load_rejected_in_pure_subset(self):
        for opcode,registers in ((98,2),(133,0),(133,32),(24,1),(195,2)):
            with self.subTest(opcode=opcode,registers=registers):
                self.rejects(lambda s:s['programs'][0]['instructions'].insert(0,[opcode,registers,0,1]))

    def test_backward_and_out_of_bounds_branches_reject(self):
        for offset in (-1,999):
            self.rejects(lambda s:s['programs'][0]['instructions'].insert(0,[5,0,offset,0]))

    def test_other_program_types_and_offloaded_identity_reject(self):
        for field,value in (('type',6),('type',3),('ifindex',1),('netnsInode',1),('createdByUid',1000)):
            self.rejects(lambda s:s['programs'][0].update({field:value}))

    def test_changed_helper_target_or_stack_destination_rejects(self):
        self.rejects(lambda s:s['programs'][2]['resolvedCalls'][0].update(symbols=['bpf_redirect']))
        self.rejects(lambda s:s['programs'][2]['instructions'][14].__setitem__(1,1))
        self.rejects(lambda s:s['programs'][2]['instructions'].insert(0,[183,0,0,1]))

    def test_stale_symbol_resolution_cannot_cover_changed_call_bytes(self):
        for index in (9,15):
            self.rejects(lambda s:s['programs'][2]['instructions'][index].__setitem__(3,1234567))

    def test_map_shape_and_relocation_reject(self):
        for mutate in (lambda s:s['maps']['9'].update(type=3),lambda s:s['maps']['9'].update(valueSize=4),
                       lambda s:s['programs'][2]['instructions'][10].__setitem__(3,7),
                       lambda s:s['programs'][2].update(mapIds=[7])):self.rejects(mutate)

    def test_hid_wrong_attachment_btf_identity_or_return_rejects(self):
        for mutate in (lambda s:s['links'][0].update(attachType=24),
                       lambda s:s['links'][0]['targetBtf'].update(name='other_function'),
                       lambda s:s['btfObjects']['1'].update(sha256='d'*64),
                       lambda s:s['programs'][1]['instructions'][5].__setitem__(3,1)):
            self.rejects(mutate)

    def test_populated_or_partially_read_tail_array_rejects(self):
        self.rejects(lambda s:s['maps']['7'].update(populatedSlots=[{'slot':1023,'programId':2}]))
        self.rejects(lambda s:s['maps']['7'].update(slotsRead=1023))
        self.rejects(lambda s:s['maps']['7'].update(maxEntries=2048))

    def test_unknown_link_or_incomplete_enumeration_rejects(self):
        self.rejects(lambda s:s['links'].append({'type':7,'id':2,'programId':1}))
        self.rejects(lambda s:s.update(enumerationComplete=False))
        self.rejects(lambda s:s['programs'].append(copy.deepcopy(s['programs'][0])))

    def test_kernel_evidence_cannot_be_reused_for_another_kernel(self):
        q={'kernelRelease':'other-kernel','vmlinuxBtfSha256':'a'*64,
           'sourceRevision':'b'*40,'inspectionEvidenceSha256':'c'*64}
        with self.assertRaises(ValueError):b.admit(self.snapshot,q)

    def test_changed_inventory_between_samples_rejects(self):
        changed=copy.deepcopy(self.snapshot);changed['programs'][0]['id']=99
        with patch.object(b,'observe_once',side_effect=[self.snapshot,changed]),self.assertRaises(ValueError):b.observe()

    def test_source_hash_is_bound_to_decoded_instruction_bytes(self):
        self.snapshot['programs'][0]['translatedSha256']='e'*64
        with self.assertRaises(ValueError):b.check_inventory(self.snapshot)


if __name__=='__main__':unittest.main()
