#!/usr/bin/env python3
"""Packet-path provenance mutations, without privileged network changes."""
import copy
import importlib.util
import struct
from pathlib import Path
import unittest
from unittest.mock import patch, MagicMock

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('network_admission',ROOT/'ops/hetzner/network-recovery-admission.py')
n=importlib.util.module_from_spec(spec);spec.loader.exec_module(n)
CID='a'*64;NID='b'*64;ENDPOINT='c'*64;BRIDGE='br-'+NID[:12];MAC='02:00:00:00:00:01'


def network(links):
    defaults=[{'priority':0,'src':'all','table':'local'},{'priority':32766,'src':'all','table':'main'}]
    return {'links':links,'addresses':[],'routes4':[],'routes6':[],
            'rules4':defaults+[{'priority':32767,'src':'all','table':'default'}],'rules6':defaults,
            'nft':{'nftables':[]},'qdiscs':[{'dev':row['ifname'],'kind':'noqueue','root':True} for row in links],
            'tcFilters':{row['ifname']:{k:[] for k in ('root','ingress','egress')} for row in links},
            'legacyAndPacketSockets':{**{k:None for k in ('ip_tables_names','ip6_tables_names','arp_tables_names','eb_tables_names')},'packetSockets':[]}}


def fixture():
    lo={'ifindex':1,'ifname':'lo','link_type':'loopback'}
    host=network([lo,{'ifindex':2,'ifname':'eth0','link_type':'ether'},
        {'ifindex':3,'ifname':BRIDGE,'link_type':'ether','linkinfo':{'info_kind':'bridge','info_data':{'vlan_filtering':0,'stp_state':0}}},
        {'ifindex':4,'ifname':'veth-test','link_index':2,'link_netnsid':7,'master':BRIDGE,'linkinfo':{'info_kind':'veth'}}])
    container=network([copy.deepcopy(lo),{'ifindex':2,'ifname':'eth0','link_index':4,'link_netnsid':9,'address':MAC,'linkinfo':{'info_kind':'veth'}}])
    return {'schemaVersion':1,'hostNamespaceInode':1,'host':host,
      'runningContainers':[{'id':CID,'pid':10,'namespaceInode':2,'peerNamespaceIds':{'containerInHost':7,'hostInContainer':9},'network':container,'networkMode':'private',
        'privileged':False,'capAdd':None,'networks':{'private':{'NetworkID':NID,'EndpointID':ENDPOINT,'MacAddress':MAC}}}],
      'dockerNetworks':[{'Id':NID,'Name':'private','Driver':'bridge','Scope':'local','Options':{},
        'Containers':{CID:{'EndpointID':ENDPOINT,'MacAddress':MAC}}}]}


def rule(expression):
    return {'rule':{'family':'ip','table':'filter','chain':'FORWARD','handle':1,'expr':[expression]}}


class AdmissionTests(unittest.TestCase):
    def setUp(self):
        self.snapshot=fixture();self.policy={'runningContainerIds':[CID],'admittedNetworkIds':[NID],'uplink':'eth0'}

    def rejects(self,mutate):
        changed=copy.deepcopy(self.snapshot);mutate(changed)
        with self.assertRaises(ValueError):n.topology(changed,self.policy)

    def test_complete_pair_passes_without_complete_host_claim(self):
        result=n.topology(self.snapshot,self.policy)
        self.assertEqual(result['boundVethPairs'],1);self.assertFalse(result['hostBypassAdmissionVerified'])

    def test_host_macvlan_or_custom_bridge_cannot_bypass_prefix(self):
        for mode in ('host','none','bridge','container:other'):
            self.rejects(lambda s:s['runningContainers'][0].update(networkMode=mode))
        self.rejects(lambda s:s['dockerNetworks'][0].update(Driver='macvlan'))
        self.rejects(lambda s:s['dockerNetworks'][0]['Options'].update({'com.docker.network.bridge.name':'custom0'}))

    def test_unknown_running_container_or_endpoint_rejects(self):
        self.rejects(lambda s:s['runningContainers'][0].update(id='d'*64))
        self.rejects(lambda s:s['dockerNetworks'][0]['Containers'].update({'d'*64:{}}))
        self.rejects(lambda s:s['runningContainers'][0]['networks']['private'].update(EndpointID='d'*64))

    def test_changed_peer_master_namespace_or_privilege_rejects(self):
        self.rejects(lambda s:s['host']['links'][3].update(master='external'))
        self.rejects(lambda s:s['host']['links'][3].update(link_index=99))
        self.rejects(lambda s:s['runningContainers'][0].update(namespaceInode=1))
        self.rejects(lambda s:s['runningContainers'][0].update(privileged=True))
        self.rejects(lambda s:s['runningContainers'][0].update(capAdd=['NET_ADMIN']))

    def test_readonly_namespace_query_protocol(self):
        scope={};exec(n.NETNS_QUERY,scope)
        good=struct.pack('=IHHII',28,88,0,1,123)+bytes(4)+struct.pack('=HHi',8,1,7)
        mock=MagicMock();sock=mock.return_value.__enter__.return_value
        sock.getsockname.return_value=(123,0);sock.sendto.return_value=28
        sock.recvmsg.return_value=(good,[],0,(0,0))
        with patch.object(scope['socket'],'socket',mock), patch.object(scope['socket'],'AF_NETLINK',16,create=True), patch.object(scope['socket'],'NETLINK_ROUTE',0,create=True):
            self.assertEqual(scope['query_nsid'](42),7)
            sent=sock.sendto.call_args.args
            self.assertEqual(sent,(struct.pack('=IHHII',28,90,1,1,123)+bytes(4)+struct.pack('=HHI',8,3,42),(0,0)))
            invalid=[(good,[],0,(99,0)),(good,[],32,(0,0)),(good+b'extra',[],0,(0,0)),
                     (good[:-4]+struct.pack('=i',-1),[],0,(0,0))]
            for offset,fmt,value in ((4,'H',2),(8,'I',2),(12,'I',999),(22,'H',3)):
                changed=bytearray(good);struct.pack_into('='+fmt,changed,offset,value)
                invalid.append((bytes(changed),[],0,(0,0)))
            for result in invalid:
                sock.recvmsg.return_value=result
                with self.assertRaises(ValueError):scope['query_nsid'](42)

    def test_namespace_mapping_is_required_in_both_directions(self):
        self.rejects(lambda s:s['runningContainers'][0].pop('peerNamespaceIds'))
        for key in ('containerInHost', 'hostInContainer'):
            for value in (-1, None, True, '7', 999):
                self.rejects(lambda s:s['runningContainers'][0]['peerNamespaceIds'].update({key:value}))
        self.rejects(lambda s:s['host']['links'][3].pop('link_netnsid'))
        self.rejects(lambda s:s['runningContainers'][0]['network']['links'][1].pop('link_netnsid'))

    def test_duplicate_interface_index_in_third_namespace_is_not_a_peer(self):
        # Index reciprocity and bridge/MAC/endpoint metadata still match.
        self.rejects(lambda s:s['host']['links'][3].update(link_netnsid=999))
        self.rejects(lambda s:s['runningContainers'][0]['network']['links'][1].update(link_netnsid=998))

    def test_unknown_bridge_port_rejects_even_when_network_inventory_complete(self):
        def mutate(s):
            s['host']['links'].append({'ifname':'veth-unknown','ifindex':5,'master':BRIDGE,'linkinfo':{'info_kind':'veth'}})
            s['host']['tcFilters']['veth-unknown']={k:[] for k in ('root','ingress','egress')}
            s['host']['qdiscs'].append({'dev':'veth-unknown','kind':'noqueue','root':True})
        self.rejects(mutate)

    def test_tc_xdp_or_missing_qdisc_inventory_rejects(self):
        self.rejects(lambda s:s['host']['links'][1].update(xdp={'prog':{'id':99}}))
        self.rejects(lambda s:s['host']['tcFilters']['eth0']['ingress'].append({'kind':'bpf'}))
        self.rejects(lambda s:s['host']['qdiscs'].pop())

    def test_duplication_queue_and_opaque_tee_actions_reject(self):
        for expression in ({'dup':{'addr':'192.0.2.1'}},{'fwd':{'dev':'eth0'}},{'queue':{'num':0}},
                           {'xt':{'type':'target','name':'TEE'}},{'xt':{'type':'target','name':'NFQUEUE'}}):
            self.rejects(lambda s:s['host']['nft']['nftables'].append(rule(expression)))

    def test_flowtables_and_legacy_tables_reject(self):
        self.rejects(lambda s:s['host']['nft']['nftables'].append({'flowtable':{'name':'fast'}}))
        self.rejects(lambda s:s['host']['legacyAndPacketSockets'].update(ip_tables_names='mangle\n'))

    def test_packet_consumer_must_be_external_lldp_only(self):
        row={'type':3,'protocol':0x88cc,'interface':2,'uid':998,'inode':100}
        self.snapshot['host']['legacyAndPacketSockets']['packetSockets']=[row]
        n.topology(self.snapshot,self.policy)
        for key,value in (('interface',3),('protocol',0x800),('type',2)):
            self.rejects(lambda s:s['host']['legacyAndPacketSockets']['packetSockets'][0].update({key:value}))

    def test_policy_routes_and_encapsulation_reject(self):
        self.rejects(lambda s:s['host']['rules4'].append({'priority':100,'table':'custom'}))
        self.rejects(lambda s:s['host']['routes4'].append({'dst':'default','dev':'eth0','encap':{'type':'bpf'}}))

    def test_nat_is_allowed_but_unknown_semantics_are_not(self):
        for target in ('DNAT','SNAT','MASQUERADE'):
            n.nft_boundary({'nftables':[rule({'xt':{'type':'target','name':target}})]})
        with self.assertRaises(ValueError):n.nft_boundary({'nftables':[rule({'mangle':{'key':{'meta':{'key':'mark'}},'value':1}})]})

    def test_counters_are_not_policy_but_new_rules_are(self):
        first=fixture();second=fixture()
        first['host']['nft']['nftables']=[rule({'counter':{'packets':1,'bytes':20}})]
        second['host']['nft']['nftables']=[rule({'counter':{'packets':2,'bytes':40}})]
        self.assertEqual(n.stable(first),n.stable(second))
        second['host']['nft']['nftables'].append(rule({'drop':None}))
        self.assertNotEqual(n.stable(first),n.stable(second))

    def test_change_between_samples_rejects(self):
        changed=copy.deepcopy(self.snapshot);changed['runningContainers'][0]['pid']=11
        with patch.object(n,'observe_once',side_effect=[self.snapshot,changed]),self.assertRaises(ValueError):n.observe(self.policy)


if __name__=='__main__':unittest.main()
