#!/usr/bin/env python3
"""Read-only Docker bridge provenance and alternative packet-path admission.

Caller supplies canonical running/container/bridge identities, previously qualified
UFW/BPF/process/mount boundaries and exclusive release ownership. No installer.
"""
import copy
from contextlib import ExitStack
import hashlib
import json
import os
import re
import select
import subprocess
from pathlib import Path

def require(value):
    if not value:
        raise ValueError('Network recovery admission rejected')

# Fixed read-only request. RTM_GETNSID calls peernet2id(), never alloc_netid().
# Run in the observed namespace while the opposite namespace FD stays inherited.
NETNS_QUERY = r"""
import socket, struct

def query_nsid(fd):
    def check(value):
        if not value: raise ValueError('Namespace identity query rejected')
    check(type(fd) is int and fd >= 0)
    with socket.socket(socket.AF_NETLINK, socket.SOCK_RAW, socket.NETLINK_ROUTE) as sock:
        sock.settimeout(5)
        sock.bind((0, 0))
        port = sock.getsockname()[0]
        # nlmsghdr + aligned rtgenmsg + NETNSA_FD (u32).
        request = struct.pack('=IHHII', 28, 90, 1, 1, port) + bytes(4) + struct.pack('=HHI', 8, 3, fd)
        check(sock.sendto(request, (0, 0)) == len(request))
        data, ancillary, flags, sender = sock.recvmsg(4096)
        check(sender == (0, 0) and not ancillary and not flags and len(data) == 28)
        length, kind, flags, sequence, destination = struct.unpack_from('=IHHII', data)
        check((length, kind, flags, sequence, destination) == (28, 88, 0, 1, port))
        check(data[16:20] == bytes(4))
        size, attribute, nsid = struct.unpack_from('=HHi', data, 20)
        check(size == 8 and attribute == 1 and nsid >= 0)
        return nsid
"""


def observe_once():
    """Read-only metadata, retaining namespace/process handles for the sample."""
    with ExitStack() as held:
        return _observe_once(held)


def _observe_once(held):
    """Read-only namespace/packet-path metadata; no network configuration writes."""
    require(os.geteuid() == 0)
    ENV = {'PATH': '/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8'}

    def hold(fd):
        held.callback(os.close, fd)
        return fd

    host_ns = hold(os.open('/proc/self/ns/net', os.O_RDONLY))
    process_handles = []

    def run(argv, fd=None):
        fds = () if fd is None else fd if isinstance(fd, tuple) else (fd,)
        p = subprocess.run(argv, env=ENV, capture_output=True, text=True, timeout=20, pass_fds=fds)
        require(p.returncode == 0 and len(p.stdout) <= 8 * 1024 ** 2)
        return p.stdout

    def nsid(source, target):
        return json.loads(run(['nsenter', '--net=/proc/self/fd/' + str(source),
                              'python3', '-c', NETNS_QUERY + '\nprint(query_nsid(' + str(target) + '))'],
                             (source, target)))

    def network(fd=None):
        prefix = [] if fd is None else ['nsenter', '--net=/proc/self/fd/' + str(fd)]

        def query(args):
            return json.loads(run(prefix + args, fd))
        links = query(['ip', '-json', '-details', 'link', 'show'])
        filters = {}
        extra = json.loads(run(prefix + ['python3', '-c', "import json;from pathlib import Path;names=('ip_tables_names','ip6_tables_names','arp_tables_names','eb_tables_names','packet');print(json.dumps({name:Path('/proc/net',name).read_text() if Path('/proc/net',name).exists() else None for name in names}))"], fd))
        packet=extra.pop('packet');require(isinstance(packet,str) and len(packet)<=1024**2)
        packet_rows=[]
        for line in packet.splitlines()[1:]:
            fields=line.split();require(len(fields)==9)
            packet_rows.append({'type':int(fields[2]),'protocol':int(fields[3],16),
                                'interface':int(fields[4]),'uid':int(fields[7]),'inode':int(fields[8])})
        extra['packetSockets']=packet_rows  # Never retain kernel pointer columns.
        for link in links:
            name = link['ifname']
            require(re.fullmatch('[A-Za-z0-9_.:-]{1,15}', name))
            filters[name] = {parent: query(['tc', '-json', 'filter', 'show', 'dev', name, parent]) for parent in ('root', 'ingress', 'egress')}
        return {'legacyAndPacketSockets': extra, 'links': links, 'addresses': query(['ip', '-json', 'address', 'show']), 'routes4': query(['ip', '-json', '-4', 'route', 'show', 'table', 'all']), 'routes6': query(['ip', '-json', '-6', 'route', 'show', 'table', 'all']), 'rules4': query(['ip', '-json', '-4', 'rule', 'show']), 'rules6': query(['ip', '-json', '-6', 'rule', 'show']), 'nft': query(['nft', '--json', 'list', 'ruleset']), 'qdiscs': query(['tc', '-json', 'qdisc', 'show']), 'tcFilters': filters}
    d = ['docker', '--host', 'unix:///var/run/docker.sock']
    ids = run(d + ['ps', '--all', '--quiet', '--no-trunc']).split()
    require(len(ids) <= 128 and all((re.fullmatch('[a-f0-9]{64}', v) for v in ids)))
    containers = json.loads(run(d + ['inspect', *ids])) if ids else []
    netids = run(d + ['network', 'ls', '--quiet', '--no-trunc']).split()
    require(len(netids) <= 128 and all((re.fullmatch('[a-f0-9]{64}', v) for v in netids)))
    networks = json.loads(run(d + ['network', 'inspect', *netids]))
    rows = []
    for c in containers:
        if not c['State']['Running']:
            continue
        cid = c['Id']
        pid = c['State']['Pid']
        require(type(pid) is int and pid > 0)
        proc = hold(os.open('/proc/' + str(pid), os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW))
        pf = hold(os.pidfd_open(pid))
        process_handles.append(pf)
        group = os.open('cgroup', os.O_RDONLY | os.O_NOFOLLOW, dir_fd=proc)
        try:
            groups = os.read(group, 65537).decode().splitlines()
        finally:
            os.close(group)
        require(any((line.split(':', 2)[-1].endswith('/docker-' + cid + '.scope') or line.split(':', 2)[-1].endswith('/docker/' + cid) for line in groups)))
        ns = hold(os.open('ns/net', os.O_RDONLY, dir_fd=proc))
        require(not select.select([pf], [], [], 0)[0])
        mappings = {'containerInHost': nsid(host_ns, ns), 'hostInContainer': nsid(ns, host_ns)}
        observed = network(ns)
        require(mappings == {'containerInHost': nsid(host_ns, ns), 'hostInContainer': nsid(ns, host_ns)})
        require(not select.select([pf], [], [], 0)[0])
        closing = json.loads(run(d + ['inspect', cid]))[0]
        require(closing['State']['Running'] and closing['State']['Pid'] == pid and (closing['State']['StartedAt'] == c['State']['StartedAt']) and (closing['NetworkSettings'] == c['NetworkSettings']) and (closing['HostConfig'] == c['HostConfig']))
        rows.append({'id': cid, 'pid': pid, 'namespaceInode': os.fstat(ns).st_ino, 'peerNamespaceIds': mappings, 'network': observed, 'networkMode': c['HostConfig']['NetworkMode'], 'privileged': c['HostConfig']['Privileged'], 'capAdd': c['HostConfig']['CapAdd'], 'networks': c['NetworkSettings']['Networks']})
    require(run(d + ['ps', '--all', '--quiet', '--no-trunc']).split() == ids)
    require(run(d+['network','ls','--quiet','--no-trunc']).split()==netids)
    host_network = network(host_ns)
    require(all(not select.select([fd], [], [], 0)[0] for fd in process_handles))
    return {'schemaVersion': 1,'hostNamespaceInode':os.fstat(host_ns).st_ino,
            'host': host_network, 'runningContainers': rows, 'dockerNetworks': [{k: n[k] for k in ('Id', 'Name', 'Driver', 'Scope', 'Internal', 'EnableIPv6', 'Options', 'Containers', 'IPAM')} for n in networks]}


def nft_boundary(value):
    """Only observed filter/NAT semantics; cloning, queueing and flowtables reject.

    Opaque xtables compatibility records are admitted by target/match class,
    never treated as generic harmless expressions. Other families/hooks and
    future syntax need separate qualification. Counters/handles are not policy.
    """
    require(isinstance(value,dict) and set(value)=={'nftables'})
    data_keys={'op','left','right','payload','protocol','field','prefix','addr','len',
               'meta','key','ct','set','concat'}
    def operand(row):
        if isinstance(row,dict):
            require(set(row)<=data_keys)
            for child in row.values():operand(child)
        elif isinstance(row,list):
            for child in row:operand(child)
        else:require(row is None or type(row) in (str,int,bool))
    result=[]
    for entry in value['nftables']:
        require(isinstance(entry,dict) and len(entry)==1)
        kind=next(iter(entry));row=entry[kind]
        if kind=='metainfo':continue
        require(kind in ('table','chain','rule') and row['family'] in ('ip','ip6','inet'))
        if kind=='table':require(set(row)<={'family','name','handle'})
        elif kind=='chain':
            require(set(row)<={'family','table','name','handle','type','hook','prio','policy'})
            if 'hook' in row:
                require(row['hook'] in ('prerouting','input','forward','output','postrouting')
                        and row['type'] in ('filter','nat') and row['policy'] in ('accept','drop'))
        else:
            require(set(row)<={'family','table','chain','handle','expr','comment'})
            require(isinstance(row['expr'],list))
            for expression in row['expr']:
                require(isinstance(expression,dict) and len(expression)==1)
                action=next(iter(expression));body=expression[action]
                require(action in ('match','counter','jump','accept','drop','return','xt'))
                if action=='match':
                    require(set(body)=={'op','left','right'} and body['op'] in ('==','!=','in'))
                    operand(body['left']);operand(body['right'])
                elif action=='counter':require(set(body)=={'packets','bytes'})
                elif action=='jump':require(set(body)=={'target'} and isinstance(body['target'],str))
                elif action=='xt':
                    require(set(body)=={'type','name'} and
                            ((body['type']=='match' and body['name'] in ('addrtype','conntrack'))
                             or (body['type']=='target' and body['name'] in ('MASQUERADE','DNAT','SNAT'))))
                else:require(body is None)
        saved=copy.deepcopy(row);saved.pop('handle',None)
        if kind=='rule':saved['expr']=[x for x in saved['expr'] if 'counter' not in x]
        result.append({kind:saved})
    return result


def packet_path(network,*,uplink_index=None):
    names={row['ifname'] for row in network['links']}
    require(len(names)==len(network['links']) and set(network['tcFilters'])==names)
    for filters in network['tcFilters'].values():
        require(set(filters)=={'root','ingress','egress'} and all(value==[] for value in filters.values()))
    for row in network['links']:
        require(not row.get('xdp') and not row.get('vfinfo_list'))
        if row['ifname']=='lo':require(row['link_type']=='loopback' and not row.get('master') and not row.get('linkinfo'))
    require('lo' in names)
    require({row['dev'] for row in network['qdiscs']}==names)
    for row in network['qdiscs']:
        require(row['dev'] in names and row['kind'] in ('noqueue','fq_codel')
                and row.get('root') is True and 'parent' not in row)
    legacy=network['legacyAndPacketSockets']
    require(set(legacy)=={'ip_tables_names','ip6_tables_names','arp_tables_names','eb_tables_names','packetSockets'}
            and all(legacy[k] in ('',None) for k in legacy if k!='packetSockets'))
    for row in legacy['packetSockets']:
        # LLDP bound only to the separately admitted external interface cannot
        # consume bridged IP traffic. Its owner still needs process admission.
        require(uplink_index is not None and row['interface']==uplink_index
                and row['protocol']==0x88cc and row['type']==3)
    defaults=[{'priority':0,'src':'all','table':'local'},{'priority':32766,'src':'all','table':'main'}]
    require(network['rules4']==defaults+[{'priority':32767,'src':'all','table':'default'}]
            and network['rules6']==defaults)
    for row in network['routes4']+network['routes6']:
        require(set(row)<={'dst','gateway','dev','protocol','scope','prefsrc','metric','flags','type','table','pref','expires'}
                and row.get('table','main') in ('main','local') and row.get('dev') in names
                and row.get('type','unicast') in ('unicast','local','broadcast','multicast'))
    return nft_boundary(network['nft'])


def topology(snapshot,policy):
    """Policy IDs come from the canonical deployment plan, not this observation."""
    require(snapshot['schemaVersion']==1 and set(policy)=={'runningContainerIds','admittedNetworkIds','uplink'})
    running=snapshot['runningContainers'];expected=policy['runningContainerIds']
    require(isinstance(expected,list) and len(expected)<=128 and len(expected)==len(set(expected))
            and {c['id'] for c in running}==set(expected) and len(running)==len(expected))
    require(isinstance(policy['admittedNetworkIds'],list)
            and 1<=len(policy['admittedNetworkIds'])<=4
            and policy['admittedNetworkIds']==sorted(set(policy['admittedNetworkIds'])))
    require(all(isinstance(value,str) and re.fullmatch('[a-f0-9]{64}',value)
                for value in expected+policy['admittedNetworkIds']))
    networks={n['Id']:n for n in snapshot['dockerNetworks']}
    require(len(networks)==len(snapshot['dockerNetworks']) and set(policy['admittedNetworkIds'])<=set(networks))
    bridges={}
    for nid,network in networks.items():
        require(network['Scope']=='local' and network['Driver'] in ('bridge','host','null'))
        if network['Driver']!='bridge':
            require(not network['Containers']);continue
        options=network['Options']
        default=options.get('com.docker.network.bridge.default_bridge')=='true'
        bridge='docker0' if default else 'br-'+nid[:12]
        require(options.get('com.docker.network.bridge.name',bridge)==bridge and bridge not in bridges)
        require(set(network['Containers'])<=set(expected))
        if network['Containers']:require(nid in policy['admittedNetworkIds'])
        bridges[bridge]=nid
    require(all(bridges.get('br-'+nid[:12])==nid for nid in policy['admittedNetworkIds']))
    host=snapshot['host'];host_links={l['ifindex']:l for l in host['links']}
    require(len(host_links)==len(host['links']))
    uplinks=[l for l in host['links'] if l['ifname']==policy['uplink']]
    require(len(uplinks)==1 and uplinks[0]['link_type']=='ether' and not uplinks[0].get('master')
            and not uplinks[0].get('linkinfo'))
    packet_path(host,uplink_index=uplinks[0]['ifindex'])
    bridge_links={l['ifname']:l for l in host['links'] if l.get('linkinfo',{}).get('info_kind')=='bridge'}
    require(set(bridge_links)==set(bridges))
    for row in bridge_links.values():
        require(not row.get('master') and row['linkinfo']['info_data']['vlan_filtering']==0
                and row['linkinfo']['info_data']['stp_state']==0)
    observed_peers=set();netns=set()
    for container in running:
        require(container['privileged'] is False and set(container['capAdd'] or [])<={'CAP_NET_BIND_SERVICE','NET_BIND_SERVICE'}
                and container['networkMode'] not in ('host','none','bridge')
                and not container['networkMode'].startswith('container:'))
        require(container['namespaceInode'] not in netns and container['namespaceInode']!=snapshot['hostNamespaceInode'])
        netns.add(container['namespaceInode'])
        mappings = container.get('peerNamespaceIds', {})
        require(set(mappings) == {'containerInHost', 'hostInContainer'}
                and all(type(value) is int and value >= 0 for value in mappings.values()))
        network=container['network'];packet_path(network)
        declared=container['networks'];require(declared)
        actual=[link for link in network['links'] if link['ifname']!='lo']
        require(len(actual)==len(declared))
        used=set()
        for link in actual:
            require(link.get('linkinfo',{}).get('info_kind')=='veth' and not link.get('master'))
            matches=[(name,endpoint) for name,endpoint in declared.items() if endpoint['MacAddress']==link['address']]
            require(len(matches)==1);name,endpoint=matches[0];nid=endpoint['NetworkID']
            require(name not in used and nid in policy['admittedNetworkIds']);used.add(name)
            docker_network=networks[nid];require(docker_network['Name']==name)
            binding=docker_network['Containers'][container['id']]
            require(binding['EndpointID']==endpoint['EndpointID'] and binding['MacAddress']==endpoint['MacAddress'])
            peer=host_links[link['link_index']]
            require(peer['ifindex'] not in observed_peers and peer.get('linkinfo',{}).get('info_kind')=='veth'
                    and peer['link_index']==link['ifindex'] and peer.get('master')=='br-'+nid[:12]
                    and peer.get('link_netnsid') == mappings['containerInHost']
                    and link.get('link_netnsid') == mappings['hostInContainer'])
            observed_peers.add(peer['ifindex'])
        require(used==set(declared))
    for link in host['links']:
        if link['ifindex'] in observed_peers:continue
        require(link['ifname'] in {'lo',policy['uplink'],*bridges})
    return {'schemaVersion':1,'observedRunningContainers':len(running),'boundVethPairs':len(observed_peers),
            'admittedBridges':sorted('br-'+nid[:12] for nid in policy['admittedNetworkIds']),
            'hostBypassAdmissionVerified':False}


def stable(snapshot):
    """Retain policy/identity, excluding only sampled counters and lifetimes."""
    result=copy.deepcopy(snapshot)
    for network in [result['host'],*[row['network'] for row in result['runningContainers']]]:
        network['nft']=nft_boundary(network['nft'])
        for link in network['links']:
            for key in ('info_data','info_slave_data'):
                data=link.get('linkinfo',{}).get(key,{})
                for name in ('hello_timer','tcn_timer','topology_change_timer','gc_timer',
                             'hold_timer','message_age_timer','forward_delay_timer'):
                    data.pop(name,None)
        for row in network['qdiscs']:row.pop('refcnt',None)
        for row in network['addresses']:
            for address in row.get('addr_info',[]):
                address.pop('valid_life_time',None);address.pop('preferred_life_time',None)
        for row in network['routes4']+network['routes6']:row.pop('expires',None)
    return result


def observe(policy):
    before=observe_once();result=topology(before,policy);identity=stable(before)
    after=observe_once();topology(after,policy);require(stable(after)==identity)
    result['snapshotSha256']=hashlib.sha256(json.dumps(identity,sort_keys=True,separators=(',',':')).encode()).hexdigest()
    # Callers must combine this with qualified process/mount/BPF/UFW admission,
    # packet/reboot evidence and verified persistent policy under release ownership.
    return {'result':result,'snapshot':identity}
