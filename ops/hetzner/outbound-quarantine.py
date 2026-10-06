#!/usr/bin/env python3
"""Restricted Docker egress for an admitted dedicated recovery host.

No deployment CLI. Policy generation and live validation are separate from
installation. The coordinator must establish persistent boot ordering before
admitting a legacy shutdown. Host/root compromise and DNS relay are excluded.
"""
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys

TABLE = 'tdf_outbound_quarantine'
ENV = {'PATH': '/usr/sbin:/usr/bin:/sbin:/bin', 'LANG': 'C.UTF-8'}
DIRECTORY = Path('/etc/tdf-outbound-quarantine')
UNIT = 'tdf-outbound-quarantine.service'
UNIT_PATH = Path('/etc/systemd/system')/UNIT
DROPIN_PATH = Path('/etc/systemd/system/docker.service.d/40-tdf-outbound-quarantine.conf')
SERVICE = '''[Unit]
Description=TDF recovery outbound transport quarantine
Before=docker.service
After=nftables.service

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/bin/python3 -I /etc/tdf-outbound-quarantine/guard.py enforce
TimeoutStartSec=30
UMask=0077
'''
DROPIN = '''[Unit]
Requires=tdf-outbound-quarantine.service
After=tdf-outbound-quarantine.service
[Service]
ExecStartPre=/usr/bin/python3 -I /etc/tdf-outbound-quarantine/guard.py verify-start
'''


def require(value):
    if not value:
        raise ValueError('Outbound quarantine admission rejected')


def canonical(value):
    return (json.dumps(value, sort_keys=True, separators=(',', ':')) + '\n').encode()


def match(key, value):
    return {'match': {'op': '==', 'left': {'meta': {'key': key}}, 'right': value}}


def direction(value):
    return {'match': {'op': '==', 'left': {'ct': {'key': 'direction'}}, 'right': value}}


def objects(bridges):
    """Exact stable nft JSON. Never permit arbitrary expressions or interface names.

    All br-* and docker0 sources are caught, including newly created networks;
    only the explicitly admitted bridges may communicate within that same bridge.
    There is deliberately no blanket established/related permit.
    """
    require(isinstance(bridges, list) and 1 <= len(bridges) <= 4
            and bridges == sorted(set(bridges))
            and all(isinstance(b, str) and re.fullmatch(r'br-[a-f0-9]{12}', b) for b in bridges))
    result = [{'table': {'family': 'inet', 'name': TABLE}}]
    for name, hook in (('host_input', 'input'), ('routed', 'forward')):
        result.append({'chain': {'family': 'inet', 'table': TABLE, 'name': name,
                                'type': 'filter', 'hook': hook, 'prio': -210, 'policy': 'accept'}})
        def rule(expressions):
            result.append({'rule': {'family': 'inet', 'table': TABLE, 'chain': name,
                                    'expr': expressions}})
        if name == 'routed':
            for bridge in bridges:
                rule([match('iifname', bridge), match('oifname', bridge), {'accept': None}])
        for interface in ('docker0', 'br-*'):
            # Explicit reply direction permits replies to incoming HTTP/TLS and
            # operator probes. Original-direction packets of an already-open
            # provider connection still hit the drop rule below.
            rule([match('iifname', interface), direction('reply'), {'accept': None}])
            rule([match('iifname', interface), {'drop': None}])
    # nft's dump places all chain declarations before the ordered rule lists.
    return ([item for item in result if 'table' in item or 'chain' in item]
            + [item for item in result if 'rule' in item])


def program(bridges):
    return {'nftables': [{'add': obj} for obj in objects(bridges)]}


def normalize(value):
    require(isinstance(value, dict) and set(value) == {'nftables'} and isinstance(value['nftables'], list))
    result = []
    for item in value['nftables']:
        require(isinstance(item, dict) and len(item) == 1)
        kind, content = next(iter(item.items()))
        if kind == 'metainfo':
            require(isinstance(content, dict))
            continue
        require(kind in ('table', 'chain', 'rule') and isinstance(content, dict))
        # Kernel-assigned handles are not policy; every other field is compared.
        result.append({kind: {key: val for key, val in content.items() if key != 'handle'}})
    return result


def admit(value, bridges):
    expected = objects(bridges)
    require(normalize(value) == expected)
    return {'schemaVersion': 1, 'policySha256': hashlib.sha256(canonical(expected)).hexdigest(),
            'bridges': bridges, 'ipv4AndIpv6': True, 'establishedOutboundAllowed': False,
            'bootOrderingVerified': False, 'scope': 'Docker bridge provider transport; host DNS relay excluded'}


def run(command, payload=None):
    result = subprocess.run(command, input=payload, capture_output=True, env=ENV, timeout=15)
    require(result.returncode == 0 and len(result.stdout) <= 131072)
    return result.stdout


def observe(bridges):
    return admit(json.loads(run(['nft', '--json', 'list', 'table', 'inet', TABLE])), bridges)


def load_absent(bridges):
    """One atomic netlink transaction; rejects an existing owned table.

    No flush/delete/replacement, including on failed validation. This is a narrow
    library effect for a coordinator or owned fixture, not production authority.
    """
    tables = json.loads(run(['nft', '--json', 'list', 'tables']))
    require(not any(row.get('table', {}).get('name') == TABLE for row in tables['nftables']))
    run(['nft', '--json', '--file', '-'], canonical(program(bridges)))
    return observe(bridges)


def read_private(path, maximum=65536):
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    try:
        info = os.fstat(fd)
        require(stat.S_ISREG(info.st_mode) and info.st_uid == 0 and info.st_nlink == 1
                and stat.S_IMODE(info.st_mode) == 0o600 and 0 < info.st_size <= maximum)
        value = os.read(fd, maximum + 1)
        closing = os.fstat(fd); named = os.stat(path, follow_symlinks=False)
        require(len(value) == info.st_size and (closing.st_dev, closing.st_ino, closing.st_mtime_ns, closing.st_size)
                == (info.st_dev, info.st_ino, info.st_mtime_ns, info.st_size)
                and (named.st_dev, named.st_ino) == (info.st_dev, info.st_ino))
        return value
    finally:
        os.close(fd)


def persistent_configuration():
    require(os.geteuid() == 0)
    info = DIRECTORY.lstat()
    require(stat.S_ISDIR(info.st_mode) and info.st_uid == 0 and stat.S_IMODE(info.st_mode) == 0o700)
    value = read_private(DIRECTORY/'policy.json', maximum=4096)
    config = json.loads(value)
    require(set(config) == {'schemaVersion', 'bridges', 'guardSha256'} and config['schemaVersion'] == 1
            and canonical(config) == value and re.fullmatch('[a-f0-9]{64}', config['guardSha256']))
    objects(config['bridges'])
    require(hashlib.sha256(read_private(DIRECTORY/'guard.py')).hexdigest() == config['guardSha256'])
    require(read_private(UNIT_PATH).decode() == SERVICE and read_private(DROPIN_PATH).decode() == DROPIN)
    return config


def enforce():
    """Boot entrypoint: load a missing policy, but never overwrite a changed one.

    Called before dockerd. It must not query Docker or depend on Docker startup.
    """
    config = persistent_configuration()
    tables = json.loads(run(['nft', '--json', 'list', 'tables']))['nftables']
    if any(row.get('table', {}).get('name') == TABLE for row in tables):
        result = observe(config['bridges'])
    else:
        result = load_absent(config['bridges'])
    require(persistent_configuration() == config)
    return result


def properties(unit, names):
    raw = run(['systemctl','show',unit,'--property='+','.join(names)]).decode()
    result = {}
    for line in raw.splitlines():
        key, sep, value = line.partition('=')
        require(sep and key in names and key not in result)
        result[key] = value
    require(set(result) == set(names))
    return result


def observe_persistent():
    config = persistent_configuration()
    common = ('Id','LoadState','ActiveState','FragmentPath','DropInPaths','NeedDaemonReload','Transient','Job')
    guard = properties(UNIT, common + ('Before','After','SubState','RemainAfterExit'))
    require(guard['Id'] == UNIT and guard['LoadState'] == 'loaded' and guard['ActiveState'] == 'active'
            and guard['SubState'] == 'exited' and guard['RemainAfterExit'] == 'yes'
            and guard['FragmentPath'] == str(UNIT_PATH) and not guard['DropInPaths']
            and guard['NeedDaemonReload'] == 'no' and guard['Transient'] == 'no' and not guard['Job']
            and 'docker.service' in guard['Before'].split() and 'nftables.service' in guard['After'].split())
    docker = properties('docker.service', common + ('Requires','After','ExecStartPre'))
    require(docker['Id'] == 'docker.service' and docker['LoadState'] == 'loaded'
            and docker['NeedDaemonReload'] == 'no' and docker['Transient'] == 'no' and not docker['Job']
            and UNIT in docker['Requires'].split() and UNIT in docker['After'].split()
            and str(DROPIN_PATH) in docker['DropInPaths'].split()
            and docker['ExecStartPre'].count('{ path=') == 1
            and docker['ExecStartPre'].startswith('{ path=/usr/bin/python3 ; argv[]=/usr/bin/python3 -I '
                '/etc/tdf-outbound-quarantine/guard.py verify-start ; ignore_errors=no ;'))
    policy = observe(config['bridges'])
    require(persistent_configuration() == config)
    return {**policy, 'bootOrderingVerified': True,
            'persistentConfigurationSha256': hashlib.sha256(canonical(config)).hexdigest(),
            'hostBypassAdmissionVerified': False, 'rebootQualified': False}


if __name__ == '__main__':
    # Installation and removal are intentionally absent. Only an explicitly
    # installed, root-private boot policy can invoke this entrypoint.
    try:
        require(sys.argv[1:] in (['enforce'], ['verify-start']))
        if sys.argv[1:] == ['enforce']:
            enforce()
        else:
            # Docker restarts must recheck even when the oneshot service remains
            # active. Missing/changed live policy is not silently repaired here.
            config = persistent_configuration()
            observe(config['bridges'])
            require(persistent_configuration() == config)
    except Exception:
        print('Outbound quarantine enforcement failed', file=sys.stderr)
        raise SystemExit(1)
