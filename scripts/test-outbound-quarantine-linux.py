#!/usr/bin/env python3
"""Owned empty Linux VM only: real Docker bridges and synthetic TCP peers.

No production configuration, credentials or external addresses are used. This
packet boundary is distinct from persistent installation/reboot qualification.
"""
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parent.parent
spec = importlib.util.spec_from_file_location('quarantine_packet', ROOT/'ops/hetzner/outbound-quarantine.py')
q = importlib.util.module_from_spec(spec); spec.loader.exec_module(q)
require = q.require
DOCKER = ['docker', '--host', 'unix:///var/run/docker.sock']
LABEL = 'net.tdf.synthetic-quarantine'
SERVER = r'''
import json,os,socket,sys,threading
address=sys.argv[1]
s=socket.socket(socket.AF_INET6 if ':' in address else socket.AF_INET,socket.SOCK_STREAM)
s.bind((address,0));s.listen(16)
print(json.dumps({'port':s.getsockname()[1]}),flush=True)
def echo(c):
 try:
  while True:
   data=c.recv(1024)
   if not data:return
   c.sendall(data)
 finally:c.close()
while True:
 c,_=s.accept()
 with open(sys.argv[2],'ab',buffering=0) as f:f.write(b'accepted\n');os.fsync(f.fileno())
 threading.Thread(target=echo,args=(c,),daemon=True).start()
'''
CONTINUOUS = r'''
import socket,sys,time
address,port,path=sys.argv[1:]
while True:
 with open(path,'ab',buffering=0) as f:f.write(b'attempt\n')
 try:
  with socket.socket(socket.AF_INET6 if ':' in address else socket.AF_INET,socket.SOCK_STREAM) as s:
   s.settimeout(.05);s.connect((address,int(port)))
 except OSError:pass
 time.sleep(.01)
'''
CLIENT = r'''
import json,socket,sys
s=None
for line in sys.stdin:
 command=json.loads(line)
 try:
  if command['operation']=='connect':
   s=socket.socket(socket.AF_INET6 if ':' in command['address'] else socket.AF_INET,socket.SOCK_STREAM)
   s.settimeout(3);s.connect((command['address'],command['port']))
  else:
   s.sendall(b'synthetic-probe');assert s.recv(128)==b'synthetic-probe'
  result={'passed':True}
 except (OSError,AssertionError) as error:result={'passed':False,'failure':type(error).__name__}
 print(json.dumps(result),flush=True)
'''


def run(args):
    return q.run(args).decode().strip()


def main():
    machine = Path('/etc/machine-id').read_text().strip()
    require(os.geteuid() == 0 and re.fullmatch('[a-f0-9]{32}', machine)
            and os.environ.get('TDF_SYNTHETIC_QUARANTINE_HOST') == machine)
    require(not run(DOCKER+['ps', '--all', '--quiet']) and not Path('/opt/tdf/production').exists())
    require(not any(row.get('table', {}).get('name') == q.TABLE
                    for row in json.loads(run(['nft','--json','list','tables']))['nftables']))
    image = os.environ.get('TDF_QUARANTINE_TEST_IMAGE', '')
    require(re.fullmatch(r'pgvector/pgvector@sha256:[a-f0-9]{64}', image))
    run(DOCKER+['image', 'inspect', image])
    initial_volumes = sorted(run(DOCKER+['volume','ls','--quiet']).split())
    nonce = os.urandom(6).hex(); name = 'tdf-quarantine-'+nonce
    audit = tempfile.TemporaryDirectory(prefix='tdf-quarantine-packets-')
    receiver_files = []
    network = container = peer_container = None
    namespace = None; link = None; children = []; installed = False; assertions = 0
    def child(prefix, program, *args):
        process = subprocess.Popen(prefix+['python3','-u','-c',program,*args], env=q.ENV,
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        children.append(process); return process
    def server(prefix, address):
        path = Path(audit.name)/str(len(receiver_files));receiver_files.append(path)
        process = child(prefix, SERVER, address, str(path))
        return json.loads(process.stdout.readline())['port']
    def client(prefix): return child(prefix, CLIENT)
    def request(process, operation, expected=True, **kwargs):
        nonlocal assertions
        process.stdin.write(json.dumps({'operation':operation,**kwargs})+'\n');process.stdin.flush()
        result = json.loads(process.stdout.readline())
        if result.get('passed') is not expected:
            raise AssertionError(json.dumps({'operation':operation,'expected':expected,'fixtureTarget':kwargs,'result':result}))
        assertions += 1
    try:
        network = run(DOCKER+['network','create','--driver','bridge','--ipv6',
                    '--subnet','172.30.251.0/24','--subnet','fd55:7df:251::/64','--label',LABEL+'='+nonce,name])
        require(re.fullmatch('[a-f0-9]{64}', network)); bridge = 'br-'+network[:12]
        container = run(DOCKER+['run','--detach','--pull','never','--name',name,
                    '--label',LABEL+'='+nonce,'--network',network,'--entrypoint','sleep',image,'infinity'])
        row = json.loads(run(DOCKER+['inspect',container]))[0]
        require(row['Id'] == container and row['State']['Running'])
        prefix = ['nsenter','--target',str(row['State']['Pid']),'--net']
        endpoint = row['NetworkSettings']['Networks'][name]
        peer_container = run(DOCKER+['run','--detach','--pull','never','--name',name+'-db',
                    '--label',LABEL+'='+nonce,'--network',network,'--entrypoint','sleep',image,'infinity'])
        peer_row = json.loads(run(DOCKER+['inspect',peer_container]))[0]
        peer_prefix = ['nsenter','--target',str(peer_row['State']['Pid']),'--net']
        peer_endpoint = peer_row['NetworkSettings']['Networks'][name]
        db_addresses = (peer_endpoint['IPAddress'],peer_endpoint['GlobalIPv6Address'])
        db_ports = [server(peer_prefix,address) for address in db_addresses]
        # A routed synthetic provider, outside all Docker bridges. Docker's
        # ordinary outbound routing remains in force; no external host is used.
        namespace = 'tdfq-'+nonce; link = 'qt'+nonce; peer_link = 'qp'+nonce
        run(['ip','netns','add',namespace])
        run(['ip','link','add',link,'type','veth','peer','name',peer_link])
        run(['ip','link','set',peer_link,'netns',namespace])
        run(['ip','addr','add','172.30.252.1/24','dev',link])
        run(['ip','-6','addr','add','fd55:7df:252::1/64','dev',link,'nodad'])
        run(['ip','link','set',link,'up'])
        routed_prefix = ['ip','netns','exec',namespace]
        run(routed_prefix+['ip','link','set','lo','up'])
        run(routed_prefix+['ip','addr','add','172.30.252.2/24','dev',peer_link])
        run(routed_prefix+['ip','-6','addr','add','fd55:7df:252::2/64','dev',peer_link,'nodad'])
        run(routed_prefix+['ip','link','set',peer_link,'up'])
        run(routed_prefix+['ip','route','add','default','via','172.30.252.1'])
        run(routed_prefix+['ip','-6','route','add','default','via','fd55:7df:252::1'])
        routed_addresses = ('172.30.252.2','fd55:7df:252::2')
        routed_ports = [server(routed_prefix,address) for address in routed_addresses]
        routed_clients = [client(prefix) for _ in routed_addresses]
        host = ('172.30.251.1','fd55:7df:251::1')
        addresses = (endpoint['IPAddress'], endpoint['GlobalIPv6Address'])
        host_ports = [server([], address) for address in host]
        api_ports = [server(prefix,address) for address in addresses]
        outbound = [client(prefix) for _ in host]
        incoming = [client([]) for _ in addresses]
        for index in range(2):
            request(routed_clients[index],'connect',address=routed_addresses[index],port=routed_ports[index])
            request(routed_clients[index],'send')
            request(outbound[index],'connect',address=host[index],port=host_ports[index])
            request(outbound[index],'send')
            request(incoming[index],'connect',address=addresses[index],port=api_ports[index])
            request(incoming[index],'send')
        # The production generator must parse and then match the actual kernel
        # output, not a synthetic list or a saved copy of our own request.
        installed = True
        q.load_absent([bridge])
        # Force Neighbor Discovery instead of depending on a warm cache whose
        # randomized expiry previously hid an IPv6 reply-denial defect.
        run(['ip','-6','neigh','flush','dev',bridge])
        run(prefix+['ip','-6','neigh','flush','dev','eth0'])
        for index in range(2):
            request(routed_clients[index],'send',expected=False)
            request(client(prefix),'connect',expected=False,address=routed_addresses[index],port=routed_ports[index])
            db_client = client(prefix)
            request(db_client,'connect',address=db_addresses[index],port=db_ports[index])
            request(db_client,'send')
            request(outbound[index],'send',expected=False)
            request(client(prefix),'connect',expected=False,address=host[index],port=host_ports[index])
            request(incoming[index],'send')
            new_incoming = client([])
            request(new_incoming,'connect',address=addresses[index],port=api_ports[index])
            request(new_incoming,'send')
        ufw_checked = os.environ.get('TDF_QUARANTINE_TEST_UFW') == '1'
        if ufw_checked:
            require(run(['dpkg-query','-W','-f','${Version}','ufw'])=='0.36.2-6')
            require('(nf_tables)' in run(['iptables','--version'])
                    and '(nf_tables)' in run(['ip6tables','--version']))
            require(run(['systemctl','is-active','ufw.service'])=='active')
            require('MANAGE_BUILTINS=no' in Path('/etc/default/ufw').read_text().splitlines())
            require(not any(os.access(Path('/etc/ufw')/name,os.X_OK) for name in ('before.init','after.init')))
            def counts(paths):return [p.stat().st_size if p.exists() else 0 for p in paths]
            providers=receiver_files[2:6]
            require(len(providers)==4)
            accepted=counts(providers)
            attempts = [];continuous = []
            for i,(address,port) in enumerate(zip((*host,*routed_addresses),(*host_ports,*routed_ports))):
                path=Path(audit.name)/('attempts-'+str(i));attempts.append(path)
                continuous.append(child(prefix,CONTINUOUS,address,str(port),str(path)))
            time.sleep(.3)
            require(all(value>=24 for value in counts(attempts)))
            # Every successful accept, including one during an intermediate UFW
            # rule update, remains in the host-owned receiver logs. Do not infer
            # continuity merely from equal before/after policy snapshots.
            for command in (['ufw','reload'],['systemctl','restart','ufw.service']):
                before=counts(attempts)
                run(command);time.sleep(.3)
                require(all(p.poll() is None for p in children))
                require(all(a>b for a,b in zip(counts(attempts),before)))
                require(counts(providers)==accepted)
                q.observe([bridge]);assertions+=1
                for index in range(2):
                    request(incoming[index],'send')
                    db=client(prefix)
                    request(db,'connect',address=db_addresses[index],port=db_ports[index])
                    request(db,'send')
                require(counts(providers)==accepted)
            # Stop only these synthetic senders before the permissive negative
            # control, which intentionally makes their destinations reachable.
            for process in continuous:
                process.terminate();process.wait(timeout=3)
                children.remove(process)
            require(counts(providers)==accepted)
        # A permissive replacement is a negative control: the same blocked
        # fresh provider probe must become reachable, and admission must reject.
        require(all(process.poll() is None for process in children))
        rules=json.loads(run(['nft','--json','list','table','inet',q.TABLE]))['nftables']
        nd_rules=[row['rule'] for row in rules if 'rule' in row and any(
            expression.get('match',{}).get('left',{}).get('payload',{}).get('protocol')=='icmpv6'
            for expression in row['rule']['expr'])]
        require(len(nd_rules)==2)
        for rule in nd_rules:
            run(['nft','delete','rule','inet',q.TABLE,'host_input','handle',str(rule['handle'])])
        run(['ip','-6','neigh','flush','dev',bridge])
        run(prefix+['ip','-6','neigh','flush','dev','eth0'])
        request(incoming[1],'send',expected=False)
        try:q.observe([bridge])
        except ValueError:assertions += 1
        else:raise AssertionError('Removed Neighbor Discovery passed admission')
        run(['nft','flush','chain','inet',q.TABLE,'host_input'])
        try:q.observe([bridge])
        except ValueError: assertions += 1
        else:raise AssertionError('Removed denial passed admission')
        for index in range(2):
            restored = client(prefix)
            request(restored,'connect',address=host[index],port=host_ports[index])
            request(restored,'send')
        # UFW must not silently mask a missing routed denial either.
        run(['nft','flush','chain','inet',q.TABLE,'routed'])
        for index in range(2):
            restored=client(prefix)
            request(restored,'connect',address=routed_addresses[index],port=routed_ports[index])
            request(restored,'send')
        print(json.dumps({'packetAssertions':assertions,'ipv4AndIpv6':True,
             'preExistingOutboundBlocked':True,'incomingRepliesPreserved':True,
             'ufwReloadAndRestartRestricted':ufw_checked,
             'removedDenialNegativeDetected':True,'rebootQualified':False}))
    finally:
        for process in reversed(children):
            process.terminate()
            try:process.wait(timeout=3)
            except subprocess.TimeoutExpired:process.kill();process.wait()
        if installed:
            # This invocation began with the exact table absent on its explicitly
            # acknowledged empty VM. Production has no automatic removal path.
            run(['nft','delete','table','inet',q.TABLE])
        if namespace:
            run(['ip','netns','delete',namespace])
        if link and Path('/sys/class/net',link).exists():
            # Removing the namespace asynchronously removes its paired host
            # interface. It may disappear between this observation and delete.
            result=subprocess.run(['ip','link','delete',link],env=q.ENV,capture_output=True,timeout=15)
            require(result.returncode==0 or not Path('/sys/class/net',link).exists())
        for target in (peer_container,container):
            if target:
                row = json.loads(run(DOCKER+['inspect',target]))[0]
                require(row['Id']==target and row['Config']['Labels'].get(LABEL)==nonce)
                run(DOCKER+['rm','--force','--volumes',target])
        if network:
            row = json.loads(run(DOCKER+['network','inspect',network]))[0]
            require(row['Id']==network and row['Labels'].get(LABEL)==nonce and not row['Containers'])
            run(DOCKER+['network','rm',network])
        require(sorted(run(DOCKER+['volume','ls','--quiet']).split()) == initial_volumes)
        audit.cleanup()


if __name__ == '__main__': main()
