#!/usr/bin/env python3
"""Owned Linux namespace identity controls; no host interfaces or Docker changes.

Requires an explicit machine ID acknowledgment and an empty Docker inventory.
All veth interfaces are created inside three temporary unshared namespaces.
"""
from contextlib import ExitStack
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('network_admission',ROOT/'ops/hetzner/network-recovery-admission.py')
n=importlib.util.module_from_spec(spec);spec.loader.exec_module(n)
ENV={'PATH':'/usr/sbin:/usr/bin:/sbin:/bin','LANG':'C.UTF-8'}


def require(value):
    if not value:raise ValueError('Owned namespace fixture rejected')


def run(args,fds=(),ok=True):
    p=subprocess.run(args,env=ENV,capture_output=True,text=True,timeout=10,pass_fds=fds)
    if ok and p.returncode:raise ValueError('Owned namespace command failed: '+p.stderr[-1000:])
    return p


def main():
    require(sys.platform=='linux' and os.geteuid()==0)
    machine=Path('/etc/machine-id').read_text().strip()
    require(os.environ.get('TDF_NETWORK_FIXTURE_MACHINE')==machine)
    require(not run(['docker','--host','unix:///var/run/docker.sock','ps','--all','--quiet']).stdout.strip())
    host_inode=os.stat('/proc/self/ns/net').st_ino
    host_links=run(['ip','-json','link','show']).stdout
    with ExitStack() as held:
        spaces=[]
        def cleanup(process):
            if process.poll() is None:process.terminate()
            try:process.wait(timeout=5)
            except subprocess.TimeoutExpired:process.kill();process.wait(timeout=5)
        for _ in range(3):
            process=subprocess.Popen(['unshare','--net','--','sleep','120'],env=ENV,stdout=subprocess.DEVNULL,stderr=subprocess.PIPE)
            held.callback(cleanup,process)
            deadline=time.monotonic()+5
            while os.stat('/proc/'+str(process.pid)+'/ns/net').st_ino==host_inode:
                require(process.poll() is None and time.monotonic()<deadline);time.sleep(.01)
            fd=os.open('/proc/'+str(process.pid)+'/ns/net',os.O_RDONLY)
            held.callback(os.close,fd);spaces.append((process,fd))
        require(len({os.fstat(fd).st_ino for _,fd in spaces})==3)
        def inside(index,args):
            fd=spaces[index][1]
            return run(['nsenter','--net=/proc/self/fd/'+str(fd),*args],(fd,)).stdout
        def query(source,target,ok=True):
            a=spaces[source][1];b=spaces[target][1]
            return run(['nsenter','--net=/proc/self/fd/'+str(a),'python3','-c',n.NETNS_QUERY+'\nprint(query_nsid('+str(b)+'))'],(a,b),ok=ok)
        inside(0,['ip','link','add','a0','index','10','type','veth','peer','name','b0','index','20'])
        inside(0,['ip','link','set','b0','netns',str(spaces[1][0].pid)])
        # GETLINK exposes namespace-local peer identity; no traffic is sent.
        links_a=json.loads(inside(0,['ip','-json','-details','link','show']))
        links_b=json.loads(inside(1,['ip','-json','-details','link','show']))
        a=next(row for row in links_a if row['ifname']=='a0')
        b=next(row for row in links_b if row['ifname']=='b0')
        ab=int(query(0,1).stdout);ba=int(query(1,0).stdout)
        require(a['link_netnsid']==ab and b['link_netnsid']==ba and a['link_index']==b['ifindex'] and b['link_index']==a['ifindex'])
        before=inside(0,['ip','-json','netns','list-id'])
        require(query(0,2,ok=False).returncode!=0)
        require(inside(0,['ip','-json','netns','list-id'])==before)
        # The third namespace deliberately reuses B's interface index.
        inside(0,['ip','link','add','a1','index','11','type','veth','peer','name','c0','index','20'])
        inside(0,['ip','link','set','c0','netns',str(spaces[2][0].pid)])
        links_a=json.loads(inside(0,['ip','-json','-details','link','show']))
        links_c=json.loads(inside(2,['ip','-json','-details','link','show']))
        c=next(row for row in links_c if row['ifname']=='c0')
        ac=int(query(0,2).stdout)
        require(c['ifindex']==b['ifindex']==20 and ac!=ab)
        require(next(row for row in links_a if row['ifname']=='a1')['link_netnsid']==ac)
        require(int(query(0,1).stdout)==ab and int(query(1,0).stdout)==ba)
    require(run(['ip','-json','link','show']).stdout==host_links)
    require(not run(['docker','--host','unix:///var/run/docker.sock','ps','--all','--quiet']).stdout.strip())
    return {'schemaVersion':1,'machineId':machine,'kernel':os.uname().release,
            'bidirectionalFdBinding':True,'unassignedMappingRejectedWithoutAllocation':True,
            'duplicateIndexDifferentNamespaceDistinguished':True,'hostInterfacesUnchanged':True,
            'cleanupCompleted':True}


if __name__=='__main__':print(json.dumps(main()))
