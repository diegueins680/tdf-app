#!/usr/bin/env python3
"""Read-only binding of a separately reviewed UFW recovery qualification.

This observer never creates its own trusted policy. Matching hashes establish
identity, not safety of arbitrary shell configuration or firewall rules. The
caller supplies reviewed policy and packet/reboot evidence under release ownership.
OS, Python bytecode cache and shared-library integrity remain trusted assumptions.
"""
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess

spec=importlib.util.spec_from_file_location('ufw_files',Path(__file__).with_name('recovery-files.py'))
files=importlib.util.module_from_spec(spec);spec.loader.exec_module(files)
ENV={'PATH':'/usr/sbin:/usr/bin:/sbin:/bin','LANG':'C.UTF-8'}
CONFIG=Path('/etc/ufw')
MODULES=Path('/usr/lib/python3/dist-packages/ufw')
BINARY_NAMES=('ufw','iptables','ip6tables','iptables-restore','ip6tables-restore','nft','python3')


def require(value):
    if not value:raise ValueError('UFW recovery qualification rejected')


def run(command):
    result=subprocess.run(command,env=ENV,capture_output=True,text=True,timeout=20)
    require(result.returncode==0 and len(result.stdout)<=1024**2)
    return result.stdout.strip()


def fingerprint(path,*,content=False):
    path=Path(path)
    with files.directory(str(path.parent)) as parent:
        fd=os.open(path.name,os.O_RDONLY|os.O_NOFOLLOW|os.O_NONBLOCK,dir_fd=parent)
        try:
            before=os.fstat(fd)
            require(stat.S_ISREG(before.st_mode) and before.st_uid==0 and before.st_nlink==1
                    and not before.st_mode & 0o022 and before.st_size<=16*1024**2)
            raw=b''
            while len(raw)<=before.st_size:
                block=os.read(fd,65536)
                if not block:break
                raw+=block
            require(len(raw)==before.st_size and files.identity(os.fstat(fd))==files.identity(before)
                    and files.identity(os.stat(path.name,dir_fd=parent,follow_symlinks=False))==files.identity(before))
            row={'path':str(path),'sha256':hashlib.sha256(raw).hexdigest(),'bytes':len(raw),
                 'mode':stat.S_IMODE(before.st_mode),'uid':before.st_uid}
            return (row,raw) if content else row
        finally:os.close(fd)


def tree(path,*,python_only=False):
    """Reject links and special files; bind names as well as file bytes."""
    result=[]
    with files.directory(str(path)) as fd:
        before=os.fstat(fd);names=sorted(os.listdir(fd));require(len(names)<=128)
        require(before.st_uid==0 and not before.st_mode & 0o022)
        for name in names:
            info=os.stat(name,dir_fd=fd,follow_symlinks=False)
            if python_only and name=='__pycache__':continue
            child=path/name
            if stat.S_ISDIR(info.st_mode):result.extend(tree(child,python_only=python_only))
            else:
                require(stat.S_ISREG(info.st_mode))
                if not python_only or name.endswith('.py'):result.append(fingerprint(child))
        require(names==sorted(os.listdir(fd)) and files.identity(os.fstat(fd))==files.identity(before)
                and files.identity(path.lstat())==files.identity(before))
    return result


def configured_setting(path,key,expected):
    row,raw=fingerprint(path,content=True);require(row==expected)
    # UFW sources these files as shell code. Accept only complete, unique,
    # literal assignments; ignoring a later exported/quoted/conditional value
    # could otherwise report a different effective setting than the loader.
    require(all(byte in (9,10) or 32<=byte<=126 for byte in raw))
    assignments={}
    for line in raw.decode('ascii').split('\n'):
        line=line.strip(' \t')
        if not line or line.startswith('#'):continue
        match=re.fullmatch(r'''([A-Z][A-Z0-9_]*)=(?:"([A-Za-z0-9_./,: +\-]*)"|'([A-Za-z0-9_./,: +\-]*)'|([A-Za-z0-9_./,:+\-]*))''',line)
        require(match and match[1] not in assignments)
        assignments[match[1]]=next(value for value in match.groups()[1:] if value is not None)
    require(key in assignments and assignments[key] in ('yes','YES','no','NO'))
    return assignments[key].lower()=='yes'


def kernel_rules(command):
    """Observe only UFW chains/hooks, excluding Docker's changing rule bodies."""
    rows=[];chains=[];hooks=[]
    for line in run([command,'--wait','2','-S']).splitlines():
        words=line.split()
        if len(words)>=2 and words[0] in ('-N','-A') and words[1].startswith('ufw'):
            rows.append(line)
            if words[0]=='-N':chains.append(words[1])
        elif any(word in ('-j','-g') and i+1<len(words) and words[i+1].startswith('ufw')
                 for i,word in enumerate(words)):
            rows.append(line)
            if len(words)==4 and words[0]=='-A' and words[1] in ('INPUT','OUTPUT','FORWARD') and words[2]=='-j':
                hooks.append({'chain':words[1],'target':words[3]})
    hooked=all(any(h['chain']==name for h in hooks) for name in ('INPUT','OUTPUT','FORWARD'))
    state='hooked' if hooked else 'absent' if not rows else 'partial'
    return {'state':state,'chains':sorted(chains),'hooks':hooks,
            'rulesSha256':hashlib.sha256(('\n'.join(rows)+'\n').encode()).hexdigest()}


def observe():
    require(os.geteuid()==0)
    config=tree(CONFIG)+[fingerprint('/etc/default/ufw')]
    hooks=[row for row in config if row['path'] in ('/etc/ufw/before.init','/etc/ufw/after.init')]
    require(len(hooks)==2 and all(not row['mode'] & 0o111 for row in hooks))
    by_path={row['path']:row for row in config}
    settings={name:configured_setting(path,key,by_path[path]) for name,path,key in (
        ('enabled','/etc/ufw/ufw.conf','ENABLED'),('ipv6','/etc/default/ufw','IPV6'),
        ('manageBuiltins','/etc/default/ufw','MANAGE_BUILTINS'))}
    implementation=tree(MODULES,python_only=True)
    require(implementation)
    for name in ('ufw-init','ufw-init-functions'):
        implementation.append(fingerprint(Path('/usr/lib/ufw')/name))
    binaries=[]
    for name in BINARY_NAMES:
        entry=shutil.which(name,path=ENV['PATH']);require(entry)
        resolved=Path(entry).resolve(strict=True)
        binaries.append({'name':name,'entry':entry,'resolved':fingerprint(resolved)})
    version=run(['dpkg-query','-W','-f','${Version}','ufw'])
    require(version=='0.36.2-6')
    backend={name:run([name,'--version']) for name in BINARY_NAMES[1:5]}
    require(all('(nf_tables)' in value for value in backend.values()))
    keys=('Id','LoadState','ActiveState','UnitFileState','Transient','NeedDaemonReload','FragmentPath','DropInPaths')
    rows=run(['systemctl','show','ufw.service','--property='+','.join(keys)]).splitlines()
    unit=dict(row.split('=',1) for row in rows)
    require(set(unit)==set(keys) and len(rows)==len(keys) and unit['Id']=='ufw.service'
            and unit['LoadState']=='loaded' and unit['ActiveState']=='active'
            and unit['UnitFileState']=='enabled' and unit['Transient']=='no' and unit['NeedDaemonReload']=='no')
    paths=[unit['FragmentPath'],*unit['DropInPaths'].split()]
    require(1<=len(paths)<=16 and len(set(paths))==len(paths))
    unit_files=[fingerprint(Path(path).resolve(strict=True)) for path in paths]
    return {'schemaVersion':1,'version':version,'backend':backend,'binaries':binaries,
            'implementation':sorted(implementation,key=lambda row:row['path']),
            'configuration':sorted(config,key=lambda row:row['path']),'unit':unit,'unitFiles':unit_files,
            'settings':settings,'kernelRules':{'ipv4':kernel_rules('iptables'),'ipv6':kernel_rules('ip6tables')}}


def admit(observed,policy):
    """The policy is reviewed input, never learned or approved by this function."""
    require(isinstance(policy,dict) and set(policy)=={'schemaVersion','qualification','snapshot'}
            and type(policy['schemaVersion']) is int and policy['schemaVersion']==1)
    evidence=policy['qualification']
    require(isinstance(evidence,dict) and set(evidence)=={'sourceRevision','packetEvidenceSha256','rebootEvidenceSha256'})
    require(isinstance(evidence['sourceRevision'],str) and re.fullmatch('[a-f0-9]{40}',evidence['sourceRevision']))
    for key in ('packetEvidenceSha256','rebootEvidenceSha256'):
        require(isinstance(evidence[key],str) and re.fullmatch('[a-f0-9]{64}',evidence[key]))
    require(isinstance(observed,dict) and set(observed)=={'schemaVersion','version','backend','binaries',
            'implementation','configuration','unit','unitFiles','settings','kernelRules'} and observed['schemaVersion']==1
            and observed['version']=='0.36.2-6'
            and all(isinstance(observed[key],list) and observed[key]
                    for key in ('binaries','implementation','configuration','unitFiles'))
            and observed==policy['snapshot'])
    settings=observed['settings'];kernel=observed['kernelRules']
    require(isinstance(settings,dict) and set(settings)=={'enabled','ipv6','manageBuiltins'}
            and all(type(value) is bool for value in settings.values())
            and settings['ipv6'] and not settings['manageBuiltins'])
    require(isinstance(kernel,dict) and set(kernel)=={'ipv4','ipv6'}
            and all(row['state']==('hooked' if settings['enabled'] else 'absent') for row in kernel.values()))
    return {'schemaVersion':1,'ufwIdentityMatchesReviewedQualification':True,
            'qualification':evidence,'hostBypassAdmissionVerified':False,
            'snapshotSha256':hashlib.sha256(json.dumps(observed,sort_keys=True,separators=(',',':')).encode()).hexdigest()}


def observe_qualified(policy):
    before=observe();result=admit(before,policy)
    require(observe()==before)
    return result
