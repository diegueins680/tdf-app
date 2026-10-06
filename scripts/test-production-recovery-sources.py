#!/usr/bin/env python3
"""Canonical source topology admission; no Docker mutation or production fixtures."""
import copy
import importlib.util
from pathlib import Path
import unittest

spec=importlib.util.spec_from_file_location('sources',Path(__file__).resolve().parent.parent/'ops/hetzner/production-recovery-sources.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)


def fixture(stopped=False):
    volumes={name:{'Name':name,'Driver':'local','Scope':'local','Options':{},
                   'Mountpoint':'/var/lib/docker/volumes/'+name+'/_data'} for name in s.VOLUMES.values()}
    def bind(source,target,rw):return {'Type':'bind','Source':source,'Destination':target,'RW':rw,'Propagation':'rprivate'}
    def volume(role,target):
        name=s.VOLUMES[role]
        return {'Type':'volume','Name':name,'Source':volumes[name]['Mountpoint'],'Destination':target,'RW':True}
    mounts={'db':[volume('database','/var/lib/postgresql/data'),bind(s.DIRECTORY+'/postgres_password','/run/secrets/postgres_password',False)],
            'api':[bind(s.DIRECTORY+'/assets','/data/assets',True)],
            'edge':[bind(s.DIRECTORY+'/Caddyfile','/etc/caddy/Caddyfile',False),volume('edge-data','/data'),volume('edge-config','/config')]}
    expected={};containers=[]
    for index,service in enumerate(('api','db','edge'),1):
        cid=str(index)*64;image='example/test@sha256:'+str(index)*64;image_id='sha256:'+str(index+3)*64
        labels={'com.docker.compose.project':s.PROJECT,'com.docker.compose.service':service,
                'com.docker.compose.project.working_dir':s.DIRECTORY,
                'com.docker.compose.project.config_files':s.DIRECTORY+'/compose.yaml'}
        networks={'db':{s.PROJECT+'_database':{}},'edge':{s.PROJECT+'_outbound':{}},
                  'api':{s.PROJECT+'_database':{},s.PROJECT+'_outbound':{}}}[service]
        item={'Id':cid,'Image':image_id,'Config':{'Image':image,'Labels':labels,
              'Env':['PGDATA=/var/lib/postgresql/data','APP_ENV=production','SYNTHETIC_SECRET=not-a-real-secret']},
              'State':{'Running':not stopped,'Restarting':False,'Paused':False,'Dead':False,'OOMKilled':False,
                       'Pid':0 if stopped else 100+index,'ExitCode':0,'Status':'exited' if stopped else 'running'},
              'HostConfig':{'RestartPolicy':{'Name':'unless-stopped'},'AutoRemove':False,'Privileged':False,'IpcMode':'private'},
              'NetworkSettings':{'Networks':networks,'Ports':{}},'Mounts':mounts[service]}
        containers.append(item);expected[service]={'containerId':cid,'image':image,'imageId':image_id}
    return containers,volumes,expected


class SourceTests(unittest.TestCase):
    def test_initial_and_stopped_samples_bind_same_private_configuration(self):
        result=s.admit(*fixture())
        stopped=s.admit(*fixture(True),stopped=True)
        self.assertEqual(result['runtimeConfigurationSha256'],stopped['runtimeConfigurationSha256'])
        self.assertEqual(set(result['roots']),{'database','production','edge-data','edge-config'})
        self.assertIs(result['legacyUploads'],True);self.assertIs(result['dockerWritersStopped'],False)
        self.assertIs(stopped['dockerWritersStopped'],True)
        self.assertIs(stopped['hostWorkersFenced'],False);self.assertIs(stopped['databaseCleanShutdownVerified'],False)
        self.assertNotIn('SYNTHETIC_SECRET',str(result));self.assertNotIn('not-a-real-secret',str(result))

    def test_identity_lifecycle_restart_and_privilege_denials(self):
        for mutate in (lambda c:c['Config'].update(Image='mutable:latest'),lambda c:c.update(Image='sha256:'+'f'*64),
                       lambda c:c['State'].update(Running=False),lambda c:c['State'].update(Pid=True),
                       lambda c:c['State'].update(OOMKilled=True),lambda c:c['HostConfig'].update(Privileged=True),
                       lambda c:c['HostConfig'].update(AutoRemove=True),lambda c:c['HostConfig'].update(IpcMode='host'),
                       lambda c:c['HostConfig'].update(PidMode='host'),lambda c:c['HostConfig'].update(CapAdd=['SYS_ADMIN']),
                       lambda c:c['HostConfig'].update(CapAdd=['CAP_SYS_ADMIN']),
                       lambda c:c['HostConfig'].update(CapAdd=['cap_sys_ptrace']),
                       lambda c:c['HostConfig'].update(CapAdd=['NET_BIND_SERVICE']),
                       lambda c:c['HostConfig']['RestartPolicy'].update(Name='always')):
            values=fixture();mutate(values[0][0])
            with self.assertRaises(ValueError):s.admit(*values)
        for key,value in (('ExitCode',137),('Pid',123),('Running',True),('Restarting',True),('Status','created')):
            values=fixture(True);values[0][1]['State'][key]=value
            with self.assertRaises(ValueError):s.admit(*values,stopped=True)

    def test_unknown_running_or_restartable_container_and_duplicate_service_denied(self):
        values=fixture();other=copy.deepcopy(values[0][0]);other['Id']='a'*64;other['Config']['Labels']={}
        values[0].append(other)
        with self.assertRaises(ValueError):s.admit(*values)
        other['State'].update(Running=False,Pid=0,Status='exited')
        s.admit(*values)
        other['HostConfig']['RestartPolicy']['Name']='always'
        with self.assertRaises(ValueError):s.admit(*values)
        other['HostConfig']['RestartPolicy']['Name']='no'
        other['Config']['Labels']=copy.deepcopy(values[0][0]['Config']['Labels'])
        with self.assertRaises(ValueError):s.admit(*values)
        other['Config']['Labels']['com.docker.compose.service']='canary'
        s.admit(*values)

    def test_only_edge_bind_capability_is_permitted_in_both_docker_spellings(self):
        for cap in ('NET_BIND_SERVICE','CAP_NET_BIND_SERVICE','cap_net_bind_service'):
            c,v,e=fixture();c[2]['HostConfig']['CapAdd']=[cap];s.admit(c,v,e)
        for caps in (['CAP_SYS_ADMIN'],['ALL'],['SYS_PTRACE'],['CAP_NET_BIND_SERVICE','NET_BIND_SERVICE']):
            c,v,e=fixture();c[2]['HostConfig']['CapAdd']=caps
            with self.assertRaises(ValueError):s.admit(c,v,e)

    def test_missing_shadowed_aliased_or_remote_storage_denied(self):
        for change in (lambda c,v:c[0]['Mounts'].append(dict(c[0]['Mounts'][0],Destination='/data/assets/hidden')),
                       lambda c,v:c[0]['Mounts'][0].update(Source=s.DIRECTORY+'/other'),
                       lambda c,v:c[1]['Mounts'][1].update(RW=True),
                       lambda c,v:c[2]['Mounts'][1].update(Type='bind'),
                       lambda c,v:v[s.VOLUMES['database']].update(Driver='nfs'),
                       lambda c,v:v[s.VOLUMES['database']].update(Options={'device':'remote'}),
                       lambda c,v:c[2]['NetworkSettings']['Networks'].update({s.PROJECT+'_database':{}})):
            c,v,e=fixture();change(c,v)
            with self.assertRaises(ValueError):s.admit(c,v,e)
        c,v,e=fixture();root=v[s.VOLUMES['database']]['Mountpoint'];nested=root+'/alias'
        v[s.VOLUMES['edge-data']]['Mountpoint']=nested;c[2]['Mounts'][1]['Source']=nested
        with self.assertRaises(ValueError):s.admit(c,v,e)
        v[s.VOLUMES['edge-data']]['Mountpoint']='/'+nested;c[2]['Mounts'][1]['Source']='/'+nested
        with self.assertRaises(ValueError):s.admit(c,v,e)

    def test_only_canonical_persistent_upload_mount_replaces_legacy_capture(self):
        c,v,e=fixture();c[0]['Mounts'].append({'Type':'bind','Source':s.DIRECTORY+'/uploads',
            'Destination':'/app/uploads','RW':True,'Propagation':'rprivate'})
        result=s.admit(c,v,e);self.assertIs(result['legacyUploads'],False)
        c[0]['Mounts'][-1]['Source']=s.DIRECTORY+'/assets'
        with self.assertRaises(ValueError):s.admit(c,v,e)


if __name__=='__main__':unittest.main()
