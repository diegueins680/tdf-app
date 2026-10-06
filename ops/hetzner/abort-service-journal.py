#!/usr/bin/env python3
"""Fixed original-service recovery epochs under the permanent abort lock.

Callbacks remain responsible for actual original-target admission and observation.
No Docker, reboot, migration, database restore or service effect is implemented here.
"""
import importlib.util
import json
import os
from pathlib import Path
import stat

spec=importlib.util.spec_from_file_location('service_abort',Path(__file__).with_name('interrupted-release-recovery.py'))
a=importlib.util.module_from_spec(spec);spec.loader.exec_module(a)
require,canonical,sha=a.require,a.canonical,a.sha
STAGES=('remove-disposables','recover-db','recover-api','recover-edge','restore-timer','complete-abort')
MAX_RECORDS=512


class ServiceJournal:
    def __init__(self,abort):
        self.abort=abort
        self.owner=os.getpid()
        self.closed=False

    def guard(self):
        require(not self.closed and os.getpid()==self.owner)
        latch,intent=self.abort._read();require(intent is not None)
        return latch,intent

    def directory(self):
        self.guard()
        parent=os.open('abort',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=self.abort.parent)
        try:
            try:os.mkdir('recovery',0o700,dir_fd=parent);os.fsync(parent)
            except FileExistsError:pass
            fd=os.open('recovery',os.O_RDONLY|os.O_DIRECTORY|os.O_NOFOLLOW,dir_fd=parent)
            info=os.fstat(fd);require(info.st_uid==os.geteuid() and stat.S_IMODE(info.st_mode)==0o700)
            os.fsync(fd)
            return fd
        finally:os.close(parent)

    def records(self):
        latch,initial=self.guard();fd=self.directory()
        try:
            names=set(os.listdir(fd));require(len(names)<=MAX_RECORDS and names=={'%03d.json'%i for i in range(len(names))})
            records=[];previous=None;epoch=0;boot=None;completed=0;pending=None;reboot=initial
            for index in range(len(names)):
                raw,_=a.read_file(fd,'%03d.json'%index,maximum=a.j.MAX_RECORD)
                row=json.loads(raw)
                require(canonical(row)==raw and isinstance(row,dict)
                        and set(row)=={'schemaVersion','sequence','previousHash','latchHash','epoch','bootId','event'}
                        and type(row['schemaVersion']) is int and row['schemaVersion']==1
                        and type(row['sequence']) is int and row['sequence']==index
                        and row['previousHash']==previous and row['latchHash']==sha(canonical(latch)))
                event=row['event'];require(isinstance(event,dict))
                a.validate_host({'machineId':initial['host']['machineId'],'bootId':row['bootId']})
                require(type(row['epoch']) is int)
                if event.get('kind')=='epoch':
                    require(reboot is not None and row['epoch']==epoch+1
                            and row['bootId']!=reboot['host']['bootId']
                            and set(event)=={'kind','rebootIntentHash'} and event['rebootIntentHash']==sha(canonical(reboot)))
                    epoch=row['epoch'];boot=row['bootId'];completed=0;pending=None;reboot=None
                else:
                    require(epoch>0 and row['epoch']==epoch and row['bootId']==boot and reboot is None
                            and completed<len(STAGES))
                    if event.get('kind')=='reboot-intent':
                        require(set(event)=={'kind','host','newWritesPossible'} and event['newWritesPossible'] is True)
                        a.validate_host(event['host']);require(event['host']=={'machineId':initial['host']['machineId'],'bootId':boot})
                        reboot=event
                    elif event.get('kind')=='intent':
                        require(pending is None and set(event)=={'kind','stage','targetsHash','operationId'}
                                and event['stage']==STAGES[completed] and a.j.hash_value(event['targetsHash']))
                        context={'latchHash':row['latchHash'],'epoch':epoch,'bootId':boot,
                                 'stage':event['stage'],'targetsHash':event['targetsHash']}
                        require(event['operationId']==sha(canonical(context)));pending=event
                    else:
                        require(event.get('kind')=='observed' and pending is not None
                                and set(event)=={'kind','stage','operationId','evidenceHash'}
                                and event['stage']==pending['stage'] and event['operationId']==pending['operationId']
                                and a.j.hash_value(event['evidenceHash']))
                        completed+=1;pending=None
                records.append(row);previous=sha(raw)
            return records,{'epoch':epoch,'bootId':boot,'completedStages':list(STAGES[:completed]),
                'pendingStage':None if pending is None else pending['stage'],'pendingReboot':reboot,
                'originalDeploymentRecoverySequenceComplete':completed==len(STAGES),
                'newWritesPossible':True,'releaseContinuationAllowed':False}
        finally:os.close(fd)

    def _append(self,epoch,boot,event):
        records,_=self.records();require(len(records)<MAX_RECORDS)
        latch,_=self.guard()
        row={'schemaVersion':1,'sequence':len(records),
             'previousHash':sha(canonical(records[-1])) if records else None,
             'latchHash':sha(canonical(latch)),'epoch':epoch,'bootId':boot,'event':event}
        fd=self.directory()
        try:a.publish(fd,'%03d.json'%len(records),row)
        except BaseException:self.closed=True;raise
        finally:os.close(fd)

    def begin_epoch(self):
        _,state=self.records();reboot=state['pendingReboot'];require(reboot is not None)
        host=a.boot_identity();require(host['machineId']==reboot['host']['machineId'] and host['bootId']!=reboot['host']['bootId'])
        self._append(state['epoch']+1,host['bootId'],{'kind':'epoch','rebootIntentHash':sha(canonical(reboot))})
        return self.records()[1]

    def current(self):
        _,state=self.records();require(state['epoch']>0 and state['pendingReboot'] is None)
        _,initial=self.guard();host=a.boot_identity()
        require(host=={'machineId':initial['host']['machineId'],'bootId':state['bootId']})
        return state

    def perform(self,stage,targets_hash,effect):
        state=self.current();require(state['pendingStage'] is None and not state['originalDeploymentRecoverySequenceComplete']
            and stage==STAGES[len(state['completedStages'])] and a.j.hash_value(targets_hash))
        latch,_=self.guard()
        context={'latchHash':sha(canonical(latch)),'epoch':state['epoch'],'bootId':state['bootId'],
                 'stage':stage,'targetsHash':targets_hash}
        context['operationId']=sha(canonical(context))
        self._append(state['epoch'],state['bootId'],{'kind':'intent','stage':stage,
                     'targetsHash':targets_hash,'operationId':context['operationId']})
        prefix,_=self.records();self.current()
        observation=effect(dict(context))
        self.current();require(self.records()[0]==prefix and isinstance(observation,dict)
            and set(observation)==set(context)|{'evidenceHash'}
            and all(observation[key]==value for key,value in context.items()) and a.j.hash_value(observation['evidenceHash']))
        self._append(state['epoch'],state['bootId'],{'kind':'observed','stage':stage,
                     'operationId':context['operationId'],'evidenceHash':observation['evidenceHash']})
        return self.records()[1]

    def request_next_reboot(self,effect):
        state=self.current();require(not state['originalDeploymentRecoverySequenceComplete'])
        _,initial=self.guard();host={'machineId':initial['host']['machineId'],'bootId':state['bootId']}
        self._append(state['epoch'],state['bootId'],{'kind':'reboot-intent','host':host,'newWritesPossible':True})
        self.records()
        effect()  # No same-boot retry; observe another boot before a new epoch.
