#!/usr/bin/env python3
"""Closed scheduler policy and mutation controls; no host lifecycle effect."""
import copy
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('scheduler',Path(__file__).resolve().parent.parent/'ops/hetzner/host-scheduler-admission.py')
s=importlib.util.module_from_spec(spec);spec.loader.exec_module(s)

class SchedulerTests(unittest.TestCase):
    def fixture(self):
        row={'path':'/etc/crontab','sha256':'a'*64,'bytes':12}
        unit={'unit':'os.timer','fragment':row,'dropIns':[]}
        return {'schemaVersion':1,'provenance':'synthetic','scheduledFiles':[row],
                'units':{'os.timer':unit},'runningServices':['os.service'],'timers':['os.timer'],'userManagers':{}}
    def run_observation(self,policy=None,*,services=None,timers=None,rows=None,units=None,queue='',second_services=None):
        policy=policy or self.fixture();calls={'service':0,'timer':0}
        def names(kind,state,*,user=False):
            calls[kind]+=1
            if kind=='service':
                if calls[kind]>1 and second_services is not None:return second_services
                return {'os.service'} if services is None else services
            return {'os.timer',s.TDF_TIMER} if timers is None else timers
        with patch.object(s.os,'geteuid',return_value=0),patch.object(s,'unit_names',side_effect=names), \
             patch.object(s,'scheduled_files',return_value=policy['scheduledFiles'] if rows is None else rows), \
             patch.object(s,'unit_row',side_effect=lambda name,**kw:(units or policy['units'])[name]), \
             patch.object(s,'execute',return_value=queue):
            return s.observe(policy)
    def test_sample_admits_reviewed_policy_without_authorizing_deployment(self):
        result=self.run_observation()
        self.assertTrue(result['atQueueEmpty']);self.assertTrue(result['readOnly'])
        self.assertFalse(result['processInventoryVerified'])
        self.assertFalse(result['continuousWriterExclusion'])
        self.assertFalse(result['deploymentAuthorized'])
    def test_unknown_or_active_backup_service_denied(self):
        for name in ('unknown.service',s.TDF_SERVICE):
            with self.assertRaises(ValueError):self.run_observation(services={'os.service',name})
    def test_missing_or_extra_timer_denied(self):
        for timers in ({'os.timer'},{'os.timer',s.TDF_TIMER,'unknown.timer'}):
            with self.assertRaises(ValueError):self.run_observation(timers=timers)
    def test_at_job_and_changed_or_extra_cron_denied(self):
        with self.assertRaises(ValueError):self.run_observation(queue='opaque-job\n')
        for rows in ([],[{'path':'/etc/crontab','sha256':'b'*64,'bytes':12}],self.fixture()['scheduledFiles']*2):
            with self.assertRaises(ValueError):self.run_observation(rows=rows)
    def test_unit_fragment_and_dropin_changes_denied(self):
        for field in ('fragment','dropIns'):
            units=copy.deepcopy(self.fixture()['units'])
            units['os.timer'][field]=[]
            if field=='dropIns':units['os.timer'][field]=[{'path':'/tmp/unreviewed'}]
            with self.assertRaises(ValueError):self.run_observation(units=units)
    def test_service_change_during_sample_denied(self):
        with self.assertRaises(ValueError):self.run_observation(second_services={'os.service','unknown.service'})
    def test_unit_change_during_sample_is_rejected(self):
        policy=self.fixture();changed=copy.deepcopy(policy['units']['os.timer'])
        changed['fragment']['sha256']='b'*64
        def names(kind,state,**kw):return {'os.service'} if kind=='service' else {'os.timer',s.TDF_TIMER}
        with patch.object(s.os,'geteuid',return_value=0),patch.object(s,'unit_names',side_effect=names), \
             patch.object(s,'scheduled_files',return_value=policy['scheduledFiles']), \
             patch.object(s,'unit_row',side_effect=[policy['units']['os.timer'],changed]), \
             patch.object(s,'execute',return_value=''):
            with self.assertRaises(ValueError):s.observe(policy)
    def test_unexamined_user_manager_is_rejected(self):
        policy=self.fixture();policy['runningServices'].append('user@0.service')
        with self.assertRaises(ValueError):self.run_observation(policy,services={'os.service','user@0.service'})
    def test_user_timer_and_config_changes_are_rejected(self):
        for invalid in ('timer','config','late-config',None):
            policy=self.fixture();policy['runningServices'].append('user@0.service')
            unit=copy.deepcopy(policy['units']['os.timer']);unit['unit']='user.timer'
            policy['userManagers']={'user@0.service':{'timers':['user.timer'],'units':{'user.timer':unit}}}
            calls=0
            def names(kind,state,*,user=False):
                if user:return {'unknown.timer'} if invalid=='timer' else {'user.timer'}
                return set(policy['runningServices']) if kind=='service' else {'os.timer',s.TDF_TIMER}
            def rows(name,*,user=False):
                nonlocal calls
                if not user:return policy['units'][name]
                calls+=1
                value=copy.deepcopy(unit)
                if invalid=='config' or (invalid=='late-config' and calls==2):value['fragment']['sha256']='b'*64
                return value
            with patch.object(s.os,'geteuid',return_value=0),patch.object(s,'unit_names',side_effect=names), \
                 patch.object(s,'scheduled_files',return_value=policy['scheduledFiles']), \
                 patch.object(s,'unit_row',side_effect=rows),patch.object(s,'execute',return_value=''):
                if invalid:
                    with self.assertRaises(ValueError):s.observe(policy)
                else:self.assertEqual(s.observe(policy)['userManagersObserved'],1)
    def test_unit_properties_reject_pending_reload_and_transient(self):
        values={'Id':'os.timer','LoadState':'loaded','FragmentPath':'/usr/lib/systemd/system/os.timer',
                'DropInPaths':'','NeedDaemonReload':'no','Transient':'no'}
        for key,value in (('NeedDaemonReload','yes'),('Transient','yes'),('Id','wrong.timer'),('LoadState','not-found')):
            changed={**values,key:value};raw='\n'.join(k+'='+v for k,v in changed.items())
            with patch.object(s,'execute',return_value=raw),patch.object(s,'file_row') as read:
                with self.assertRaises(ValueError):s.unit_row('os.timer')
                read.assert_not_called()

if __name__=='__main__':unittest.main()
