#!/usr/bin/env python3
"""Restore only the originally active/enabled backup timer in a recovery epoch.

This restores scheduling, not backup success. An active/failed backup at a sampled
admission or a changed unit definition prevents completion. A short backup can
complete between samples; this is not dispatch exclusion. No service job is killed or reset.
"""
import importlib.util
from pathlib import Path

_spec=importlib.util.spec_from_file_location('timer_original_database',Path(__file__).with_name('original-database-recovery.py'))
d=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(d)
require,canonical,sha=d.require,d.canonical,d.sha


class OriginalTimer(d.OriginalDatabase):
    def recover(self):
        self.guard()
        require(self.saved['units']['timerEnabled'] is True and self.saved['units']['timerActive'] is True)
        def effect(context):
            self.guard();before=d.observe(self.saved);self.guard()
            require(all(before['running'][service] for service in ('db','api','edge'))
                    and d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            submitted=before['units']['timerStopped']
            require(type(submitted) is bool)
            if submitted:
                require(not d.o.fence.execute(['systemctl','start',d.o.fence.TIMER]).strip())
            self.guard();after=d.observe(self.saved)
            require(all(after['running'][service] for service in ('db','api','edge'))
                    and after['units']['timerStopped'] is False and after['units']['backupServiceInactive'] is True
                    and d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            evidence={'timerEnabled':True,'timerActive':True,'startSubmitted':submitted,
                      'unitConfigurationSha256':after['units']['unitConfigurationSha256'],
                      'backupSuccessVerified':False,'futureWorkerExclusionVerified':False,
                      'newWritesPossible':True,'originalDeploymentRecovered':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('restore-timer',self.targets_hash,effect)
