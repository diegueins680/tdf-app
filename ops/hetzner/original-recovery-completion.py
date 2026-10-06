#!/usr/bin/env python3
"""Final read-only observation for an original-service abort recovery epoch.

This seals the fixed journal sequence, never the normal release. Public-network
reachability and complete business conformance are outside this observation.
"""
import importlib.util
import os
from pathlib import Path
import re

_spec=importlib.util.spec_from_file_location('completion_original_application',Path(__file__).with_name('original-application-recovery.py'))
a=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(a)
require,canonical,sha=a.require,a.canonical,a.sha


class OriginalRecoveryCompletion(a.d.OriginalDatabase):
    def observe_ready(self):
        self.guard();value=a.d.observe(self.saved)
        require(not os.path.lexists(self.reservation.directory/a.d.restore.PENDING_NAME))
        for label in (a.d.restore.LABEL,'net.tdf.application-canary'):
            require(not a.d.o.sources.inspector.capture(a.d.o.sources.inspector.DOCKER+
                    ['ps','--all','--quiet','--no-trunc','--filter','label='+label]).strip())
        require(all(value['running'][name] for name in ('db','api','edge'))
                and self.saved['units']['timerEnabled'] is True and self.saved['units']['timerActive'] is True
                and value['units']['timerStopped'] is False and value['units']['backupServiceInactive'] is True
                and a.d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
        return value

    def recover(self):
        self.guard();expected_revision=a.revision(self.saved)
        def effect(context):
            before=self.observe_ready()
            for service in ('api','edge'):
                health=a.probe(self.saved,'/health',service)
                require(health['valid'] and health['code']==200)
                version=a.probe(self.saved,'/version',service)
                require(version['valid'] and version['code']==200 and version['metadata'].get('name')=='tdf-hq'
                        and version['metadata'].get('commit')==expected_revision
                        and re.fullmatch(r'[0-9]+(?:\.[0-9]+){2,3}',version['metadata'].get('version','')))
                rooms=a.probe(self.saved,'/rooms/public',service)
                denial=a.probe(self.saved,'/bookings',service)
                require(rooms['valid'] and rooms['code']==200 and denial['valid'] and denial['code']==401)
            after=self.observe_ready()
            require(before['units']['unitConfigurationSha256']==after['units']['unitConfigurationSha256'])
            evidence={'revision':expected_revision,'runtimeHash':self.saved['runtimeConfigurationSha256'],
                      'databaseHash':sha(canonical(self.saved['database'])),
                      'unitConfigurationSha256':after['units']['unitConfigurationSha256'],
                      'originalServicesRunning':True,'namespaceApiAndTlsEdgeVerified':True,
                      'anonymousBookingDenied':True,'timerSchedulingRestored':True,
                      'publicNetworkReachabilityVerified':False,'backupSuccessVerified':False,
                      'continuousWriterExclusionVerified':False,'wholeSystemConformanceVerified':False,
                      'newWritesPossible':True,'releaseContinuationAllowed':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('complete-abort',self.targets_hash,effect)
