#!/usr/bin/env python3
"""Recover original Caddy after the journaled DB/API stages; may restore public writes.

Namespace-local TLS/routing qualification does not establish public DNS, host port
forwarding or firewall reachability. No certificate verification bypass is exposed.
"""
import importlib.util
from pathlib import Path
import re
import time

_spec=importlib.util.spec_from_file_location('edge_original_application',Path(__file__).with_name('original-application-recovery.py'))
a=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(a)
require,canonical,sha=a.require,a.canonical,a.sha


class OriginalEdge(a.d.OriginalDatabase):
    def recover(self):
        self.guard();expected_revision=a.revision(self.saved)
        def effect(context):
            self.guard();before=a.d.observe(self.saved);self.guard()
            require(before['running']['db'] and before['running']['api']
                    and a.d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            cid=self.saved['expected']['edge']['containerId'];submitted=not before['running']['edge']
            if submitted:require(a.d.o.fence.execute(a.d.o.sources.inspector.DOCKER+['start',cid]).strip()==cid)
            deadline=time.monotonic()+60
            while True:
                self.guard();current=a.d.observe(self.saved)
                require(all(current['running'][name] for name in ('api','db','edge')))
                health=a.probe(self.saved,'/health','edge')
                if health['code']==200:
                    require(health['valid']);break
                require((health['code'] is None and health['transportUnavailable']) or health['code']==503)
                require(time.monotonic()<deadline);time.sleep(0.25)
            version=a.probe(self.saved,'/version','edge')
            require(version['valid'] and version['metadata'].get('name')=='tdf-hq'
                    and version['metadata'].get('commit')==expected_revision
                    and re.fullmatch(r'[0-9]+(?:\.[0-9]+){2,3}',version['metadata'].get('version','')))
            rooms=a.probe(self.saved,'/rooms/public','edge');denial=a.probe(self.saved,'/bookings','edge')
            require(rooms['valid'] and rooms['code']==200 and denial['valid'] and denial['code']==401)
            self.guard();after=a.d.observe(self.saved)
            require(all(after['running'][name] for name in ('api','db','edge'))
                    and a.d.o.database_identity(self.saved['expected']['db']['containerId'])==self.saved['database'])
            evidence={'revision':expected_revision,'startSubmitted':submitted,'tlsHostnameVerified':'api.tdfrecords.net',
                      'namespaceLocalEdgeRoutingVerified':True,'publicDatabaseReadVerified':True,'anonymousBookingDenied':True,
                      'databaseHash':sha(canonical(self.saved['database'])),'newWritesPossible':True,
                      'publicNetworkReachabilityVerified':False,'originalDeploymentRecovered':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('recover-edge',self.targets_hash,effect)
