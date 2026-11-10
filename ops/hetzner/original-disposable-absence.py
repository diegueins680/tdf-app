#!/usr/bin/env python3
"""Admit an empty disposable set after a recorded fresh-boot abort epoch.

No container or file is removed. Any extra container or pending creation marker
requires the separate identity-bound cleanup path, which is not implemented here.
"""
import importlib.util
import os
from pathlib import Path

_spec=importlib.util.spec_from_file_location('absence_original_database',Path(__file__).with_name('original-database-recovery.py'))
d=importlib.util.module_from_spec(_spec);_spec.loader.exec_module(d)
require,canonical,sha=d.require,d.canonical,d.sha


class OriginalDisposableAbsence(d.OriginalDatabase):
    def observe_absence(self):
        self.guard()
        require(set(self.saved['expected'])=={'api','db','edge'})
        expected=[v['containerId'] for v in self.saved['expected'].values()]
        require(len(set(expected))==3 and all(d.o.abort.j.hash_value(cid) for cid in expected))
        require(not os.path.lexists(self.reservation.directory/d.restore.PENDING_NAME))
        ids=d.o.sources.inspector.capture(d.o.sources.inspector.DOCKER+
                                         ['ps','--all','--quiet','--no-trunc']).split()
        require(len(ids)==3 and sorted(ids)==sorted(expected))
        self.guard()
        require(not os.path.lexists(self.reservation.directory/d.restore.PENDING_NAME))
        return sorted(ids)

    def recover(self):
        self.guard()
        def effect(context):
            before=self.observe_absence();after=self.observe_absence();require(before==after)
            evidence={'originalContainerIds':after,'disposableSetObservedEmpty':True,
                      'pendingCreationMarkerAbsent':True,'containersRemoved':0,'filesRemoved':0,
                      'newWritesPossible':True,'continuousPrivilegedWriterExclusionVerified':False,
                      'releaseContinuationAllowed':False}
            return {**context,'evidenceHash':sha(canonical(evidence))}
        return self.journal.perform('remove-disposables',self.targets_hash,effect)
