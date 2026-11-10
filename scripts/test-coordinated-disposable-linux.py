#!/usr/bin/env python3
"""Owned writer-fence fixture extension; never an independent production command.

Creates a real isolated cold-copy database and canary with journal-bound records,
then deliberately interrupts cleanup so the abort adapter has actual targets.
"""
import os
from pathlib import Path


def prepare(f,archive,saved):
    f.require(os.environ.get('TDF_TEST_DISPOSABLE_ABORT_RECOVERY')=='1')
    with f.j.open_journal(str(archive/'journal')) as journal:
        f.require(journal.status()['completedStages']==['maintenance','stop-writers','stop-database']
                  and journal.status()['pendingStage'] is None)
    original_db=f.inspect(saved['originalDeployment']['expected']['db']['containerId'])
    f.require(original_db['State']['Running'] is False and original_db['State']['ExitCode']==0
              and original_db['State']['OOMKilled'] is False)
    c=f.load('linux_coordinated_creation','coordinated-disposable-creation.py')
    p=c.r.s.physical;canary=c.r.s.canary
    nonce=saved['releaseNonce'];directory=p.HOST_ROOT/('rehearsal-'+nonce)
    directory.mkdir(mode=0o700);st=directory.stat()
    f.normalize_empty_fixture(directory,(st.st_dev,st.st_ino))
    original=saved['originalDeployment'];db=original['expected']['db']
    manifest=p.files.capture(original['roots']['database']['path'],str(directory/'cold-source.tar'))
    p.files.restore(str(directory/'cold-source.tar'),manifest,str(directory/'physical-data'))
    clone=p.PhysicalClone(db['containerId'],db['image'],db['imageId'],nonce,directory,original['database']['systemIdentifier'])
    clone.prepare(manifest)
    interrupted=False;application=None
    with f.j.open_journal(str(archive/'journal')) as journal:
        # Synthetic stage observations only. No encryption or off-host claim.
        for stage in ('capture','encrypt','retrieve-off-host'):
            journal.perform(stage,'1'*64,lambda ctx:{**ctx,'evidenceHash':'2'*64})
        plan=journal.records()[0]['event']['plan']
        clone.creation_records=c.CoordinatedCreation(journal,archive)
        try:
            with clone.reserved():
                def effect(context):
                    nonlocal application,interrupted
                    clone.start()
                    application=canary.Canary(p.restore,clone,directory,
                        'diegueins680/tdf-hq@'+plan['candidateImage'],plan['sourceRevision'])
                    with clone.with_application(application):
                        evidence=application.run()
                        f.require(evidence['databaseRecovery']=='passed')
                        def fail_cleanup():
                            nonlocal interrupted
                            interrupted=True;raise ValueError('Synthetic interrupted disposable cleanup')
                        application.cleanup=fail_cleanup
                    raise AssertionError('Synthetic cleanup interruption was not exercised')
                journal.perform('restore-isolate','3'*64,effect)
        except ValueError:
            f.require(interrupted and journal.status()['pendingStage']=='restore-isolate'
                      and clone.active_application is application and application.target is not None
                      and clone.target is not None and clone.creation_records.closed)
        else:raise AssertionError('Uncertain cleanup was accepted')
    # Synthetic equivalent of restart=no containers remaining stopped on boot.
    # No actual reboot is claimed; the abort journal uses explicit synthetic epochs.
    for target in (application.target,clone.target):
        f.run(f.DOCKER+['stop','--time','20',target])
        row=f.inspect(target)
        f.require(row['State']['Running'] is False and row['HostConfig']['RestartPolicy']=={'Name':'no','MaximumRetryCount':0})
    return {'database':clone.target,'canary':application.target,'directory':str(directory),
            'realCreationAndCanaryVerified':True,'normalCleanupInterruptionRetained':True}
