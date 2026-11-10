#!/usr/bin/env python3
"""Same-boot container admission at acknowledged cold-release boundaries.

This is a read-only predicate, not a stop/recovery controller. The externally
reviewed baseline is independent authority; journal acknowledgements do not prove
correct effects. Pending effects, reboot and disposable creation are unsupported.
"""
import copy
import importlib.util
import os
from pathlib import Path


def load(name, filename):
    spec=importlib.util.spec_from_file_location(name,Path(__file__).with_name(filename))
    module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module);return module


d=load('cold_dormant','dormant-container-admission.py')
o=load('cold_original','original-deployment-admission.py')
j=o.abort.j
require=j.require
# Number of fully acknowledged stages -> running original services.
STAGES=('maintenance','stop-writers','stop-database','capture','encrypt','retrieve-off-host')
RUNNING=(frozenset(('api','db','edge')),frozenset(('api','db')),frozenset(('db',)),
         frozenset(),frozenset(),frozenset(),frozenset())


def expected_running(journal, original):
    require(j.STAGES[:len(STAGES)]==STAGES)
    records=journal.records()
    require(records and len(records)%2==1)
    completed=(len(records)-1)//2
    require(completed<len(RUNNING)
            and records[0]['releaseNonce']==original['releaseNonce']
            and records[0]['planHash']==original['planHash'])
    return records,RUNNING[completed]


class ColdContainerAdmission:
    def __init__(self,journal,directory,reviewed_policy):
        """Construct before the first intent with independently reviewed policy.

        Policy evidence references must already have been authenticated by the
        coordinator. Matching hashes here do not authenticate those artifacts.
        """
        records=journal.records();require(len(records)==1)
        original=o.read_prepared(directory,records[0]['releaseNonce'],records[0]['planHash'])
        policy=copy.deepcopy(reviewed_policy)
        d.admit(policy['snapshot'],policy)
        saved=original['originalDeployment'];require(set(saved['containers'])=={'api','db','edge'})
        bindings={service:container['Id'] for service,container in saved['containers'].items()}
        require(len(set(bindings.values()))==3 and set(saved['expected'])==set(bindings))
        rows={row['id']:row for row in policy['snapshot']['containers']}
        require(set(bindings.values())<=set(rows))
        for service,cid in bindings.items():
            require(saved['expected'][service]['containerId']==cid and rows[cid]['running'] is True
                    and rows[cid]['configurationSha256']==d.configuration_fingerprint(saved['containers'][service]))
        require({cid for cid,row in rows.items() if row['running']}==set(bindings.values()))
        self.journal,self.directory,self.original=journal,directory,original
        self.policy,self.bindings,self.owner=policy,bindings,os.getpid()
        self.observe()

    def admit(self,snapshot):
        require(os.getpid()==self.owner and o.abort.boot_identity()==self.original['host'])
        records,running=expected_running(self.journal,self.original)
        require(o.read_prepared(self.directory,self.original['releaseNonce'],self.original['planHash'])==self.original)
        baseline=self.policy['snapshot']
        require(isinstance(snapshot,dict) and set(snapshot)==set(baseline))
        require({k:v for k,v in snapshot.items() if k!='containers'}==
                {k:v for k,v in baseline.items() if k!='containers'})
        # Apply the existing structural/manual-stop validator to current rows,
        # then compare every stable field against independent baseline authority.
        # This local structural call is not qualification of a fresh snapshot.
        d.admit(snapshot,{**self.policy,'snapshot':snapshot})
        rows={row['id']:row for row in snapshot['containers']}
        before={row['id']:row for row in baseline['containers']}
        require(set(rows)==set(before))
        stopped={cid for service,cid in self.bindings.items() if service not in running}
        for cid,row in rows.items():
            initial=before[cid]
            if cid not in stopped:
                require(row==initial)
                continue
            require(set(row)==set(initial) and row['running'] is False and row['status']=='exited')
            d.check_dormant(row)
            # Persisted bytes necessarily change on stop; keep their digest as
            # evidence and verify literal stop flags, never discard configuration.
            metadata=row['metadata']
            require(set(metadata)=={'id','sha256','manuallyStopped','startedBefore'}
                    and metadata['id']==cid and j.hash_value(metadata['sha256'])
                    and metadata['manuallyStopped'] is True and metadata['startedBefore'] is True)
            changed={'running','pid','status','metadata'}
            require({k:v for k,v in row.items() if k not in changed}==
                    {k:v for k,v in initial.items() if k not in changed})
        require(self.journal.records()==records and o.abort.boot_identity()==self.original['host'])
        return {'schemaVersion':1,'releaseNonce':self.original['releaseNonce'],
                'planHash':self.original['planHash'],'journalPrefixSha256':j.sha(j.canonical(records)),
                'snapshotSha256':j.sha(j.canonical(snapshot)),
                'runningOriginalServices':sorted(running),'completedStages':(len(records)-1)//2,
                'qualificationReferencesVerified':False,'hostBypassAdmissionVerified':False,
                'stopAuthorized':False,'recoveryAuthorized':False}

    def observe(self):
        # Freeze the prefix across both samples; no intent may be issued here.
        records,_=expected_running(self.journal,self.original)
        first=d.observe(self.policy['snapshot']['socket']);result=self.admit(first)
        require(d.observe(self.policy['snapshot']['socket'])==first and self.journal.records()==records)
        require(o.abort.boot_identity()==self.original['host'])
        return result
