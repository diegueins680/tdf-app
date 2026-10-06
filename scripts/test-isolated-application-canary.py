#!/usr/bin/env python3
import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import socketserver
import socket
import sys
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from types import SimpleNamespace
from contextlib import ExitStack
from unittest.mock import Mock, patch

ROOT=Path(__file__).resolve().parent.parent
spec=importlib.util.spec_from_file_location('canary',ROOT/'ops/hetzner/isolated-application-canary.py')
canary=importlib.util.module_from_spec(spec);spec.loader.exec_module(canary)
SOURCE='a'*64; DB='b'*64; APP='c'*64; NONCE='d'*32
IMAGE='diegueins680/tdf-hq@sha256:'+'e'*64; IMAGE_ID='sha256:'+'f'*64; REV='1'*40
DIRECTORY=Path('/opt/tdf/backups')/('rehearsal-'+NONCE)


def make():
    database=SimpleNamespace(target=DB,source=SOURCE,nonce=NONCE,inspect=Mock(),admit=Mock())
    subject=canary.Canary(SimpleNamespace(DOCKER=['docker']),database,DIRECTORY,IMAGE,REV)
    subject.image_id=IMAGE_ID
    return subject


def container():
    return {'Id':APP,'Image':IMAGE_ID,'Config':{'Labels':{canary.LABEL:NONCE},'Image':IMAGE,
            'Cmd':canary.COMMAND.copy(),'Entrypoint':None,'User':'1000:1000','WorkingDir':'/app'},
        'HostConfig':{'RestartPolicy': {'Name':'no','MaximumRetryCount':0}, 'AutoRemove': False, 'NetworkMode':'container:'+DB,'ReadonlyRootfs':True,'Memory':canary.MEMORY,
            'MemorySwap':canary.MEMORY,'NanoCpus':500000000,'PidsLimit':128,'CapDrop':['ALL'],
            'IpcMode':'private','SecurityOpt':['no-new-privileges:true'],
            'Tmpfs':{'/tmp':'rw,nosuid,nodev,size=16777216'}},
        'NetworkSettings':{'Networks':{}},'State':{'Running':True,'Pid':124},
        'Mounts':[{'Type':'bind','Source':str(DIRECTORY/'canary-assets'),'Destination':'/data/assets','RW':True},
                  {'Type':'bind','Source':str(DIRECTORY/'canary-uploads'),'Destination':'/app/uploads','RW':True},
                  {'Type':'tmpfs','Destination':'/tmp','RW':True}]}


class CanaryTests(unittest.TestCase):
    def test_durable_creation_failure_prevents_canary_dispatch(self):
        subject=make();publisher=Mock(side_effect=ValueError('synthetic uncertain publication'))
        subject.creation_records=SimpleNamespace(publish=publisher)
        with patch.object(subject,'prepare'),patch.object(subject,'execute') as execute:
            with self.assertRaises(ValueError):subject.run()
        publisher.assert_called_once_with('application-canary',subject)
        execute.assert_not_called();self.assertFalse(subject.creation_attempted)

    def test_durable_record_precedes_uncertain_canary_create(self):
        subject=make();events=[]
        subject.creation_records=SimpleNamespace(publish=lambda role,obj:events.append('published'))
        def lost(command):events.append('create');raise TimeoutError('synthetic lost creation reply')
        with patch.object(subject,'prepare'),patch.object(subject,'execute',side_effect=lost):
            with self.assertRaises(TimeoutError):subject.run()
        self.assertEqual(events,['published','create']);self.assertTrue(subject.creation_attempted)

    def test_disposable_never_restarts_or_auto_removes(self):
        for policy in ({"Name":"always","MaximumRetryCount":0},{"Name":"unless-stopped","MaximumRetryCount":0},{"Name":"on-failure","MaximumRetryCount":3},{"Name":"no","MaximumRetryCount":1},None):
            data=container();data["HostConfig"]["RestartPolicy"]=policy
            with self.subTest(policy=policy),self.assertRaises(ValueError):make().admit(data)
        data=container();data["HostConfig"]["AutoRemove"]=True
        with self.assertRaises(ValueError):make().admit(data)

    def test_synthetic_startup_diagnostics_reject_real_source_before_inspection(self):
        spec=importlib.util.spec_from_file_location('synthetic_diagnostics',ROOT/'scripts/test-physical-application-docker.py')
        fixture=importlib.util.module_from_spec(spec);spec.loader.exec_module(fixture)
        application=SimpleNamespace(database=SimpleNamespace(source=SOURCE),inspect=Mock(),target=APP)
        with patch.object(fixture.subprocess,'run') as run:
            with self.assertRaises(ValueError):fixture.synthetic_application_diagnostics(application)
        application.inspect.assert_not_called();run.assert_not_called()

    def test_synthetic_diagnostics_bound_logs_and_preserve_exit_state(self):
        spec=importlib.util.spec_from_file_location('synthetic_diagnostics',ROOT/'scripts/test-physical-application-docker.py')
        fixture=importlib.util.module_from_spec(spec);spec.loader.exec_module(fixture)
        application=SimpleNamespace(database=SimpleNamespace(source='0'*64),target=APP,
            inspect=Mock(return_value={'State':{'ExitCode':1,'OOMKilled':False,'Running':False}}))
        with patch.object(fixture.subprocess,'run',return_value=SimpleNamespace(returncode=0,stdout='x'*20000,stderr='')):
            result=fixture.synthetic_application_diagnostics(application)
        self.assertEqual(result['exitCode'],1);self.assertEqual(len(result['startupLog']),16384)
        with patch.object(fixture.subprocess,'run',return_value=SimpleNamespace(returncode=0,stdout='x'*65537,stderr='')):
            with self.assertRaises(ValueError):fixture.synthetic_application_diagnostics(application)

    def test_persisted_regional_defaults_replace_compiled_defaults_only(self):
        value = {'locales': [{'code':'en','default':False},{'code':'es','default':True}],
                 'currencies': [{'code':'USD','default':True}]}
        environment = canary.regional_environment(value)
        self.assertEqual(environment, {'SUPPORTED_LOCALES':'en,es','DEFAULT_LOCALE':'es',
                                      'SUPPORTED_CURRENCIES':'USD','DEFAULT_CURRENCY':'USD'})
        value['locales'][1]['code']='fr'
        self.assertEqual(environment['DEFAULT_LOCALE'],'es')

    def test_regional_projection_rejects_environment_injection_and_ambiguous_defaults(self):
        valid = {'locales':[{'code':'es','default':True}], 'currencies':[{'code':'USD','default':True}]}
        invalids=[]
        for code in ('es,fr','es\nPAYPAL_CLIENT_SECRET=bad','../es','ES',''):
            value=copy.deepcopy(valid);value['locales'][0]['code']=code;invalids.append(value)
        for default in (False,1,'true',None):
            value=copy.deepcopy(valid);value['locales'][0]['default']=default;invalids.append(value)
        value=copy.deepcopy(valid);value['locales'].append({'code':'en','default':True});invalids.append(value)
        value=copy.deepcopy(valid);value['locales'].append({'code':'es','default':False});invalids.append(value)
        value=copy.deepcopy(valid);value['currencies']=[];invalids.append(value)
        value=copy.deepcopy(valid);value['PAYPAL_CLIENT_SECRET']='synthetic';invalids.append(value)
        value=copy.deepcopy(valid);value['currencies'][0]['code']='usd';invalids.append(value)
        for value in invalids:
            with self.subTest(value=value),self.assertRaises(ValueError):canary.regional_environment(value)

    def test_admits_exact_owned_disposable_runtime(self):
        subject=make();subject.admit(container());self.assertEqual(subject.target,APP)

    def test_omitted_tmpfs_inspection_still_requires_exact_declaration(self):
        data=container();data['Mounts'].pop()
        self.assertEqual(make().admit(data)['Id'],APP)
        data['HostConfig']['Tmpfs']={}
        with self.assertRaises(ValueError):make().admit(data)

    def test_rejects_unknown_source_image_or_revision(self):
        for image,rev in [(IMAGE.replace('@sha256:',':'),REV),('other/tdf@sha256:'+'e'*64,REV),(IMAGE,'main')]:
            with self.subTest(image=image,rev=rev),self.assertRaises(ValueError):
                canary.Canary(make().restore,make().database,DIRECTORY,image,rev)
        database=make().database;database.target=SOURCE
        with self.assertRaises(ValueError):canary.Canary(make().restore,database,DIRECTORY,IMAGE,REV)

    def test_cannot_admit_production_database_or_foreign_application(self):
        for key,value in [('Id',SOURCE),('Id',DB),('Image','sha256:'+'0'*64)]:
            data=container();data[key]=value
            with self.subTest(key=key,value=value),self.assertRaises(ValueError):make().admit(data)
        for key,value in [('Labels',{canary.LABEL:'foreign'}),('Image','tdf:latest'),('Cmd',['sh']),
                          ('Entrypoint',['sh']),('User','root'),('WorkingDir','/')]:
            data=container();data['Config'][key]=value
            with self.subTest(key=key),self.assertRaises(ValueError):make().admit(data)

    def test_rejects_network_privilege_resource_and_mount_escapes(self):
        cases={'NetworkMode':'host','ReadonlyRootfs':False,'Memory':0,'MemorySwap':-1,
            'NanoCpus':0,'PidsLimit':0,'CapDrop':[],'Privileged':True,'PortBindings':{'8080/tcp':[]},
            'Devices':['host'],'CapAdd':['SYS_ADMIN'],'VolumesFrom':['production'],'Binds':['/opt/tdf:/data'],
            'PidMode':'host','UTSMode':'host','IpcMode':'host','SecurityOpt':[],'Tmpfs':{}}
        for key,value in cases.items():
            data=container();data['HostConfig'][key]=value
            with self.subTest(key=key),self.assertRaises(ValueError):make().admit(data)
        data=container();data['NetworkSettings']['Networks']={'outbound':{}}
        with self.assertRaises(ValueError):make().admit(data)
        for key,value in [('Source','/opt/tdf/production/assets'),('Type','volume'),('RW',False)]:
            data=container();data['Mounts'][0][key]=value
            with self.subTest(key=key),self.assertRaises(ValueError):make().admit(data)
        data=container();data['Mounts'].append(copy.deepcopy(data['Mounts'][0]))
        with self.assertRaises(ValueError):make().admit(data)

    def test_creation_clears_image_environment_and_excludes_identity_override(self):
        command=make().command();self.assertIn('--pull=never',command)
        self.assertIn('--network=container:'+DB,command)
        self.assertEqual(command[-len(canary.COMMAND):],canary.COMMAND)
        self.assertEqual(canary.COMMAND[:2],['env','-i'])
        for bad in ['--env-file','--publish','--privileged',SOURCE]:self.assertNotIn(bad,command)
        for key in ['GIT_SHA','SOURCE_COMMIT','PAYPAL_CLIENT_SECRET','SMTP_PASSWORD','DATABASE_URL']:
            self.assertNotIn(key,canary.ENVIRONMENT)

    def test_physical_database_requires_dependency_registration_before_docker(self):
        subject=make()
        subject.database.require_application_owner=lambda app: (_ for _ in ()).throw(ValueError('Unregistered application'))
        with patch.object(subject,'execute') as execute,patch.object(subject,'inspect_database') as inspect:
            with self.assertRaisesRegex(ValueError,'Unregistered application'):subject.prepare()
        execute.assert_not_called();inspect.assert_not_called()

    def test_restored_content_input_is_not_a_mutable_caller_alias(self):
        manifests = {'assets': {'sentinel': ['original']}, 'uploads': {}}
        subject = canary.Canary(make().restore, make().database, DIRECTORY, IMAGE, REV,
                                restored_content=manifests)
        manifests['assets']['sentinel'].append('changed')
        self.assertEqual(subject.restored_content['assets']['sentinel'], ['original'])

    def test_database_network_must_still_be_disconnected(self):
        subject=make();data={'NetworkSettings':{'Networks':{'none':{}}},'State':{'Running':True,'Pid':123}}
        with patch.object(subject,'execute',return_value=json.dumps([data])):subject.inspect_database()
        data['NetworkSettings']['Networks']['production']={}
        with patch.object(subject,'execute',return_value=json.dumps([data])):
            with self.assertRaises(ValueError):subject.inspect_database()

    def test_lost_creation_response_removes_only_admitted_owned_container(self):
        subject=make();subject.creation_attempted=True
        with patch.object(subject,'execute',return_value=json.dumps([container()])) as run:
            subject.cleanup()
            self.assertEqual(run.call_args.args[0],['rm','--force',APP])
            self.assertFalse(subject.creation_attempted)
        subject=make();subject.creation_attempted=True;data=container();data['Id']=SOURCE
        with patch.object(subject,'execute',return_value=json.dumps([data])) as run:
            with self.assertRaises(ValueError):subject.cleanup()
            self.assertEqual(run.call_count,1)
            self.assertTrue(subject.creation_attempted)

    def test_probe_only_allows_metadata_paths(self):
        with self.assertRaises(ValueError):make().probe('/parties')

    def test_private_docker_errors_never_escape(self):
        subject=make()
        with patch.object(subprocess,'run',return_value=SimpleNamespace(returncode=1,stdout='SYNTHETIC_SECRET')):
            with self.assertRaisesRegex(ValueError,'^Isolated application canary boundary rejected$'):
                subject.execute(['inspect',APP])

    def test_received_invalid_pause_response_never_becomes_success(self):
        for code in [None,200,302,500,503]:
            with self.subTest(code=code),ExitStack() as stack:
                subject=make()
                for method in ['prepare','inspect','inspect_database']:
                    stack.enter_context(patch.object(subject,method))
                ready=stack.enter_context(patch.object(subject,'await_ready'))
                def execute(args,**_):
                    if args[0]=='create':return APP
                    if 'cat' in args:return REV
                    if 'sha256sum' in args:return 'a'*64+'  /app/tdf-hq-exe'
                    return ''
                run=stack.enter_context(patch.object(subject,'execute',side_effect=execute))
                stack.enter_context(patch.object(subject,'probe',side_effect=[
                    {'code':200,'valid':True,'body':{'commit':REV}},
                    {'code':code,'valid':False,'transportUnavailable':False,'body':{},'cacheControl':None}]))
                with self.assertRaises(ValueError):subject.run()
                ready.assert_called_once()
                self.assertFalse(subject.paused)
                self.assertIn(['unpause',DB],[call.args[0] for call in run.call_args_list])

    def test_real_refused_connection_is_distinct_from_protocol_failure(self):
        with socket.socket() as listener:
            listener.bind(('127.0.0.1',0))  # Reserve the port without listening.
            program=canary.PROBE.replace('http://127.0.0.1:8080','http://127.0.0.1:'+str(listener.getsockname()[1]))
            result=subprocess.run([sys.executable,'-c',program,'/health'],capture_output=True,text=True,timeout=10)
            self.assertEqual(result.returncode,0,result.stderr)
            value=json.loads(result.stdout)
            self.assertIsNone(value['code']);self.assertFalse(value['valid']);self.assertTrue(value['transportUnavailable'])

    def test_malformed_http_status_is_not_transport_unavailability(self):
        class Handler(socketserver.BaseRequestHandler):
            def handle(self):
                self.request.recv(4096)
                self.request.sendall(b'THIS IS NOT AN HTTP STATUS\r\n\r\n')
        with socketserver.TCPServer(('127.0.0.1',0),Handler) as server:
            thread=threading.Thread(target=server.serve_forever);thread.start()
            try:
                program=canary.PROBE.replace('http://127.0.0.1:8080','http://127.0.0.1:'+str(server.server_address[1]))
                result=subprocess.run([sys.executable,'-c',program,'/health'],capture_output=True,text=True,timeout=10)
                self.assertEqual(result.returncode,0,result.stderr)
                value=json.loads(result.stdout)
                self.assertIsNone(value['code']);self.assertFalse(value['valid']);self.assertFalse(value['transportUnavailable'])
            finally:server.shutdown();thread.join()

    def test_real_http_probe_success_unavailable_redirect_and_private_body(self):
        mode={}
        class Handler(BaseHTTPRequestHandler):
            def log_message(self,*_):pass
            def do_GET(self):
                self.send_response(mode['status']);self.send_header('Cache-Control','no-store')
                if mode['status']==302:self.send_header('Location','http://example.invalid/private')
                self.end_headers();self.wfile.write(mode['body'])
        server=ThreadingHTTPServer(('127.0.0.1',0),Handler)
        thread=threading.Thread(target=server.serve_forever);thread.start()
        try:
            program=canary.PROBE.replace('http://127.0.0.1:8080','http://127.0.0.1:'+str(server.server_port))
            for code,body,valid in [(200,b'{"status":"ok","db":"ok"}',True),
                (503,b'{"status":"degraded","db":"unavailable"}',True),
                (200,b'{"private":"SYNTHETIC_SECRET"}',False),(302,b'not json',False),
                (200,b'x'*65537,False)]:
                mode.update(status=code,body=body)
                result=subprocess.run([sys.executable,'-c',program,'/health'],capture_output=True,text=True,timeout=10,
                    env={'HTTP_PROXY':'http://example.invalid:1','HTTPS_PROXY':'http://example.invalid:1'})
                self.assertEqual(result.returncode,0,result.stderr)
                self.assertEqual(json.loads(result.stdout)['code'],code)
                self.assertEqual(json.loads(result.stdout)['valid'],valid)
                self.assertFalse(json.loads(result.stdout)['transportUnavailable'])
                self.assertNotIn('SYNTHETIC_SECRET',result.stdout+result.stderr)
        finally:server.shutdown();server.server_close();thread.join()


if __name__=='__main__':unittest.main()
